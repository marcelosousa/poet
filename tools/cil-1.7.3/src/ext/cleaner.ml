open Pretty
open Cil
open Ciltools

module IH = Inthash
module StringSet = Set.Make(String)

(* Created by Nathan Cooprider at the University of Utah for CIL *)
(* coop@cs.utah.edu *)

(* A bunch of inefficient but effective procedures and visitors for cleaning
 * up the CIL code. *)

(* Contents:
 *  - dumpVolatile and returnVolatile : removes volatile attributes from
 *    variables for an analysis and then adds them back in afterwards
 *  - push_down_functions : takes all the fundecs of a file and pushes them 
 *    to the bottom of the file
 *  - copyPropagation : a visitor which performs a conservative copy 
 *    propagation on the file
 *  - removeUnusedVars : goes through and removes unreferenced vars. This can
 *    change function signatures. It is also implemented very stupidly, just 
 *    looping until nothing changes in the file.
 *  - removeFalseIfs : takes branching statements which are unnecessary and 
 *    fixes them to be non-branching.
 *  - removeBrackets : get rid of "blocks" that are unnecessary
 *  - removeGotos : get rid of gotos to the next statement
 *  - removeLabels : get rid of unreferenced labels
 *  - derefAddrOf : get rid of "*(&x)" code
 *  - updateVAddrOf : updates the vaddrof field of varinfos 
 * 
 *  - clean_up : does all of the above, plus Cil.compactStmts and 
 *    Rmtmps.removeUnusedTemps, all in a loop. This is another source of 
 *    inefficiency. All the cleaning stages are performed repeatably until 
 *    none of them change the program any more. This also resets the vids
 *    and the sids. MAKE SURE YOU SPECIFY ALL YOUR ENTRY POINTS!
 *  - clean_fcn : this is a function level version of clean up. It was written 
 *    for the smart inliner.
 *)

(* There is more to come! I am actively working on this file as of: *)
(* Sept 27, 2006 *)

(* ***************************************************************************)

let removed_volatiles = ref []

let global_entry_points = ref (StringSet.add "main" StringSet.empty)

class dumpVolatile = object (self)
  inherit nopCilVisitor 
  method vvdec v =
    if ((is_volatile_vi v) && (not (hasAttribute "asm" v.vattr))) then begin
      removed_volatiles := v :: !removed_volatiles;
      v.vtype <- typeRemoveAttributes ["volatile"] v.vtype;
      DoChildren
    end else 
      match v.vtype with
	TArray(t,eo,a) when ((is_volatile_tp t) && 
			     (not (hasAttribute "asm" v.vattr))) 
	-> 
	  removed_volatiles := v :: !removed_volatiles;
	  v.vtype <- TArray (typeRemoveAttributes ["volatile"] t, eo, a);
	  DoChildren
      | _ -> DoChildren
end
let remove_volatile f =
  ignore (visitCilFileSameGlobals (new dumpVolatile) f)
let return_volatile f =
  List.iter 
    (fun v -> 
      match v.vtype with 
	TArray (t,eo,a) -> 
	  v.vtype <- 
	    TArray (typeAddAttributes [Attr("volatile",[])] t, eo, a)
      | _ -> 
	  v.vtype <- typeAddAttributes [Attr("volatile",[])] v.vtype
    ) !removed_volatiles
    
(* ***************************************************************************)
(* Procedure to take all of the fundecs and move them to the bottom of the 
 * file. This makes global constant propagation possible *)

(* Blast module does not take advantage of this (but we never use it) *)
(* copy propagation of globals still not done *)

let push_down_functions f = 
  let nofds, fds = foldGlobals f  
      (fun (nfa, fa) g ->
	match g with 
	  GFun _ -> nfa, fa @ [g]
	| _ -> nfa @ [g] , fa
      ) ([],[])
  in
  f.globals <- nofds @ fds

(* ***************************************************************************)

(* tells if we are modeling the tos interrupt *)
let tos_interrupts = ref false

(* Specific to AVR motes *)
(* COPIED FROM CONCURRENCY TO MAKE LIFE EASIER. IN OTHER WORDS: UGLY *)
let accessing_sreg e =
  match e with
    CastE(TPtr (tp,_),
	  Const(CInt64(x,
		       IInt,
		       None))) 
    when (Int64.to_int x = 95) && (Ciltools.is_volatile_tp tp) -> true
  | _ -> false

class hasVolatile flag = object (self)
  inherit nopCilVisitor   
  method vlval l = (* This finds them *)
    let tp = typeOfLval l in
    (* ignore (Pretty.printf "%a <- %a@!" d_plainlval l d_plaintype tp); *)
    if (is_volatile_tp tp) then flag := true;
    DoChildren
  (* This does not *)
  (* 
   * method vattr (Attr(name,params)) = 
   *   if (name = "volatile") then flag := true;
   *   DoChildren
   *)
  method vexpr e =
    DoChildren
end

let exp_has_volatile e = 
  let flag = ref false in
  ignore (visitCilExpr (new hasVolatile flag) e);
  !flag

(* Simple, conservative, intraprocedural copy propagation ****************)

class expOKvisitor flag vid= object (self)
  inherit nopCilVisitor
  method vexpr e =
    match e with 
      AddrOf (Var _ , _ ) ->
	SkipChildren
    | _ ->
	DoChildren
  method vvrbl v =
    if ((v.vid = vid) || (v.vglob) || (v.vaddrof)) then begin
      flag := false;
      SkipChildren
    end else
      DoChildren
  method vlval l = 
    match l with 
      Var v, _ -> 
	let tp = typeOfLval l in
	if (is_volatile_tp tp) then begin
	  flag := false;
	  SkipChildren
	end else
	  DoChildren
    | Mem _, _ -> 
	flag := false ;
	SkipChildren
end

let exp_ok e vid = 
  let flag = ref true in
  ignore (visitCilExpr (new expOKvisitor flag vid) e);  
  !flag

class expPropagation eh changed = object (self)
  inherit nopCilVisitor as super
  method vexpr e =
    match e with 
      Lval (Var v, NoOffset) when (IH.mem eh v.vid) ->
	let i = IH.find eh v.vid in
	if (exp_ok i v.vid) then begin
	  let i' = (visitCilExpr (new expPropagation eh changed) i) in
	  changed := true ;
	  let type_e = typeOf e 
	  and type_i' = typeOf i' in
	  if (compare type_e type_i' = 0) then
	    ChangeTo (i')
	  else
	    ChangeTo (CastE(type_e, i'))
	end else
	  DoChildren
    | _ ->
	DoChildren
end

class copyPropagation changed = object (self)
  inherit Availexps.aeVisitorClass as super

  method vfunc f = 
   Cfg.clearCFGinfo f;
   ignore (Cfg.cfgFun f);
   Availexps.computeAEs f;
   DoChildren
   
   method vexpr e = 
    match e with
      Lval (Var v,NoOffset) when (not v.vglob) && (not v.vaddrof) && 
	(not (is_volatile_vi v)) -> begin  (* for concurrency? *)
	  let cosa = self#get_cur_eh () in
	  match cosa with 
	    Some(eh) -> 
	      ChangeTo (visitCilExpr (new expPropagation eh changed) e)
	  | _ -> 
	      SkipChildren
	end
    | _ -> DoChildren
end

(* ***************************************************************************)
class resetVariableReferences = object (self)
  inherit nopCilVisitor
  method vvdec v =
    v.vreferenced <- false;
    SkipChildren
end
(* *)
class setAllReferences = object (self)
  inherit nopCilVisitor
  method vvrbl v =
    v.vreferenced <- true;
    SkipChildren
end
let lval_set_referenced l =
  ignore(visitCilLval (new setAllReferences) l)
(* *)    
class setVariableReferences = object (self)
  inherit nopCilVisitor
  val mutable in_exp = false  
  val mutable in_asm = false
  val mutable is_volatile = false

  val mutable current_stmt = dummyStmt

  method vstmt s = 
    current_stmt <- s;
    DoChildren

  method vexpr e = 
    let old_in_exp = in_exp in
    in_exp <- true;
    ChangeDoChildrenPost (e, (fun a -> in_exp <- old_in_exp; a))
  method vinst i = 
    match i with
      Asm _ ->    
	in_asm <- true;
	ChangeDoChildrenPost ([i], (fun a -> in_asm <- false; a))
    | Set (l,e,o) -> 
	if(exp_has_volatile e) then begin
	  match e with 
	    CastE(_,Lval(Mem x,NoOffset)) when 
	      (!tos_interrupts) && (accessing_sreg x) -> ()
	  | _ -> lval_set_referenced l;
	end;
	DoChildren
    | _ ->
	DoChildren
  method vlval l = 
    (*
    ignore(Pretty.printf "LVAL DIES: %a\n STMT DIES: %a\n" d_plainlval l
	  d_stmt current_stmt );
     *)
    let tp = typeOfLval l in
    let old_is_volatile = is_volatile in
    if (is_volatile_tp tp) then
      is_volatile <-true;
    ChangeDoChildrenPost (l, (fun a -> is_volatile <- old_is_volatile ;  a))
  method vvrbl v =
    if ((in_exp) || (in_asm) || (is_volatile)) then v.vreferenced <- true;
    SkipChildren
end
(* *)
class removeUnreferenced globals flag = object (self)
  inherit nopCilVisitor
  method vinst i =
    match i with 
      Set((Var v,o),e,c) when (not v.vreferenced) && 
	((not v.vglob) || globals) -> 
	flag := true;
	ChangeTo ([])
    | Call((Some (Var v,o)),e,el,c) when (not v.vreferenced) && 
	((not v.vglob) || globals) ->
	flag := true;
	ChangeTo ([Call (None,e,el,c)])
    | _ ->
	DoChildren
end
(* ***************************************************************************)
(** An optimization would be to do all the calls at once **)
class argumentRemover fd del_list = object (self) 
(* called from formal remover *)
  inherit nopCilVisitor 
  method vinst i = 
    match i with 
      Call (lo,e,el,l) -> begin
	match e with 
	  Lval (Var v, NoOffset) when (fd.svar.vid = v.vid) ->
	    let count = ref 0 in
	    let el' = 
	      List.fold_right
		(fun arg arg_list ->
		  count := !count + 1;
		  if (List.mem !count del_list) then begin
		    arg_list
		  end else
		    arg :: arg_list)
		el
		[]
	    in
	    ChangeTo [Call(lo,e,el',l)]
	| _ -> (* Function pointers are ignored *)
	    SkipChildren   
      end
    | _ -> SkipChildren
end    
class formalRemover flag file = object (self)
  inherit nopCilVisitor
  method vfunc f = 
    if (not f.svar.vaddrof) then begin
      let to_delete = ref [] in
      let count = ref 0 in
      f.sformals <- List.fold_right
	  (fun formal formal_list -> 
	    count := !count + 1;
	    if (not formal.vreferenced) then begin
	      (* then we can drop it *)
	      (* plus you have to go through the file *)
	      (* and delete the argument *)
	      to_delete := !to_delete @ [!count];
	      flag := true;
	      formal_list
	    end else
	      formal :: formal_list)
	  f.sformals
	  [];
      let fix_all_args = new argumentRemover f !to_delete in
      visitCilFileSameGlobals fix_all_args file; 
      SkipChildren
    end else
      SkipChildren 
end
(* ***************************************************************************)

let output_check s =
  print_string "     ";
  print_float (Sys.time ());
  print_endline s

let removeUnusedVars remove_formals f changed = 
  let flag = ref true in 
  let count = ref 0 in
  while !flag do 
    count := !count+1;
    output_check ("     * (" ^ (string_of_int !count) ^ ")");
    flag := false;
    (* First we reset the variable references *)
    let resetVisitor = new resetVariableReferences in
    (* Then we set them *)
    let setVisitor = new setVariableReferences in
    (* Then we remove anything that wasn't referenced . . . *)
    let removeVisitor = new removeUnreferenced true flag in
    (* . . . including function arguments and parameters *)
    let callVisitor = new formalRemover flag f in
    visitCilFileSameGlobals resetVisitor f;
    visitCilFileSameGlobals setVisitor f; (* BUG *)
    visitCilFile removeVisitor f;
    if (remove_formals) then
      visitCilFileSameGlobals callVisitor f
    else
      ()
	(* We loop until no further change is made (not that fast!) *)
  done;
  if (!count > 1) then changed:=true else () 
      
(* ***************************************************************************)
class hasLabels flag = object (self)
  inherit nopCilVisitor     
      (* could possibly short-circuit this more *)
  method vstmt s = 
    match s.labels with
      [] ->
	if !flag = true then 
	  SkipChildren
	else
	  DoChildren
    | hd :: tl -> 
	flag := true;
	SkipChildren
end

class removeFalseIfs changed = object (self)
  inherit nopCilVisitor
      (** Remove if(0) if(!0) if(1) if(x){} **)
      
  method vstmt s = 
    match s.skind with 
      If(e,b1,b2,l) -> begin
	(* ignore(Pretty.printf "IF STATEMENT\n%a\n" d_stmt s); *)
	if ((b1.bstmts = []) && (b2.bstmts = []) && (not (exp_has_volatile e)))
	then begin
	  changed := true;
	  s.skind <- Instr([]);
	  DoChildren
	end
	else
	  let flag = ref false in
	  match isInteger e with
	    None -> DoChildren
	  | Some x when x = Int64.zero -> 
	      let labelVisitor = new hasLabels flag in 
	      ignore (visitCilStmt labelVisitor (mkStmt (Block(b1))));
	      if not !flag then begin
		changed := true;
		s.skind <- Block (b2); 
	      end;
	      DoChildren
	  | Some _ -> 
	      let labelVisitor = new hasLabels flag in 
	      ignore (visitCilStmt labelVisitor (mkStmt (Block(b2))));
	      if not !flag then begin
		changed := true;
		s.skind <- Block (b1); 
	      end;
	      DoChildren
      end
    | Loop ( { battrs=[] ; 
	       bstmts = { labels=[] ; skind=Break _ }
	       :: [] } ,_,_,_) when s.labels = [] -> 
		 changed := true;
		 s.skind <- Instr([]);
		 DoChildren
    | _ -> 
	DoChildren
	  
end
(* ***************************************************************************)
(* Lifts child blocks into parents if the block has no attributes or labels *)
let rec fold_blocks b changed =
    b.bstmts <- List.fold_right
	(fun s acc -> 
	  match s.skind with
	    Block ib -> 
	      fold_blocks ib changed;
	      if (List.length ib.battrs = 0 && 
		  List.length s.labels = 0) then begin
		    changed := true;
		    ib.bstmts @ acc
		  end else
		s::acc
	  | Instr il when il = [] && s.labels = [] -> 
	      changed := true;
	      acc
	  | _ -> s::acc)
	b.bstmts
	[]

class removeBrackets changed = object (self)
  inherit nopCilVisitor
  method vblock b =
    fold_blocks b changed ;
    DoChildren
end

(* ***************************************************************************)
class removeGotos changed = object (self)
    inherit nopCilVisitor
  method vblock b = 
    let new_bstmts = ref [] in
    let num_of_stmts = List.length b.bstmts in 
    for i = 0 to num_of_stmts - 2 do
      let current = List.nth b.bstmts i in
      let next = List.nth b.bstmts (i+1) in
      match current.skind with 
	Goto (rs,l) when (compare !rs next = 0) -> 
	  changed := true;
	  if (List.length current.labels = 0) then 
	    ()
	  else begin
	    current.skind <- Instr([]);
	    new_bstmts := !new_bstmts @ [current]
	  end
      | If (e, b1, b2, l) ->
	  let do_branch x = 
	    try
	      let rv = List.rev x.bstmts in
	      let h = List.hd rv in
	      let t = List.tl rv in
	      match h.skind with 
		Goto (rs,l) when (compare !rs next = 0) ->
		  changed := true;
		  if (List.length h.labels != 0) then begin
		    h.skind <- Instr([]);
		    mkBlock (List.rev (h :: t))
		  end else 
		    mkBlock (List.rev t)
	      | _ -> x
	    with Failure "hd" -> x
	  in
	  current.skind <- If (e, do_branch b1, do_branch b2, l);
	  new_bstmts := !new_bstmts @ [current]
      | _ -> new_bstmts := !new_bstmts @ [current]
    done;
    if (num_of_stmts > 1) then 
      b.bstmts <- !new_bstmts @ [List.nth b.bstmts (num_of_stmts-1)];
    DoChildren
end
(* ***************************************************************************)

class removeLabels changed = object (self)
  inherit nopCilVisitor
  val mutable referenced = []
  val mutable labels = []
      
  method vfunc f =
    referenced <- [];
    labels <- [];
    ChangeDoChildrenPost (f, 
			  (fun x -> 
			    List.iter 
			      (fun s ->
				if (not (List.mem s referenced)) then begin
				  s.labels <- [];
				  changed := true
				end ) labels;
			    x))
      
  method vstmt s =
    begin
      match s.skind with 
	Goto (dest, _) -> referenced <- !dest :: referenced 
      | Switch (_,_,dl,_) -> referenced <- dl @ referenced
      | _ -> ()
    end;
    if (s.labels <> []) then labels <- s::labels;
    DoChildren

end

(* ***************************************************************************)
(* New CIL STUFF CLASS *******************************************************)
class newCilStuffClass : cilVisitor = object(self)
  inherit nopCilVisitor
  method vfunc fd =
    Cfg.clearCFGinfo fd;
    ignore (Cfg.cfgFun fd);
    let fd' = Deadcodeelim.elim_dead_code fd in
    ChangeTo(fd')
end
(* End of new CIL STUFF CLASS ************************************************)
class derefAddrOf changed  = object(self)
  inherit nopCilVisitor
  method vlval l =
    match l with 
      Mem (AddrOf lv), off -> 
	changed := true;
	ChangeDoChildrenPost (addOffsetLval off lv, (fun x -> x))
    | _ -> 
	DoChildren
end

let sequencing_peephole = ref true

(* Reset vaddrof labels ******************************************************)
class resetVAddrOf = object(self)
  inherit nopCilVisitor
  method vvdec v =
    v.vaddrof <- false;
    SkipChildren
end

(* EXTERNAL FUNCTIONS DON'T COUNT! *)
class setVAddrOf = object(self)
  inherit nopCilVisitor 
  val mutable current_stmt = -1
  method vstmt s = 
    current_stmt <- s.sid;
    DoChildren 
  method vexpr e =
    match e with 
      AddrOf (Var v,_) | StartOf (Var v,_) -> 
	v.vaddrof <- true; 
	DoChildren 
    | _ -> DoChildren
end
let updateVAddrOf f =
  visitCilFileSameGlobals (new resetVAddrOf) f; 
  visitCilFileSameGlobals (new setVAddrOf) f 
(* ***************************************************************************)

(* The main thing! *)
let clean_up remove_formals do_zach tinyos_interrupts entry_points f = 
  output_check " setup";
  tos_interrupts := tinyos_interrupts ;
  Ciltools.globally_unique_vids f;
  Ciltools.globally_unique_sids f;
  let root_filter g = (* must not kill entry points! *)
    match g with 
    | GFun (fd, _) when (StringSet.mem fd.svar.vname entry_points) -> true
    | _ -> false
  in
  
  let changed = ref true in 
  let count = ref 0 in

  while !changed do 
    count := !count+1;
    changed := false;

    (* cesar: constant folding within expressions *)
    output_check (" folding constants" ^ (string_of_int !count));
    ignore (visitCilFileSameGlobals (constFoldVisitor true) f);
    
    if (!sequencing_peephole) then begin
      output_check (" remove unused temps " ^ (string_of_int !count));
      Rmtmps.removeUnusedTemps ~isRoot:root_filter f
    end;
    
    output_check (" compact stmts " ^ (string_of_int !count));
    Cil.iterGlobals f 
      (fun glob -> 
         match glob with
           Cil.GFun(fd,_) -> 
             fd.sbody.bstmts <- Cil.compactStmts fd.sbody.bstmts;
         | _ -> ());
    
    output_check (" updating vaddrof flags " ^ (string_of_int !count));
    updateVAddrOf f;
    
    if (true && (do_zach)) then begin
      output_check (" copy propagation " ^ (string_of_int !count));
      visitCilFile (new copyPropagation changed :> cilVisitor) f; 
      (* 
	 output_check (" dead code elimination " ^ (string_of_int !count));
	 visitCilFile (new newCilStuffClass) f;
       *)
    end;

    output_check (" remove unused vars " ^ (string_of_int !count));
    removeUnusedVars remove_formals f changed;
    
    output_check (" remove useless ifs and loops " ^ (string_of_int !count));
    visitCilFileSameGlobals (new removeFalseIfs changed) f;

    output_check (" dereferencing address-of " ^ (string_of_int !count));
    visitCilFileSameGlobals (new derefAddrOf changed) f;
    
    output_check (" brackets and dummy stmts " ^ (string_of_int !count));
    visitCilFileSameGlobals (new removeBrackets changed) f;
    
    output_check (" remove useless gotos " ^ (string_of_int !count));
    visitCilFileSameGlobals (new removeGotos changed) f;
    
    if (!sequencing_peephole) then begin
      output_check (" remove useless labels " ^ (string_of_int !count));
      visitCilFileSameGlobals (new removeLabels changed) f;
    end;
    
    changed := false; 

  done;
    
  if (!sequencing_peephole) then begin
    output_check " remove unused temps FINAL";
    Rmtmps.removeUnusedTemps ~isRoot:root_filter f
  end

  (* one last time . . . *)
  
(* No iteration, just does everything once *)

let fcnRemoveUnusedVars fcn changed = 
  let old_status = fcn.svar.vreferenced in
  (* First we reset the variable references *)
  let resetVisitor = new resetVariableReferences in
  (* Then we set them *)
  let setVisitor = new setVariableReferences in
  fcn.svar.vreferenced <- old_status;
  (* Then we remove anything that wasn't referenced . . . *)
  let removeVisitor = new removeUnreferenced false changed in
  (* . . . including function arguments and parameters *)
  ignore (visitCilFunction resetVisitor fcn);
  ignore (visitCilFunction setVisitor fcn); 
  visitCilFunction removeVisitor fcn

let clean_fcn fcn = 
  let changed = ref false in
  ignore (visitCilFunction (constFoldVisitor true) fcn);
  if (!sequencing_peephole) then 
    ignore (visitCilFunction (new removeLabels changed) fcn);
  ignore (Cil.compactStmts fcn.sbody.bstmts);
  ignore (visitCilFunction (new copyPropagation changed :> cilVisitor) fcn); 
  ignore (fcnRemoveUnusedVars fcn changed); 
  ignore (visitCilFunction (new removeFalseIfs changed) fcn);
  ignore (visitCilFunction (new derefAddrOf changed) fcn);
  ignore (visitCilFunction (new removeBrackets changed) fcn);
  ignore (visitCilFunction (new removeGotos changed) fcn);
  if (!sequencing_peephole) then 
    ignore (visitCilFunction (new removeLabels changed) fcn);
  !changed
      
let flag = ref false

let feature : featureDescr = 
  { fd_name = "cleaner";
    fd_enabled = flag ;
    fd_description = "Cleans up CIL as much as possible to become normal C.";
    fd_extraopt = [];
    fd_doit = 
    (function (f: file) -> 
      clean_up false true false !global_entry_points f
    );
    
    fd_post_check = true;
  } 

