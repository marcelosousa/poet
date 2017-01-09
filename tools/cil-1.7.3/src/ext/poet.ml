
open Cil
open Liveness
open Printf
open Pretty
(*
open Inliner
open Rmtmps
open Partial
*)
open Callgraph
module H = Hashtbl
module IH = Inthash
module E = Errormsg

let do_poet = ref false
let do_prepoet = ref false
let do_cesar = ref false
let do_rmunreach = ref false

let rmunreach_fun = ref "main"

let typ_to_string t =
   sprint ~width:80 (d_type () t)
;;

let exp_to_string (e: exp) : string =
   sprint ~width:80 (d_exp () e)

let inst_to_string (i: instr) : string =
   let argl_to_str l =
      let str = ref "" in
      ignore (List.fold_left
         (fun fst e ->
            if not fst then str := !str ^ ",";
            str := !str ^ " " ^ (exp_to_string e);
            false)
         true
         l);
      !str
   in
   match i with
   | Set (lval, e, _) ->
      sprintf "%s = %s;" (exp_to_string (Lval lval)) (exp_to_string e)

   | Call (Some lval, f, argl, _) ->
      sprintf "%s = %s (%s);"
         (exp_to_string (Lval lval))
         (exp_to_string f)
         (argl_to_str argl)

   | Call (None, f, argl, _) ->
      sprintf "%s (%s);"
         (exp_to_string f)
         (argl_to_str argl)

   | Asm (_) ->
      "__asm__ (\"...\");"
;;

let stmt_to_string (s: stmt) : string =
   match s.skind with
   | Instr _ -> "Instr"
   | Return (Some e, _) -> sprintf "return %s" (exp_to_string e)
   | Return (None, _) -> sprintf "return (void)"
   | Goto _ -> "goto"
   | ComputedGoto (e, _) -> sprintf "goto *(%s)" (exp_to_string e)
   | Break _ -> "break"
   | Continue _ -> "continue"
   | If (e, _, _, _) -> sprintf "if (%s)" (exp_to_string e)
   | Switch (e, _, _, _) -> sprintf "switch (%s)" (exp_to_string e)
   | Loop _ -> "while (1)"
   | Block _ -> "Block"
   | TryFinally _ -> "TryFinally"
   | TryExcept _ ->  "TryExcept"
;;

let print_global = function
   | GType (_, {line = l; file = f}) ->
      E.log "** Typedef, %s:%d\n" f l;
   | GCompTag (_, {line = l; file = f}) ->
      E.log "** Struct or Union definition, %s:%d\n" f l;
   | GCompTagDecl (_, {line = l; file = f}) ->
      E.log "** Struct or Union declaration, %s:%d\n" f l;
   | GEnumTag (_, {line = l; file = f}) ->
      E.log "** Enum definition, %s:%d\n" f l;
   | GEnumTagDecl (_, {line = l; file = f}) ->
      E.log "** Enum declaration, %s:%d\n" f l;
   | GVarDecl (info, {line = l; file = f}) ->
      E.log "** Variable declaration, %s:%d (GVarDecl):\n" f l;
      E.log " name '%s'\n" info.vname;
      E.log " type '%s'\n" (typ_to_string info.vtype);
   | GVar (info, _, {line = l; file = f}) ->
      E.log "** Variable definition or Function declaration, %s:%d (GVar):\n" f l;
      E.log " name '%s'\n" info.vname;
      E.log " type '%s'\n" (typ_to_string info.vtype);
      E.log " id   '%d'\n" info.vid;
      E.log " refe '%B'\n" info.vreferenced;
   | GFun (fundec, {line = l; file = f}) ->
      E.log "** Function definition, %s:%d (GFun)\n" f l;
      E.log " name '%s'\n" fundec.svar.vname;
      E.log " type '%s'\n" (typ_to_string fundec.svar.vtype);
      E.log " id   '%d'\n" fundec.svar.vid;
      E.log " refe '%B'\n" fundec.svar.vreferenced;
   | GAsm (str, {line = l; file = f}) ->
      E.log "** Assembler definition, %s:%d\n" f l;
      E.log " asm '%s'\n" str
   | GPragma (_, {line = l; file = f}) ->
      E.log "** Pragma, %s:%d\n" f l;
   | GText (str) ->
      E.log "** Text: '%s'\n" str
;;

let report_globals (f: file) =
   let funs = ref [] in
   let vars = ref [] in
   let counter g =
      match g with
      | GVar (info, init, loc) ->
         vars := (info, init, loc) :: !vars
      | GFun (fd, loc) ->
         funs := (fd, loc) :: !funs
      | _ -> ()
   in
   List.iter counter f.globals;
   (!funs, !vars)
;;

let print_globals (f: file) =
   E.log "=============================\n";
   E.log "Printing %d globals:\n" (List.length f.globals);
   List.iter print_global f.globals;
   E.log "=============================\n";
   E.log "Summary: %d globals,\n" (List.length f.globals);
   let funs, vars = report_globals f in
   let l1 = List.length funs in
   let l2 = List.length vars in
   let l3 = (List.length f.globals) - l1 - l2 in
   E.log "  - Functions  : %d\n" l1;
   E.log "  - Global vars: %d\n" l2;
   E.log "  - Others     : %d\n" l3;
   E.log "=============================\n";
;;

(** remove all functions and variables that are "syntactically unreachable" from
    the main function *)
let clean_unreachable_globals (fn: string) (f: file) =
   let is_root = function
   | GFun (fd,_) when fd.svar.vname = fn -> true
   | _ -> false
   in

   E.log "Poet: unreach: removing code syntactically unreachable from '%s'...\n" fn;
   E.log "Poet: unreach: before: %d globals (decl + defs)\n" (List.length f.globals);
   Rmtmps.removeUnusedTemps ~isRoot:is_root f;
   E.log "Poet: unreach: done, now %d globals\n" (List.length f.globals);
;;

let make_vi_to_fundec_hashtbl (f: file) =
   let tab = H.create (List.length f.globals) in
   List.iter
      (fun g -> match g with GFun (fd,_) -> H.add tab fd.svar fd | _ -> ())
      f.globals;
   tab;
;;

let find_fun_by_name (f: file) (n: string) : (fundec * location) option =
   let test = function
      | GFun (fd, _) when fd.svar.vname = n -> true
      | _ -> false
   in
   try
      match List.find test f.globals with
      | GFun (fd, loc) -> Some (fd, loc)
      | _ -> None (* unreachable *)
   with
      Not_found -> None
;;

let rec cg_dfs_find_lasso
      (stack: Callgraph.callnode IH.t)
      (n: Callgraph.callnode)
      (*callback: (varinfo -> unit) option*)
      callback
      : varinfo list =
   let backtrace = ref [] in
   let callee_folder _ (suc: Callgraph.callnode) (skip: bool) : bool =
      if skip then
         true
      else begin
         backtrace := cg_dfs_find_lasso stack suc callback;
         !backtrace <> []
      end
   in
   match n.cnInfo with
   | NIIndirect (_, _) -> (* base case: if this is an indirect node, we stop *)
      []
   | NIVar (vi, _) ->
      (* otherwise we have a direct call to function (vi: varinfo) *)
      (match callback with Some f -> f vi | None -> ());
      (* base case: we are closing the loop *)
      if IH.mem stack n.cnid then
         [vi]
      else begin
         (* otherwise add this node to the stack and scan all function calls
         from here until finding one that calls a another already in the stack *)
         IH.add stack n.cnid n;
         if IH.fold callee_folder n.cnCallees false then
            backtrace := vi :: !backtrace;
         IH.remove stack n.cnid;
         !backtrace
      end
;;

(* this function does a DFS over the call graph starting from the function "fn"
and detects whether a cycle is recursively reachable from "fn"; it returns the
lasso from "fn" if a cycle is found or [] if no cycle is found *)
let cg_has_cycles_from (g: Callgraph.callgraph) (fn: string) : varinfo list =
   try
      cg_dfs_find_lasso (IH.create 31) (H.find g fn) None
   with
      Not_found ->
         E.warn "Poet: callgraph cycle detection: '%s': not a function name" fn;
         []
;;

(* tab[host] is a hash table that maps every function to the number of times it
 * has been (recursively) inlined into "host"
 *
 * This gets updated as follows: whenever you copy the body of "callee" into
 * "host", we increment by 1 the number of copies of "calle" in host, and for
 * every function that already inlined into "calle", we add that number to the
 * corresponding number in "host"
 *)
let _irtab : (string, (string, int ref) H.t) H.t = H.create 301;;

(* this is called everytime callee is inlined into the body of host *)
let inlining_report_callback (host: varinfo) (callee: varinfo) : unit =
   let get_or_empty v =
      try H.find _irtab v
      with
         Not_found ->
            let empty = H.create 301 in
            H.add _irtab v empty;
            empty
   in
   let get_or_zero tab v =
      try H.find tab v
      with
         Not_found ->
            let zero = ref 0 in
            H.add tab v zero;
            zero
   in
   let host_tab   = get_or_empty host.vname in
   let callee_tab = get_or_empty callee.vname in
   let cnt        = get_or_zero  host_tab callee.vname in

   (* one more copy of callee inlined into the host *)
   cnt := !cnt + 1;

   (* for each entry in callee_tab, we increment the corresponding count in
   host_tab *)
   let calle_tab_iter v count_in_callee =
      let count_in_host = get_or_zero host_tab v in
      count_in_host := !count_in_host + !count_in_callee
   in
   H.iter calle_tab_iter callee_tab;
;;

let inlining_report_clean () =
   H.clear _irtab;
;;

let inlining_report_print (f: file) (fn: string) : unit =
   (* folder to transform hash table into list *)
   let htb_list_folder (v: string) (i: int ref) (l: 'a list) : 'a list =
      (v, !i) :: l
   in
   (* print one entry of the hash table *)
   let print_one (v, count) =
      E.log "Poet: inliner: report: nr %-5d fun %s\n" count v
   in
   (* comparison function to sort entries of the table *)
   let int_cmp (v1, i1) (v2, i2) = i2 - i1 in

   (* find the report for function fn *)
   let tab = try H.find _irtab fn with Not_found -> H.create 0 in

   (* transform hash table into list of pairs (string,int), then sort it *)
   let l = H.fold htb_list_folder tab [] in
   let l' =  List.sort int_cmp l in

   (* print it *)
   E.log "Poet: inliner: report: nr of inlined (indirect) calls from '%s'\n" fn;
   E.log "Poet: inliner: report: ===================================\n";
   List.iter print_one l';
   E.log "Poet: inliner: report: ===================================\n";
;;


let cg_make_inlining_topo_sort (g: Callgraph.callgraph) : varinfo list =
   (* the topological sorting, a list of callnode values *)
   let sortedl = ref [] in

   (* a hash table to quikly determine if a function is already in the list
   above: mapping callnode ids (int) to callnodes *)
   let sorted = IH.create 293 in

   (* the list of unsorted nodes, initially all NIVar nodes *)
   let unsorted = ref [] in

   (* if n is already sorted, then 0; if not we scan all function calls from n
   and count the ones that are not already sorted *)
   let nr_unsorted_callees (n: Callgraph.callnode) : int =
      let folder_callees (id: int) (suc: Callgraph.callnode) (count: int) : int =
         match suc.cnInfo with
         | NIVar _ -> count + (if IH.mem sorted id then 0 else 1)
         | NIIndirect _ -> count (* indirect calls not accounted *)
      in
      if IH.mem sorted n.cnid then
         0
      else
         IH.fold folder_callees n.cnCallees 0
   in

   (* iterator for initializing the unsorted list with all NIVar callnodes *)
   let iter_init_unsorted _ (cn : Callgraph.callnode) =
      match cn.cnInfo with
      | NIVar _ -> unsorted := cn :: !unsorted
      | NIIndirect _ -> ()
   in

   (* see below *)
   let rec topo_sort_pass (l: Callgraph.callnode list) =
      match l with
      | [] -> []
      | cn::tail ->
         let x = nr_unsorted_callees cn in
         (* let fn = Callgraph.nodeName cn.cnInfo in
         E.log "Poet: toposort: fun '%s': %d unsorted callees\n" fn x; *)
         if x = 0 then begin
            sortedl := cn :: !sortedl;
            IH.add sorted cn.cnid cn;
            topo_sort_pass tail;
         end else
            cn::(topo_sort_pass tail)
   in

   (* the code: initialize the list of unsorted graph nodes and then iteratively
   remove from the list those which have all its callees already sorted *)
   H.iter iter_init_unsorted g;
   while (List.length !unsorted) >= 1 do
      E.log "Poet: toposort: starting pass: %d sorted, %d unsorted\n"
            (List.length !sortedl) (List.length !unsorted);
      unsorted := topo_sort_pass !unsorted
   done;

   (* extracting the varinfo structures + reversing the list *)
   List.fold_left
      (fun l cn -> match cn.cnInfo with NIVar (vi,_) -> vi::l | NIIndirect (_) -> l)
      [] !sortedl
;;

let fix_proto_order (f: file) =
   (* we record the list of pairs (fundec,location) for all GFun in the file *)
   let bodies = ref [] in

   (* recorded here, during the first pass *)
   let mapper1 = function
   | GFun (fd, loc) -> bodies := (fd,loc)::!bodies; GVarDecl (fd.svar, loc)
   | other -> other
   in

   (* create a function definition, for the second pass *)
   let mapper2 (fd, lo) = GFun (fd, lo) in

   (* 1st pass: amputate the body to all function definitions, leaving only prototype *)
   f.globals <- List.map mapper1 f.globals;

   (* 2nd pass: append all function bodies to the end of the file, in any order :) *)
   f.globals <- f.globals @ (List.map mapper2 !bodies)
;;

let inline_all (f: file) (maxnr: int) : unit =
   let vi_printer n (vi: varinfo) =
      E.log " #%-3d %s\n" n vi.vname;
      n + 1
   in

   E.log "Poet: inliner: computing the syntactic call graph ...\n";
   let g = Callgraph.computeGraph f in
   E.log "Poet: inliner: done, %d nodes\n" (H.length g);
   (* Callgraph.printGraph stderr g; *)
   let lasso: varinfo list = cg_has_cycles_from g "main" in

   if lasso <> [] then begin
      E.log "Poet: inliner: call graph: lasso found (length %d):\n" (List.length lasso);
      E.log "BEGIN\n";
      ignore (List.fold_left vi_printer 1 lasso);
      E.log "END\n";

   end else begin
      E.log "Poet: inliner: call graph: it is acyclic, building a topological sort :)\n";
      let sorting: varinfo list = cg_make_inlining_topo_sort g in
      E.log "Poet: inliner: call graph: topo sort: done, %d nodes:\n" (List.length sorting);
      (*
      E.log "BEGIN\n";
      ignore (List.fold_left vi_printer 1 sorting);
      E.log "END\n\n";
      *)

      (* a hash table mapping varinfos to fundecs for all defined function *)
      let tab: (Cil.varinfo, Cil.fundec) H.t ref = ref (make_vi_to_fundec_hashtbl f) in

      (* a maping giving the body that should replace each function call *)
      let inline_what (vi: varinfo) : fundec option =
         (* we never inline __VERIFIER_* functions *)
         let len = min (String.length vi.vname) 11 in
         if String.sub vi.vname 0 len = "__VERIFIER_" then
            None
         else
            try Some (H.find !tab vi) with Not_found -> None
      in

      (* inlines the function identified by "vi" *)
      let len = List.length sorting in
      let inline_iter (n: int) (vi: varinfo) =
         (* every xx inlined functions, run the cleaner *)
         if n mod 29 = 0 then begin
            Cleaner.feature.fd_doit f;
            tab := make_vi_to_fundec_hashtbl f;
         end;
         try
            if maxnr != 0 && n > maxnr then raise Not_found;
            let body: fundec = H.find !tab vi in
            E.log "Poet: inliner: inlining fun '%s' (%d of %d)\n" vi.vname n len;
            Inliner.doFunctionRepeat body inline_what inlining_report_callback;
            n + 1
         with Not_found -> begin
            E.log "Poet: inliner: skiping fun  '%s' (%d of %d)\n" vi.vname n len;
            n + 1
         end
      in

      (* inline all functions, in the topological order *)
      ignore (List.fold_left inline_iter 1 sorting);

      (* print a summary of the inlining that we just did *)
      inlining_report_print f "main";
      inlining_report_clean ();

      (* the inliner has a bug, it forgets to raise the prototypes of functions
      called in inlined code; fix it here *)
      E.log "Poet: inliner: fixing Inliner's prototype bug...\n";
      fix_proto_order f;
      E.log "Poet: inliner: done\n";
   end
;;

let doit_full_inlining (f: file) = 
   E.log "Poet: doit: starting\n";

   (* remove unused globals *)
   clean_unreachable_globals "main" f;

(*
   (* get a list of pairs (function body, file location) for every defined
   function *)
   let l = List.fold_left
               (fun l' g -> match g with GFun (fd,lo) -> (fd,lo)::l' | _ -> l')
               []
               f.globals

   (* get the "string" names of all defined functions, to inline them *)
   in let l' = List.map (fun (fd,_) -> fd.svar.vname) l in

   E.log "Poet.doit1: found %d function definitions (with body):\n" (List.length l');
   List.iter (fun x -> E.log "- %s\n" x) l';

   (* ask the inliner to inline all defined functions *)
   E.log "Poet.doit1: requesting to inline all of them:\n";
   Inliner.toinline := l';
   Inliner.doit f;
   E.log "Poet.doit1: inlining done\n"; *)

   for i = 0 to 0 do
      E.log "\n";
      E.log "Poet: inliner: xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx\n";
      E.log "Poet: inliner: iteration %d\n" i;
      E.log "Poet: inliner: xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx\n";
      (* inline all functions *)
      inline_all f 0;
      (* cleaning unused globals again *)
      clean_unreachable_globals "main" f;
      (* call the cleaner *)
      Cleaner.feature.fd_doit f;
   done;

   (*
   print_globals f;
   Cil.dumpFile Cil.defaultCilPrinter Pervasives.stdout "stdout" f;
   *)

   let funs, vars = report_globals f in
   let l0 = List.length f.globals in
   let l1 = List.length funs in
   let l2 = List.length vars in
   let l3 = l0 - l1 - l2 in
   E.log "Poet: doit: summary: %d globals,\n" l0;
   E.log "Poet: doit: summary: - Functions  : %d\n" l1;
   E.log "Poet: doit: summary: - Global vars: %d\n" l2;
   E.log "Poet: doit: summary: - Others     : %d\n" l3;
   E.log "Poet: doit: done, returning\n";
;;

let doit_poet (f: file) = 
   E.log "Poet: doit: starting\n";

   (* remove unused globals *)
   clean_unreachable_globals "main" f;

   for i = 0 to 0 do
      E.log "\n";
      E.log "Poet: doit: inliner: xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx\n";
      E.log "Poet: doit: inliner: iteration %d\n" i;
      E.log "Poet: doit: inliner: xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx\n";
      (* inline all functions *)
      inline_all f 0;
      (* cleaning unused globals again *)
      clean_unreachable_globals "main" f;
      (* call the cleaner *)
      Cleaner.feature.fd_doit f;
   done;

   (*
   print_globals f;
   Cil.dumpFile Cil.defaultCilPrinter Pervasives.stdout "stdout" f;
   *)

   let funs, vars = report_globals f in
   let l0 = List.length f.globals in
   let l1 = List.length funs in
   let l2 = List.length vars in
   let l3 = l0 - l1 - l2 in
   E.log "Poet: doit: summary: %d globals,\n" l0;
   E.log "Poet: doit: summary: - Functions  : %d\n" l1;
   E.log "Poet: doit: summary: - Global vars: %d\n" l2;
   E.log "Poet: doit: summary: - Others     : %d\n" l3;
   E.log "Poet: doit: done, returning\n";
;;

let vs_to_string vs = 
   let d: Pretty.doc ref = ref Pretty.nil in
   let folder vi fst =
      if not fst then d := !d ++ text ",";
      d := !d ++ text " " ++ text vi.vname;
      false
   in
   ignore (VS.fold folder vs true);
   sprint ~width:160 !d;
;;

class my_visitor = object
   inherit nopCilVisitor

   val mutable idx = 0

   method vstmt (s: stmt) =
      E.log "Poet: visitor: stmt: id %d xxxxxxxxx\n" s.sid;
      idx <- 0;
      DoChildren

   method vinst (i: instr) =
      E.log "Poet: visitor: - instr: #%-2d %a\n" idx d_instr i;
      idx <- idx + 1;
      SkipChildren
end

class my_liveness_visitor = object
   inherit Liveness.livenessVisitorClass false as super

   method vstmt (s: stmt) =
      ignore(super#vstmt s);
      E.log "Poet: visitor: # %s (id %d)\n" (stmt_to_string s) s.sid;
      let vs = Liveness.getLiveness s in
      E.log "Poet: visitor:   live:%s (len %d)\n"
            (vs_to_string vs) (Liveness.VS.cardinal vs);
      DoChildren

   method vinst (i: instr) =
      ignore(super#vinst i);
      E.log "Poet: visitor: - \"%s\"\n" (inst_to_string i);
      let _,def = Usedef.computeUseDefInstr i in
      (match cur_liv_dat with
      | None ->
         E.log "Poet: visitor:   live: None\n"
      | Some vs ->
         E.log "Poet: visitor:   live:%s (len %d)\n"
               (vs_to_string vs) (Liveness.VS.cardinal vs));
      E.log "Poet: visitor:   def :%s (len %d)\n"
            (vs_to_string def) (Liveness.VS.cardinal def);
      SkipChildren
end

let testit f =

   E.log "Poet: testit: f: %d globals\n" (List.length f.globals);

   (* old way to compute the CFG
   Cil.prepareCFG fd;
   Cil.computeCFGInfo fd true;
   *)

   (* Cfg.computeFileCFG f; --> entire file *)

   clean_unreachable_globals "main" f;
   Cfg.clearFileCFG f;

   E.log "Poet: testit: computing CFG ...\n";
   let fn = "main" in
   let fd,_ =
      match find_fun_by_name f fn with
      | Some (f,l) -> f,l
      | None -> failwith ("Poet: function '" ^ fn ^ "' not found!")
   in

   E.log "Poet: testit: computing CFG for function\n";
   ignore (Cfg.cfgFun fd);

   E.log "Poet: testit: computing variable liveness info for function\n";
   Liveness.computeLiveness fd;
   E.log "Poet: testit: done, %d entries!\n" (IH.length LiveFlow.stmtStartData);

   (* let set = IH.find LiveFlow.stmtStartData 1 in *)

   Liveness.print_everything ();

   (* E.log "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx\n"; *)
   Cfg.printCfgFilename "aha.dot" fd;
   (* E.log "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx\n";*)

   let vis = new my_liveness_visitor in
   E.log "BEGIN VISITOR\n";
   ignore (visitCilFunction vis fd);
   E.log "END VISITOR\n";
;;

let doit_rmunreach f =
   E.log ("Poet: rmunreach: " ^^ 
         "removing symbols syntactically unreachable from function '%s'\n")
         !rmunreach_fun;

   (* remove unused globals *)
   clean_unreachable_globals !rmunreach_fun f;
;;

let feature1 : featureDescr = 
  { fd_name = "fullinline";
    fd_enabled = do_poet;
    fd_description = "full inlining pass";
    fd_extraopt = [];
    fd_doit = doit_full_inlining;
    fd_post_check = true;
  }

let feature2 : featureDescr = 
  { fd_name = "cesar";
    fd_enabled = do_cesar;
    fd_description = "placeholder for testing things";
    fd_extraopt = [];
    fd_doit = testit;
    fd_post_check = true;
  }

let feature3 : featureDescr = 
  { fd_name = "rmunreach";
    fd_enabled = do_rmunreach;
    fd_description = "remove symbols syntactically unreachable from given function name";
    fd_extraopt = [
         ("--rm-unreach-from",
         Arg.String (fun s -> do_rmunreach := true; rmunreach_fun := s),
               "<name> Remove symbols syntactically unreachable from <name> (default 'main')")
    ];
    fd_doit = doit_rmunreach;
    fd_post_check = true;
  }

let feature4 : featureDescr = 
  { fd_name = "poet";
    fd_enabled = do_prepoet;
    fd_description = "Preprocessing transformations for POET action :)";
    fd_extraopt = [];
    fd_doit = doit_poet;
    fd_post_check = true;
  }


(*
 * - remove unused code
 * - inline
 * - clean
 * - propagate
 * - clean again
 *
 *)

(* Ideas to improve:
 *
 * remove globals if defined in same file/line and with = name, with notice
 * clean unnecessary variables, blocks, and gotos
 *)

