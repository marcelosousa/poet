poet - v0.1@03'2015
======

Contact: Marcelo Sousa (msousa at cs dot ox dot ac dot uk). University of Oxford, UK.  
Overview
-------------------------------------------------

POET (Partial Order Exploration Tools) is a 
set of state-of-the-art exploration techniques
that use partial orders to compactly
represent the state space of programs.

Partial order representations are particularly
relevant for exploration of concurrent systems, 
the main motivation of this project.

The two main techniques implemented are:
 - Unfoldings:
   Inspired by Petri Net unfoldings, the goal
   is to efficiently explore a partial order 
   semantics known as prime event structures.
 - Partial Order Reduction:
   Based on Abdulla et al. ODPOR@POPL'14.

Installation Notes
-------------------

1. Install the Haskell Platform (https://www.haskell.org/platform/).  
  Make sure you can run *ghc* and *cabal*
   
2. Install the simple-c package:  
   git clone git@github.com:marcelosousa/simplec.git  
   cd simplec  
   cabal install  
3. Install poet:  
   git clone git@github.com:marcelosousa/poet.git  
   cd poet  
   cabal install  
  
This will create an executable *poet* at dist/build/poet/

Running Poet
-------------
  
  So far, only the frontend mode is enabled that can be executed with the following command:  
    *poet frontend file.c*

  Example: $ poet frontend /tmp/t.c      
    ORIGINAL PROGRAM  
    TRANSFORMED PROGRAM  
    
  The first program (ORIGINAL) is an ugly pretty-print from the initial simplec AST.  
  The second program (TRANSFORMER) is the simplec program that will be passed to the converter
  that will generate an instance of the model of computation.

TODO
----

1. Testcases for stateful vs stateless
2. Testcases where cutoffs are beneficial 
3. Print sorted states of maximal configurations to check that stateful and stateless have the same result
4. Profile to find out why stateful is taking longer than stateless

Message of experiments
----------------------
1. Stateful is better than stateless if the unfolding can fit in memory
2. Cutoffs can result in exponential savings
