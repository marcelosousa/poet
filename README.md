poet - v0.2@09'2015
======

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
  Make sure you can run the commands *ghc*, *cabal*, and *happy*.
   
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
  
  There are several available modes:
   1. poet frontend - prints the result of the front-end
   2. poet execute  - executes the system with a non-deterministic schedule
   3. poet explore  - invokes the explicit-state model checker
   
  For more information, poet --help.
  
Note:
 Due to a difference in the Haskell library for command arguments, 
 it may be the case for some operating systems it is required to prefix
 the input file with -i=.
 
