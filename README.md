Marcelo Sousa (msousa at cs dot ox dot ac dot uk)
-------------------------------------------------
University of Oxford, UK

poet - v0.1@03'2015
======

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
