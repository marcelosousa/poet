# POET (Partial Order Exploration Tools)

**POET** (Partial Order Exploration Tools) is a 
set of state-of-the-art exploration techniques
that use partial orders to compactly
represent the state space of programs.

Partial order representations are particularly
relevant for exploration of concurrent systems, 
the main motivation of this project.

## Wiki

For stable versions and to reproduce experimental results in research papers please consult [POET's wiki](https://github.com/marcelosousa/poet/wiki)

## Installation (@master)

1. Install the [Haskell Platform](https://www.haskell.org/platform/).  
   Make sure you can run the commands *ghc*, *cabal*, and *happy*.
   
2. Install the [simple-c package](https://github.com/marcelosousa/simplec)
 
3. Install poet:
   git clone git@github.com:marcelosousa/poet.git
   cd poet
   cabal install
  
This will create the executable **poet** @ dist/build/poet/

Note that it is likely that master is currently unstable. 

## More info
  
  There are several available modes:
   1. poet frontend - prints the result of the front-end
   2. poet execute  - executes the system with a non-deterministic schedule
   3. poet explore  - invokes the explicit-state model checker
   
  For more information, poet --help.
  
Note:
 Due to a difference in the Haskell library for command arguments, 
 it may be the case for some operating systems it is required to prefix
 the input file with -i=.

Also check the repository [con-benchmarks](https://github.com/marcelosousa/con-benchmarks) for a collection of benchmarks used during the development of **POET**.

### Contact
Marcelo Sousa (msousa at cs dot ox dot ac dot uk). 
University of Oxford, UK.  
