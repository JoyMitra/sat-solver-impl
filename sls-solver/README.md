# An implementation of a Stochastic Local SAT solver

The implementation based on the WalkSAT algorithm

# Instructions to build and execute run

1. `$ ghc --make solver.hs`
2. `$ ./solver <cnf file> <max flips> <max tries> <noise>` where noise is between 0 and 1
3. The solver will terminate after 10 minutes if a result is not found. To change the timeout edit the *timeout* parameter in *main* of *solver.hs*

The output will be in [this](http://www.satcompetition.org/2009/format-solvers2009.html) format. Following is an example output:

s SATISFIABLE

v 4 -2 -1 -3

c Done with time 0.001s
