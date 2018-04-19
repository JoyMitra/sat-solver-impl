# An implementation of a randomized SAT solver

The implementation randomly assigns TRUE to exactly 1 literal in every clause to build an assignment. If the assignment satisfies the formula then the algorithm terminates with a model else the algorithm repeats the process n times where n is set by the user.

# Instructions to build and execute run

1. `$ ghc --make solver.hs`
2. `$ ./solver /path/to/file-in-DIMACS MAX-TRIES`
3. The solver will terminate after 10 minutes if a result is not found. To change the timeout edit the *timeout* parameter in *main* of *solver.hs*

The output will be in [this](http://www.satcompetition.org/2009/format-solvers2009.html) format. Following is an example output:

s SATISFIABLE

v 4 -2 -1 -3

c Done with time 0.001s
