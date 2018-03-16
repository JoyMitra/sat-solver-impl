# An implementation of a SAT solver

The key idea behind this algorithm is to find a clause in the formula is unsatisfiable. If  such a clause exists then the formula is unsatisfiable else the formula is satisfiable.

The algorithm takes a formula in CNF form as input and assigns a true value to the first literal of the first clause in the formula. It then removes all the clauses in the formula that contains the literal. This process is repeated till all the clauses in the formula are removed (in which case the algorithm will terminate and return with a satisfiable assignment) or a conflict occurs. A conflict occurs when the algorithm assigns a variable v to true when (-v) is already set to true. When a conflict occurs, the algorithm backtracks to the state where (-v) was set to true. At this state it chooses the next literal in the clause that will not cause a conflict. If such a literal is found then the literal is assigned to true and the clause removal  process continues. If such a literal is not found then all assignments that result in a conflict are found and the algorithm backtracks to each of the states where the assignments were made. At each of those states the next literal that does not result is a conflict is picked and the clause removal process continues. The algorithm terminates with an assignment whenever all clauses from the formula are removed or it returns with an empty assignment if all literals in the first clause of the formula have been set to true and for each of the literal assignments a conflict has been detected.

# instructions to run

1. `$ ./solver /path/to/file-in-DIMACS`

The output will be in [this](http://www.satcompetition.org/2009/format-solvers2009.html) format. Following is an example output:

s SATISFIABLE
v 4 -2 -1 -3
c Done with time 0.001s
