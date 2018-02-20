A random SAT generator that generates CNFs in [DIMACS format](http://www.satcompetition.org/2009/format-benchmarks2009.html).

# How to compile and execute

1. Compile the source code for the random SAT generator

    `$ ghc --make ./random.hs`

2. You can run the compiled program in the following two ways:

    `$ ./random f n k l` or

    `$ ./random f n k l "strict"`   where,

    1. f : name of the file where the CNF will be saved
    2. n : Number of non-empty clauses that should occur in a formula
    3. k : Maximum number of variables that can occur in a formula
    4. l : Maximum size of each clause
    5. "strict": Every clause should have l literals
