# AoC2023-Haskell
My solutions to the Advent of Code 2023 puzzles

### Thoughts
The solutions are run all together in the [Main](src/Main.hs) module,
compiled down to a single optimized executable.
<br>
The answers are obviously specific to the inputs, which is why I made it so that anyone
can build and run the project with their own inputs (although I tested the program only on Linux).

This was my very first ever [**Advent of Code**](https://adventofcode.com/2023)
and I had a lot of conflicting feelings and opinions about it.
<br>
In the end, I'm glad I tried and made it to the end, despite taking a lot of time
and the puzzle solutions not being all original (had some help from **Reddit** [here](https://www.reddit.com/r/adventofcode/)
and [here](https://www.reddit.com/r/haskell/)) or optimized enough to run in less than a second.

The best part was surely learning a lot of new stuff about Haskell and all the different topics that inspired the puzzles,
as well as how to approach problems with a "heavily functional mindset" 😆

### Days
Instead of going through each day and writing any sort of write-up,
I would like to list some of the most interesting and useful concepts I reviewed and studied for the problems
(if you're intrigued, you can always explore the repository and check out the code in detail).

1. [Trebuchet](src/Trebuchet.hs) ->
   [*megaparsec*](https://hackage.haskell.org/package/megaparsec) basics
2. [Cube Conundrum](src/CubeConundrum.hs) ->
   more *megaparsec*,
   [Kleisli arrows](https://hackage.haskell.org/package/base/docs/Control-Arrow.html#t:Kleisli),
   [*containers*](https://hackage.haskell.org/package/containers)
3. [Gear Ratios](src/GearRatios.hs) ->
   [*matrix*](https://hackage.haskell.org/package/matrix),
   [*unordered-containers*](https://hackage.haskell.org/package/unordered-containers),
   more complex *megaparsec* parser combinators
4. [Scratchcards](src/Scratchcards.hs) ->
   even more *megaparsec* exploration
5. [If You Give A Seed A Fertilizer](src/IfYouGiveASeedAFertilizer.hs) ->
   [Functor instances](https://hackage.haskell.org/package/base/docs/Prelude.html#t:Functor),
   ranges calculations
6. [Wait For It](src/WaitForIt.hs) ->
   simple closed form algebraic solutions
7. [Camel Cards](src/CamelCards.hs) ->
   sum types and sorting
8. [Haunted Wasteland](src/HauntedWasteland.hs) ->
   synchronizing cycles LCM
9. [Mirage Maintenance](src/MirageMaintenance.hs) ->
    number successions
10. [Pipe Maze](src/PipeMaze.hs) ->
    more *matrix* operations,
    loop in a "maze" traversal,
    [Pick's theorem](https://en.wikipedia.org/wiki/Pick%27s_theorem),
    [shoelace formula](https://en.wikipedia.org/wiki/Shoelace_formula)
11. [Cosmic Expansion](src/CosmicExpansion.hs) ->
    [Manhattan distance](https://en.wikipedia.org/wiki/Taxicab_geometry),
    [*vector*](https://hackage.haskell.org/package/vector)
12. [Hot Springs](src/HotSprings.hs) ->
    [dynamic programming](https://en.wikipedia.org/wiki/Dynamic_programming),
    [memoization](https://en.wikipedia.org/wiki/Memoization),
    [*array*](https://hackage.haskell.org/package/array),
    [*bytestring*](https://hackage.haskell.org/package/bytestring)
13. [Point Of Incidence](src/PointOfIncidence.hs) ->
    visual matrix partial symmetry
14. [Parabolic Reflector Dish](src/ParabolicReflectorDish.hs) ->
    2D tilting physics simulation,
    modular arithmetic
15. [Lens Library](src/LensLibrary.hs) ->
    silly hashes,
    more *containers* and *bytestring*
16. [The Floor Will Be Lava](src/TheFloorWillBeLava.hs) ->
    [BFS](https://en.wikipedia.org/wiki/Breadth-first_search),
    [*parallel*](https://hackage.haskell.org/package/parallel)
17. [Clumsy Crucible](src/ClumsyCrucible.hs) ->
    [Dijkstra's algorithm](https://en.wikipedia.org/wiki/Dijkstra%27s_algorithm),
    [*search-algorithms*](https://hackage.haskell.org/package/search-algorithms),
    cost functions
18. [Lavaduct Lagoon](src/LavaductLagoon.hs) ->
    Pick's theorem and shoelace formula again
19. [Aplenty](src/Aplenty.hs) ->
    [*lens*](https://hackage.haskell.org/package/lens) attempts,
    more complex parsing and ranges calculations
20. [Pulse Propagation](src/PulsePropagation.hs) ->
    synchronizing cycles LCM again
21. [Step Counter](src/StepCounter.hs) ->
    BFS again,
    [*hmatrix*](https://hackage.haskell.org/package/hmatrix),
    input analysis,
    data [interpolation](https://en.wikipedia.org/wiki/Interpolation),
    [quadratic growth](https://en.wikipedia.org/wiki/Quadratic_growth),
    [linear equations systems](https://en.wikipedia.org/wiki/System_of_linear_equations)
22. [Sand Slabs](src/SandSlabs.hs) ->
    [graph theory](https://en.wikipedia.org/wiki/Graph_theory),
    graph analysis,
    [graphviz](https://graphviz.org/)
23. [A Long Walk](src/ALongWalk.hs) ->
    [longest path problem](https://en.wikipedia.org/wiki/Longest_path_problem),
    types of graphs,
    graph simplification by dead-end edge pruning
24. [Never Tell Me The Odds](src/NeverTellMeTheOdds.hs) ->
    [line-line intersection](https://en.wikipedia.org/wiki/Line%E2%80%93line_intersection),
    [linear optimization](https://en.wikipedia.org/wiki/Linear_programming),
    [constraint programming](https://en.wikipedia.org/wiki/Constraint_programming),
    [boolean satifiability problems](https://en.wikipedia.org/wiki/Boolean_satisfiability_problem),
    [satifiability modulo theories](https://en.wikipedia.org/wiki/Satisfiability_modulo_theories),
    [cross product](https://en.wikipedia.org/wiki/Cross_product) properties
    for [computational geometry](https://en.wikipedia.org/wiki/Cross_product#Computational_geometry)
25. [Snowverload](src/Snowverload.hs) ->
    [graph components](https://en.wikipedia.org/wiki/Component_(graph_theory)),
    [graph connectivity](https://en.wikipedia.org/wiki/Connectivity_(graph_theory)),
    [graph partitioning](https://en.wikipedia.org/wiki/Graph_partition),
    [min-cut problem](https://en.wikipedia.org/wiki/Minimum_cut),
    [Karger-Stein algorithm](https://en.wikipedia.org/wiki/Karger's_algorithm#Karger%E2%80%93Stein_algorithm),
    [*random*](https://hackage.haskell.org/package/random),
    [spectral partitioning and spectral bisection](https://en.wikipedia.org/wiki/Graph_partition#Spectral_partitioning_and_spectral_bisection),
    [spectral graph theory](https://en.wikipedia.org/wiki/Spectral_graph_theory),
    [Laplacian matrix](https://en.wikipedia.org/wiki/Laplacian_matrix),
    [eigendecomposition](https://en.wikipedia.org/wiki/Eigendecomposition_of_a_matrix),
    [eigenvalues and eigenvectors](https://en.wikipedia.org/wiki/Eigenvalues_and_eigenvectors),
    [Fiedler vector](https://en.wikipedia.org/wiki/Algebraic_connectivity#Fiedler_vector)

Some additional things I learned during **AoC2023** are: [Stack](https://docs.haskellstack.org/en/stable/)
project management, Haskell program profiling, multiple language extensions and pragmas usage, some more commonly used and useful libraries,
point-free style (slightly obfuscating code), better understanding of Functor-Applicative-Monad, file handling and resources embedding.
