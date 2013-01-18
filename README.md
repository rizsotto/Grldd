Grldd
=====

Grldd is a toy Haskell project. To help to learn this beautiful language.
Published for the benfits for other learners.

Grldd is a graphical dependency tracking tool, based on the [ldd][1] command.
It is using [BFS][2] to build a dependency graph and print the result as
[graphviz][3] output.

During implementation got to solve the following difficulties:

- How to execute a command on local machine?
  (Used [process][4] library.)
- How to grab its standard output and error to process it?
  (Used [process][4] library.) 
- How to parse the output?
  (Used [parsec][5] library.)
- How to write BFS algorithm, to be able to test with different source of
  dependency information? (Wrote higher order function.)
- How to write graphviz ouput?
  (Used [fgl][6] library.)
- How to handle command line arguments?
- How to write test for my code? (Used [HUnit][7] for this.)
- How to [cabal][8] my project? (Read the manual. :))

Source code is published under GPL version 3.

[1]: http://en.wikipedia.org/wiki/Ldd_(Unix)
[2]: http://en.wikipedia.org/wiki/Breadth_first_search
[3]: http://www.graphviz.org/
[4]: http://hackage.haskell.org/package/process
[5]: http://hackage.haskell.org/package/parsec
[6]: http://hackage.haskell.org/package/fgl
[7]: http://hackage.haskell.org/package/HUnit
[8]: https://github.com/haskell/cabal/
