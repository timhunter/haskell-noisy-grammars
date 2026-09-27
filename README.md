# noisy-grammar-learner

Haskell implementation of the Noisy Grammar Learner described in 
[Perkins & Hunter 2026](https://doi.org/10.1016/j.cognition.2025.106352).

Just the basics: hypothesis spaces consisting of a collection of noisy grammars, each of 
which can assign probabilities (likelihoods) to a collection of trees. 
No parsing. No sampling.

A hypothesis space is specified using a yaml file. A collection of trees is specified in a file using 
the familiar labeled-bracket notation; a tree prefixed by a number means that tree repeated that number 
of times. See the ```examples``` directory for some ... examples.

## How to use it

Start the REPL:
```
$ cabal repl
```

Load the hypothesis space and trees from the mini word-order example from Figure 6b and Figure 8a of 
[Perkins & Hunter 2026](https://doi.org/10.1016/j.cognition.2025.106352):
```
ghci> hs <- readHypothesisSpace "examples/word-order-mini-hypspace-Fig6b.yaml"
ghci> trees <- readTrees "examples/word-order-mini-data-Fig8a.trees"
```

Grab the SVO and SOV grammars from that hypothesis space, and calculate likelihoods for the trees:
```
ghci> getProbForTrees (hs M.! "SVO") trees
1.807841943896371e-8
ghci> getProbForTrees (hs M.! "SOV") trees
1.1047922990477822e-8
```

Or, putting things together:
```
ghci> allLikelihoods hs trees
fromList [("SOV",1.1047922990477822e-8),("SVO",1.807841943896371e-8)]
ghci> posterior hs trees
fromList [("SOV",0.3793103448275862),("SVO",0.6206896551724138)]
```

The 'ka'/'bo' example from Austin et al.:
```
ghci> hs <- readHypothesisSpace "examples/AustinEtAl2022-hypspace.yaml"
ghci> trees <- readTrees "examples/AustinEtAl2022-data.trees"
ghci> allLikelihoods hs trees
fromList [("G_bo",6.3582231759722806e-37),("G_ka",1.6980419337651005e-36)]
ghci> posterior hs trees
fromList [("G_bo",0.2724332905078281),("G_ka",0.7275667094921718)]
```
