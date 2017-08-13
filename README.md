Lambda sampler
---------------

*Lambda sampler* is a library for random generation of lambda terms in the de
Bruijn notation (see [1,4,5]) utilising the powerful framework of Boltzmann
samplers [2].

#### Features
1. Fast uniform random sampling of plain lambda terms.
2. Fast uniform random sampling of closed lambda terms with bounded variable
   distance (closed h-shallow lambda terms) as well as closed lambda terms with
   unbounded variables.
3. Support for the full range of size notions in the framework of Gittenberger
   and Gołębiewski [3].
4. Effective predicate-based rejection filtering.

#### Install
*Lambda sampler* is developed using `stack` on top of `cabal`.  It is also
available as a `hackage` package [lambda-sampler](https://hackage.haskell.org/package/lambda-sampler).

```
cabal install lambda-sampler
```

#### References
1. [M. Bendkowski, K. Grygiel, P.Lescanne, M. Zaionc: Combinatorics of
   lambda-terms: a natural approach](https://arxiv.org/abs/1609.07593)
2. [P. Duchon, P. Flajolet, G. Louchard. G. Schaeffer: Boltzmann Samplers for
   the random generation of combinatorial
structures](http://algo.inria.fr/flajolet/Publications/DuFlLoSc04.pdf)
3. [B. Gittenberger, Z. Gołębiewski: On the number of lambda terms with
   prescribed size of their De Bruijn
representation](https://arxiv.org/abs/1509.06139)
4. [O. Bodini, B. Gittenberger, Z. Gołębiewski: Enumerating Lambda Terms by
   Weighted Length of Their De Bruijn
Representation](https://arxiv.org/abs/1707.02101)
5. [K. Grygiel, P. Lescanne: Counting and generating terms in the binary lambda
   calculus](https://arxiv.org/abs/1511.05334)
6. [P. Lescanne: Boltzmann samplers for random generation of lambda
   terms](https://arxiv.org/abs/1404.3875)
