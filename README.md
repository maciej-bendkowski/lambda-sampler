Lambda sampler
---------------

*Lambda sampler* is a library for random generation of lambda terms in the de
Bruijn notation (see, e.g. [1]), utilizing the powerful framework of Boltzmann samplers [2].

#### Included features
1. Fast uniform random sampling of plain lambda terms.
2. Fast uniform random sampling of closed lambda terms with bounded variable
   distance (closed h-shallow lambda terms).
3. Support for the full range of size notions in the framework of Gittenberger
   and Gołębiewski [3].
4. Effective predicate-based rejection filtering.

#### References
1. [M. Bendkowski, K. Grygiel, P.Lescanne, M. Zaionc: Combinatorics of lambda-terms: a natural approach](http://arxiv.org/pdf/1609.07593v1.pdf)
2. [P. Duchon, P. Flajolet, G. Louchard. G. Schaeffer: Boltzmann Samplers for the random generation of combinatorial structures](http://algo.inria.fr/flajolet/Publications/DuFlLoSc04.pdf)
3. [B. Gittenberger, Z. Gołębiewski: On the number of lambda terms with
   prescribed size of their De Bruijn
representation](https://arxiv.org/abs/1509.06139)
