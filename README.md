Lambda sampler
---------------

*Lambda sampler* is a library for random generation of lambda terms in the de
Bruijn notation (see [1,4,5]) utilising the powerful framework of Boltzmann
samplers [2]. 

 Lambda terms in the de Bruijn notation are lambda terms where instead of
variables, we use natural indices encoded as a sequence of successors of zero.
 Each index captures the distance to its binding lambda - `Z` (read zero)
corresponds to a variable bound by the first lambda abstraction on its way to
the term root. The successor `S` increases the distance of its argument by one.
If an index points to a lambda abstraction outside of the term, then that index
represents a free variable in the term. Such a representation for lambda terms
avoids using explicit variable names and hence eliminates the use of
alpha-conversion.

Boltzmann samplers guarantee that terms of equal size have equal probability of
being sampled. The price for the uniform outcome distribution is the
non-deterministic size of the outcome size -- in the Boltzmann model it becomes
a random variable.  Regardless, it is still possible to control the desired
size lower and upper bounds using anticipated rejection, discarding unwanted
terms until a suitable term is constructed. 

*Lambda sampler* provides a simple interface for rejection-based 
filters in both the setting of plain terms or closed ones (unbounded as well
as with bounded, so-called shallow indices).

#### Example usage
We start with choosing an appropriate sampler variant, for instance a singular
rejection sampler for plain terms (i.e. open or closed) calibrated using the
so-called dominating singularity of the combinatorial system:

```
import qualified Data.Lambda.Random.PlainSystem as P
plainNatSampler = P.rejectionSampler natural 1.0e-9
```

In the above code snippet, we construct the sampler using the `natural` size notion
where abstractions, applications, successors and zeros get weight one. Moreover,
we request a singularity approximation with an error of `1.0e-09`.

Next we have to decide on the target size giving suitable bounds, as well as
on the source of random numbers (see `Data.Lambda.Random`).
For simplicity, let us choose the `IO` monad and the target 
size of `[500; 50,000]`:

```
plainSampler :: IO Lambda
plainSampler = plainLambdaIO plainNatSampler 500 50000
```

And so, we have a monadic function `plainSampler` generating uniformly random
plain lambda terms within the desired target size interval. 

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
