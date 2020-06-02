# extensible-diff
Hash-based structural diffing and merging for heterogeneous nested Clojure data, focused on extensibility and usability. This library borrows ideas from the innovative hash-based generic algorithm from [Miraldo and Swierska 2019](https://victorcmiraldo.github.io/data/icfp2019.pdf).

# Features:
* Supports diffing and merging of nested persistent Clojure datatypes.
* Easy extensibility of the algorithm with multimethods.
* Tools for transforming recursive data without tears.

# Building
Ensure that Leiningen and a recent JVM are installed on your machine.


# Recursion schemes
This library implements, and makes heavy use of, so-called recursion schemes, which capture generalized folds and unfolds. Informally, these factor out the recursive part of recursive computations, removing the need of the programmer to deal with explicit recursion. 

The recursion schemes all use the multimethod `fmap` from `clojure.algo.generic.functor`. This is just like the `map` we all know and love, except it returns the same kind of container that it is given. While `map` always returns a lazy seq (at least with 2-arity) no matter what data structure it is given, `fmap` will return a vector if it's given a vector, etc. This is very useful in our case, because it turns out recursive data can be written as fixed points of non-recursive functors.

`fmap` dispatches on the type of its second argument, the data structure it is given. It comes standard with methods for Clojure datatypes, which work the way you think they do. In this parlance, all of these data structures are known as functors.

The key function encapsulating this is `refold-free`:
```clojure
(refold-free [branch? f coalgebra algebra])
```
Don't worry, this isn't as scary as it sounds. If you've used recursion schemes in Haskell, this is something like a generalized refold/hylomorphism where the coalgebra unfolds into a free monad. If you don't know what that last sentence means, don't worry about it.

A coalgebra is something that unfolds data into a functor full of the data. For example,
```clojure #(repeat % 10)```
is a coalgebra taking something and repeating it ten times in a lazy seq. More trivially, `identity` is coalgebra for any functor.

One the flip side are algebras, which fold a functor-full of data. Reducing functions are all algebras for sequential containers, for example.

The `branch?` argument retuns a truthy value of whether a data structure is a leaf or a recursive branch. This corresponds to something like lifting the data structure into a value of the free monad. It tells `refold-free` when we've hit the bottom of a tree, and we should stop unfolding (we could do without this argument, but things would be more awkward).

`f` is simply a transformation to perform on leaf nodes.
