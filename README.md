# extensible-diff
Hash-based structural diffing and merging for heterogeneous nested Clojure data, focused on extensibility and usability. This library borrows ideas from the innovative hash-based generic algorithm from [Miraldo and Swierska 2019](https://victorcmiraldo.github.io/data/icfp2019.pdf).

# Features:
* Supports diffing and merging of nested persistent Clojure datatypes.
* Small diffs - replaces unchanged subtrees inside of changes with their hashes.
* Easy extensibility of the algorithm with multimethods.
* Tools for transforming recursive data without tears.

# Building
The package can be built as a standalone uberjar. Ensure that Leiningen and a recent JVM are installed on your machine.

# Recursion schemes
This library implements, and makes heavy use of, so-called recursion schemes, which capture generalized folds and unfolds. Informally, these factor out the recursive part of recursive computations, removing the need of the programmer to deal with explicit recursion. 

The recursion schemes all use the multimethod `fmap` from `clojure.algo.generic.functor`. This is just like the `map` we all know and love, except it returns the same kind of container that it is given. While `map` always returns a lazy seq (at least with 2-arity) no matter what data structure it is given, `fmap` will return a vector if it's given a vector, etc. This is very useful in our case, because it turns out recursive data can be written as fixed points of non-recursive functors.

`fmap` dispatches on the type of its second argument, the data structure it is given. It comes standard with methods for Clojure datatypes, which work the way you think they do. In this parlance, all of these data structures are known as functors.

The key function encapsulating this is `refold-free`:
```clojure
(require 'extensible-diff/rec :refer [refold-free])
(refold-free [branch? f coalgebra algebra])
```
Don't worry, this isn't as scary as it sounds. If you've used recursion schemes in Haskell, this is something like a generalized refold/hylomorphism where the coalgebra unfolds into a free monad. If you don't know what that last sentence means, don't worry about it.

A coalgebra is something that unfolds data into a functor full of the data. For example,
```clojure #(repeat % 10)```
is a coalgebra taking something and repeating it ten times in a lazy seq. More trivially, `identity` is coalgebra for any functor.

One the flip side are algebras, which fold a functor-full of data. Reducing functions are all algebras for sequential containers, for example.

The `branch?` argument retuns a truthy value of whether a data structure is a leaf or a recursive branch. This corresponds to something like lifting the data structure into a value of the free monad. It tells `refold-free` when we've hit the bottom of a tree, and we should stop unfolding (we could do without this argument, but things would be more awkward).

`f` is simply a transformation to perform on leaf nodes.

# Diffing
```clojure
(require 'extensible-diff/diff :refer [diff-unannotated diff-alg diff-coalg])
```
As our running example, suppose we have an initial version of some data, the deletion context:
```clojure
(def deletion {:foo 1 :hoge {:bar {:baz 2} }}
```
Next, we have a new version of the data, the insertion context:
```clojure
(def insertion {:foo 2 :hoge {:bar {:baz 2} :qux 3}}
```
Together, these form a `Change`. Right now, it is coarse, with the insertion and deletion contexts carrying the entire dataset. We want to localize this one big change into smaller, isolated change(s). This is done by first zipping togethr the two contexts along their common keys. To diff these datasets,
```clojure
(def patch (diff-unannotated map? insertion deletion)) =>
{:hoge
  {:ins
    {:bar
      {:substituted #0 :qux 3}}}
  :del #1}
```
where the hashes represent md5 hashes of the subtree `{:baz 2}` and the deletion context, respectively. The subtree can be substituted because it appears in the deletion context, so we can unsubstitute the subtree if we have access to the deletion context. The hash of the deletion context serves to check compatibility of a dataset when merging.

Similarly, the patch can be merged with the deletion dataset to produce the insertion dataset with
```clojure
(merge-unannotated map? patch deletion)
```
Nested changes where the hash of the change's deletion context differs from the hash of the unmerged subtree produce a `Conflict`. Similar to `Change`, the conflict is pushed down the tree as much as possible, to better localize it.

# Acknowledgments
The development of this library was supported by the Alfred B. Sloan Foundation in a grant to [OCTO](https://www.octogroup.org/).

