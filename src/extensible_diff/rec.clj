(ns extensible-diff.rec
  (:require [cloroutine.core :refer [cr]]
            [net.cgrand.xforms :as x]
            [clojure.algo.generic.functor :as f]
            [valuehash.api :as v]))
(defprotocol Tagged
  (untag [a]))
(defn- untag-default [tagged] (:untagged tagged))
(defrecord Left [untagged])
(defrecord Right [untagged])
(extend-protocol Tagged
  Left
  (untag [self] (untag-default self))
  Right
  (untag [self] (untag-default self)))

(defrecord Branch [branch])
(defrecord Annotated [ann unannotated])

(defprotocol Comonad
  (extract [self])
  (cobind [self f]))

;; (defprotocol Maplike
;;   ;view the derived map.
;;   (view-map [self])
;;   ;diff right from left.
;;   (diff [del ins])
;;   (patch [self diff]))
;; (defprotocol Difflike
;;   (merge [del ins]))

(defrecord DiffFocus [])

(defrecord TreeZipper [focus parent children])
(defmethod f/fmap
  TreeZipper [f {:keys [focus parent children]}]
  (->TreeZipper focus parent (f/fmap f children)))

(defn coproduct-coalg [on-left on-right]
  (fn [tr]
    (if (instance? Left tr)
      (on-left (untag tr))
      (on-right (untag tr)))))

(extend-protocol Comonad
  TreeZipper
  (extract [self] (:focus self))
  (cobind [self f] (->TreeZipper (f self) (:parent self) (:children self))))

(defn tree-zipper [children]
  (fn [tr]
    (let [des (children tr)]
     (->TreeZipper nil tr (f/fmap )))))

(defn- apply-when [ch]
  (if (fn? ch)
    (ch)
    ch))
(defn refold-free [branch? f coalg alg tr]
  (letfn
   [(handle-node [ta]
      (if (branch? ta)
        (fn []
          (handle-branch (coalg ta)))
         ;coalgebras and algebras
         ;don't touch leaves.
        (f ta)))
    (handle-branch
      [b]
      (alg (f/fmap (comp apply-when handle-node) b)))]
    (trampoline handle-node tr)))

(defn transfold
"Transforming refold, analogous to transduce. Refolds
with coalgebras run through supplied transformations." [branch? f coalg alg coalg-t alg-t]
  (refold-free branch? f (coalg-t coalg) (alg-t alg)))

(defn to-zipper [coalg]
  ;change a coalgebra to make a zipper.
  (fn [parent]
    (->TreeZipper
     nil
     parent
     (coalg parent))))

(defn zipper-alg [branch? alg]
  (fn [{:keys [focus parent children]}]
    (->TreeZipper (alg (f/fmap #(if (branch? %) (:focus %) %) children)) parent children)))
(defn annotate-alg-rf
  "Transforms an algebra of the form (r, f a) -> r
   into TreeZipper r f a -> r, which
   replaces the focus with the result of rf-alg called
   on previous focus and updated structure."
  [branch? rf-alg]
  (fn [{:keys [focus parent children]}]
    (->TreeZipper (rf-alg focus children) parent children)))

(defn to-keyed [coalg]
  (fn [keyed-alg]
    (fn [a]
      (keyed-alg)
      (let [acc (volatile! (keyed-alg))
            m (coalg a)
            ks (keys m)]
        (for [k ks]
          (vreset! acc (keyed-alg k (get m k))))
        (keyed-alg a)))))

(defn natural [f coalg]
  (fn [a]
    (f (coalg a))))

(defn dyna [branch? f coalg alg]
  (refold-free branch? f
               (to-zipper coalg)
               (zipper-alg branch? alg)))

(defn diffing-coalg [])

((dyna coll? v/md5-str identity identity) [1 [{:a 3}]])

(defn fold-zipper [branch? children f alg tr]
  ((refold-free branch? children
                f
                (fn [childs]
                  (->TreeZipper 
                   (alg 
                    (f/fmap
                     (fn [ch] (if (satisfies? Comonad ch) (extract ch) ch)) childs))
                   nil
                   childs))) tr))
