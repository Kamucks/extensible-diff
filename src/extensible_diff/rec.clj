(ns mdiff.fx
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

(defn refold-free "refold-free unfolds data and then refolds
with provided coalgebra and algebra. branch? determines
when the unfold should continue." [branch? f coalg alg]
  (fn [tr]
   (letfn
    [(handle-node [ta]
       (if (branch? ta)
         (handle-branch (coalg ta))
         ;coalgebras and algebras
         ;don't touch leaves.
         (f ta)))
     (handle-branch [b]
       (alg (f/fmap handle-node b)))]
     (handle-node tr))))

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
(def ^:dynamic *coroutine*)
(def ^:dynamic *result*)
(def ^:dynamic *bind*)

(defn run [c b]
  (binding [*coroutine* c
            *bind* (fn [f fa] #(b f fa))] (c)))
(defn fork [c b r]
  (binding [*result* r]
    (c run b)))
(defn perform [m]
  (*bind* (partial fork *coroutine* *bind*) m))
(defn resume [] *result*)
(defmacro mdo [monad & body]
  `(trampoline ( run (cr {perform resume} ((first ~monad) (do ~@body))) (second ~monad))))

(defrecord Free [run])
(defrecord Flatmap [sub cont])

(def free-monad
  [identity
   (fn [f free-fa]
     { :sub f :cont free-fa})])
(defn unwrap [fa] (:unwrapped fa))

(defmethod f/fmap Free [f fa]
  (->Free (f/fmap f (unwrap fa))))

(def pure (fn [a] (fn [ar alg] (ar a))))
(def flatmap (fn [f fa]
               (fn [br alg]
                 ((fa)
                  (fn [a]
                    ((f a) br alg))
                  alg))))

(defn fltmap [fmap f free-a]
  (if (fn? free-a)
     #(fmap (partial fltmap fmap f) (free-a))
     #(f free-a)))

(defn impure [fmap fa]
  (fn [ar alg] (alg (fmap ar fa))))
(defn liftf [fmap f]
  (fn [a] (impure fmap (f a))))
(def monad-f
  [pure flatmap])

(defn ffmap [f fa]
  (flatmap (fn [a] (pure (f a))) fa))
(defn wrap
  ([fmap ffa]
   (fn [ar alg]
     (alg (fmap (fn [fa] (fa ar alg)) ffa)))))

(defn lift-free [branch? children a]
  (fn [ar alg]
    (mdo (monad-f ar alg)
     (perform (fn [_ _]
                (if (branch? a)
                 (alg
                  (f/fmap
                   (partial lift-free branch? children)
                   (children a)))
                 (ar a)))))))
(def frei (mdo free-monad
      (let [x (perform (->Flatmap inc 1))]
        (+ x 1))))
(def inner (:cont frei))
((:sub frei) ((:sub inner) (:cont inner)))
