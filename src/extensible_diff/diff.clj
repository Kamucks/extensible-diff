(ns mdiff.diff

  (:require [mdiff.fx :as fx :refer [->Annotated]]
            [clojure.algo.generic.functor :as f]
            [valuehash.api :as v]
            [clojure.pprint]
            [clojure.set]
            [plumbing.core :as pl]
            [net.cgrand.xforms :as x]
            [diffit.vec])
  (:import [mdiff.fx Annotated]))

(def none-hash (v/md5-str ::none))
(def annotated? (partial instance? Annotated))
(defn unannotate [x] (if (annotated? x) (:unannotated x) x))

(defn this-hash [ann] (get-in ann [:ann :this-hash]))
(defn this-id [x] (if-let [hash (this-hash x)] hash x))
(defprotocol Maplike
  (zip-with [self other]))

;(defrecord Annotated [ann unannotated])
(extend-protocol Maplike
  Annotated
  (zip-with [{:keys [ann unannotated]} other]
    (->Annotated
     (zip-with ann (:ann other))
     (zip-with unannotated (:unannotated other)))))

;Hashed collects all subtrees bottom-up
(defrecord Hashed [this-hash child-hashes])
(defrecord Unchanged [unchanged])

(defmethod f/fmap Unchanged [f {:keys [unchanged]}]
  (->Unchanged (f unchanged)))
(defrecord DiffHashed [ins-hash del-hash substituted-children])
;Substituted subtree.
(defrecord Substituted [hash])
(defrecord Unbound [hash])
(defrecord Inserted [inserted])
(defrecord VecChange [del-hash edit-script kfn])
(defmethod f/fmap Substituted [_ del] del)
;Deleted subtree (just hash)
(defrecord Deleted [hash])
(defrecord InChange [dictionary child])
(defrecord NestedChange [dictionary ins del])
(defmethod f/fmap NestedChange [f {:keys [dictionary ins del]}]
  (defmethod f/fmap :default [f d] (f d))
  (->NestedChange dictionary (f/fmap f ins) (f/fmap f del)))



(defmethod f/fmap InChange [f {:keys [dictionary child]}]
  (->InChange dictionary (f/fmap f child)))

(defmethod f/fmap Deleted [_ del] del)

;Inserted child.
(defrecord Inserted [child])
(defmethod f/fmap Inserted [f {:keys [child]}]
  (->Inserted (f/fmap f child)))

(defrecord DiffAnnotated [this-hash substituted-children unannotated])
; a -> (annotated a);
(defmethod f/fmap Annotated [f {:keys [ann unannotated]}]
  (->Annotated ann (f/fmap f unannotated)))
(defn lift-annotated-coalg [coalg]
  (fn [annotated]
    (coalg (:unannotated annotated))))

(defrecord Change [ins del])
(defmethod f/fmap Change [f {:keys [ins del]}]
  (defmethod f/fmap :default [f d] (f d))
  (->Change (f/fmap f ins) (f/fmap f del)))

(defn annotate-branch-alg
  [alg]
  (fn [node]
      ;if node is a branch, reduce it into 
      ;an Annotated, with the annotation as the result
      ;of the algebra, with the original, unmodified
      ;structure put into the unannotated.
      ;!alg must work on both branches and not branches
    (->Annotated (alg (:unannotated node)) (:unannotated node))))
      ;If its not a branch, just return unmodified. ))
(defn- annotated-child-hashes [annotated]
  (conj (get-in annotated [:ann :child-hashes])
        {(get-in annotated [:ann :this-hash]) annotated}))

(defn- get-or-empty [child]
  (if (instance? Annotated child)
    (annotated-child-hashes child)
    {}))

(defn- get-child-hash [child]
  (if (instance? Annotated child)
    (get-in child [:ann :this-hash])
    (v/md5-str child)))

(defn- map-unannotated [f]
  (fn [a]
    (if (instance? Annotated a)
      (f (:unannotated a))
      (f a))))
(defmulti annotate-hashes
  class)

(defmethod annotate-hashes :default
  [childs]
  (let [all-desc #(reduce conj (map get-or-empty %))
        this-hash (v/md5-str
                   (f/fmap
                    get-child-hash
                    childs))]
    (cond
      (sequential? childs)
      (->Annotated (->Hashed this-hash (all-desc childs))
                   childs)
      (map? childs)
      (->Annotated
       (->Hashed this-hash (all-desc (vals childs)))
       childs)
      :else
      (throw
       "collect-child hashes can only do maps or sequentials."))))

(defn hash-alg [branch?]
  (fn [c]
    (println (branch? c))
    (if (branch? c)
      (->Annotated
       (v/md5-str (f/fmap (fn [x] (if (instance? Annotated x)
                                    (:ann x)
                                    (v/md5-str x)))
                          c))
       c)
      c)))

(defn annotate-rec "annotates with hashes, recursively."
  ([branch? alg coalg tr]
   ((fx/refold-free
     branch?
     alg
     coalg
     annotate-hashes) tr))
  ([branch? children tr]
   (annotate-rec branch? identity children tr)))

;If the two trees don't have all matching keys, we have hit our Change.
;We substitute for variables occuring in _both_ insertion and deletion
;context, leaving the other ones be.
;Otherwise, we have an irreducible change, and we should wrap
;all children with either Deleted, Substituted, or Unchanged
;if a subtree occurs in the deletion context but not the insertion, it must
;have been deleted. If a subtree occurs in the insertion, but not the deletion,
;we must keep the subtree.
;Change -> Ann commmon-subtrees (deleted | substituted | inserted)

(defn get-common-subtrees [{:keys [ins del]}]
  (let [ins-ch (conj (get-in ins [:ann :child-hashes]))
        del-ch (get-in del [:ann :child-hashes])
        ins-keys (into #{} (keys ins-ch))
        del-keys (into #{} (keys del-ch))
        deleted-keys
        (clojure.set/difference del-keys ins-keys)
        both (select-keys
              ins-ch
              (clojure.set/intersection ins-keys del-keys))]
    {:ins-hash (get-in ins [:ann :this-hash])
     :del-hash (get-in del [:ann :this-hash])
     :deleted (select-keys del-ch deleted-keys)
     :inserted (select-keys ins-ch
                            (clojure.set/difference ins-keys del-keys))
     :common both}))

(defmulti diff (juxt class class))
;Merge either returns the same class or a Conflict.
;Merging a vecmap with a diffvecmap 
(defmulti merged (juxt class class))

;Push down an annotated change if all the keys match.
;Otherwise, substitute common subtrees and mark deleted subtrees.
;Unfold will need to continue with outer alg.
;A coalgebra change annotated -> f (change annotated | annotated (unchanged))
(defmulti gcp-level
  (fn [{:keys [ins del]}]
    [(class (:unannotated ins)) (class (:unannotated del))]))
;default is just to leave change be.
(defmethod gcp-level :default [_ change] change)

(defn- used-subs [child-dict children]
  (reduce-kv (fn [acc _ v]
               (if (instance? Substituted v)
                 (do (println v)
                     (assoc acc
                            (:hash v)
                            (get child-dict (:hash v))))
                 acc))
             {}
             children))

(used-subs {:a 1} {:b (->Substituted :a)})

;; (defn- in-change-coalg
;;   "In a change. Try to sub annotated children.
;;    Unsubbable children are transformed to InChange.
;;    Unwrap outer change, since all children are subbed."
;;   [child]
;;   (if (instance? InChange child)
;;     (let [{:keys [dictionary child]} child
;;           {:keys [ann unannotated]} child]
;;      (if (get dictionary (:this-hash ann))
;;        ;Current child is in the dictionary. Substitute
;;        ;it.
;;        (->Substituted (:this-hash ann))
;;         ;not in dictionary; don't sub, just unwrap.
;;        ()))
;;     ;leave in child
;;     child))

(defmethod gcp-level [clojure.lang.PersistentArrayMap clojure.lang.PersistentArrayMap]
  [{:keys [ins del] :as ch}]
  (let [ins-children (:unannotated ins)
        del-children (:unannotated del)
        {:keys [ins-hash del-hash deleted inserted common]}
        (get-common-subtrees ch)]
    (if (= (keys ins-children) (keys del-children))
    ;push change down the insertion tree.
      (->Annotated
       (->DiffHashed ins-hash del-hash nil)
       (reduce-kv
       ;mark all children as changes.
        (fn [acc k v] (assoc acc k (->Change v (get del-children k))))
        {}
        (:unannotated ins)))
    ;keys are not the same. Irreducible change.
    ;Go through and mark Inserted/Deleted/Substituted
     ;on insertion context, keeping outer change.
      (let [new-children
            (f/fmap
             (fn [a]
               (if-let [hash (get-in a [:ann :this-hash])]
                 (let [subst (get common hash)
                       deleted (get deleted hash)
                       inserted (get inserted hash)]
                   (cond
                     subst (->Substituted hash)
                         ;can't substitute. Must be either deleted
                         ;or inserted. 
                     deleted (->Deleted a)
               ;if it's inserted, don't annotate. is assumed.
                     :else (->InChange common a)))
         ;Not annotated. Must be a leaf node. Don't annotate.
                 a))
        ;children of _both_ contexts.
             (merge (:unannotated ins) (:unannotated del)))
            subst (used-subs common new-children)
            deleted (filter (partial instance? Deleted) ())]
        (->Annotated
         (->DiffHashed ins-hash del-hash subst)
         new-children)))))
(defn gcp-assoc-branch [{:keys [ins del] :as ch}]
  (if (= (this-hash ins) (this-hash del))
    (->Substituted (this-hash ins))
    (let [ins-children (:unannotated ins)
          del-children (:unannotated del)
          {:keys [ins-hash del-hash deleted inserted common]}
          (get-common-subtrees (f/fmap :unannotated ch))
         ;still zip common keys of insertion and deletion contexts.
         ;Common keys get Change. Non-common keys in insertion
         ;context get InChange. 
          irred-rf (fn [acc k ins-ch]
                     (println (str "acc " acc))
                     (println (str "key " k))
                     (println (str "ins-child " ins-ch))
                     (if-let [del-ch (get del-children k)]
                      ;common, so push down changes
                      ;
                       (assoc acc k (->Change ins-ch del-ch))
                      ;not common. just put InChange to allow
                      ;substitution.
                       (assoc acc k (->InChange common ins-ch))))

          substitute (fn [child]
                       (if (and (get common (this-hash child))
                                (annotated? child))
                         (->Substituted (this-hash child))
                         (->InChange common child)))]
      (if (= (keys ins-children) (keys del-children))
    ;push change down the insertion tree.
       ;(->Annotated
        ;(->Hashed ins-hash common)
        (reduce-kv
       ;mark all children as changes.
         (fn [acc k v]
           (let [deleted-child
                 (get del-children k)
                 subs-child (if (annotated? deleted-child)
                              (this-hash deleted-child)
                              deleted-child)]
             (if (= v deleted-child)
               acc
               (assoc acc k
                      (->Change v
                                deleted-child)))))
         {}
         (:unannotated ins))
    ;keys are not the same. Irreducible change.
    ;Wrap inserted children with InChange.
    ;Sub as many as possible
       ;(->Change (f/fmap substitute (:ins ch)) (:del ch))
    ;;  (->Change (:unannotated (f/fmap substitute (:ins ch)))
    ;;            ;(:unannotated (f/fmap substitute (:del ch)))
    ;;            (this-hash (:del ch))
    ;;            )
        (->Change (reduce-kv irred-rf
                             {}
                             (get-in ch
                                     [:ins :unannotated]))
                  (:del ch))))))

(defn- substitute-children
  [dictionary ins]
  (let [f (fn [child]
            (if
             (get dictionary
                  (this-hash child))
              (->Substituted (this-hash child))
              child))]
    ;(->Annotated ann (f/fmap f unannotated))
    (f/fmap (comp unannotate f) ins)))

(defrecord DiffVec [hash diffed])
(defmethod f/fmap DiffVec [_ dv] dv)

(defn- insert-at [start inserts v]
  (let [[before after] (split-at start v)]
    (concat before inserts after)))
(defn- delete-at [start n v]
  (let [[before after] (split-at start v)
        deleted (take n after)]
    (concat before (drop n after))))

(defn- app-edit
  ([del [tag start x]]
   (case tag
     :+ [tag start x
         ;(+ offset (count x))
         (insert-at start x del)]
     :- (let [[deletion edited]
              (delete-at start x del)]
          [tag start x
           (v/md5-str (vec deletion))
           edited])
     :else [tag start x del])))

(defn- edit-step
  ([acc [tag start x]]
   (println tag start x)
   (let [new-edit (case tag
                    :+ (insert-at start x acc)
                    :- (delete-at start x acc))]
     new-edit)))

(defn edit-to-map [[tag start x]]
  (case tag
    :+ {:tag :+ :start start :data x}
    :- {:tag :- :start start :data x}))

(defn with-offset [{:keys [tag data] :as m}]
  (if (= tag :+)
    (update m :offset
            (fnil
             (partial + (count data))
             0))
    (update m :offset
            (fnil
             (partial + (- data))
             0))))

(defn- original-block-index
  "Get the original index *after*
   this block, by subtracting
   the current offset from the block.
   type orig-index current-offset diff-index
   = 0 0 0
   = 1 0 1
   + 1 1 2
   + 1 2 3
   - 2 1 3
   = 3 1 4
   "
  [{:keys [start offset]}])
;[0 5] [+ 5 1] [- 10 n]
;[+ 1 5] 
;(transduce identity (completing edit-step)
;           [0 1 2] [[:- 1 1] [:+ 0 [4]]])
(defn diff5 [xf del diff]
  (transduce identity (completing edit-step) del diff))

(defn- with-original-ix
  "Annotate blocks with start/end
   indices from original.
   Inserts do not increase index"
  [xf]
  (let [offset (volatile! 0)
        prev-end (volatile! 0)
        acc (volatile! nil)]
    (fn
      ([] (vreset! acc (xf)))
      ([acc {:keys [tag start data] :as item}]
       (let [orig-block-start (- start @offset)
             block-length (case tag
                            :+ (count data)
                            :- (- data))
             orig-block-end (case tag
                              :+ orig-block-start
                              :- (+ orig-block-start data))]
         (vswap! offset (partial + block-length))
         (merge item {:orig-start orig-block-start
                      :orig-end orig-block-end})))
      ([acc] (xf acc)))))
(defn xdiff [xf ins del]
  (transduce xf
             (reductions
              (juxt (fn [_ a] a) (fn [[_ acc] edit]
                                   (edit-step acc edit))) ins del)
             del
             (diffit.vec/diff del ins)))

(def apply-vdiff
  (partial reduce
           edit-step))

(defn- with-offset
  "Annotate edits with offset
  at *end* of each edit. Subtracting the offset
  from the original index yields original index
  mappings for the unchanged block between
  the end of the current block and the start
  of the next changed block.
  
  unedited block gets
  the matching index of the patched." [xf]
  (let [offset (volatile! 0)]
    (fn
      ([] (xf))
      ([acc [tag start x]]
       (xf acc
           (case tag
             :+ (do (vswap! offset (partial + (count x)))
                    [tag start x @offset])
             :- (do (vswap! offset (partial + (- x)))
                    [tag start x @offset])))))))

(defn- acc-hashes [del diff]
  (reduce
   (fn [edited edit]
     (let [;new-offset (+ current-offset (get-offset edit))
           [new-edited new-edits]
           (app-edit edit edited)
                     ;now update the start of the new-edit
                     ;with the original offset. (from before this edit)
                     ;
           ]
       [new-edited new-edits]))
   [del []]
   diff))
;associative nodes like {:a 3} {:b 4} diffs compared with equality/hash
;to get to Deleted hash | Inserted subbed-insert | Unchanged
;
;
(defn diff3 [ins del]
  (let [[_ diff] (diffit.vec/diff del ins)]
    (apply-vdiff del diff)))

    ;; (comment (f/fma
    ;;   (fn [[tag start x]]

    ;;     (if (= tag :+)
    ;;       [tag start x]
    ;;       [tag start x
    ;;        (println (str "del " del))
    ;;        (println start)
    ;;        (println x)
    ;;        (println (v/md5-str (subvec del start (min (count del) (+ start x)))))
    ;;        (println (subvec del start (min (count del) (+ start x))))
    ;;              ;(v/md5-str (subvec del start (min (count del) (+ start x))))
    ;;        ]
    ;;       ))
    ;;   diff))))

;; (v/md5-str [3])
;; (v/md5-str (subvec [1 2 2] 1 1))
(defn gcp-seq [{:keys [ins del] :as ch}]
  (let [dict (get-in del [:ann :child-hashes])
        sub-ins (substitute-children dict ins)
        sub-del (substitute-children dict del)
        ins-v (:unannotated ins)
        del-v (:unannotated del)]

    (->DiffVec
     (this-hash del)
     (diffit.vec/diff sub-ins sub-del))))

(gcp-seq  (annotate-rec coll? identity identity
                        (->Change [2 3 4] [4 3 2])))

(gcp-seq (->Change (->Annotated
                    (->Hashed 0 {2 "data"})
                    [(->Annotated {:this-hash 1} "data")])
                   (->Annotated
                    (->Hashed 2 {1 "asdf"})
                    [(->Annotated {:this-hash 2} "old")])))


(defmulti gcp
  (fn [{:keys [ins del]}]
      ;(println "ins")
      ;(clojure.pprint/pprint ins)
    [(class (:unannotated ins))
     (class (:unannotated del))]))

(defmethod gcp :default [{:keys [ins del] :as change}]
  (if (and (annotated? ins) (annotated? del))
    (if (= (this-hash ins) (this-hash del))
      (->Substituted (this-hash ins))
      (let [ins-ch (:unannotated ins)
            del-ch (:unannotated del)]
        (println (and (sequential? ins-ch) (sequential? del-ch)))
        (println (and (associative? ins-ch) (associative? del-ch)))
        (cond
          (and
           (sequential? ins-ch)
           (sequential? del-ch))
          (gcp-seq change)
          (and
           (associative? ins-ch)
           (associative? del-ch))
          (gcp-assoc-branch change)
          :else change)))
    (->Change ins
              ;(if (annotated? del) (this-hash del) del)
              del)))

;; (defmethod gcp [clojure.lang.PersistentArrayMap
;;                 clojure.lang.PersistentArrayMap]
;;   [change]
;;   (println "in gcp map")
;;   (gcp-assoc-branch change))

;Class must contain an Annotated or a leaf. Coalgebra
;must emit a map!
;
(defmulti diff-coalg
  (fn [branch? coalg node]
    (class node)))

(defmethod diff-coalg :default [_ _ node] node)

;need to find gcp of change. Also, mark children with InChange.
(defmethod diff-coalg Change
  [branch? coalg {:keys [ins del] :as change}]
  (let [f #(get-in change [% :unannotated])
        unwrapped-ins (get-in change [:ins :unannotated])
        unwrapped-del (get-in change [:ins :unannotated])]
    ;(println "in-change") ;here, it is not nested
    ;(clojure.pprint/pprint change)
    (cond
      (and (annotated? ins) (annotated? del))
      (gcp change)
      (= ins del)
      change
      :else
      change)))

;; (defmethod diff-coalg InChange
;;   [branch? coalg {:keys [dictionary child]}]
;;   (case (instance? Annotated child)
;;     ;if its annotated, and the hash is in there,
;;     ;sub it.
;;     (if (get dictionary (get-in child [:ann :this-hash]))
;;       (->Substituted (this-hash child))
;;       ;if that doesn't work, and it's a branch,
;;       ;we make children InChanges, so they can
;;       ;be subbed in further steps.
;;       (if (branch? (:unannotated child))
;;         (let [children (coalg (:unannotated child))]
;;           (f/fmap
;;            (fn [ch]
;;              (->InChange dictionary ch))
;;            ch))
;;         child))
;;     :else child))

;; (defmethod diff-coalg Change [branch? coalg {:keys [ins del]}]
;;   ;Find the GCP, trying to push changes down the tree.
;;   ;First unwrap each change's unannotated with coalg.
;;   (gcp (->Change (update ins :unannotated coalg)
;;                  (update del :unannotated coalg))))
(defn change-to-vec-change [])
(defmethod diff-coalg Substituted [_ _ sub] sub)

(defmulti diff-alg (fn [child? tr] (class tr)))
(defmethod diff-alg :default [_ tr] tr)
(defmethod diff-alg Annotated [_ tr] (println "415 annotated")
  (unannotate tr))
;see if we can
(defmethod diff-alg NestedChange [_ {:keys [dictionary ins del]}]
  (let [un-ins (:unannotated ins)
        un-del (:unannotated del)]
    (if (and (vector? un-ins) (vector? un-del))
      (diff3 (substitute-children
              dictionary
              un-ins)
             (substitute-children
              dictionary
              un-del))
      (substitute-children dictionary un-ins)
                ;(if-let [dhash (this-hash del)] dhash del)
      )))

(defmethod diff-alg Change [_ {:keys [ins del]}]
  (let [un-ins (unannotate ins)
        un-del (unannotate del)]
    (if (and (vector? un-ins) (vector? un-del))
      (let [dictionary (get-in del [:ann :child-hashes])]
        (diff3 (substitute-children
                dictionary
                (:unannotated ins))
               (substitute-children
                dictionary
                (:unannotated del))))
      (do (println "not vect ")
          (->Change (unannotate ins) (unannotate del)));(:unannotated ins)
                ;(if-let [dhash (this-hash del)] dhash del)
      )))

;Unsub if child is Substituted, and the hash is in the dictionary.
;; (defmethod diff-alg InChange
;;   [{:keys [dictionary child]}]
;;   (if (instance? Substituted child)
;;     (do (println "subbed!")
;;         (if-let [subst (get dictionary (:hash child))]
;;           subst
;;           (->Unbound (:hash child))))
;;     ;definitely bounded recursion.
;;     (do (println "not-subbed ") (diff-alg child))))
(defmethod diff-alg InChange
  [branch? {:keys [dictionary child]}]
  (if (annotated? child)
    (do (println (str "subbed! " dictionary child))
        (if-let [subst (get dictionary (this-hash child))]
          (->Substituted (this-hash child))
          (:unannotated child)))
    ;definitely bounded recursion.
    (do (println "not-subbed ") (diff-alg branch? child))))

(defn diff-with-annotated
  ([branch? coalg change]
   (diff-with-annotated branch? coalg
                        (partial diff-alg branch?) change))
  ([branch? coalg alg change]
   ((fx/refold-free branch? identity
                    (partial diff-coalg branch? coalg)
                    alg) change)))
(defn diff-unannotated [branch? coalg ins del]
  (diff-with-annotated
   branch?
   coalg
   (->Change (annotate-rec branch? coalg ins)
             (annotate-rec branch? coalg del))))

(defn unannotate-rec [branch? children {:keys [ins del]}]
  (let [alg (fn [x] (if (annotated? x) (:unannotated x) x))]
    (->Change ((fx/refold-free branch? identity children alg) ins)
              del)))

(defn diff-maps [ins del]
  (diff-unannotated map? identity ins del))
(defrecord Conflict [patch unmerged])
(defprotocol ResolveHash
  (resolve-hash [self hash]))
(extend-protocol ResolveHash
  Annotated
  (resolve-hash [self hash]
    (get-in self [:ann :child-hashes hash]))
  Conflict
  (resolve-hash [self hash]
    (resolve-hash (:unmerged self) hash)))

;A Conflict which we can attempt
; to push down.
(defmethod f/fmap Conflict [f {:keys [patch unmerged]}]
  (defmethod f/fmap :default [f x] (f x))
  (->Conflict (f/fmap f patch) (f/fmap f unmerged)))

(defn unsub [{:keys [hash]}
             {:keys [ann unannotated] :as unmerged}]
  (let [{:keys [this-hash child-hashes]} ann]
    (if-let [unsubbed
             (get child-hashes hash)]
      unsubbed
      (->Unbound hash))))

(defn detect-conflict [del unmerged]
  (if (annotated? unmerged)
    (= del (this-hash unmerged))
    (= del unmerged)))

(defn merge-subst
  [{:keys [hashed]} unmerged]
  (unsub hashed unmerged))
(defrecord Mergable [diff unmerged])

(defrecord Merging [left right])
(defrecord Flipped [flipped])
(defrecord Conflicting [left right])
;vector conflict is a pair of edits.
(defrecord VectorConflict [left right])
(defprotocol IsChange
  (is-change? [self]))
(extend-protocol IsChange
  java.lang.Object
  (is-change? [_] false)
  Change
  (is-change? [_] true)
  DiffVec
  (is-change? [_] true))

(defmethod f/fmap Merging [f {:keys [left right]}]
  (->Merging (f/fmap f left) (f/fmap f right)))
(defmulti merge-alg* (fn [{:keys [left right]}]
                       [(class left) (class right)]))
;just pushes the conflict down the keys of the tree.
;We can definitely do this, because we have two associatives.
;
(defn merge-assoc [{:keys [left right]}]
  (let [left-keys (into #{} (keys left))
        right-keys (into #{} (keys right))
        all-keys (clojure.set/union left-keys right-keys)
        common-keys
        (clojure.set/intersection left-keys right-keys)]
    (reduce
     (fn [acc k]
       (assoc acc k
              (cond
                (contains? common-keys k)
         ;key conflicts. Put in conflict.
                (->Conflicting (get left k) (get right k))
                (contains? left-keys k)
                (get left k)
                :else
                (get right k))))
     {}
     all-keys)))

(defn merge-diffvec [{:keys [left right]}]
  (let [left (:diffed left)
        right (:diffed right)]
    ()))

(defmulti merging
  (fn [{:keys [diff unmerged]}]
    ;Diff could be, e.g. map,  scalar or
    ;change. unmerged is annotated or leaf.
    ;
    (class diff)))

(defmethod merging :default
  [{:keys [diff unmerged]}]
  (let [pushdown (fn [acc k v]
                   (if-let [sub-ins (get k diff)]
                     ;if diff has this key, push down
                     ;conflict.
                     (assoc acc k (->Conflict sub-ins v))
                     ;if only in unmerged.
                     (assoc acc k v)))]
    (cond (and (associative? (:unannotated unmerged))
               (associative? diff))
          (reduce-kv pushdown {} (:unannotated unmerged))
          (and (instance? DiffVec diff)
               (sequential? (unannotate unmerged)))
          (diffit.vec/patch unmerged diff)
          ;ins must be a leaf. Unconditionally
          ;replace unmerged with leaf.
          :else
          diff)))


(defmethod merging Change
  [{:keys [diff unmerged] :as conflict}]
  (cond (and (annotated? unmerged)
             (= (this-hash unmerged) (:del diff)))
        (->Mergable (:ins diff) unmerged)
        (= (:del diff) unmerged)
        (:del diff)
        :else conflict))

(defmethod merging Substituted [{:keys [diff unmerged]}]
  (unsub diff unmerged))

(defn merge-vec [{:keys [diffed]}
                 {:keys [ann unannotated] :as unmerged}]
  (let [current (volatile! unmerged)
        insert-at (fn [start x]
                    (let [[before after] (split-at start current)]
                      (vreset! current (into [] (concat before x after)))))
        delete-at (fn [start n hash]
                    (let
                     [[before after]
                      (split-at start current)
                      deletion (into [] (take after n))
                      deletion-hash (v/md5-str deletion)]
                      (if (= hash deletion-hash)
                        (vreset!
                         current
                         (into []
                               (concat before
                                       (drop n after))))
                        (->Conflict hash deletion))))]
    (for [[tag start x hash] diffed]
      (if (= tag :+)
        (insert-at start x)
        (delete-at start x hash)))))

(defn pushdown-conflict
  "Pushes down conflict as far as possible.
  If we have (hash del) = (thishash unmerged)
  or, del = unmerged, return a mergable.
  Otherwise, if (keys unmerged = keys del),
  push down conflict to childrin.
  Lastly, if (keys ins != keys unmerged), cannot push
  change down more. Stop." [{:keys [change unmerged]}]
  (let [{:keys [ins del]} change]
    (cond
      (or (= del unmerged)
          (= del (this-hash unmerged)))
      (->Mergable ins unmerged))))

;; (defmulti merge-change
;;   (fn [{:keys [patch unmerged]}]
;;     (class (:ins patch))))
;; (defmethod merge-change :default
;;   [{:key [patch unmerged]}]
;;   ())
;; (defmulti conflict-gcp
;;   (fn [{:keys [patch unmerged] :as conflict} & _]
;;     (class unmerged)))

;; (defmethod conflict-gcp Annotated
;;   [{:keys [patch unmerged] :as conflict} patcher]
;;   (let [{:keys [ins del]} patch
;;         {:keys [ann unannotated]} unmerged]
;;     ;first, check if we can apply the patch right here.
;;     (if (= (this-hash del) (this-hash patch))
;;       ;apply patch.
;;       (patcher patch ins)
;;       conflict)))

;; (defmethod merge-change Annotated
;;   [{:keys [patch unmerged] :as conflict}]
;;   (let [{:keys [ins del]} patch]
;;     (if (and (annotated? unmerged)
;;              (= del (this-hash unmerged)))
;;       ins
;;       conflict)))

;; (defmulti merge-patch
;;   ;handle cases where patch is Change, Substituted,
;;   ;or other. unmerged MUST be annotated.
;;   (fn [{:keys [patch unmerged] :as conflict}]
;;     (class patch)))
;; (defmethod merge-patch :default
;;   [conflict] conflict)
;; (defmethod merge-patch Change
;;   [conflict] (merge-change conflict))
;; (defmethod merge-patch Substituted
;;   [{:keys [hash]} unmerged] )
;; (defmethod merge-patch Substituted)
;; (defmethod merge-change [Annotated Annotated]
;;   [{:keys [patch unmerged]}]
;;   (let [{:keys [ins del]} patch]
;;     ))


;; (defmulti merge-annotated
;;   ;need cases for the patch being a Change,
;;   ;a VectorChange, or an annotated recursive
;;   ;node that's associative, or a leaf node.
;;   ;Need cases for unmerged being an Annotated
;;   ;or a leaf node.

;;   (fn [{:keys [patch unmerged]}]
;;     [(class (:ins patch))
;;      (class unmerged)]))

;; (defmethod merge-annotated :default
;;   [conflict])

;; (defmethod merge-annotated [Annotated Annotated]
;;   [{:keys [ins del] :as change}
;;    unmerged]
;;   (if (= (this-hash ins) (this-hash del) (this-hash unmerged)))
;;   (if (= (this-hash ins) (this-hash del))
;;     (->Substituted (this-hash ins))
;;     (let [ins-children (:unannotated ins)
;;           del-children (:unannotated del)
;;           {:keys [ins-hash del-hash deleted inserted common]}
;;           (get-common-subtrees (f/fmap :unannotated ch))]
;;       (if (= (keys ins-children) (keys del-children))
;;     ;push change down the insertion tree.
;;         (->Annotated
;;          (->DiffHashed ins-hash del-hash nil)
;;          (reduce-kv
;;        ;mark all children as changes.
;;           (fn [acc k v]
;;             (let [deleted-child
;;                   (get del-children k)
;;                   subs-child (if (annotated? deleted-child)
;;                                (this-hash deleted-child)
;;                                deleted-child)]
;;               (if (= v deleted-child)
;;                 acc
;;                 (assoc acc k
;;                        (->Change v
;;                                  subs-child)))))
;;           {}
;;           (:unannotated ins)))
;;     ;keys are not the same. Irreducible change.
;;     ;Wrap inserted children with InChange.
;;         (f/fmap
;;          (fn [child] (->InChange common child)) (:ins ch))))))



;; (defmethod merge-annotated [Change true]
;;   [{:keys [patch
;;            unmerged] :as conflict}]
;;   (let [{:keys [ins del]} patch
;;         {:keys [ann unanotated]} unmerged]
;;     (if (= (:this-hash ann) (:this-hash del))
;;       (->Inserted (:unannotated ins))
;;       conflict)))
;;     ;The insert should either match the hash
;;     ;of del, or be equal.
;;   (let [{:keys [ins del]} patch]
;;     (if (annotated? unmerged)

;;       (if (= unmerged ins)
;;         (->Inserted ins)
;;         conflict))))

;; ;Contains map of metavariables mapping to subtrees.
;; (defrecord VecMap [m v])


;; ;{ hash0: {:a {:d 1} :b 2} hash2: {:a {:d 1} :c 3} }
;; ;gcp: hash0: {:outer {:a $d :b 2 } hash2: {:outer {:a $d :c 3}}
;; ;pull up common subtrees for every tree in the vector,
;; ;putting them in m. Then, values can be recursively
;; ;substituted to recreate the unsubbed vector.

;; ;Like VecMap, except contains an edit-script
;; ;[[:- _deleted] [:+ _inserted] ...]


;; (extend-protocol Maplike
;;   VecChange
;;   (view-map [self] (:m self))
;;   (merge-with [self other]
;;     ()))
