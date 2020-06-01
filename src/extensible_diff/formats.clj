(ns mdiff.formats
  (:require [mdiff.diff :as d]
            [mdiff.fx :as fx]
            [cheshire.core :as json]
            [clojure.tools.cli :as cli]
            [clojure.data.xml :as xml]
        ))

(defn read-stdin [] (slurp *in*))
(defmulti diff (fn [tag ins del]
                 tag))
(defn diff-with-reader [reader ins del]
  (pr-str (d/diff-unannotated (reader ins) (reader del))))
(defmethod diff :default [_ _ _] :no-method)
(defmethod diff "json" [_ ins del]
  (diff-with-reader json/decode ins del))
(defmethod diff "csv" [_ ins del]
  (diff-with-reader json/decode ins del))
(defmethod diff "xml" [_ ins del]
  (diff-with-reader xml/parse-str ins del))

(defmethod diff "pdf" [_ ins del]
  ())
