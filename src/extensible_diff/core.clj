(ns extensible-diff.core
  (:gen-class)
  (:require [clojure.string]
            [extensible-diff.formats :as formats]
            [clojure.tools.cli :as cli]))
(def supported-formats ["json" "xml" "pdf"])
(def cli-options
  [["-d" "--deletion Data" "Filename of the data to be
                            used in the deletion context."]
   ["-f" "--format FORMAT" (str "Format to be used in diff. One
                            of " (clojure.string/join ", "
                                  supported-formats))]
   ["-i" "--insertion DATA" "File of data to be inserted."]
   ["-h" "--help"]])

(defn -main
  "Performs diff of two files, returns edn by stdout."
  [args]
  (let [{:keys [options]}
        (cli/parse-opts args cli-options)
        {:keys [deletion format insertion]} options
        del (slurp deletion)
        ins (slurp insertion)]
    (spit *out* (formats/diff format del ins))))
