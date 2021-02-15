(ns capr-nvnc.core
  (:require [clojure.java.io :as io])
  (:gen-class))

(defn get-file-text [file-path]
  (let [file (.getCanonicalFile (io/file file-path))]
    (if (.exists file)
      (slurp file)
      (throw (Exception. "File does not exist!")))))

(defn -main
  [& args]
  (prn (get-file-text (first args))))
