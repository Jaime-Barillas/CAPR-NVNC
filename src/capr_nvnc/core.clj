(ns capr-nvnc.core
  (:require [clojure.java.io :as io])
  (:gen-class))

(defn get-file-text [file-path]
  (let [file (.getCanonicalFile (io/file file-path))]
    (if (.exists file)
      (slurp file)
      (throw (Exception. "File does not exist!")))))

(defn split-at-dialogue [story]
  (.split story " (?=\")|(?<=[\\.?!]\") "))

(defn -main
  [& args]
  (prn (-> (first args)
           get-file-text
           split-at-dialogue
           seq ; Convert Java array to printable format.
           )))
