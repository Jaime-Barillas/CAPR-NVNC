(ns capr-nvnc.core
  (:require [clojure.java.io :as io]
            [jsonista.core :as json])
  (:gen-class))

(defn get-file-text [file]
  (if (.exists file)
    (slurp file)
    (throw (Exception. "File does not exist!"))))

(defn split-at-dialogue [story]
  (.split story " (?=\")|(?<=[\\.?!]\") "))

(defn make-narration [text]
  {:type :narration
   :text text})

(defn make-dialogue
  ([text] (make-dialogue text ""))
  ([text speaker]
   {:type :dialogue
    :speaker speaker
    :text text}))

(defn make-object [block]
  (condp re-matches (str (first block))
    #"\w" (make-narration block)
    #"\"" (make-dialogue block)))

(defn emit-json [file objs]
  (json/write-value file objs))

(defn convert [file-path]
  (let [file (io/file file-path)
        out-file-name (.replaceFirst (.getName file) "\\.[a-zA-Z]+$" ".json")]
    (->> file
         get-file-text
         split-at-dialogue
         (mapv make-object)
         (emit-json (io/file out-file-name)))))

(defn -main
  [& args]
  (prn (convert (first args))))
