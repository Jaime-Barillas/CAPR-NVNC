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

(defn -main
  [& args]
  (prn (->> (first args)
            get-file-text
            split-at-dialogue
            (map make-object))))
