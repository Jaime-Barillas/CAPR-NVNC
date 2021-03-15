(ns capr-nvnc.parse
  (:require [clojure.java.io :as io]
            [clojure.string :as string])
  (:import [java.io PushbackReader])
  (:gen-class))

(def ^:private eos
  "The value returned by a Java Reader.read() method when the end of stream
  has been reached."
  -1)

(def ^:private annotation-chars
  #{\@})

(def ^:private dialogue-delimiters
  #{\"})

(def ^:private end-of-sentence-chars
  #{\. \! \?})

(defn make-block [type block-data]
  ;; TODO: multiple annotation types?
  (condp = type
    :annotation
    {:type :scene-change
     :image-path (:data block-data)}

    :dialogue
    {:type :dialogue
     :speaker ""
     :text (:text block-data)}

    :narration
    {:type :narration
     :text (:text block-data)}
    
    (ex-info (str "Unsupported block type: " type)
             {:block-type type
              :block-data block-data})))

(defn unread-char! [rdr char]
  (.unread rdr (int char)))

(defn read-char! [rdr]
  (let [curr-char (.read rdr)]
    (when-not (= curr-char eos)
      (char curr-char))))

(defn skip-whitespace! [rdr]
  (loop [curr-char (read-char! rdr)]
    (if (and (some? curr-char) (Character/isWhitespace curr-char))
      (recur (read-char! rdr))
      (unread-char! rdr curr-char))))

(defn peek-char [rdr]
  (let [next-char (.read rdr)]
    (when-not (= next-char eos)
      (unread-char! rdr next-char)
      (char next-char))))

;; Does not include end-char, make option to include end-char?
(defn read-until! [rdr end-char]
  (loop [text nil
         next-char (peek-char rdr)]
    (if (or (nil? next-char) (= end-char next-char))
      text
      (recur (str text (read-char! rdr)) (peek-char rdr)))))

(defn read-annotation-type! [rdr]
  (read-char! rdr) ; Skip @ char.
  (-> rdr
    (read-until! \[)
    string/trim
    keyword))

(defn read-annotation-data! [rdr]
  (read-char! rdr) ; Skip opening [ char.
  (let [data (read-until! rdr \])]
    (read-char! rdr) ; Skip closing ] char.
    data))

;; Currently, annotations only support specifying images.
(defn parse-annotation! [rdr]
  (make-block :annotation
              {:type (read-annotation-type! rdr)
               :data (read-annotation-data! rdr)}))

(defn read-dialogue! [rdr]
  (read-char! rdr) ; Skip opening " char.
  (loop [dialogue nil
         next-char (peek-char rdr)]
    (if (or (nil? next-char) (dialogue-delimiters next-char))
      (do
        (read-char! rdr) ; Skip closing " char.
        dialogue)
      (recur (str dialogue (read-char! rdr)) (peek-char rdr)))))

(defn parse-dialogue! [rdr]
  (make-block :dialogue
              {:text (read-dialogue! rdr)}))

(defn read-sentence! [rdr]
  (loop [sentence nil
         next-char (peek-char rdr)]
    (cond
      (nil? next-char)
      (string/trim sentence)

      (dialogue-delimiters next-char)
      (string/trim sentence)

      (end-of-sentence-chars next-char)
      (str sentence (read-char! rdr))

      :else
      (recur (str sentence (read-char! rdr)) (peek-char rdr)))))

(defn parse-sentence! [rdr]
  (make-block :narration
              {:text (read-sentence! rdr)}))

(defn parse [rdr]
  (loop [blocks []
         next-char (peek-char rdr)]
    (cond
      (nil? next-char)
      blocks

      (Character/isWhitespace next-char)
      (do
        (skip-whitespace! rdr)
        (recur blocks (peek-char rdr)))
      
      (annotation-chars next-char)
      (recur (conj blocks (parse-annotation! rdr)) (peek-char rdr))
      
      (dialogue-delimiters next-char)
      (recur (conj blocks (parse-dialogue! rdr)) (peek-char rdr))
      
      :else
      (recur (conj blocks (parse-sentence! rdr)) (peek-char rdr)))))

(defn parse-story
  [file-path]
  (-> (io/reader file-path)
      PushbackReader.
      parse))
