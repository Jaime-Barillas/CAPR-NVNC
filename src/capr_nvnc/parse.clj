(ns capr-nvnc.parse
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure.zip :as zip]
            [opennlp.nlp :as nlp])
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
     :text (.replaceAll (:text block-data) "\\s+" " ")}

    :narration
    {:type :narration
     :text (.replaceAll (:text block-data) "\\s+" " ")}
    
    (ex-info (str "Unsupported block type: " type)
             {:block-type type
              :block-data block-data})))

(defn unread-char! [rdr char]
  (when char ; Ensure we do not try to unread a null char.
    (.unread rdr (int char))))

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

;; ===== NLP Stuff ===== ;;
;; This stuff is [WIP]
;; Really needs to be cleaned up.

(def tokenize (nlp/make-tokenizer "resources/en-token.bin"))

(def name-find (nlp/make-name-finder "resources/en-ner-person.bin"))

(defn get-neighbour-text [loc]
  (let [left (zip/left loc)
        right (zip/right loc)]
    (->> (vector (when left (zip/node left))
                 (when right (zip/node right)))
         (mapv #(if (and (some? %) (= :narration (:type %)))
                  (:text %)
                  nil)))))

(defn find-speaker [[left-text right-text :as neighbour-text]]
  (let [left-names (when left-text (name-find (tokenize left-text)))
        right-names (when right-text (name-find (tokenize right-text)))
        left-index (or (when (and left-text (seq left-names))
                         (.lastIndexOf left-text (last left-names)))
                        0)
        right-index (or (when (and right-text (seq right-names))
                          (.indexOf right-text (first right-names)))
                        9999)]
    (cond
      (every? nil? neighbour-text)
      "" ; Leave the speaker empty to not deal with nils.

      (nil? left-text)
      (or (first right-names) "")

      (nil? right-text)
      (or (last left-names) "")

      (< (- (count left-text) left-index)
           right-index)
      (or (last left-names) "")

      :else
      (or (first right-names) ""))))

(defn set-speaker [loc]
  (let [neighbour-text (get-neighbour-text loc)
        speaker (find-speaker neighbour-text)]
    (zip/edit loc assoc :speaker speaker)))

(defn analyse [loc]
  (if (seq (zip/rights loc))
    (condp = (:type (zip/node loc))
      :dialogue
      (recur (zip/right (set-speaker loc)))

      (recur (zip/right loc)))
    (zip/root loc)))

(defn analyse-story [story]
  (-> (zip/vector-zip story)
    zip/down
    analyse))

;; ===== Public Functions ===== ;;

(defn parse-story
  [file-path]
  (-> (io/reader file-path)
      PushbackReader.
      parse))
