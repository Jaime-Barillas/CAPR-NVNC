(ns capr-nvnc.ui
  (:require [clojure.java.io :as io]
            [capr-nvnc.parse :as parse]
            [seesaw.core :as ui]
            [seesaw.chooser :as chooser]
            [seesaw.color :as color]
            [seesaw.icon :as icon])
  (:import [java.awt.event KeyEvent])
  (:gen-class))

(def ^:private *state
  (atom {:width 640
         :height 500
         :split-ratio 2/3
         :curr-image nil
         :curr-text nil
         :story-root ""
         :story nil}))

;; Set native-like style *before* creating ui widgets.
(ui/native!)

(def ^:private imagebox
  (let [width (:width @*state)
        height (* (:height @*state) (:split-ratio @*state))]
    (ui/canvas :size [width :by height])))

(def ^:private textbox
  (let [width (:width @*state)
        height (* (:height @*state) (- 1 (:split-ratio @*state)))]
    (ui/text :text "Hello, World!"
             :multi-line? true
             :editable? false
             :wrap-lines? true
             :background (color/color 64 64 64)
             :foreground :white
             :font {:name :sans-serif :size 18}
             :size [width :by height])))

(def ^:private root-container
  (ui/vertical-panel :items [imagebox textbox]))

(def ^:private load-story
  (ui/action :name "Load Story"))

(def ^:private menu
  (ui/menubar :items [load-story]))

(def ^:private frame
  (ui/frame :title "NVNC"
            :icon "icon.png"
            :minimum-size [640 :by 480]
            :size [(:width @*state) :by (:height @*state)]
            :menubar menu
            :content root-container
            :on-close :hide))

(defn load-image [path]
  (->> (str (:story-root @*state) "/" path)
       io/file
       icon/icon
       .getImage))

(defn paint [c g]
  (when-let [image (:curr-image @*state)]
    (let [resize-factor (/ (.getHeight c) (.getHeight image))
          width (* (.getWidth image) resize-factor)
          height (* (.getHeight image) resize-factor)
          left (- (/ (.getWidth c) 2) (/ width 2))
          top 0]
      (.drawImage g image left top width height nil))))

(defn parse-story [_]
  (let [file (chooser/choose-file :dir "./resources")
        _ (ui/config! textbox :text "Loading...")
        file-path (.getAbsolutePath file)
        story (parse/parse-story file-path)]
    (ui/request-focus! textbox)
    (swap! *state assoc :story story
                        :story-root (-> file .getAbsoluteFile .getParent))
    (ui/config! textbox :text "Complete! Press <enter> to continue...")))

(defn set-textbox-text [block]
  (let [type (:type block)
        text (:text block)]
    (ui/config! textbox :text text)
    (condp = type
      :narration
      (ui/config! textbox :foreground :white)

      :dialogue
      (ui/config! textbox :foreground :gold))))

(defn advance-story [state]
  (let [story (:story state)
        next-block (first story)]
    (cond
      (empty? story)
      (assoc state :story [])

      (= :scene-change (:type next-block))
      (recur (assoc state :curr-image (load-image (:image-path next-block))
                          :story (rest story)))

      (#{:narration :dialogue} (:type next-block))
      (do
        (set-textbox-text next-block)
        (assoc state :story (rest story))))))

(defn init-ui []
  (ui/config! imagebox :paint paint)
  (add-watch *state :paint
    (fn [_key _reference _old-state _new-state]
      ;; This is naughty, repaint! calls the paint function which derefs the
      ;; *state atom. The watch func should not do this, see (doc add-watch).
      (ui/repaint! imagebox)))
  (ui/config! load-story :handler parse-story)
  (ui/listen textbox :key-pressed #(when (= (.getKeyCode %) KeyEvent/VK_ENTER)
                                     (swap! *state advance-story)))
  (ui/pack! frame)
  (ui/show! frame)
  (.revalidate frame)
  (ui/request-focus! textbox))
