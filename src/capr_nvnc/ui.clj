(ns capr-nvnc.ui
  (:require [clojure.java.io :as io]
            [capr-nvnc.parse :as parse]
            [seesaw.core :as ui]
            [seesaw.chooser :as chooser]
            [seesaw.color :as color]
            [seesaw.icon :as icon])
  (:gen-class))

(def ^:private *state
  (atom {:width 640
         :height 480
         :split-ratio 2/3
         :curr-image nil
         :curr-text nil
         :story nil}))

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
  (ui/action :name "Load Story"
             :handler (fn [_] (swap! *state assoc :story
                                (chooser/choose-file
                                  :success-fn (fn [_ file] (.getAbsolutePath file)))))))

(def ^:private menu
  (ui/menubar :items [load-story]))

; listen (key-released), content, menubar
(def ^:private frame
  (ui/frame :title "NVNC"
            :icon "icon.png"
            :minimum-size [640 :by 480]
            :size [(:width @*state) :by (:height @*state)]
            :menubar menu
            :content root-container
            :on-close :exit))

(defn load-image [path]
  (->> (io/file path)
      icon/icon
      .getImage
      (swap! *state assoc :curr-image)))

(defn paint [c g]
  (when-let [image (:curr-image @*state)]
    (let [width (.getWidth image)
          left (- (/ (.getWidth c) 2) (/ width 2))
          top 0]
      (.drawImage g image left top nil))))

(defn init-ui []
  (ui/native!)
  (ui/config! imagebox :paint paint)
  (ui/show! frame)
  (.revalidate frame))

(comment
  (init-ui)
  (load-image "resources/kongou.jpg")
  @*state
  ,)
