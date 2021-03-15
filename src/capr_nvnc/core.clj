(ns capr-nvnc.core
  (:require [capr-nvnc.ui :as ui])
  (:gen-class))

(defn -main
  [& args]
  (ui/init-ui))
