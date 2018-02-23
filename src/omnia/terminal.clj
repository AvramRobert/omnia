(ns omnia.terminal
  (:require [lanterna.terminal :as t]
            [lanterna.input :as i]))

(defrecord Terminal
  [background!
   foreground!
   clear!
   size
   move!
   put!
   stop!
   start!
   keystroke!])

(defn background! [terminal colour]
  ((:background! terminal) colour))

(defn foreground! [terminal colour]
  ((:foreground! terminal) colour))

(defn clear! [terminal]
  ((:clear! terminal)))

(defn move! [terminal x y]
  ((:move! terminal) x y))

(defn put! [terminal ch x y]
  ((:put! terminal) ch x y))

(defn stop! [terminal]
  ((:stop! terminal)))

(defn start! [terminal]
  ((:start! terminal)))

(defn keystroke! [terminal]
  ((:keystroke! terminal)))

(defn size [terminal]
  ((:size terminal)))

(defn terminal [kind]
  (let [terminal (t/get-terminal kind)]
    (map->Terminal
      {:background! (fn [colour] (t/set-bg-color terminal colour))
       :foreground! (fn [colour] (t/set-fg-color terminal colour))
       :clear!      (fn [] (t/clear terminal))
       :size        (fn [] (-> terminal (.getTerminalSize) (.getRows)))
       :move!       (fn [x y] (t/move-cursor terminal x y))
       :put!        (fn [ch x y] (t/put-character terminal ch x y))
       :stop!       (fn [] (t/stop terminal))
       :start!      (fn [] (t/start terminal))
       :keystroke!  (fn [] (i/get-keystroke-blocking terminal))})))