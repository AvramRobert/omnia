(ns omnia.core
  (:gen-class)
  (require [lanterna.screen :as s]
           [omnia.processing :as t])
  (:import (com.googlecode.lanterna.input PuttyProfile Key KeyMappingProfile CtrlAndCharacterPattern BasicCharacterPattern CommonProfile Key$Kind)
           (com.googlecode.lanterna.screen Screen)))


(comment
  "This is basically a good approach, but the version of lanterna in clojure-lanterna doesn't allow me to
  remap important key-bindings for me.
  The simplest way to get around this would be to either clone clojure-laterna and
  bump the version myself, develop directly using lanterna or use the newest version but write a very thin
  wrapper for it here.

  I shall try the first and the second. Maybe, with a little bit of luck, just cloning the thing and bumping the version
  would work.")

(def repl-profile
  (proxy [CommonProfile] []
    (getPatterns []
       (let [profile (new CommonProfile)]
         (.getPatterns profile)))))

(defn print! [screen seeker]
  (dorun
    (map-indexed
      (fn [idx line]
        (s/put-string screen 0 idx (apply str line))) (:lines seeker))))

(defn move! [screen seeker]
  (let [[x y] (:cursor seeker)]
    (s/move-cursor screen x y)))


(defn reads [screen seeker]
  (doto screen
    (s/clear)
    (print! seeker)
    (move! seeker)
    (s/redraw))
  (if-let [k (s/get-key-blocking screen)]
    (case k
      :escape (s/stop screen)
      (recur screen (t/inputs seeker k)))
    (recur screen seeker)))

(defn with-profile [screen ^KeyMappingProfile profile]
  (let [term (.getTerminal screen)
        _ (.addInputProfile term profile)]
    (new Screen term)))

(defn -main [& args]
  (let [screen (-> :text (s/get-screen)                     ;(with-profile repl-profile)
                   )
        _ (s/start screen)]
    ;(reads screen t/empty-seeker)
    (s/put-string screen 0 0 "HAHAAH" {:styles #{:bold}})
    (s/redraw screen)
    ))
