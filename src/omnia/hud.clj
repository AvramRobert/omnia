(ns omnia.hud
  (:require [schema.core :as s]
            [omnia.input :as i]
            [omnia.terminal :as t]
            [omnia.more :refer [Point Region omnia-version ++ -- mod* inc< dec<]]
            [omnia.input :refer [Seeker]]
            [omnia.terminal :refer [Terminal]]
            [omnia.server :refer [REPLServer]]))

(def clj-version (i/from-string (format "-- Clojure v%s --" (clojure-version))))
(def java-version (i/from-string (format "-- Java v%s --" (System/getProperty "java.version"))))
(def greeting (i/from-string (format "Welcome to Omnia! (Ω) v%s" (omnia-version))))
(def caret (i/from-string "Ω =>"))
(defn nrepl-info [host port] (i/from-string (str "-- nREPL server started on nrepl://" host ":" port " --")))
(def continuation (i/from-string "..."))

(def Hud
  {:seeker  Seeker
   :lor     s/Int
   :fov     s/Int
   :ov      s/Int
   :scroll? s/Bool})

(s/defn hud [fov & prelude] :- Hud
  "lor = line of reference
     indicates the amount of lines that have been viewed so far
     when going through the history. (seeker + hud)
   fov = field of view
     indicates the amount of lines that can be viewed at one time
     in the terminal screen
   ov  = overview
     indicates the amount of lines that have been viewed so far
     when going back just through the input seeker itself. (seeker)
     Conceptually it is the same as the `lor`, but only on input seeker level.
   scroll? = scrolling flag
     indicates if there should be scrolled currently"
  {:seeker  (i/conjoined prelude)
   :fov     fov
   :lor     fov
   :ov      0
   :scroll? false})

(def empty-hud
  (hud i/empty-seeker))

(s/defn init-hud [terminal :- Terminal,
                  repl     :- REPLServer] :- Hud
  (let [fov       (t/size terminal)
        repl-info (nrepl-info (:host repl) (:port repl))]
    (hud fov
         greeting
         repl-info
         clj-version
         java-version
         i/empty-line
         caret)))

(s/defn bottom-y [hud :- Hud] :- s/Int
  "The lower y bound of a page (exclusive)
  bottom-y = height - ov - 1
  Subtract 1 because we count from 0"
  (let [ov     (:ov hud)
        height (-> hud (:seeker) (:height))]
    (-- height ov 1)))

(s/defn top-y [hud :- Hud] :- s/Int
  "The upper y bound of a page (inclusive)
  top-y = (height - fov - ov)"
  (let [fov    (:fov hud)
        ov     (:ov hud)
        height (-> hud (:seeker) (:height))]
    (-- height fov ov)))

(s/defn project-y [hud :- Hud, y :- s/Int] :- s/Int
  "given hud-y, screen-y = hud-y - top-y
   given screen-y, hud-y = screen-y + top-y"
  (let [fov (:fov hud)
        h   (-> hud (:seeker) (:height))
        ys  (top-y hud)]
    (if (> h fov) (- y ys) y)))

(s/defn project-cursor [hud :- Hud] :- Point
  (let [[x hy] (-> hud :seeker :cursor)
        y      (project-y hud hy)]
    [x y]))

(s/defn project-selection [hud       :- Hud
                           selection :- Region] :- Region
  (let [fov             (:fov hud)
        h               (-> hud (:seeker) (:height))
        [xs ys]         (:start selection)
        [xe ye]         (:end selection)
        top             (top-y hud)
        bottom          (bottom-y hud)
        unpaged?        (< h fov)
        clipped-top?    (< ys top)
        clipped-bottom? (> ye bottom)
        visible-top?    (<= top ys bottom)
        visible-bottom? (<= top ye bottom)
        end-bottom      (-> hud (:seeker) (i/reset-y bottom) (i/end-x) (:cursor))]
    (cond
      unpaged? selection
      (and visible-top?
           visible-bottom?) selection
      (and visible-top?
           clipped-bottom?) {:start [xs ys]
                             :end   end-bottom}
      (and visible-bottom?
           clipped-top?) {:start [0 top]
                          :end   [xe ye]}
      :else {:start [0 bottom]
             :end   [0 bottom]})))

(s/defn project-hud [hud :- Hud] :- Seeker
  (let [{seeker  :seeker
         lor     :lor
         fov     :fov
         ov      :ov
         scroll? :scroll?} hud
        take-right (fn [n coll] (lazy-seq (take-last n coll)))]
    (if scroll?
      (i/rebase seeker #(->> % (take-right lor) (take fov)))
      (i/rebase seeker #(->> % (drop-last ov) (take-right fov))))))

(s/defn correct-ov
  [hud :- Hud
   previous-hud :- Hud] :- s/Int
  (let [{fov                       :fov
         ov                        :ov
         {h :height [_ y] :cursor} :seeker} hud
        {pfov         :fov
         {ph :height} :seeker} previous-hud
        upper-y     (top-y hud)                             ;; the top viewable y
        lower-y     (bottom-y hud)                          ;; the lower viewable y
        over-upper? (< y upper-y)
        over-lower? (> y lower-y)
        at-lower?   (= y lower-y)
        smaller?    (< h ph)
        smaller?    (< h ph)
        larger?     (> h ph)
        unpaged?    (and (<= h fov)
                         (<= ph fov))
        resized?    (and (not= pfov fov)
                         (not= 0 ov))]
    (cond
      resized? (++ ov (- pfov fov))                         ;; we've changed the terminal size
      unpaged? ov                                           ;; we've not exceeded the fov
      (and larger? at-lower?) ov                            ;; we've gotten bigger but we're still at the bottom
      (or larger? smaller?) (++ ov (- h ph))                ;; we've changed in size
      over-upper? (++ ov (- upper-y y))                     ;; we've exceeded the upper bound
      over-lower? (-- ov (- y lower-y))                     ;; we've exceeded the lower bound
      :else ov)))

(s/defn corrected
  ([hud :- Hud] :- Hud
   (corrected hud hud))
  ([hud :- Hud,
    previous-hud :- Hud] :- Hud
   (assoc hud :ov (correct-ov hud previous-hud))))

(s/defn paginate [hud :- Hud] :- Seeker
  (let [cursor (project-cursor hud)]
    (if (not (zero? (:ov hud)))
      (-> hud
          (update :seeker #(i/reset-to % cursor))
          (project-hud)
          (i/adjoin continuation)
          (i/indent 1))
      (-> hud
          (update :seeker #(i/reset-to % cursor))
          (project-hud)
          (i/indent 1)))))

(s/defn window [seeker :- Seeker
                size   :- s/Int] :- Hud
  (->> seeker (i/start) (i/end-x) (hud size) (corrected)))

(s/defn riffle [hud :- Hud] :- Hud
  (let [[_ y]   (-> hud :seeker :cursor)
        height  (-> hud :seeker :height)
        y'      (mod* (inc y) height)]
    (-> hud (update :seeker #(i/reset-y % y')) (corrected))))

;; === Screen scrolling ===

(s/defn nowards [hud :- Hud] :- s/Int
  (let [fov    (:fov hud)
        ov     (:ov hud)
        height (-> hud :seeker :height)]
    (if (> height fov) (+ fov ov) height)))

(s/defn upwards [hud :- Hud] :- s/Int
  (let [lor    (:lor hud)
        height (-> hud :seeker :height)]
    (inc< lor height)))

(s/defn downwards [hud :- Hud] :- s/Int
  (let [lor (:lor hud)]
    (dec< lor (nowards hud))))

(s/defn scroll [hud :- Hud f] :- Hud
  (-> hud
      (assoc :scroll? true)
      (assoc :lor (f hud))))

(s/defn scroll-stop [hud :- Hud] :- Hud
  (-> hud
      (scroll nowards)
      (assoc :scroll? false)))

(s/defn scroll-up [hud :- Hud] :- Hud
  (scroll hud upwards))

(s/defn scroll-down [hud :- Hud] :- Hud
  (scroll hud downwards))

(s/defn reseek [hud :- Hud, f] :- Hud
  (update hud :seeker f))

(s/defn hollow? [hud :- Hud] :- s/Bool
  (-> hud :seeker :lines empty?))