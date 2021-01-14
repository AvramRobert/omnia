(ns omnia.hud
  (:require [schema.core :as s]
            [omnia.input :as i]
            [omnia.more :refer [Point Region omnia-version ++ -- mod* inc< dec<]]
            [omnia.input :refer [Seeker]]
            [omnia.terminal :refer [Terminal]]
            [omnia.nrepl :refer [REPLClient]]))

(def caret (i/from-string "Ω =>"))
(def continuation (i/from-string "..."))
(def delimiter (i/from-string "------"))

(def Hud
  {:text          Seeker
   :field-of-view s/Int
   :scroll-offset s/Int
   :view-offset   s/Int})

(s/defn hud :- Hud
  "A hud is a structure enclosing some form of text that supports
   projecting that text within a bounded view.
   It uses the following attributes to keep track of the projection.

   field-of-view:
      * the size of the projected view. Amount of lines that can be viewed at one time.

   view-offset:
      * offset withing the bounded view.
      * Even though the view is bounded by `field-of-view`, the cursor can still navigate the entire text.
        To move the view properly, this offset keeps track of when the cursor has moved beyond the bounds
        of `field-of-view` and by how many lines

   scroll-offset:
      * when scrolling, how many lines have been scrolled"
  [seeker :- Seeker
   fov    :- s/Int]
  {:text        seeker
   :field-of-view fov
   :view-offset   0
   :scroll-offset 0})

(s/defn hud-of :- Hud
  [fov :- s/Int]
  (hud i/empty-seeker fov))

(s/def empty-hud :- Hud
  (hud-of 0))

(s/defn view-offset :- s/Int
  [hud :- Hud]
  (:view-offset hud))

(s/defn field-of-view :- s/Int
  [hud :- Hud]
  (:field-of-view hud))

(s/defn scroll-offset :- s/Int
  [hud :- Hud]
  (:scroll-offset hud))

(s/defn text :- Seeker
  [hud :- Hud]
  (:text hud))

(s/defn hollow? :- s/Bool
  [hud :- Hud]
  (-> hud (text) (:lines) (empty?)))

(s/defn equivalent? :- s/Bool
  [this :- Hud, that :- Hud]
  (i/equivalent? (text this) (text that)))

(s/defn current-line :- [Character]
  [hud :- Hud]
  (-> hud (text) (i/line)))

(s/defn engulfed-size :- s/Int
  [hud :- Hud]
  (let [fov      (field-of-view hud)
        s-off    (scroll-offset hud)
        v-off    (view-offset hud)]
    (+ fov v-off s-off)))

(s/defn bottom-y :- s/Int
  [hud :- Hud]
  "The lower y bound of a page (exclusive)
  bottom-y = height - view-offset - 1
  Subtract 1 because we count from 0"
  (let [v-off  (view-offset hud)
        height (-> hud (text) (:height))]
    (-- height v-off 1)))

(s/defn top-y :- s/Int
  [hud :- Hud]
  "The upper y bound of a page (inclusive)
  top-y = (height - fov - ov)"
  (let [fov    (field-of-view hud)
        v-off  (view-offset hud)
        height (-> hud (text) (:height))]
    (-- height fov v-off)))

(s/defn project-y :- s/Int
  [hud :- Hud, y :- s/Int]
  "given hud-y, screen-y = hud-y - top-y
   given screen-y, hud-y = screen-y + top-y"
  (let [fov (field-of-view hud)
        h   (-> hud (text) (:height))
        ys  (top-y hud)]
    (if (> h fov) (- y ys) y)))

(s/defn project-cursor :- Point
  [hud :- Hud, [x hy] :- Point]
  [x (project-y hud hy)])

(s/defn project-hud-cursor :- Point
  [hud :- Hud]
  (project-cursor hud (-> hud (text) (:cursor))))

(s/defn project-hud :- Seeker
  [hud :- Hud]
  (let [text          (text hud)
        fov           (field-of-view hud)
        v-off         (view-offset hud)
        s-off         (scroll-offset hud)
        viewable-chunk (+ fov v-off s-off)
        cursor         (project-hud-cursor hud)]
    (-> text
        (i/rebase #(->> % (take-last viewable-chunk) (take fov)))
        (i/reset-to cursor))))

(s/defn clip-selection :- Region
  [hud :- Hud
   selection :- Region]
  (let [fov             (field-of-view hud)
        h               (-> hud (text) (:height))
        [xs ys]         (:start selection)
        [xe ye]         (:end selection)
        top             (top-y hud)
        bottom          (bottom-y hud)
        unpaged?        (< h fov)
        clipped-top?    (< ys top)
        clipped-bottom? (> ye bottom)
        visible-top?    (<= top ys bottom)
        visible-bottom? (<= top ye bottom)
        end-bottom      (-> hud (text) (i/reset-y bottom) (i/end-x) (:cursor))]
    (cond
      unpaged?              selection
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

(s/defn project-selection :- Region
  "projecting y outside the bounds leads to:
   a) -n when upper bound is exceeded by n
   b) fov + n numbers, when lower bound exceeded by n"
  [hud :- Hud
   region :- Region]
  (-> hud
      (clip-selection region)
      (update :start (partial project-cursor hud))
      (update :end (partial project-cursor hud))))

(s/defn correct-between :- s/Int
  [hud :- Hud
   previous-hud :- Hud]
  (let [fov         (field-of-view hud)
        v-off       (view-offset hud)
        h           (-> hud (text) (:height))
        [_ y]       (-> hud (text) (:cursor))
        pfov        (field-of-view previous-hud)
        ph          (-> previous-hud (text) (:height))
        upper-y     (top-y hud)                             ;; the top viewable y
        lower-y     (bottom-y hud)                          ;; the lower viewable y
        over-upper? (< y upper-y)
        over-lower? (> y lower-y)
        at-lower?   (= y lower-y)
        smaller?    (< h ph)
        larger?     (> h ph)
        unpaged?    (and (<= h fov)
                         (<= ph fov))
        resized?    (and (not= pfov fov)
                         (not= 0 v-off))]
    (cond
      resized? (++ v-off (- pfov fov))                         ;; we've changed the terminal size
      unpaged? v-off                                           ;; we've not exceeded the fov
      (and larger? at-lower?) v-off                            ;; we've gotten bigger but we're still at the bottom
      (or larger? smaller?) (++ v-off (- h ph))                ;; we've changed in size
      over-upper? (++ v-off (- upper-y y))                     ;; we've exceeded the upper bound
      over-lower? (-- v-off (- y lower-y))                     ;; we've exceeded the lower bound
      :else v-off)))

(s/defn corrected :- Hud
  ([hud :- Hud]
   (corrected hud hud))
  ([hud :- Hud,
    previous-hud :- Hud]
   (assoc hud :view-offset (correct-between hud previous-hud))))

(s/defn enrich-with :- Hud
  [hud :- Hud, seekers :- [Seeker]]
  (update hud :text #(apply i/conjoin % seekers)))

(s/defn riffle-window :- Hud
  [seeker :- Seeker
   size :- s/Int]
  (let [content (->> seeker (i/start) (i/end-x))]
    (-> (hud-of size) (enrich-with [content]) (corrected))))

(s/defn riffle [hud :- Hud] :- Hud
  (let [[_ y]  (-> hud (text) :cursor)
        height (-> hud (text) :height)
        y'     (mod* (inc y) height)]
    (-> hud (update :text #(i/reset-y % y')) (corrected))))

(s/defn scroll-up :- Hud
  [hud :- Hud]
  (let [offset     (scroll-offset hud)
        chunk-size (engulfed-size hud)
        text-size  (-> hud (text) (:height))
        result     (if (>= chunk-size text-size) text-size (inc offset))]
    (assoc hud :scroll-offset result)))

(s/defn scroll-down :- Hud
  [hud :- Hud]
  (let [offset (scroll-offset hud)
        result (if (zero? offset) offset (dec offset))]
    (assoc hud :scroll-offset result)))

(s/defn scroll-stop :- Hud
  [hud :- Hud]
  (assoc hud :scroll-offset 0))

(s/defn view :- String
  [hud :- Hud]
  (-> hud (text) (i/debug-string)))

(s/defn deselect :- Hud
  [hud :- Hud]
  (update hud :text i/deselect))

(s/defn reset-view :- Hud
  ([hud :- Hud]
   (reset-view hud 0))
  ([hud :- Hud, overview :- s/Int]
   (assoc hud :view-offset overview)))

(s/defn resize :- Hud
  [hud :- Hud, size :- s/Int]
  (assoc hud :field-of-view size :scroll-offset 0))

(s/defn paginate :- Seeker
  [hud :- Hud]
  (let [truncated? (-> hud (view-offset) (zero?) (not))
        extend    #(if truncated? (i/adjoin % continuation) %)]
    (-> hud
        (project-hud)
        (extend)
        (i/indent 1))))

(s/defn pop-up :- Hud
  [hud :- Hud, embedded :- Hud]
  (let [text      (text hud)
        paginated (paginate embedded)
        ph        (:height text)
        top       (-> text (i/peer (fn [l [x & _]] (conj l x))))
        bottom    (-> text (i/peer (fn [_ [_ & r]] (drop (+ ph 2) r))))]
    (assoc hud :text (-> (i/conjoin top delimiter paginated)
                         (i/end-x)
                         (i/adjoin delimiter bottom)))))