(ns omnia.repl.hud
  (:require [schema.core :as s]
            [omnia.repl.text :as t]
            [omnia.util.arithmetic :refer [++ -- mod* inc< dec<]]
            [omnia.util.collection :refer [bounded-subvec assoc-new]]
            [omnia.util.misc :refer [omnia-version]]
            [omnia.schema.hud :refer [Hud]]
            [omnia.schema.common :refer [Point Region]]
            [omnia.schema.text :refer [Seeker Line]]
            [omnia.schema.nrepl :refer [NReplClient]]))

(def continuation (t/from-string "..."))
(def delimiter (t/from-string "------"))

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
  (hud t/empty-seeker fov))

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

(s/defn with-view-offset :- Hud
  [hud :- Hud, offset :- s/Int]
  (assoc hud :view-offset offset))

(s/defn with-scroll-offset :- Hud
  [hud :- Hud, offset :- s/Int]
  (assoc hud :scroll-offset offset))

(s/defn current-line :- [Character]
  [hud :- Hud]
  (-> hud (text) (t/current-line)))

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
        height (-> hud (text) (:size))]
    (-- height v-off 1)))

(s/defn top-y :- s/Int
  [hud :- Hud]
  "The upper y bound of a page (inclusive)
  top-y = (height - fov - ov)"
  (let [fov    (field-of-view hud)
        v-off  (view-offset hud)
        height (-> hud (text) (:size))]
    (-- height fov v-off)))

(s/defn project-y :- s/Int
  [hud :- Hud, y :- s/Int]
  "given hud-y, screen-y = hud-y - top-y
   given screen-y, hud-y = screen-y + top-y"
  (let [fov (field-of-view hud)
        h   (-> hud (text) (:size))
        ys  (top-y hud)]
    (if (> h fov) (-- y ys) y)))

(s/defn project-cursor :- Point
  [hud :- Hud, [x hy] :- Point]
  [x (project-y hud hy)])

(s/defn project-hud-cursor :- Point
  [hud :- Hud]
  (project-cursor hud (-> hud (text) (:cursor))))

(s/defn project-view :- [Line]
  [hud :- Hud]
  (let [text           (text hud)
        fov            (field-of-view hud)
        v-off          (view-offset hud)
        s-off          (scroll-offset hud)
        viewable-chunk (+ fov v-off s-off)
        y-start        (-- (:size text) viewable-chunk)
        y-end          (++ y-start fov)]
    (bounded-subvec (:lines text) y-start y-end)))

(s/defn project-hud :- Seeker
  [hud :- Hud]
  (-> hud (project-view) (t/seeker) (t/reset-to (project-hud-cursor hud))))

(s/defn clip-selection :- Region
  [hud :- Hud
   selection :- Region]
  (let [fov             (field-of-view hud)
        h               (-> hud (text) (:size))
        [xs ys]         (:start selection)
        [xe ye]         (:end selection)
        top             (top-y hud)
        bottom          (bottom-y hud)
        unpaged?        (< h fov)
        clipped-top?    (< ys top)
        clipped-bottom? (> ye bottom)
        visible-top?    (<= top ys bottom)
        visible-bottom? (<= top ye bottom)
        end-bottom      (-> hud (text) (t/reset-y bottom) (t/end-x) (:cursor))]
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
        h           (-> hud (text) (:size))
        [_ y]       (-> hud (text) (:cursor))
        pfov        (field-of-view previous-hud)
        ph          (-> previous-hud (text) (:size))
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
  (update hud :text #(apply t/join-many % seekers)))

(s/defn riffle-window :- Hud
  [seeker :- Seeker
   size :- s/Int]
  (let [content (->> seeker (t/start) (t/end-x))]
    (-> (hud-of size) (enrich-with [content]) (corrected))))

(s/defn riffle [hud :- Hud] :- Hud
  (let [[_ y]  (-> hud (text) :cursor)
        height (-> hud (text) :size)
        y'     (mod* (inc y) height)]
    (-> hud (update :text #(t/reset-y % y')) (corrected))))

(s/defn scroll-up :- Hud
  [hud :- Hud]
  (let [offset     (scroll-offset hud)
        chunk-size (engulfed-size hud)
        text-size  (-> hud (text) (:size))
        result     (if (>= chunk-size text-size) text-size (inc offset))]
    (assoc-new hud :scroll-offset result)))

(s/defn scroll-down :- Hud
  [hud :- Hud]
  (let [offset (scroll-offset hud)
        result (if (zero? offset) offset (dec offset))]
    (assoc-new hud :scroll-offset result)))

(s/defn scroll-stop :- Hud
  [hud :- Hud]
  (assoc-new hud :scroll-offset 0))

(s/defn view :- String
  [hud :- Hud]
  (-> hud (text) (t/debug-string)))

(s/defn deselect :- Hud
  [hud :- Hud]
  (update hud :text t/deselect))

(s/defn selection? :- s/Bool
  [hud :- Hud]
  (-> hud (text) (t/selecting?)))

(s/defn reset-view :- Hud
  ([hud :- Hud]
   (reset-view hud 0))
  ([hud :- Hud, overview :- s/Int]
   (assoc-new hud :view-offset overview)))

(s/defn resize :- Hud
  [hud :- Hud, size :- s/Int]
  (assoc hud :field-of-view size :scroll-offset 0))

(s/defn paginate :- Seeker
  [hud :- Hud]
  (let [truncated? (-> hud (view-offset) (zero?) (not))
        extend    #(if truncated? (t/append % continuation) %)]
    (-> hud
        (project-hud)
        (extend)
        (t/indent 1))))

(s/defn pop-up :- Hud
  [hud :- Hud, embedded :- Hud]
  (let [text      (text hud)
        paginated (paginate embedded)
        ph        (:size text)
        top       (-> text (t/peer (fn [l [x & _]] (conj l x))))
        bottom    (-> text (t/peer (fn [_ [_ & r]] (drop (+ ph 2) r))))]
    (assoc hud :text (-> (t/join-many top delimiter paginated)
                         (t/end-x)
                         (t/append delimiter bottom)))))