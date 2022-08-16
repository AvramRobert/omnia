(ns omnia.repl.view
  (:require [schema.core :as s]
            [omnia.repl.text :as t]
            [omnia.util.arithmetic :refer [++ -- mod*]]
            [omnia.util.collection :refer [bounded-subvec assoc-new]]
            [omnia.schema.view :refer [View]]
            [omnia.schema.common :refer [Point Region]]
            [omnia.schema.text :refer [Text Line]]))

(def continuation (t/from-string "..."))
(def delimiter (t/from-string "------"))

(s/defn create-view :- View
  "A `View` is a structure enclosing some form of text that supports
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
  [text :- Text
   size :- s/Int]
  {:text          text
   :field-of-view size
   :view-offset   0
   :scroll-offset 0})

(s/defn empty-view-with-size :- View
  [size :- s/Int]
  (create-view t/empty-text size))

(s/def empty-view :- View
  (empty-view-with-size 0))

(s/defn view-offset :- s/Int
  [view :- View]
  (:view-offset view))

(s/defn field-of-view :- s/Int
  [view :- View]
  (:field-of-view view))

(s/defn scroll-offset :- s/Int
  [view :- View]
  (:scroll-offset view))

(s/defn text :- Text
  [view :- View]
  (:text view))

(s/defn reset-scroll :- View
  [view :- View]
  (assoc-new view :scroll-offset 0))

(s/defn reset-text :- View
  [view :- View
   text :- Text]
  (assoc view :text text))

(s/defn reset-view-offset :- View
  [view :- View]
  (assoc-new view :view-offset 0))

(s/defn resize :- View
  [view :- View, field-of-view :- s/Int]
  (assoc view :field-of-view field-of-view :scroll-offset 0))

(s/defn hollow? :- s/Bool
  [view :- View]
  (-> view (text) (:lines) (empty?)))

(s/defn with-view-offset :- View
  [view :- View, offset :- s/Int]
  (assoc view :view-offset offset))

(s/defn with-scroll-offset :- View
  [view :- View, offset :- s/Int]
  (assoc view :scroll-offset offset))

(s/defn current-line :- [Character]
  [view :- View]
  (-> view (text) (t/current-line)))

(s/defn total-offset :- s/Int
  [view :- View]
  (let [fov   (field-of-view view)
        s-off (scroll-offset view)
        v-off (view-offset view)]
    (+ fov v-off s-off)))

(s/defn bottom-y :- s/Int
  [view :- View]
  "The lower y bound of a page (exclusive)
  bottom-y = height - view-offset - 1
  Subtract 1 because we count from 0"
  (let [v-off  (view-offset view)
        height (-> view (text) (:size))]
    (-- height v-off 1)))

(s/defn top-y :- s/Int
  [view :- View]
  "The upper y bound of a page (inclusive)
  top-y = (height - fov - ov)"
  (let [fov    (field-of-view view)
        v-off  (view-offset view)
        height (-> view (text) (:size))]
    (-- height fov v-off)))

(s/defn project-y :- s/Int
  [view :- View, y :- s/Int]
  "given view-y, screen-y = view-y - top-y
   given screen-y, view-y = screen-y + top-y"
  (let [fov (field-of-view view)
        h   (-> view (text) (:size))
        ys  (top-y view)]
    (if (> h fov) (-- y ys) y)))

(s/defn project-cursor :- Point
  [view :- View, [x hy] :- Point]
  [x (project-y view hy)])

(s/defn project-view-cursor :- Point
  [view :- View]
  (project-cursor view (-> view (text) (:cursor))))

(s/defn project-view-text :- [Line]
  [view :- View]
  (let [text           (text view)
        fov            (field-of-view view)
        v-off          (view-offset view)
        s-off          (scroll-offset view)
        viewable-chunk (+ fov v-off s-off)
        y-start        (-- (:size text) viewable-chunk)
        y-end          (++ y-start fov)]
    (bounded-subvec (:lines text) y-start y-end)))

(s/defn project :- Text
  [view :- View]
  (-> view (project-view-text) (t/create-text) (t/reset-cursor (project-view-cursor view))))

(s/defn clip-selection :- Region
  [view :- View
   selection :- Region]
  (let [fov             (field-of-view view)
        h               (-> view (text) (:size))
        [xs ys]         (:from selection)
        [xe ye]         (:until selection)
        top             (top-y view)
        bottom          (bottom-y view)
        unpaged?        (< h fov)
        clipped-top?    (< ys top)
        clipped-bottom? (> ye bottom)
        visible-top?    (<= top ys bottom)
        visible-bottom? (<= top ye bottom)
        end-bottom      (-> view (text) (t/reset-y bottom) (t/end-x) (:cursor))]
    (cond
      unpaged? selection
      (and visible-top?
           visible-bottom?) selection
      (and visible-top?
           clipped-bottom?) {:from  [xs ys]
                             :until end-bottom}
      (and visible-bottom?
           clipped-top?) {:from  [0 top]
                          :until [xe ye]}
      :else {:from  [0 bottom]
             :until [0 bottom]})))

(s/defn project-selection :- Region
  "projecting y outside the bounds leads to:
   a) -n when upper bound is exceeded by n
   b) fov + n numbers, when lower bound exceeded by n"
  [view :- View
   region :- Region]
  (-> view
      (clip-selection region)
      (update :from (partial project-cursor view))
      (update :until (partial project-cursor view))))

(s/defn correct-between :- s/Int
  [view :- View
   previous-view :- View]
  (let [fov         (field-of-view view)
        v-off       (view-offset view)
        h           (-> view (text) (:size))
        [_ y]       (-> view (text) (:cursor))
        pfov        (field-of-view previous-view)
        ph          (-> previous-view (text) (:size))
        upper-y     (top-y view)                             ;; the top viewable y
        lower-y     (bottom-y view)                          ;; the lower viewable y
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

(s/defn corrected :- View
  ([view :- View]
   (corrected view view))
  ([view :- View,
    previous-view :- View]
   (assoc view :view-offset (correct-between view previous-view))))

(s/defn enrich-with :- View
  [view :- View, texts :- [Text]]
  (let [text (text view)]
    (->> texts (apply t/join text) (reset-text view))))

(s/defn riffle-window :- View
  [text :- Text
   size :- s/Int]
  (let [content (->> text (t/start) (t/end-x))]
    (-> (empty-view-with-size size) (enrich-with [content]) (corrected))))

(s/defn riffle :- View
  [view :- View]
  (let [text   (text view)
        [_ y]  (:cursor text)
        height (:size text)
        y'     (mod* (inc y) height)]
    (-> view
        (reset-text (t/reset-y text y'))
        (corrected))))

(s/defn scroll-up :- View
  [view :- View]
  (let [offset       (scroll-offset view)
        total-offset (total-offset view)
        text-size    (-> view (text) (t/size))
        result       (if (>= total-offset text-size) offset (inc offset))]
    (assoc-new view :scroll-offset result)))

(s/defn scroll-down :- View
  [view :- View]
  (let [offset (scroll-offset view)
        result (if (zero? offset) offset (dec offset))]
    (assoc-new view :scroll-offset result)))

(s/defn show :- String
  [view :- View]
  (-> view (text) (t/debug-string)))

(s/defn deselect :- View
  [view :- View]
  (->> view (text) (t/deselect) (reset-text view)))

(s/defn paginate :- Text
  [view :- View]
  (let [truncated? (-> view (view-offset) (zero?) (not))
        extend    #(if truncated? (t/append % continuation) %)]
    (-> view
        (project)
        (extend)
        (t/indent 1))))

(s/defn pop-up :- View
  [view :- View, embedded :- View]
  (let [text      (text view)
        paginated (paginate embedded)
        ph        (:size text)
        top       (-> text (t/peer (fn [l [x & _]] (conj l x))))
        bottom    (-> text (t/peer (fn [_ [_ & r]] (drop (+ ph 2) r))))]
    (assoc view :text (-> (t/join top delimiter paginated)
                          (t/end-x)
                          (t/append delimiter bottom)))))
