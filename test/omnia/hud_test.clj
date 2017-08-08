(ns omnia.hud-test
  (require [clojure.test :refer [is]]
           [clojure.test.check.clojure-test :refer [defspec]]
           [clojure.test.check.properties :refer [for-all]]
           [omnia.test-utils :refer :all]
           [clojure.test.check.generators :as gen]
           [omnia.config :refer [default-keymap]]
           [omnia.highlight :refer [default-colourscheme]]
           [omnia.more :refer [--]]
           [omnia.hud :as h]
           [omnia.input :as i]))

(defprotocol TRowed
  (getRows [_]))

(defprotocol TSized
  (getTerminalSize [_]))

(defn test-terminal [size]
  (reify TSized
    (getTerminalSize [_]
      (reify TRowed
        (getRows [_] size)))))

(def ctx (h/context {:terminal nil
                     :repl nil
                     :keymap default-keymap
                     :colourscheme default-colourscheme}))

(defn gen-context [{:keys [size fov seeker]
                    :or {fov 10
                         seeker (:seeker ctx)}}]
  (assert (not (nil? size)) "A context must always have a hud size")
  (gen/fmap
    (fn [hud-seeker]
      (let [hud (h/hud fov hud-seeker)]
        (-> ctx
            (assoc :terminal (test-terminal fov)
                   :seeker seeker
                   :complete-hud hud
                   :persisted-hud hud
                   :previous-hud hud)
            (h/rebase seeker)
            (h/remember)))) (gen-seeker-of size)))

;; I. Calibrating

(defn event [action key]
  (i/->Event action key))

(def up (event :up :up))
(def down (event :down :down))
(def left (event :left :left))
(def right (event :right :right))
(def select-down (event :select-down :down))
(def select-up (event :select-up :up))
(def copy (event :copy \c))
(def paste (event :paste \v))
(def backspace (event :backspace :backspace))
(def enter (event :enter :enter))
(defn char-key [k] (event :char k))

(defn process
  ([ctx event]
    (process ctx event 1))
  ([ctx event n]
   (->> (range 0 n)
        (reduce (fn [nctx _] (second (h/process nctx event))) ctx))))

(defn ov [ctx]
  (get-in ctx [:complete-hud :ov]))

(defn move-end-fov [ctx]
  (->> (update ctx :seeker (comp i/start-x i/end))
       (h/rebase)
       (h/remember)))

(defn move-top-fov [ctx]
  (let [fov (get-in ctx [:complete-hud :fov])
        top #(-- % (dec fov))]                              ;; (dec) because you want to land on the fov'th line
    (-> (move-end-fov ctx)
        (update :seeker #(i/move-y % top))
        (h/rebase)
        (h/remember))))

(defn move-bottom-fov [ctx]
  (let [fov (get-in ctx [:complete-hud :fov])
        bottom #(+ % (dec fov))]
    (-> (update ctx :seeker #(i/move-y % bottom))
        (h/rebase)
        (h/remember))))

(defn from-start [ctx]
  (-> ctx
      (update :persisted-hud i/start-x)
      (update :seeker i/start-x)
      (h/rebase)
      (h/remember)))


(defn from-end [ctx]
  (-> ctx
      (update :persisted-hud i/end-x)
      (update :seeker i/end-x)
      (h/rebase)
      (h/remember)))

(defn exceed-upper-bound [ctx]
  (-> (move-top-fov ctx)
      (can-be #(-> % (process up) (ov) (= 1))
              #(-> % (process up 2) (ov) (= 2))
              #(-> % (process up 3) (ov) (= 3))
              #(-> % (process up 4) (ov) (= 3)))))

(defn exceed-lower-bound [ctx]
  (-> (move-top-fov ctx)
      (process up 2)
      (move-bottom-fov)
      (can-be #(= (ov %) 2)
              #(-> % (process down) (ov) (= 1))
              #(-> % (process down 2) (ov) (= 0))
              #(-> % (process down 3) (ov) (= 0)))))

(defn scroll-upper-bound [ctx]
  (-> (move-top-fov ctx)
      (process up)
      (process down 6)
      (can-be #(= (ov %) 1))))

(defn scroll-lower-bound [ctx]
  (-> (move-top-fov ctx)
      (process up)
      (move-bottom-fov)
      (process up 6)
      (can-be #(= (ov %) 1))))


(defn correct-under-deletion-top [ctx]
  (-> (from-start ctx)
      (move-top-fov)
      (process up 2)
      (can-be #(-> % (process select-down) (process backspace) (ov) (= 1))
              #(-> % (process select-down 2) (process backspace) (ov) (= 0))
              #(-> % (process select-down 3) (process backspace) (ov) (= 0)))))

(defn correct-under-deletion-end [ctx]
  (-> (from-start ctx)
      (move-end-fov)
      (can-be #(= (ov %) 0)
              #(-> % (process up) (process select-down) (process backspace) (ov) (= 0))
              #(-> % (process up 2) (process select-down) (process backspace) (ov) (= 0)))))

(defn correct-under-deletion-bottom [ctx]
  (-> (from-start ctx)
      (move-top-fov)
      (process up 2)
      (move-bottom-fov)
      (can-be #(= (ov %) 2)
              #(-> % (process up) (process select-down) (process backspace) (ov) (= 1))
              #(-> % (process up 2) (process select-down 2) (process backspace) (ov) (= 0))
              #(-> % (process select-down) (process backspace) (ov) (= 0)))))

(defn correct-under-deletion-in-multi-line [ctx]
  (-> (from-start ctx)
      (move-top-fov)
      (process up 2)
      (process down 4)
      (can-be #(-> % (process select-down) (process backspace) (ov) (= 1))
              #(-> % (process select-down 2) (process backspace) (ov) (= 0)))))

(defn correct-under-insertion-top [ctx]
  (-> (move-top-fov ctx)
      (can-be #(-> % (process enter) (ov) (= 1))
              #(-> % (process enter) (process enter) (ov) (= 2)))))

(defn correct-under-insertion-bottom [ctx]
  (-> (from-end ctx)
      (move-top-fov)
      (process up 2)
      (move-bottom-fov)
      (process enter 10)
      (can-be #(= (ov %) 2))))

(defn correct-under-insertion-end [ctx]
  (-> (move-end-fov ctx)
      (process enter 10)
      (can-be #(= (ov %) 0))))

(defn correct-under-insertion-in-multi-line [ctx]
  (-> (move-top-fov ctx)
      (process up 2)
      (process down 3)
      (can-be #(= (ov %) 2)
              #(-> % (process enter) (ov) (= 3))
              #(-> % (process enter 2) (ov) (= 4)))))

(defn correct-under-multi-copied-insertion [ctx]
  (-> (from-end ctx)
      (move-top-fov)
      (process up 2)
      (move-bottom-fov)
      (process select-up 2)
      (process copy)
      (can-be #(-> % (process paste) (ov) (= 4))
              #(-> % (process down 2) (process paste) (ov) (= 2)))))

(defn correct-under-multi-selected-deletion [ctx]
  (-> (from-start ctx)
      (move-top-fov)
      (process up 2)
      (process down 3)
      (process select-up 2)
      (process backspace)
      (can-be #(= (ov %) 0))))

(defn correct-under-change-variance [ctx]
  (-> (from-end ctx)
      (move-top-fov)
      (process up 2)
      (process down 3)
      (process enter)
      (process select-down 2)
      (process backspace)
      (can-be #(= (ov %) 1)
              #(-> % (process enter 3) (process select-up 3) (process backspace) (ov) (= 0))
              #(-> % (process select-down 2) (process backspace) (ov) (= 0))
              #(-> % (process select-up 2) (process backspace) (ov) (= 0))
              #(-> % (process enter) (process select-up) (process backspace) (ov) (= 1)))))

(defn calibrating [ctx]
  (exceed-upper-bound ctx)
  (exceed-lower-bound ctx)
  (scroll-upper-bound ctx)
  (scroll-lower-bound ctx)
  (correct-under-deletion-top ctx)
  (correct-under-deletion-bottom ctx)
  (correct-under-deletion-end ctx)
  (correct-under-deletion-in-multi-line ctx)
  (correct-under-insertion-top ctx)
  (correct-under-insertion-bottom ctx)
  (correct-under-insertion-end ctx)
  (correct-under-insertion-in-multi-line ctx)
  (correct-under-multi-selected-deletion ctx)
  (correct-under-multi-copied-insertion ctx)
  (correct-under-change-variance ctx))

(defspec calibrating-test
         100
         (for-all [tctx (gen-context {:size 20
                                      :fov 7
                                      :seeker (just-one (gen-seeker-of 10))})]
                  (calibrating tctx)))

;; II. Scrolling

;; III. Capturing

;; IV. Clearing

;; V. Evaluating

;; VI. Rolling back

;; VII. Rolling forward

;; VIII. Suggesting

;; IX. Collecting highlights

;; X. Parenthesis matching

;; XI. Rendering