(ns omnia.bench.repl-bench
  (:require [omnia.bench.common :as c]
            [omnia.test-utils :refer :all]))

(defn repl-actions [ctx]
  (let [insertion  (fn [ctx s] (reduce #(process %1 (char-key %2)) ctx s))
        navigation (fn [ctx off] (-> ctx
                                     (process down off)
                                     (process up off)
                                     (process right off)
                                     (process left off)))
        suggestion (fn [ctx am] (-> ctx (process suggest am)))
        selection  (fn [ctx am] (-> ctx
                                    (move-start-fov)
                                    (process select-right am)
                                    (process select-down am)))
        selection* (fn [ctx] (-> ctx (process select-all)))
        deletion   (fn [ctx] (-> ctx (process backspace)))
        evaluation (fn [ctx] (-> ctx (process evaluate)))]
    (-> ctx
        (insertion "(hello")
        (insertion "[WORLD]")
        (navigation 10)
        (suggestion 5)
        (selection 2)
        (selection*)
        (deletion)
        (insertion "(println")
        (evaluation))))


(defn repl-bench []
  (let [ctx (-> {:size    20
                 :fov     17
                 :seeker  (one (gen-seeker-of 10))
                 :receive (one (gen-seeker-of 15))}
                (gen-context)
                (one))]
    (c/quick-bench (fn [] (repl-actions ctx)) 100)))