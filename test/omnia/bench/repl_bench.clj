(ns omnia.bench.repl-bench
  (:require [omnia.bench.common :as c]
            [omnia.test-utils :refer :all]))

(defn repl-actions [ctx]
  (let [insertion  (fn [ctx s] (reduce #(process-one %1 (character %2)) ctx s))
        navigation (fn [ctx off] (-> ctx
                                     (process-one down off)
                                     (process-one up off)
                                     (process-one right off)
                                     (process-one left off)))
        suggestion (fn [ctx am] (-> ctx (process-one suggest am)))
        selection  (fn [ctx am] (-> ctx
                                    (at-input-start)
                                    (process-one select-right am)
                                    (process-one select-down am)))
        selection* (fn [ctx] (-> ctx (process-one select-all)))
        deletion   (fn [ctx] (-> ctx (process-one backspace)))
        evaluation (fn [ctx] (-> ctx (process-one evaluate)))]
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