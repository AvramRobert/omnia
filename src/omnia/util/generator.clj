(ns omnia.util.generator
  (:require [clojure.test.check.generators :as gen]
            [omnia.schema.keymap :as km]
            [omnia.schema.syntax :as s]
            [omnia.config.defaults :as d]))

(defmacro do-gen [binding & body]
  (let [[[n# b#] & bound] (->> binding (destructure) (partition 2) (reverse))]
    (reduce
      (fn [expr [bn# bb#]]
        `(gen/bind ~bb# (fn [~bn#] ~expr))) `(gen/fmap (fn [~n#] ~@body) ~b#) bound)))

(defn gen-or-else [generator else]
  (gen/one-of [generator (gen/return else)]))

(def gen-rgb
  (gen/vector (gen/choose 0 255) 3))

(def gen-colour
  (gen/one-of [(gen/elements s/colours) gen-rgb]))

(def gen-input-key
  (gen/one-of [gen/char-alphanumeric (gen/elements km/key-set)]))

(def gen-user-highlighting
  (->> d/default-user-highlighting
       (mapcat (fn [[elem _]] [elem gen-colour]))
       (apply gen/hash-map)))

(def gen-user-key-binding
  (do-gen [key   gen-input-key
           ctrl  (gen-or-else gen/boolean nil)
           alt   (gen-or-else gen/boolean nil)
           shift (gen-or-else gen/boolean nil)]
    (cond-> {:key key}
            ctrl  (assoc :ctrl ctrl)
            alt   (assoc :alt alt)
            shift (assoc :shift shift))))

(def gen-user-keymap
  (->> d/default-user-keymap
       (mapcat (fn [[elem _]] [elem gen-user-key-binding]))
       (apply gen/hash-map)))

(def gen-user-terminal
  (do-gen [font-path (gen-or-else (gen/map (gen/return s/font-path) gen/string-alphanumeric) {})
           font-size (gen-or-else (gen/map (gen/return s/font-size) gen/nat) {})]
    (merge font-path font-size)))

(def gen-user-persistence
  (do-gen [error-path   (gen-or-else (gen/map (gen/return :error-file-path) gen/string-alphanumeric) {})
           history-path (gen-or-else (gen/map (gen/return :history-file-path) gen/string-alphanumeric) {})
           history-size (gen-or-else (gen/map (gen/return :history-size) gen/nat) {})]
    (merge error-path history-path history-size)))
