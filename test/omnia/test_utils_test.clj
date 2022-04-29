(ns omnia.test-utils-test
  (:require [clojure.test :refer :all]
            [omnia.test-utils :refer :all]
            [omnia.repl.context :as r]
            [omnia.repl.hud :as h]))

(deftest reads-context-from-definition
  (testing "can ignore"
    (testing "view delimiter"
      (let [context   (-> ["persisted"
                           "area"
                           ---
                           "input"
                           "area|"]
                          (derive-context))
            header    (:lines default-header)
            persisted (-> context (r/persisted-hud) (h/text) (:lines))
            input     (-> context (r/input-area) (:lines))
            viewable  (-> context (r/preview-hud) (h/project-hud) (:lines))]
        (is (= persisted (concat header [[\p \e \r \s \i \s \t \e \d] [\a \r \e \a]])))
        (is (= input [[\i \n \p \u \t] [\a \r \e \a]]))
        (is (= viewable [[\p \e \r \s \i \s \t \e \d] [\a \r \e \a] [\i \n \p \u \t] [\a \r \e \a]]))))

    (testing "input delimiter"
      (let [context   (-> ["behind"
                           -x-
                           "area"
                           "input|"
                           -x-
                           "area"]
                          (derive-context))
            header    (:lines default-header)
            persisted (-> context (r/persisted-hud) (h/text) (:lines))
            input     (-> context (r/input-area) (:lines))
            viewable  (-> context (r/preview-hud) (h/project-hud) (:lines))]
        (is (= persisted header))
        (is (= input [[\b \e \h \i \n \d] [\a \r \e \a] [\i \n \p \u \t] [\a \r \e \a]]))
        (is (= viewable [[\a \r \e \a] [\i \n \p \u \t]]))))

    (testing "input delimiter and view delimiter"
      (let [context   (-> ["behind"
                           "area"
                           "input|"
                           "area"]
                          (derive-context))
            header    (:lines default-header)
            persisted (-> context (r/persisted-hud) (h/text) (:lines))
            input     (-> context (r/input-area) (:lines))
            viewable  (-> context (r/preview-hud) (h/project-hud) (:lines))]
        (is (= persisted header))
        (is (= input [[\b \e \h \i \n \d] [\a \r \e \a] [\i \n \p \u \t] [\a \r \e \a]]))
        (is (= viewable [[\b \e \h \i \n \d] [\a \r \e \a] [\i \n \p \u \t] [\a \r \e \a]])))))

  (testing "detects highlights"
    (let [context    (-> ["be⦇hind"
                          "area⦈"
                          "input|"
                          "area"]
                         (derive-context))
          header     (:lines default-header)
          persisted  (-> context (r/persisted-hud) (h/text) (:lines))
          input      (-> context (r/input-area) (:lines))
          viewable   (-> context (r/preview-hud) (h/project-hud) (:lines))
          highlights (-> context (r/highlights) (:selection) (:region))
          ys         (:size default-header)
          ye         (inc ys)]
      (is (= persisted header))
      (is (= input [[\b \e \h \i \n \d] [\a \r \e \a] [\i \n \p \u \t] [\a \r \e \a]]))
      (is (= viewable [[\b \e \h \i \n \d] [\a \r \e \a] [\i \n \p \u \t] [\a \r \e \a]]))
      (is (= highlights {:start [2 ys] :end [4 ye]}))))

  (testing "detects complete viewable input"
    (testing "engulfing persisted area"
      (let [context   (-> [-x-
                           "persisted"
                           "area"
                           ---
                           "input"
                           "area"
                           "viewable"
                           "input|"
                           -x-]
                          (derive-context))
            header    (:lines default-header)
            persisted (-> context (r/persisted-hud) (h/text) (:lines))
            input     (-> context (r/input-area) (:lines))
            viewable  (-> context (r/preview-hud) (h/project-hud) (:lines))]
        (is (= persisted (concat header [[\p \e \r \s \i \s \t \e \d] [\a \r \e \a]])))
        (is (= input [[\i \n \p \u \t] [\a \r \e \a] [\v \i \e \w \a \b \l \e] [\i \n \p \u \t]]))
        (is (= viewable [[\p \e \r \s \i \s \t \e \d] [\a \r \e \a] [\i \n \p \u \t] [\a \r \e \a] [\v \i \e \w \a \b \l \e] [\i \n \p \u \t]]))))

    (testing "omitting persisted area"
      (let [context   (-> ["persisted"
                           "area"
                           ---
                           -x-
                           "input"
                           "area"
                           "viewable"
                           "input|"
                           -x-]
                          (derive-context))
            header    (:lines default-header)
            persisted (-> context (r/persisted-hud) (h/text) (:lines))
            input     (-> context (r/input-area) (:lines))
            viewable  (-> context (r/preview-hud) (h/project-hud) (:lines))]
        (is (= persisted (concat header [[\p \e \r \s \i \s \t \e \d] [\a \r \e \a]])))
        (is (= input [[\i \n \p \u \t] [\a \r \e \a] [\v \i \e \w \a \b \l \e] [\i \n \p \u \t]]))
        (is (= viewable [[\i \n \p \u \t] [\a \r \e \a] [\v \i \e \w \a \b \l \e] [\i \n \p \u \t]])))))

  (testing "detects partial viewable input"
    (testing "view at the start"
      (testing "engulfing persisted area"
        (let [context   (-> ["persisted"
                             -x-
                             "area"
                             ---
                             "input"
                             "area"
                             "viewable|"
                             -x-]
                            (derive-context))
              header    (:lines default-header)
              persisted (-> context (r/persisted-hud) (h/text) (:lines))
              input     (-> context (r/input-area) (:lines))
              viewable  (-> context (r/preview-hud) (h/project-hud) (:lines))]
          (is (= persisted (concat header [[\p \e \r \s \i \s \t \e \d] [\a \r \e \a]])))
          (is (= input [[\i \n \p \u \t] [\a \r \e \a] [\v \i \e \w \a \b \l \e]]))
          (is (= viewable [[\a \r \e \a] [\i \n \p \u \t] [\a \r \e \a] [\v \i \e \w \a \b \l \e]]))))

      (testing "omitting persisted area"
        (let [context   (-> ["persisted"
                             "area"
                             ---
                             "input"
                             -x-
                             "area"
                             "viewable|"
                             -x-
                             "input"]
                            (derive-context))
              header    (:lines default-header)
              persisted (-> context (r/persisted-hud) (h/text) (:lines))
              input     (-> context (r/input-area) (:lines))
              viewable  (-> context (r/preview-hud) (h/project-hud) (:lines))]
          (is (= persisted (concat header [[\p \e \r \s \i \s \t \e \d] [\a \r \e \a]])))
          (is (= input [[\i \n \p \u \t] [\a \r \e \a] [\v \i \e \w \a \b \l \e] [\i \n \p \u \t]]))
          (is (= viewable [[\a \r \e \a] [\v \i \e \w \a \b \l \e]])))))

    (testing "view in the middle"
      (let [context   (-> ["persisted"
                           "area"
                           ---
                           "input"
                           -x-
                           "area"
                           "viewable|"
                           -x-
                           "input"]
                          (derive-context))
            header    (:lines default-header)
            persisted (-> context (r/persisted-hud) (h/text) (:lines))
            input     (-> context (r/input-area) (:lines))
            viewable  (-> context (r/preview-hud) (h/project-hud) (:lines))]
        (is (= persisted (concat header [[\p \e \r \s \i \s \t \e \d] [\a \r \e \a]])))
        (is (= input [[\i \n \p \u \t] [\a \r \e \a] [\v \i \e \w \a \b \l \e] [\i \n \p \u \t]]))
        (is (= viewable [[\a \r \e \a] [\v \i \e \w \a \b \l \e]]))))

    (testing "view at the end"
      (let [context   (-> ["persisted"
                           "area"
                           ---
                           "input"
                           -x-
                           "area"
                           "viewable"
                           "input|"
                           -x-]
                          (derive-context))
            header    (:lines default-header)
            persisted (-> context (r/persisted-hud) (h/text) (:lines))
            input     (-> context (r/input-area) (:lines))
            viewable  (-> context (r/preview-hud) (h/project-hud) (:lines))]
        (is (= persisted (concat header [[\p \e \r \s \i \s \t \e \d] [\a \r \e \a]])))
        (is (= input [[\i \n \p \u \t] [\a \r \e \a] [\v \i \e \w \a \b \l \e] [\i \n \p \u \t]]))
        (is (= viewable [[\a \r \e \a] [\v \i \e \w \a \b \l \e] [\i \n \p \u \t]]))))))
