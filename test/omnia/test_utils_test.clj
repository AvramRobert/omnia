(ns omnia.test-utils-test
  (:require [clojure.test :refer :all]
            [omnia.test-utils :refer :all]
            [omnia.repl.context :as c]
            [omnia.repl.hud :as h]
            [omnia.repl.view :as v]
            [omnia.schema.render :refer [manual-highlight]]))

(deftest reads-context-from-definition
  (testing "can detect"
    (testing "input tag"
      (let [context   (-> ["persisted"
                           "area"
                           ---
                           "input"
                           "area|"]
                          (derive-context))
            header    (:lines default-header)
            persisted (-> context (c/hud) (h/persisted-view) (v/text) (:lines))
            input     (-> context (c/hud) (h/input-area) (:lines))
            viewable  (-> context (c/hud) (h/current-view) (v/project) (:lines))]
        (is (= persisted (concat header [[\p \e \r \s \i \s \t \e \d] [\a \r \e \a]])))
        (is (= input [[\i \n \p \u \t] [\a \r \e \a]]))
        (is (= viewable [[\p \e \r \s \i \s \t \e \d] [\a \r \e \a] [\i \n \p \u \t] [\a \r \e \a]]))))

    (testing "view and offset tags"
      (let [context   (-> ["behind"
                           -| "area|"
                           -| "input"
                           -+ "area"]
                          (derive-context))
            header    (:lines default-header)
            persisted (-> context (c/hud) (h/persisted-view) (v/text) (:lines))
            input     (-> context (c/hud) (h/input-area) (:lines))
            viewable  (-> context (c/hud) (h/current-view) (v/project) (:lines))]
        (is (= persisted header))
        (is (= input [[\b \e \h \i \n \d] [\a \r \e \a] [\i \n \p \u \t] [\a \r \e \a]]))
        (is (= viewable [[\a \r \e \a] [\i \n \p \u \t]]))))

    (testing "scroll offset tags"
      (let [context   (-> ["behind"
                           -| "area|"
                           -| "input"
                           -$ "area"]
                          (derive-context))
            header    (:lines default-header)
            persisted (-> context (c/hud) (h/persisted-view) (v/text) (:lines))
            input     (-> context (c/hud) (h/input-area) (:lines))
            viewable  (-> context (c/hud) (h/current-view) (v/project) (:lines))]
        (is (= persisted header))
        (is (= input [[\b \e \h \i \n \d] [\a \r \e \a] [\i \n \p \u \t] [\a \r \e \a]]))
        (is (= viewable [[\a \r \e \a] [\i \n \p \u \t]]))))

    (testing "input, view and scroll offset tags"
      (let [context    (-> ["persisted"
                            ---
                            "behind"
                            -| "area|"
                            -| "input"
                            -$ "area"
                            -+ "hidden"]
                           (derive-context))
            header     (:lines default-header)
            persisted  (-> context (c/hud) (h/persisted-view) (v/text) (:lines))
            input      (-> context (c/hud) (h/input-area) (:lines))
            viewable   (-> context (c/hud) (h/current-view) (v/project) (:lines))
            view-off   (-> context (c/hud) (h/current-view) (v/view-offset))
            scroll-off (-> context (c/hud) (h/current-view) (v/scroll-offset))]
        (is (= persisted (concat header [[\p \e \r \s \i \s \t \e \d]])))
        (is (= input [[\b \e \h \i \n \d] [\a \r \e \a] [\i \n \p \u \t] [\a \r \e \a] [\h \i \d \d \e \n]]))
        (is (= viewable [[\a \r \e \a] [\i \n \p \u \t]]))
        (is (= view-off 1))
        (is (= scroll-off 1)))))

  (testing "can ignore all tags"
    (let [context   (-> ["behind"
                         "area"
                         "input|"
                         "area"]
                        (derive-context))
          header    (:lines default-header)
          persisted (-> context (c/hud) (h/persisted-view) (v/text) (:lines))
          input     (-> context (c/hud) (h/input-area) (:lines))
          viewable  (-> context (c/hud) (h/current-view) (v/project) (:lines))]
      (is (= persisted header))
      (is (= input [[\b \e \h \i \n \d] [\a \r \e \a] [\i \n \p \u \t] [\a \r \e \a]]))
      (is (= viewable [[\b \e \h \i \n \d] [\a \r \e \a] [\i \n \p \u \t] [\a \r \e \a]]))))

  (testing "detects complete viewable input"
    (testing "with highlights"
      (let [context    (-> [-| "persisted"
                            -| "area"
                            ---
                            -| "be⦇hind"
                            -| "area⦈"
                            -| "input|"
                            -| "area"]
                           (derive-context))
            header     (:lines default-header)
            persisted  (-> context (c/hud) (h/persisted-view) (v/text) (:lines))
            input      (-> context (c/hud) (h/input-area) (:lines))
            viewable   (-> context (c/hud) (h/current-view) (v/project) (:lines))
            highlights (-> context (c/hud) (h/highlights) (get manual-highlight) (:region))
            ys         (-> header (count) (+ 2))
            ye         (inc ys)]
        (is (= persisted (concat header [[\p \e \r \s \i \s \t \e \d] [\a \r \e \a]])))
        (is (= input [[\b \e \h \i \n \d] [\a \r \e \a] [\i \n \p \u \t] [\a \r \e \a]]))
        (is (= viewable [[\p \e \r \s \i \s \t \e \d] [\a \r \e \a] [\b \e \h \i \n \d] [\a \r \e \a] [\i \n \p \u \t] [\a \r \e \a]]))
        (is (= highlights {:from [2 ys] :until [4 ye]}))))

    (testing "engulfing persisted area"
      (let [context   (-> [-| "persisted"
                           -| "area"
                           ---
                           -| "input"
                           -| "area"
                           -| "viewable"
                           -| "input|"]
                          (derive-context))
            header    (:lines default-header)
            persisted (-> context (c/hud) (h/persisted-view) (v/text) (:lines))
            input     (-> context (c/hud) (h/input-area) (:lines))
            viewable  (-> context (c/hud) (h/current-view) (v/project) (:lines))]
        (is (= persisted (concat header [[\p \e \r \s \i \s \t \e \d] [\a \r \e \a]])))
        (is (= input [[\i \n \p \u \t] [\a \r \e \a] [\v \i \e \w \a \b \l \e] [\i \n \p \u \t]]))
        (is (= viewable [[\p \e \r \s \i \s \t \e \d] [\a \r \e \a] [\i \n \p \u \t] [\a \r \e \a] [\v \i \e \w \a \b \l \e] [\i \n \p \u \t]]))))

    (testing "omitting persisted area"
      (let [context   (-> ["persisted"
                           "area"
                           ---
                           -| "input"
                           -| "area"
                           -| "viewable"
                           -| "input|"]
                          (derive-context))
            header    (:lines default-header)
            persisted (-> context (c/hud) (h/persisted-view) (v/text) (:lines))
            input     (-> context (c/hud) (h/input-area) (:lines))
            viewable  (-> context (c/hud) (h/current-view) (v/project) (:lines))]
        (is (= persisted (concat header [[\p \e \r \s \i \s \t \e \d] [\a \r \e \a]])))
        (is (= input [[\i \n \p \u \t] [\a \r \e \a] [\v \i \e \w \a \b \l \e] [\i \n \p \u \t]]))
        (is (= viewable [[\i \n \p \u \t] [\a \r \e \a] [\v \i \e \w \a \b \l \e] [\i \n \p \u \t]])))))

  (testing "detects partially viewable input"
    (testing "with highlights"
      (let [context     (-> ["persisted"
                             "area"
                             ---
                             -| "be⦇hind"
                             -| "area⦈|"
                             -+ "input"
                             -+ "area"]
                            (derive-context))
            header      (:lines default-header)
            persisted   (-> context (c/hud) (h/persisted-view) (v/text) (:lines))
            input       (-> context (c/hud) (h/input-area) (:lines))
            viewable    (-> context (c/hud) (h/current-view) (v/project) (:lines))
            lights      (-> context (c/hud) (h/highlights) (get manual-highlight) (:region))
            proj-lights (-> context (c/hud) (h/current-view) (v/project-selection lights))
            ys          (-> header (count) (+ 2))
            ye          (inc ys)]
        (is (= persisted (concat header [[\p \e \r \s \i \s \t \e \d] [\a \r \e \a]])))
        (is (= input [[\b \e \h \i \n \d] [\a \r \e \a] [\i \n \p \u \t] [\a \r \e \a]]))
        (is (= viewable [[\b \e \h \i \n \d] [\a \r \e \a]]))
        (is (= lights {:from [2 ys] :until [4 ye]}))
        (is (= proj-lights {:from [2 0] :until [4 1]}))))

    (testing "view at the start"
      (let [context   (-> ["persisted"
                           -| "area"
                           ---
                           -| "input"
                           -| "area"
                           -+ "viewable|"]
                          (derive-context))
            header    (:lines default-header)
            persisted (-> context (c/hud) (h/persisted-view) (v/text) (:lines))
            input     (-> context (c/hud) (h/input-area) (:lines))
            viewable  (-> context (c/hud) (h/current-view) (v/project) (:lines))
            prev-off  (-> context (c/hud) (h/current-view) (v/view-offset))
            pers-off  (-> context (c/hud) (h/persisted-view) (v/view-offset))]
        (is (= persisted (concat header [[\p \e \r \s \i \s \t \e \d] [\a \r \e \a]])))
        (is (= input [[\i \n \p \u \t] [\a \r \e \a] [\v \i \e \w \a \b \l \e]]))
        (is (= viewable [[\a \r \e \a] [\i \n \p \u \t] [\a \r \e \a]]))
        (is (= prev-off pers-off 1))))

    (testing "view at the middle"
      (testing "with tagged empty line"
        (let [context   (-> ["persisted"
                             "area"
                             ---
                             "input"
                             -| "area"
                             -| "viewable|"
                             -|
                             -+ "input"]
                            (derive-context))
              header    (:lines default-header)
              persisted (-> context (c/hud) (h/persisted-view) (v/text) (:lines))
              input     (-> context (c/hud) (h/input-area) (:lines))
              viewable  (-> context (c/hud) (h/current-view) (v/project) (:lines))
              prev-off  (-> context (c/hud) (h/current-view) (v/view-offset))
              pers-off  (-> context (c/hud) (h/persisted-view) (v/view-offset))]
          (is (= persisted (concat header [[\p \e \r \s \i \s \t \e \d] [\a \r \e \a]])))
          (is (= input [[\i \n \p \u \t] [\a \r \e \a] [\v \i \e \w \a \b \l \e] [\i \n \p \u \t]]))
          (is (= viewable [[\i \n \p \u \t] [\a \r \e \a] [\v \i \e \w \a \b \l \e]]))
          (is (= prev-off pers-off 1))))

      (testing "with tagged offset line"
        (let [context   (-> ["persisted"
                             "area"
                             ---
                             -| "input"
                             -| "area"
                             "viewable|"
                             -+
                             -+ "input"]
                            (derive-context))
              header    (:lines default-header)
              persisted (-> context (c/hud) (h/persisted-view) (v/text) (:lines))
              input     (-> context (c/hud) (h/input-area) (:lines))
              viewable  (-> context (c/hud) (h/current-view) (v/project) (:lines))
              prev-off  (-> context (c/hud) (h/current-view) (v/view-offset))
              pers-off  (-> context (c/hud) (h/persisted-view) (v/view-offset))]
          (is (= persisted (concat header [[\p \e \r \s \i \s \t \e \d] [\a \r \e \a]])))
          (is (= input [[\i \n \p \u \t] [\a \r \e \a] [\v \i \e \w \a \b \l \e] [\i \n \p \u \t]]))
          (is (= viewable [[\i \n \p \u \t] [\a \r \e \a]]))
          (is (= prev-off pers-off 2))))

      (testing "with tagged empty and offset line"
        (let [context   (-> ["persisted"
                             "area"
                             ---
                             -| "input"
                             -| "area|"
                             -|
                             -+
                             -+ "input"]
                            (derive-context))
              header    (:lines default-header)
              persisted (-> context (c/hud) (h/persisted-view) (v/text) (:lines))
              input     (-> context (c/hud) (h/input-area) (:lines))
              viewable  (-> context (c/hud) (h/current-view) (v/project) (:lines))
              prev-off  (-> context (c/hud) (h/current-view) (v/view-offset))
              pers-off  (-> context (c/hud) (h/persisted-view) (v/view-offset))]
          (is (= persisted (concat header [[\p \e \r \s \i \s \t \e \d] [\a \r \e \a]])))
          (is (= input [[\i \n \p \u \t] [\a \r \e \a] [\i \n \p \u \t]]))
          (is (= viewable [[\p \e \r \s \i \s \t \e \d] [\a \r \e \a] [\i \n \p \u \t]]))
          (is (= prev-off pers-off 2))))

      (testing "without tagged empty line"
        (let [context   (-> ["persisted"
                             "area"
                             ---
                             "input"
                             -| "area"
                             -| "viewable|"
                             -+ "input"]
                            (derive-context))
              header    (:lines default-header)
              persisted (-> context (c/hud) (h/persisted-view) (v/text) (:lines))
              input     (-> context (c/hud) (h/input-area) (:lines))
              viewable  (-> context (c/hud) (h/current-view) (v/project) (:lines))
              prev-off  (-> context (c/hud) (h/current-view) (v/view-offset))
              pers-off  (-> context (c/hud) (h/persisted-view) (v/view-offset))]
          (is (= persisted (concat header [[\p \e \r \s \i \s \t \e \d] [\a \r \e \a]])))
          (is (= input [[\i \n \p \u \t] [\a \r \e \a] [\v \i \e \w \a \b \l \e] [\i \n \p \u \t]]))
          (is (= viewable [[\a \r \e \a] [\v \i \e \w \a \b \l \e]]))
          (is (= prev-off pers-off 1)))))

    (testing "view at the end"
      (let [context   (-> ["persisted"
                           "area"
                           ---
                           "input"
                           -| "area"
                           -| "viewable"
                           -| "input|"]
                          (derive-context))
            header    (:lines default-header)
            persisted (-> context (c/hud) (h/persisted-view) (v/text) (:lines))
            input     (-> context (c/hud) (h/input-area) (:lines))
            viewable  (-> context (c/hud) (h/current-view) (v/project) (:lines))
            prev-off  (-> context (c/hud) (h/current-view) (v/view-offset))
            pers-off  (-> context (c/hud) (h/persisted-view) (v/view-offset))]
        (is (= persisted (concat header [[\p \e \r \s \i \s \t \e \d] [\a \r \e \a]])))
        (is (= input [[\i \n \p \u \t] [\a \r \e \a] [\v \i \e \w \a \b \l \e] [\i \n \p \u \t]]))
        (is (= viewable [[\a \r \e \a] [\v \i \e \w \a \b \l \e] [\i \n \p \u \t]]))
        (is (= prev-off pers-off 0))))))

(deftest reads-text-from-lines
  (->> [{:input  ["123"]
         :expect {:lines     [[\1 \2 \3]]
                  :cursor    [0 0]
                  :selection nil}}
        {:input  ["1" "2|"]
         :expect {:lines     [[\1] [\2]]
                  :cursor    [1 1]
                  :selection nil}}

        {:input  ["1|" "2"]
         :expect {:lines     [[\1] [\2]]
                  :cursor    [1 0]
                  :selection nil}}

        {:input  ["123" ""]
         :expect {:lines     [[\1 \2 \3] []]
                  :cursor    [0 0]
                  :selection nil}}

        {:input  ["12" "" "3|2" "4"]
         :expect {:lines     [[\1 \2] [] [\3 \2] [\4]]
                  :cursor    [1 2]
                  :selection nil}}

        {:input  ["1⦇2⦈|3"]
         :expect {:lines     [[\1 \2 \3]]
                  :cursor    [2 0]
                  :selection {:from [1 0] :until [2 0]}}}

        {:input  ["1⦇23" "⦈|45"]
         :expect {:lines     [[\1 \2 \3] [\4 \5]]
                  :cursor    [0 1]
                  :selection {:from [1 0] :until [0 1]}}}

        {:input  ["123⦇" "⦈|45"]
         :expect {:lines     [[\1 \2 \3] [\4 \5]]
                  :cursor    [0 1]
                  :selection {:from [3 0] :until [0 1]}}}

        {:input  ["1⦇23" "⦈|"]
         :expect {:lines     [[\1 \2 \3] []]
                  :cursor    [0 1]
                  :selection {:from [1 0] :until [0 1]}}}]
       (run! (fn [{:keys [input expect]}]
               (let [text (derive-text input)]
                 (is (= (:lines text) (:lines expect)))
                 (is (= (:cursor text) (:cursor expect)))
                 (is (= (:selection text) (:selection expect))))))))