(ns omnia.test-utils-test
  (:require [clojure.test :refer :all]
            [omnia.test-utils :refer :all]
            [omnia.repl.context :as r]
            [omnia.repl.view :as h]))

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
            persisted (-> context (r/persisted-view) (h/text) (:lines))
            input     (-> context (r/input-area) (:lines))
            viewable  (-> context (r/current-view) (h/project) (:lines))]
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
            persisted (-> context (r/persisted-view) (h/text) (:lines))
            input     (-> context (r/input-area) (:lines))
            viewable  (-> context (r/current-view) (h/project) (:lines))]
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
            persisted (-> context (r/persisted-view) (h/text) (:lines))
            input     (-> context (r/input-area) (:lines))
            viewable  (-> context (r/current-view) (h/project) (:lines))]
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
            persisted  (-> context (r/persisted-view) (h/text) (:lines))
            input      (-> context (r/input-area) (:lines))
            viewable   (-> context (r/current-view) (h/project) (:lines))
            view-off   (-> context (r/current-view) (h/view-offset))
            scroll-off (-> context (r/current-view) (h/scroll-offset))]
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
          persisted (-> context (r/persisted-view) (h/text) (:lines))
          input     (-> context (r/input-area) (:lines))
          viewable  (-> context (r/current-view) (h/project) (:lines))]
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
            persisted  (-> context (r/persisted-view) (h/text) (:lines))
            input      (-> context (r/input-area) (:lines))
            viewable   (-> context (r/current-view) (h/project) (:lines))
            highlights (-> context (r/highlights) (:manual) (:region))
            ys         (-> header (count) (+ 2))
            ye         (inc ys)]
        (is (= persisted (concat header [[\p \e \r \s \i \s \t \e \d] [\a \r \e \a]])))
        (is (= input [[\b \e \h \i \n \d] [\a \r \e \a] [\i \n \p \u \t] [\a \r \e \a]]))
        (is (= viewable [[\p \e \r \s \i \s \t \e \d] [\a \r \e \a] [\b \e \h \i \n \d] [\a \r \e \a] [\i \n \p \u \t] [\a \r \e \a]]))
        (is (= highlights {:start [2 ys] :end [4 ye]}))))

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
            persisted (-> context (r/persisted-view) (h/text) (:lines))
            input     (-> context (r/input-area) (:lines))
            viewable  (-> context (r/current-view) (h/project) (:lines))]
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
            persisted (-> context (r/persisted-view) (h/text) (:lines))
            input     (-> context (r/input-area) (:lines))
            viewable  (-> context (r/current-view) (h/project) (:lines))]
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
            persisted   (-> context (r/persisted-view) (h/text) (:lines))
            input       (-> context (r/input-area) (:lines))
            viewable    (-> context (r/current-view) (h/project) (:lines))
            lights      (-> context (r/highlights) (:manual) (:region))
            proj-lights (-> context (r/current-view) (h/project-selection lights))
            ys          (-> header (count) (+ 2))
            ye          (inc ys)]
        (is (= persisted (concat header [[\p \e \r \s \i \s \t \e \d] [\a \r \e \a]])))
        (is (= input [[\b \e \h \i \n \d] [\a \r \e \a] [\i \n \p \u \t] [\a \r \e \a]]))
        (is (= viewable [[\b \e \h \i \n \d] [\a \r \e \a]]))
        (is (= lights {:start [2 ys] :end [4 ye]}))
        (is (= proj-lights {:start [2 0] :end [4 1]}))))

    (testing "view at the start"
      (let [context   (-> ["persisted"
                           -| "area"
                           ---
                           -| "input"
                           -| "area"
                           -+ "viewable|"]
                          (derive-context))
            header    (:lines default-header)
            persisted (-> context (r/persisted-view) (h/text) (:lines))
            input     (-> context (r/input-area) (:lines))
            viewable  (-> context (r/current-view) (h/project) (:lines))
            prev-off  (-> context (r/current-view) (h/view-offset))
            pers-off  (-> context (r/persisted-view) (h/view-offset))]
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
              persisted (-> context (r/persisted-view) (h/text) (:lines))
              input     (-> context (r/input-area) (:lines))
              viewable  (-> context (r/current-view) (h/project) (:lines))
              prev-off  (-> context (r/current-view) (h/view-offset))
              pers-off  (-> context (r/persisted-view) (h/view-offset))]
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
              persisted (-> context (r/persisted-view) (h/text) (:lines))
              input     (-> context (r/input-area) (:lines))
              viewable  (-> context (r/current-view) (h/project) (:lines))
              prev-off  (-> context (r/current-view) (h/view-offset))
              pers-off  (-> context (r/persisted-view) (h/view-offset))]
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
              persisted (-> context (r/persisted-view) (h/text) (:lines))
              input     (-> context (r/input-area) (:lines))
              viewable  (-> context (r/current-view) (h/project) (:lines))
              prev-off  (-> context (r/current-view) (h/view-offset))
              pers-off  (-> context (r/persisted-view) (h/view-offset))]
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
              persisted (-> context (r/persisted-view) (h/text) (:lines))
              input     (-> context (r/input-area) (:lines))
              viewable  (-> context (r/current-view) (h/project) (:lines))
              prev-off  (-> context (r/current-view) (h/view-offset))
              pers-off  (-> context (r/persisted-view) (h/view-offset))]
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
            persisted (-> context (r/persisted-view) (h/text) (:lines))
            input     (-> context (r/input-area) (:lines))
            viewable  (-> context (r/current-view) (h/project) (:lines))
            prev-off  (-> context (r/current-view) (h/view-offset))
            pers-off  (-> context (r/persisted-view) (h/view-offset))]
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
                  :selection {:start [1 0] :end [2 0]}}}

        {:input  ["1⦇23" "⦈|45"]
         :expect {:lines     [[\1 \2 \3] [\4 \5]]
                  :cursor    [0 1]
                  :selection {:start [1 0] :end [0 1]}}}

        {:input  ["123⦇" "⦈|45"]
         :expect {:lines     [[\1 \2 \3] [\4 \5]]
                  :cursor    [0 1]
                  :selection {:start [3 0] :end [0 1]}}}

        {:input  ["1⦇23" "⦈|"]
         :expect {:lines     [[\1 \2 \3] []]
                  :cursor    [0 1]
                  :selection {:start [1 0] :end [0 1]}}}]
       (run! (fn [{:keys [input expect]}]
               (let [text (derive-text input)]
                 (is (= (:lines text) (:lines expect)))
                 (is (= (:cursor text) (:cursor expect)))
                 (is (= (:selection text) (:selection expect))))))))