(ns provisdom.user-explain.core
  (:require
    [clojure.spec.alpha :as s]
    [clojure.walk :as walk]
    [clojure.core.match :as match]
    [flatland.ordered.map :as ordered-map]))

(comment
  (match/match '[(clojure.core/fn [%] (clojure.core/contains? % :workload/impacts))]
    [((`fn _ ((`contains? _ k) :seq)) :seq)] k)

  `(fn [x] (contains? x ::foo))
  [:min-count min :max-count max])

;; use the same ns-resolver as Spec.
(def res @#'s/res)

(defn form-as-match
  [form]
  (cond
    (seq? form)
    (walk/postwalk
      (fn [x]
        (cond
          (seq? x)
          (let [[fn-sym & args] x
                fn-sym `(quote ~fn-sym)
                new-x (cons fn-sym args)]
            (list new-x :seq))
          (= '% x)
          `(quote ~x)
          :else x))
      form)

    (symbol? form) `(quote ~form)
    :else form))

;; todo - parameterize this
(defonce matchers (atom (ordered-map/ordered-map)))

(defn add-explainer-form
  [matcher body-form]
  (swap! matchers (fn [matchers]
                    (assoc matchers
                      matcher
                      {:matcher matcher
                       :body    body-form}))))

(defmacro defexplainer
  [problem-matcher & body]
  (add-explainer-form
    (cond-> problem-matcher
      (:pred problem-matcher) (update :pred (comp form-as-match res)))
    (cons 'do body))
  nil)

(defmacro explain-first
  [spec val]
  (let [problem-matchers @matchers
        match-bindings (mapcat (fn [{:keys [matcher body]}]
                                 [[matcher] body])
                               (vals problem-matchers))]
    `(if-let [explain-data# (s/explain-data ~spec ~val)]
       (let [problems# (-> explain-data#
                           ::s/problems
                           (first)
                           (merge (select-keys explain-data# [::s/value ::s/spec])))]
         (match/match [problems#]
           ~@match-bindings))
       )))

(defn clear!
  []
  (reset! matchers (ordered-map/ordered-map)))