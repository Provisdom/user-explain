(ns provisdom.user-explain.core
  (:require
    [clojure.spec.alpha :as s]
    [clojure.walk :as walk]
    [clojure.core.match :as match]))

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

(defn first-problem
  [ed]
  (let [problem (-> ed
                    ::s/problems
                    (first)
                    (merge (select-keys ed [::s/value ::s/spec])))]
    (assoc problem ::pred (:pred problem))))

(defmacro defexplainer
  [sym problem-matcher & body]
  (let [matcher (cond-> problem-matcher
                  (and (::pred problem-matcher)) (update ::pred (comp form-as-match res)))]
    `(defn ~sym
       [explain-data#]
       (let [problem# (first-problem explain-data#)]
         (match/match [problem#]
           ~[matcher] (do ~@body)
           :else nil)))))

(comment
  (defexplainer
    contains-explainer
    {::pred #(contains? _ kw)
     :via   [::user & r]}
    (str "Missing " (stringify kw) "."))
  )

(defn default-explainer
  [explain-data]
  (when explain-data (with-out-str (s/explain-out explain-data))))

(defn explain-first
  ([spec val matchers] (explain-first spec val matchers default-explainer))
  ([spec val match-fns default-matcher]
   (let [explain-data (s/explain-data spec val)
         match-fns (conj (vec match-fns) default-matcher)]
     (some (fn [match-fn]
             (match-fn explain-data)) match-fns))))