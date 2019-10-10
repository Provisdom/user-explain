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

(defn get-spec-problems
  "Returns a list of normalized spec problem maps."
  [ed]
  (map (fn [problem]
         (-> problem
             ;; include ::s/value so the full, top-level value is available in any
             ;; explainer.
             (merge (select-keys ed [::s/value ::s/spec]))
             ;; copy :pred to ::pred so we preserve the original explain-data
             ;; ::pred will get transformed to a match-form.
             (assoc ::pred (:pred problem))))
       (::s/problems ed)))

(defmacro defexplainer
  [sym problem-matcher & body]
  (let [matcher (cond-> problem-matcher
                  (::pred problem-matcher) (update ::pred (comp form-as-match res)))]
    `(defn ~sym
       [explain-data#]
       ;; return the first matched problem
       (some (fn [problem#]
               (match/match [problem#]
                 ~[matcher] (do ~@body)
                 :else nil))
             (get-spec-problems explain-data#)))))

(comment
  (defexplainer
    contains-explainer
    {::pred #(contains? _ kw)
     :via   [::user & r]}
    (str "Missing " (stringify kw) "."))
  )

(defn clojure-spec-explainer
  "An explainer that will use Spec's explain printer to print the error message."
  [explain-data]
  (when explain-data (with-out-str (s/explain-out explain-data))))

(defn explain-first
  ([spec val explainers] (explain-first spec val explainers clojure-spec-explainer))
  ([spec val explainers default-explainer]
   (let [explain-data (s/explain-data spec val)
         match-fns (conj (vec explainers) default-explainer)]
     (some (fn [match-fn]
             (match-fn explain-data)) match-fns))))