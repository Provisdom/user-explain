(ns provisdom.user-explain.core-test
  (:require
    [clojure.test :refer :all]
    [clojure.string :as str]
    [clojure.spec.alpha :as s]
    [provisdom.user-explain.core :as splain]))

(defn not-blank?
  [x]
  (not (str/blank? x)))

(s/def :user/name (s/and string? not-blank?))
(s/def :user/age nat-int?)
(s/def ::user (s/keys :req [:user/name]
                      :opt [:user/age]))

(defn stringify
  [kw]
  (str (namespace kw) " " (name kw)))

(splain/defexplainer
  contains-explainer
  {::splain/pred #(contains? _ kw)
   :via          [::user & r]}
  (str "Missing " (stringify kw) "."))

(splain/defexplainer
  not-blank-explainer
  {::splain/pred not-blank?
   :via          [::user & r]
   :path         path
   ::s/value     full-val}
  (str "A " (stringify (last path)) " cannot be blank."))

(splain/defexplainer
  user-age-explainer
  {:via      [::user :user/age]
   ::s/value user-map}
  (str (:user/name user-map) " does not have a valid age."))

(splain/defexplainer
  default-explainer
  {:via  [::user & r]
   :path path}
  (str "Value at " (pr-str path) " is invalid."))

(deftest explain-first-test
  (let [matchers [contains-explainer not-blank-explainer user-age-explainer default-explainer]]
    (is (= "Missing user name."
           (splain/explain-first
             ::user {} matchers))
        "invalid")
    (is (= "A user name cannot be blank."
           (splain/explain-first
             ::user {:user/name ""} matchers))
        "invalid")
    (is (= "john does not have a valid age."
           (splain/explain-first
             ::user {:user/name "john"
                     :user/age  "no"} matchers))
        "invalid")
    (is (nil? (splain/explain-first
                ::user {:user/name "john"} matchers))
        "valid value returns nil")))