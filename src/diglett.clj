(ns diglett
  (:require [clojure.spec :as s]
            [clojure.string :as string]
            [clojure.walk :as walk]
            [medley.core :refer [map-kv filter-keys]]
            [schema.core :as schema])
  (:import [org.jsoup Jsoup]
           [org.jsoup.nodes Element]
           [org.jsoup.select Elements]))

(declare ->schema schema->fn pull pull? spec ->spec)

(s/def ::schema #(satisfies? schema/Schema (->schema %)))

(s/def ::extr (s/and (s/or :e (s/cat :sel (s/? string?)
                                     :fns (s/* fn?))
                           :e nil?)
                     (s/conformer last)))

(s/def ::spec (s/and (s/or :s (s/keys :req-un [::schema
                                               ::extr])
                           :s nil?)
                     (s/conformer (fn [x]
                                    (let [{:keys [schema extr]} (last x)]
                                      (update extr :fns
                                              #(->> (schema->fn (->schema schema))
                                                    (conj (vec (seq %)))
                                                    distinct
                                                    (filter identity))))))))

(defn select [node ^String css-selector]
  (let [^Elements result (.select node css-selector)]
    (if (.isEmpty result)
      nil
      result)))

(defrecord Spec [schema extr])

(defprotocol Extractable
  (extract [_ _])
  (text [_])
  (integer [_])
  (attr* [_ _])
  (->node [_]))

(extend-protocol Extractable
  Spec
  (extract [spec target]
    (let [{:keys [sel fns]} (s/conform ::spec spec)
          nodes (if sel
                  (select target sel)
                  target)]
      (condp get (type (:schema spec))
        #{clojure.lang.PersistentArrayMap
          clojure.lang.PersistentHashMap}
        (filter-keys identity
                     (map-kv (fn [k v]
                               (let [v (extract v (->node nodes))]
                                 (if (instance? schema.core.OptionalKey k)
                                   (when v
                                     [(:k k) v])
                                   [k v])))
                             (:schema spec)))

        #{clojure.lang.PersistentVector}
        (map #(extract (first (:schema spec)) %) nodes)

        (pull (->node nodes) fns))))

  Element
  (text [node] (.text node))
  (attr* [node k]
    (let [s (.get (.attributes node) (name k))]
      (when (seq s) s)))
  (->node [node] node)

  Elements
  (->node [nodes] (first nodes))

  clojure.lang.PersistentArrayMap
  (extract [spec target]
    (extract (->spec spec) target))

  clojure.lang.PersistentHashMap
  (extract [spec target]
    (extract (->spec spec) target))

  clojure.lang.PersistentVector
  (extract [[spec] target]
    (let [{:keys [sel fns]} (s/conform ::spec spec)
          nodes (if sel
                  (select target sel)
                  target)]
      (map #(extract (assoc spec :extr fns) %) nodes)))

  java.lang.Integer
  (integer [n] n)

  java.lang.String
  (text [s] s)
  (integer [s]
    (some->> s
             (string/replace #"," "")
             (re-find #"\d+")
             not-empty
             Integer.))

  nil
  (->node [_] nil)
  (text [_] nil)
  (integer [_] nil)
  (attr* [_ _] nil))

(defn schema->fn [schema]
  (let [expl (schema/explain schema)]
    (condp = (cond-> expl
               (sequential? expl) first)
      'Str text
      'Int integer
      'pred (:p? schema)
      'both #(pull % (filter fn? (map schema->fn (:schemas schema))))
      'maybe (schema->fn (:schema schema))
      nil)))

(defn pull [x fns]
  ((apply comp (reverse fns)) x))

(defn spec [schema & extr]
  (Spec. schema extr))

(def ->spec spec)

(defn attr [k]
  #(attr* % k))

(defn ->schema [x]
  (walk/postwalk #(or (and (instance? Spec %) (:schema %))
                      %) x))

(defn parse [^String html]
  (.. (Jsoup/parseBodyFragment html) (children)))
