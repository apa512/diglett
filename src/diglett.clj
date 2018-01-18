(ns diglett
  (:require [clojure.spec.alpha :as s]
            [clojure.string :as string]
            [clojure.walk :as walk]
            [medley.core :refer [map-kv filter-keys]]
            [schema.core :as schema])
  (:import [org.jsoup Jsoup]
           [org.jsoup.nodes Element]
           [org.jsoup.select Elements]))

(declare extract-map)

(s/def ::extr (s/and (s/or :e (s/cat :sel (s/? string?)
                                         :fns (s/* fn?))
                              :e nil?)
                       (s/conformer last)))

(defn select [node ^String css-selector]
  (let [^Elements result (.select node css-selector)]
    (if (.isEmpty result)
      nil
      result)))

(defn pull [x fns]
  ((apply comp (reverse fns)) x))

(defrecord Spec [schema extr])

(defrecord PassiveSchema [schema])

(defn spec [schema & extr]
  (Spec. schema extr))

(defn pass [schema]
  (PassiveSchema. schema))

(defn many? [x]
  (boolean (or (sequential? x) (instance? Elements x))))

(defn ->schema [x]
  (walk/postwalk
   #(if (or (instance? Spec %)
            (instance? PassiveSchema %))
      (:schema %)
      %)
   x))

(defprotocol Diggable
  (dig [_ _])
  (->node [_])
  (text [_])
  (own-text [_])
  (integer [_])
  (attr* [_ _]))

(defn attr [k]
  #(attr* % k))

(defn coercer [schema]
  (let [expl (schema/explain schema)]
    (condp = (cond-> expl
               (sequential? expl) first)
      'Str #(some-> % text)
      'Int #(some-> % integer)
      identity)))

(extend-protocol Diggable
  Element
  (->node [elem] elem)
  (text [elem] (.text elem))
  (own-text [elem] (.ownText elem))
  (integer [elem]
    (integer (text elem)))
  (attr* [elem k]
    (let [s (.get (.attributes elem) (name k))]
      (when (seq s) s)))

  Elements
  (->node [elems] (first elems))

  java.lang.String
  (text [s] s)
  (own-text [s] s)
  (integer [s]
    (some->> (string/replace s #"," "")
             (re-find #"\d+")
             not-empty
             BigInteger.))

  java.math.BigInteger
  (integer [n] n)

  nil
  (->node [_] nil)
  (attr* [_ _] nil))

(defprotocol Extractable
  (extract [_ _]))

(extend-protocol Extractable
  Spec
  (extract [{:keys [schema extr]} target]
    (let [{:keys [sel fns]} (s/conform ::extr extr)
          elems (cond-> target
                  sel (select sel))
          extracted (extract schema elems)
          extract* #((coercer (->schema schema))
                     (pull % fns))]
      (if (and (many? extracted) (not (instance? PassiveSchema schema)))
        (mapv extract* extracted)
        (extract* extracted))))

  PassiveSchema
  (extract [_ target]
    target)

  clojure.lang.PersistentArrayMap
  (extract [m target]
    (extract-map m target))

  clojure.lang.PersistentHashMap
  (extract [m target]
    (extract-map m target))

  clojure.lang.PersistentVector
  (extract [[spec] target]
    (mapv #(extract spec %) target))

  java.lang.Object
  (extract [x target]
    (->node target)))

(defn extract-map [m target]
  (filter-keys identity
               (map-kv (fn [k v]
                         (let [v (extract v target)]
                           (if (instance? schema.core.OptionalKey k)
                             (when v
                               [(:k k) v])
                             [k v])))
                       m)))

(defn parse [^String html]
  (.. (Jsoup/parseBodyFragment html) (children)))
