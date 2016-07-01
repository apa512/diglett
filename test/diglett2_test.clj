(ns diglett2-test
  (:require [clojure.string :as string]
            [clojure.test :refer :all]
            [diglett2 :refer :all]
            [schema.core :as s]))

(def phamdarian (slurp "dev-resources/linkedin/in_phamdarian.html"))

(prn (macroexpand '(spec '[{:name s/Str
              (s/optional-key :school) s/Str}]
           "#courses > ul > li")))

(prn (spec '[{:name s/Str
              (s/optional-key :school) s/Str}]
           "#courses > ul > li"))

(prn (spec [{:name s/Str
              (s/optional-key :school) s/Str}]
           "#courses > ul > li"))

(deftest diglett2-test
  ;(prn (prn (:schema (specc '{:hejsan 33} 33 identity))))
  )
