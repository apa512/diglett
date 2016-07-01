(ns diglett2-test
  (:require [clojure.string :as string]
            [clojure.test :refer :all]
            [diglett2 :refer :all]
            [schema.core :as s]))

(def phamdarian (slurp "dev-resources/linkedin/in_phamdarian.html"))

(def LinkedinProfile
  {:courses (spec '[{:name s/Str
                     (s/optional-key :school) s/Str}])})

(deftest diglett2-test
  (prn (->schema LinkedinProfile))
  )
