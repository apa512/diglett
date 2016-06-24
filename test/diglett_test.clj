(ns diglett-test
  (:require [clojure.string :as string]
            [clojure.test :refer :all]
            [diglett :refer :all]
            [schema.core :as s]))

(deftest diglett-test
  (testing "schema->fn"
    (are [x y] (= y (schema->fn x))
         (s/maybe s/Str) text
         s/Str text)))
