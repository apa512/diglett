(ns diglett2-test
  (:require [clojure.string :as string]
            [clojure.test :refer :all]
            [diglett2 :refer :all]
            [schema.core :as s]))

(def phamdarian (slurp "dev-resources/linkedin/in_phamdarian.html"))

(def Courses
  (spec (pass [{:name s/Str
                :school s/Str}])
        "#courses > ul > li"
        #(mapcat (fn [elem]
                   (let [school (extract (spec s/Str "h4.item-title" text) elem)
                         courses (extract (spec [s/Str] "li.course > span:first-child" text) elem)]
                     (mapv (fn [course]
                             {:school school
                              :name course})
                           courses)))
                 %)))

(def CourseNames
  (spec [s/Str] "#courses li.course > span:first-child" text))

(deftest diglett2-test
  (let [courses (extract Courses (parse phamdarian))]
    (is (= (count courses) 13))
    (is (= (first courses) {:school "California State University-Fullerton"
                            :name "Financial Accounting (ACCT 201A)"})))

  (let [course-names (extract CourseNames (parse phamdarian))]
    (is (= (count course-names) 13))
    (is (= (first course-names) "Financial Accounting (ACCT 201A)"))
    (is (= (last course-names) "International Economics (ECON 333)"))))
