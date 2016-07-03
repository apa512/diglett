(ns diglett-test
  (:require [clojure.string :as string]
            [clojure.test :refer :all]
            [diglett :refer :all]
            [schema.core :as s]))

(def phamdarian (slurp "dev-resources/linkedin/in_phamdarian.html"))

(def realdonaldtrump (slurp "dev-resources/twitter/realdonaldtrump.html"))

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
  (spec [s/Str] "#courses li.course > span:first-child" #_text))

(def LinkedinProfile
  {:courses Courses
   (s/optional-key :photo) (spec s/Str ".profile-picture img" (some-fn (attr :src) (attr :data-delayed-url)))
   :educations (spec [{:headline (spec s/Str "h5.item-subtitle")
                       :school (spec {:name (spec s/Str "h4.item-title")})
                       }]
                     "section#education li.school")
   })

(def TwitterProfile
  {(s/optional-key :following/count) (spec s/Int ".ProfileNav-stat[data-nav=following]" (attr :title))})

(deftest diglett2-test
  (let [courses (extract Courses (parse phamdarian))]
    (is (= (count courses) 13))
    (is (= (first courses) {:school "California State University-Fullerton"
                            :name "Financial Accounting (ACCT 201A)"})))

  (let [course-names (extract CourseNames (parse phamdarian))]
    (is (= (count course-names) 13))
    (is (= (first course-names) "Financial Accounting (ACCT 201A)"))
    (is (= (last course-names) "International Economics (ECON 333)")))

  (let [linkedin-profile (extract LinkedinProfile (parse phamdarian))]
    (prn linkedin-profile))

  (let [twitter-profile (extract TwitterProfile (parse realdonaldtrump))]
    (prn twitter-profile)))
