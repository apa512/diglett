(ns diglett-test
  (:require [clojure.string :as string]
            [clojure.test :refer :all]
            [diglett :refer :all]
            [schema.core :as s]))

(def realdonaldtrump (slurp "dev-resources/twitter/realdonaldtrump.html"))
(def lindsayrockw (slurp "dev-resources/twitter/lindsayrockw.html"))

(def phamdarian (slurp "dev-resources/linkedin/in_phamdarian.html"))

(def TwitterProfile
  {:twitter/username (spec s/Str ".ProfileHeaderCard-screennameLink > span")
   :images [(spec s/Str ".AdaptiveMedia-singlePhoto img" (attr :src))]
   :name (spec s/Str "h1.ProfileHeaderCard-name > a")
   :photo (spec s/Str ".ProfileAvatar img" (attr :src))
   (s/optional-key :summary) (spec (s/maybe (s/both s/Str (s/pred not-empty))) ".ProfileHeaderCard-bio")
   :verified? (spec s/Bool ".ProfileHeaderCard-badges .Icon--verified" boolean)
   :tweets/count (spec (s/maybe s/Int) ".ProfileNav-stat[data-nav=tweets]" (attr :title))
   :following/count (spec (s/maybe s/Int) ".ProfileNav-stat[data-nav=following]" (attr :title))
   :followers/count (spec (s/maybe s/Int) ".ProfileNav-stat[data-nav=followers]" (attr :title))
   :likes/count (spec (s/maybe s/Int) ".ProfileNav-stat[data-nav=favorites]" (attr :title))
   :banner (spec (s/maybe s/Str) ".ProfileCanopy-headerBg img" (attr :src))
   :location (spec (s/maybe (s/both s/Str (s/pred not-empty))) ".ProfileHeaderCard-location")
   :website/url (spec (s/maybe s/Str) ".ProfileHeaderCard-urlText a" (attr :href))})

(def LinkedinProfile
  {:courses (spec 's/Str "#courses > ul > li")
   ;:courses [(d/spec {:name s/Str
   ;                   (s/optional-key :school) s/Str}
   ;                  "#courses > ul > li"
   ;                  (fn [elem]
   ;                    (let [school (d/extract (d/spec s/Str "h4.item-title") elem)
   ;                          courses (d/extract [(d/spec s/Str "li.course > span:first-child")] elem)]
   ;                      (mapv (fn [course]
   ;                              (utils/compact {:name course
   ;                                              :school school}))
   ;                            courses))))]
   })

#_(deftest diglett-test
  ;(prn (extract LinkedinProfile phamdarian))
  (prn (->schema LinkedinProfile))
  (testing "schema->fn"
    (are [x y] (= y (schema->fn x))
         (s/maybe s/Str) text
         s/Str text)))
