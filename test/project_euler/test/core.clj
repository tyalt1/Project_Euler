(ns project-euler.test.core
  (:use [project-euler.solutions]
        [clojure.test :only (deftest are)]))

(deftest all-solutions
  (are [f n] (= (f) n)
       pe001 233168
       pe002 4613732
       pe003 6857
       pe004 906609
       pe005 232792560
       pe006 25164150
       pe007 104743
       pe008 23514624000
       pe009 31875000
       pe010 142913828922
       pe011 70600674
       pe012 76576500
       pe013 5537376230
       pe014 837799
       pe015 137846528820
       pe016 1366
       pe017 21124
       ))
