(ns project-euler.test.core
  (:use [project-euler.solutions]
        [clojure.test :only (deftest are)]))

(deftest all-solutions
  (are [f n] (do (println "Testing"
                          (re-find (re-matcher #"pe\d{3}" (str f)))
                          "...")
               (= (f) n))
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
       pe018 1074
       pe019 171
       pe020 648
       pe021 31626
       pe022 871198282
       pe023 4179871
       pe024 2783915460
       pe025 4782
       pe026 983
       pe027 -59231
       pe028 669171001
       pe029 9183
       pe030 443839
       pe031 73682
       pe032 45228
       pe033 100
       pe034 40730
       pe035 55
       pe036 872187
       pe037 748317
       pe038 932718654
       pe039 840
       pe040 210
       pe041 7652413
       pe042 162
       pe067 7273
       pe076 190569291
       ))
