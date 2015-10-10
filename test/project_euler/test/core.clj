(ns project-euler.test.core
  (:use [project-euler.solutions]
        [clojure.test :only (deftest testing is)]))

(deftest all-solutions
  (testing "All Project Euler Solutions"
    (is (= (pe001) 233168))
    (is (= (pe002) 4613732))
    (is (= (pe003) 6857))
    (is (= (pe004) 906609))
    (is (= (pe005) 232792560))
    (is (= (pe006) 25164150))
    (is (= (pe007) 104743))
    (is (= (pe008) 23514624000))
    (is (= (pe009) 31875000))
    (is (= (pe010) 142913828922))
    (is (= (pe011) 70600674))
    ))
