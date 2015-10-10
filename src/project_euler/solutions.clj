(ns project-euler.solutions
  (:require [project-euler.lib :as pelib]
            [clojure.string :only (split)]
            [clojure.math.numeric-tower :as numeric :only (expt)]))

(defn pe001 [] (->> (range 1 1000)
                 (filter #(or (zero? (mod % 3))
                              (zero? (mod % 5))))
                 (apply +)))

(defn pe002 [] (->> (pelib/lazy-fib)
                 (take-while #(< % 4e6))
                 (filter even?)
                 (apply +)))

(defn pe003 [] (->> (pelib/factors 600851475143)
                 (filter pelib/prime?)
                 (apply max)))

(defn pe004 [] (apply max (for [i (range 100 1000)
                                j (range 100 1000)
                                :let [x (* i j)
                                      f (comp pelib/seq-palindrome?
                                              pelib/digit-list)]
                                :when (f x)]
                            x)))

(defn pe005 [] (* (numeric/expt 2 4) (numeric/expt 3 2) 5 7 11 13 17 19))

(defn pe006 [] (let [nat-nums (range 1 101)]
                 (- (numeric/expt (apply + nat-nums) 2)
                    (apply + (map #(numeric/expt % 2) nat-nums)))))

(defn pe007 [] (last (take 10001 (pelib/lazy-prime))))
