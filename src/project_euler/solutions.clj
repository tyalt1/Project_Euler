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

(defn pe008 []
  (loop [x (pelib/digit-list 7316717653133062491922511967442657474235534919493496983520312774506326239578318016984801869478851843858615607891129494954595017379583319528532088055111254069874715852386305071569329096329522744304355766896648950445244523161731856403098711121722383113622298934233803081353362766142828064444866452387493035890729629049156044077239071381051585930796086670172427121883998797908792274921901699720888093776657273330010533678812202354218097512545405947522435258490771167055601360483958644670632441572215539753697817977846174064955149290862569321978468622482839722413756570560574902614079729686524145351004748216637048440319989000889524345065854122758866688116427171479924442928230863465674813919123162824586178664583591245665294765456828489128831426076900422421902267105562632111110937054421750694165896040807198403850962455444362981230987879927244284909188845801561660979191338754992005240636899125607176060588611646710940507754100225698315520005593572972571636269561882670428252483600823257530420752963450)
         maximum 0]
    (if (< (count x) 13)
      (long maximum)
      (let [product (apply * (take 13 x))]
        (if (< maximum product)
          (recur (rest x) product)
          (recur (rest x) maximum))))))

(defn pe009 []
  (first
   (for [a (range 1 1001)
         b (range a (- 1001 a))
         c (range (inc b) (- 1001 a b))
         :when (= (+ a b c) 1000)
         :when (= (+ (numeric/expt a 2) (numeric/expt b 2)) (numeric/expt c 2))]
     (* a b c))))

(defn pe010 []
  (apply + (take-while #(< % 2e6) (pelib/lazy-prime))))
