(ns project-euler.solutions
  (:require [project-euler.lib :as pelib]
            [clojure.string :only (split)]
            [clojure.math.numeric-tower :as numeric :only (expt)]))

(defn pe001
  "Sum of all the multiples of 3 and 5 below 1000."
  [] (->> (range 1 1000)
       (filter #(or (zero? (mod % 3))
                    (zero? (mod % 5))))
       (apply +)))

(defn pe002
  "Sum of all the even fibonacci numbers below 4 million."
  [] (->> (pelib/lazy-fib)
       (take-while #(< % 4e6))
       (filter even?)
       (apply +)))

(defn pe003
  "Largest prime factor of 600851475143."
  [] (->> (pelib/factors 600851475143)
       (filter pelib/prime?)
       (apply max)))

(defn pe004
  "Largest palindromic number that is a product of 2 3-digit numbers."
  [] (apply max (for [i (range 100 1000)
                      j (range 100 1000)
                      :let [x (* i j)
                            f (comp pelib/seq-palindrome?
                                    pelib/digit-list)]
                      :when (f x)]
                  x)))

(defn pe005
  "Smallest positive number evenly divisible by the numbers 1 to 20."
  [] (* (numeric/expt 2 4) (numeric/expt 3 2) 5 7 11 13 17 19))

(defn pe006
  "Difference between the sum of the squares of the first 100 natural numbers
  and the square of the sum."
  [] (let [nat-nums (range 1 101)]
       (- (numeric/expt (apply + nat-nums) 2)
          (apply + (map #(numeric/expt % 2) nat-nums)))))

(defn pe007
  "10,001st prime number."
  [] (last (take 10001 (pelib/lazy-prime))))

(defn pe008
  "13th adjacent digits of a given number with the greatest product."
  [] (loop [x (pelib/digit-list 7316717653133062491922511967442657474235534919493496983520312774506326239578318016984801869478851843858615607891129494954595017379583319528532088055111254069874715852386305071569329096329522744304355766896648950445244523161731856403098711121722383113622298934233803081353362766142828064444866452387493035890729629049156044077239071381051585930796086670172427121883998797908792274921901699720888093776657273330010533678812202354218097512545405947522435258490771167055601360483958644670632441572215539753697817977846174064955149290862569321978468622482839722413756570560574902614079729686524145351004748216637048440319989000889524345065854122758866688116427171479924442928230863465674813919123162824586178664583591245665294765456828489128831426076900422421902267105562632111110937054421750694165896040807198403850962455444362981230987879927244284909188845801561660979191338754992005240636899125607176060588611646710940507754100225698315520005593572972571636269561882670428252483600823257530420752963450)
            maximum 0]
       (if (< (count x) 13)
         (long maximum)
         (let [product (apply * (take 13 x))]
           (if (< maximum product)
             (recur (rest x) product)
             (recur (rest x) maximum))))))

(defn pe009
  "Find the product of a, b, and c such that:
  -> a < b < c
  -> a^2 + b^2 = c^2
  -> a + b + c = 100"
  [] (first
      (for [a (range 1 1001)
            b (range a (- 1001 a))
            c (range (inc b) (- 1001 a b))
            :when (= (+ a b c) 1000)
            :when (= (+ (numeric/expt a 2) (numeric/expt b 2))
                     (numeric/expt c 2))]
        (* a b c))))

(defn pe010
  "Sum of all primes below 2 million."
  [] (apply + (take-while #(< % 2e6) (pelib/lazy-prime))))

(defn pe011
  "Largest product of 4 adjacent numbers in a given 20x20 grid."
  [] (let [matrix [[8  2 22 97 38 15  0 40  0 75  4  5  7 78 52 12 50 77 91  8]
                   [49 49 99 40 17 81 18 57 60 87 17 40 98 43 69 48  4 56 62  0]
                   [81 49 31 73 55 79 14 29 93 71 40 67 53 88 30  3 49 13 36 65]
                   [52 70 95 23  4 60 11 42 69 24 68 56  1 32 56 71 37  2 36 91]
                   [22 31 16 71 51 67 63 89 41 92 36 54 22 40 40 28 66 33 13 80]
                   [24 47 32 60 99  3 45  2 44 75 33 53 78 36 84 20 35 17 12 50]
                   [32 98 81 28 64 23 67 10 26 38 40 67 59 54 70 66 18 38 64 70]
                   [67 26 20 68  2 62 12 20 95 63 94 39 63  8 40 91 66 49 94 21]
                   [24 55 58  5 66 73 99 26 97 17 78 78 96 83 14 88 34 89 63 72]
                   [21 36 23  9 75  0 76 44 20 45 35 14  0 61 33 97 34 31 33 95]
                   [78 17 53 28 22 75 31 67 15 94  3 80  4 62 16 14  9 53 56 92]
                   [16 39  5 42 96 35 31 47 55 58 88 24  0 17 54 24 36 29 85 57]
                   [86 56  0 48 35 71 89  7  5 44 44 37 44 60 21 58 51 54 17 58]
                   [19 80 81 68  5 94 47 69 28 73 92 13 86 52 17 77  4 89 55 40]
                   [ 4 52  8 83 97 35 99 16  7 97 57 32 16 26 26 79 33 27 98 66]
                   [88 36 68 87 57 62 20 72  3 46 33 67 46 55 12 32 63 93 53 69]
                   [ 4 42 16 73 38 25 39 11 24 94 72 18  8 46 29 32 40 62 76 36]
                   [20 69 36 41 72 30 23 88 34 62 99 69 82 67 59 85 74  4 36 16]
                   [20 73 35 29 78 31 90  1 74 31 49 71 48 86 81 16 23 57  5 54]
                   [ 1 70 54 71 83 51 54 69 16 92 33 48 61 43 52  1 89 19 67 48]]]
       (letfn [(vec-acces [x y] ((matrix y) x))
               (up [x y] (try (* (vec-acces x y)
                                 (vec-acces x (- y 1))
                                 (vec-acces x (- y 2))
                                 (vec-acces x (- y 3)))
                           (catch IndexOutOfBoundsException e 0)))
               (down [x y] (try (* (vec-acces x y)
                                   (vec-acces x (+ y 1))
                                   (vec-acces x (+ y 2))
                                   (vec-acces x (+ y 3)))
                             (catch IndexOutOfBoundsException e 0)))
               (right [x y] (try (* (vec-acces x y)
                                    (vec-acces (+ x 1) y)
                                    (vec-acces (+ x 2) y)
                                    (vec-acces (+ x 3) y))
                              (catch IndexOutOfBoundsException e 0)))
               (left [x y] (try (* (vec-acces x y)
                                   (vec-acces (- x 1) y)
                                   (vec-acces (- x 2) y)
                                   (vec-acces (- x 3) y))
                             (catch IndexOutOfBoundsException e 0)))
               (up-right [x y] (try (* (vec-acces x y)
                                       (vec-acces (+ x 1) (- y 1))
                                       (vec-acces (+ x 2) (- y 2))
                                       (vec-acces (+ x 3) (- y 3)))
                                 (catch IndexOutOfBoundsException e 0)))
               (up-left [x y] (try (* (vec-acces x y)
                                      (vec-acces (- x 1) (- y 1))
                                      (vec-acces (- x 2) (- y 2))
                                      (vec-acces (- x 3) (- y 3)))
                                (catch IndexOutOfBoundsException e 0)))
               (down-right [x y] (try (* (vec-acces x y)
                                         (vec-acces (+ x 1) (+ y 1))
                                         (vec-acces (+ x 2) (+ y 2))
                                         (vec-acces (+ x 3) (+ y 3)))
                                   (catch IndexOutOfBoundsException e 0)))
               (down-left [x y] (try (* (vec-acces x y)
                                        (vec-acces (- x 1) (+ y 1))
                                        (vec-acces (- x 2) (+ y 2))
                                        (vec-acces (- x 3) (+ y 3)))
                                  (catch IndexOutOfBoundsException e 0)))]
         (apply max (flatten (for [i (range 20)
                                   j (range 20)]
                               (list (up i j)
                                     (down i j)
                                     (right i j)
                                     (left i j)
                                     (up-right i j)
                                     (up-left i j)
                                     (down-right i j)
                                     (down-left i j))))))))

(defn pe012
  "First triangular number with 500 divisors"
  [] (first
      (filter #(> (count (pelib/factors %)) 501) (pelib/lazy-triangular))))
