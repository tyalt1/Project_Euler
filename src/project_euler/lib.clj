(ns project-euler.lib
  (:require [project-euler.sieve :as sieve]
            [clojure.set :as set :only (difference)]
            [clojure.math.numeric-tower :as math :only (sqrt)]))

(defn- truncate-to-long
  [n] (if (< n java.lang.Long/MAX_VALUE)
        (long n)
        n))

;;Digit Manipulation
(defn digit-list
  "Turns number n into a list of its digits.
  Assumes base 10 if no base is given"
  ([n] (digit-list n 10))
  ([n base] (loop [n n, lst '()]
              (if (< n base)
                (cons (num n) lst)
                (recur (quot n base) (cons (num (rem n base)) lst))))))

(defn from-digit-list
  "Concatenates a sequence of numbers into one number.
  Assumes base 10 if no base is given"
  ([xs] (from-digit-list xs 10))
  ([xs base] (loop [digits xs, n (bigint 0)]
               (if (empty? digits)
                 (truncate-to-long n)
                 (recur (rest digits) (+ (* n base) (first digits)))))))

;;Boolean Checks
(def prime?
  "Returns true if n is prime."
  (memoize
   (fn [n]
     (cond
       (<= n 1)  false
       (= n 2)   true
       (even? n) false
       :else     (let [root (math/sqrt n)]
                   (loop [i 3] (cond
                                 (> i root)        true
                                 (zero? (rem n i)) false
                                 :else             (recur (+ 2 i)))))))))

(defn seq-palindrome?
  "Returns true if xs is a palindrome.
  Uses = to check if ends are equal."
  [xs] (cond
         (> 2 (count xs))         true
         (= (first xs) (last xs)) (recur (rest (butlast xs)))
         :else                    false))

(defn pandigital?
  "Returns true if the digits (start to end) appear in n once."
  ([n] (pandigital? 1 9 n))
  ([start end n] (= (range start (inc end)) (sort (digit-list n)))))

(defn triangular?
  "Returns true if n is triangular."
  [n] (integer? (math/sqrt (inc (* 8 n)))))

(defn pentagonal?
  "Returns true if n is pentagonal."
  [n] (zero? (rem (inc (math/sqrt (inc (* 24 n)))) 6)))

(defn hexagonal?
  "Returns true if n is hexagonal."
  [n] (zero? (rem (inc (Math/sqrt (inc (* 8 n)))) 4)))

;;Lazy Sequences
(defn lazy-fib
  "Lazy sequence of fibonacci numbers.
  Default: x=1, y=1"
  ([] (lazy-fib 1 1))
  ([x y] (map first (iterate (fn [[a b]] [b (+ a b)]) [x y]))))

(def lazy-prime
  "Lazy sequence of prime numbers."
  sieve/lazy-primes)

(defn lazy-triangular
  "Lazy sequence of triangular numbers."
  [] (map first (iterate (fn [[i j]] [(+ i j) (inc j)]) [1 2])))

;;Utilities
(defn factors
  "Return a set of factors of n."
  [n] (into (sorted-set)
            (reduce concat
                    (for [x (range 1 (inc (math/sqrt n)))
                          :when (zero? (rem n x))]
                      [x (/ n x)]))))

(defn divisors
  "Returns a set of proper divisors.
  Same as factors, but without n."
  [n] (set/difference (factors n) #{n}))

(defn fact
  "Factorial of n. Returns a BigInt."
  [n] (apply * (range 1N (inc n))))
