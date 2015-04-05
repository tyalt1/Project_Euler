(ns project_euler_lib "Useful functions for Project Euler probelms."
  (:require [contrib.math.numeric-tower :as numeric]))

;;Boolean Checks
(defn prime?
  "Returns if n is prime."
  [n] (let [n (numeric/abs n)]
        (cond (= n 2)   true
              (= n 1)   false
              (even? n) false
              :else     (let [root (num (numeric/sqrt n))]
                          (loop [i 3] (cond (> i root) true
                                            (zero? (mod n i)) false
                                            :else (recur (+ i 2))))))))

;;Digit Manip
(defn digit-list
  "Takes number n and returns a vector of it's digits.
  Assumes Base-10 if base is not specified."
  ([n]      (digit-list n 10))
  ([n base] (loop [number n, lst '()]
              (if (< number base)
                (conj lst (num number))
                (recur (quot number base) (conj lst (num (rem number base))))))))

(defn from-digit-list
  "Takes a list of digits and returns a long if in range, else bigint."
  ([list-of-digits]      (from-digit-list list-of-digits 10))
  ([list-of-digits base] (let [truncate-to-long #(if (< % java.lang.Long/MAX_VALUE) (long %) %)]
                           (truncate-to-long
                             (loop [digits list-of-digits, number (bigint 0)]
                               (if (empty? digits)
                                 number
                                 (recur (rest digits) (+ (* number base) (first digits)))))))))

;;Lazy Sequences
(defn lazy-fib
  "Returns a lazy seqence of fibonacci numbers.
  Default: x=1, y=2."
  ([] (lazy-fib 1 2))
  ([x y] (cons x (lazy-seq (lazy-fib y (+ x y))))))

(defn lazy-primes
  "Returns a lazy sequence of prime numbers.
  Default: n=2."
  ([] (lazy-primes 2))
  ([n] (loop [i n] (if (prime? i)
                     (cons i (lazy-seq (lazy-primes (inc i))))
                     (recur (inc i))))))

(defn lazy-factorial
  "Returns a lazy sequence of factorials.
  Default: n=1, fac=1."
  ([] (lazy-factorial 1 1))
  ([n fac] (cons fac (lazy-seq (lazy-factorial (inc n) (* fac (inc n)))))))

(defn lazy-triangular
  "Returns a lazy sequence of triangular numbers.
  A triangular number is a number that is the sum of the natural numbers before it.
  Think of factorial, but the sum instead of the product.
  Ex. 7th = 1+2+3+4+5+6+7 = 28
  Default: n=1, tri=1."
  ([] (lazy-triangular 1 1))
  ([n tri] (cons tri (lazy-seq (lazy-triangular (inc n) (+ tri (inc n)))))))

;;Other
(defn factors
  "Returns a set of factors for n."
  [n] (loop [iter 2, upper n, result (set [1 n])]
        (cond
          (>= iter upper) result
          (= 0 (mod n iter)) (recur (inc iter) (/ n iter) (conj result iter (/ n iter)))
          :else (recur (inc iter) upper result))))

(defn divisors
  "Returns a set of the proper divisors of n."
  [n] (set (butlast (sort (factors n)))))

(defn factorial
  "Returns the factorial of n."
  [n] (loop [i n, result 1]
        (if (= i 0)
          result
          (recur (dec i) (* result i)))))
