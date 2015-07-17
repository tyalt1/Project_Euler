(ns ^{:doc "Useful functions for Project Euler problems."
      :author "Tyler Alterio"}
  project_euler_lib
  (:require [contrib.math.numeric-tower :as numeric]))

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
  ([list-of-digits base] (loop [digits list-of-digits, number (bigint 0)]
                           (if (empty? digits)
                             (if (< number java.lang.Long/MAX_VALUE)
                               (long number)
                               number)
                             (recur (rest digits) (+ (* number base) (first digits)))))))

;;Boolean Checks
(defn prime?
  "Returns if n is prime."
  [n] (let [n (numeric/abs n)]
        (cond (= n 2)   true
              (= n 1)   false
              (even? n) false
              :else     (let [root (numeric/sqrt n)]
                          (loop [i 3] (cond (> i root)        true
                                            (zero? (mod n i)) false
                                            :else             (recur (+ i 2))))))))

(defn triangular?
  "returns true if n is a triangular number."
  [n] (integer? (numeric/sqrt (+ 1 (* 8 n)))))

(defn pentagonal?
  "Returns true if n is a pentagonal number."
  [n] (zero? (rem (+ 1 (Math/sqrt (+ 1 (* 24 n)))) 6)))

(defn hexagonal?
  "Returns true if n is a hexagonal number."
  [n] (zero? (rem (+ 1 (Math/sqrt (+ 1 (* 8 n)))) 4)))

(defn seq-palindrome?
  "Returns true if the list is a palindrome.
  Uses = to check for equality of ends."
  [xs] (cond
          (< (count xs) 2)         true
          (= (first xs) (last xs)) (recur (drop-last (drop 1 xs)))
          :else                    false))

(defn pandigital?
  "Returns true if the number is pandigital (1 to n).
  Optinal use of ns(start) and ne(end) to check specific range."
  [n-start n-end number] (= (range n-start (inc n-end)) (sort (digit-list number))))

;;Lazy Sequences
(defn lazy-fib
  "Returns a lazy seqence of fibonacci numbers.
  Default: x=1, y=2."
  ([] (lazy-fib 1 2))
  ([x y] (cons x (lazy-seq (lazy-fib y (+ x y))))))

(defn lazy-primes
  "Returns a lazy sequence of prime numbers.
  Default: n=2."
  ([]  (filter prime? (iterate inc 2)))
  ([n] (filter prime? (iterate inc n))))

(defn lazy-triangular
  "Returns a lazy sequence of triangular numbers.
  T(n)=(n^2+n)/2"
  [] (map (fn [n] (/ (+ (* n n) n) 2)) (iterate inc 1)))

(defn lazy-pentagonal
  "Returns a lazy sequence of pentagonal numbers.
  P(n)=(3n^2-n)/2"
  [] (map (fn [n] (/ (- (* 3 n n) n) 2)) (iterate inc 1)))

(defn lazy-hexagonal
  "Returns a lazy sequence of hexagonal numbers.
  H(n)=2n^2-n"
  [] (map (fn [n] (- (* 2 n n) n)) (iterate inc 1)))

(defn lazy-accum
  "Where xs is a lazy-seq and sum is starting sum.
  Returns a lazy-seq where the elements of xs so far are summed.
  If xs is not a lazy-seq, then may throw NullPointerException."
  ([xs] (lazy-accum xs 0))
  ([xs sum] (if (empty? xs)
              '()
              (let [new-sum (+ sum (first xs)), more (rest xs)]
              (cons new-sum (lazy-seq (lazy-accum more new-sum)))))))

;;Other
(defn factors
  "Returns a set of factors for n."
  [n] (loop [iter 2, upper n, result (set [1 n])]
        (cond
          (>= iter upper)      result
          (zero? (rem n iter)) (recur (inc iter) (/ n iter) (conj result iter (/ n iter)))
          :else                (recur (inc iter) upper result))))

(defn divisors
  "Returns a set of the proper divisors of n.
  Like factors, but the set does not include n."
  [n] (set (butlast (factors n))))

(defn factorial
  "Returns the factorial of n.
  If n<=1, then return 1."
  [n] (apply * (range 1 (inc n))))

(defn gcd
  "Returns greatest common denominator of a and b."
  [a b] (if (zero? b)
          a
          (recur b (rem a b))))

(defn lcm
  "Returns least common multiple of a and b.
  Uses gcd function."
  [a b] (/ (numeric/abs (* a b)) (gcd a b)))
