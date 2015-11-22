(ns project-euler.solutions
  (:require [project-euler.lib :as pelib]
            [clojure.string :as string :only (split replace)]
            [clojure.math.numeric-tower :as math :only (expt sqrt)]
            [clojure.math.combinatorics :as combo :only (nth-permutation
                                                         permutations
                                                         subsets)]))

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
                      x [(* i j)]
                      f [(comp pelib/seq-palindrome? pelib/digit-list)]
                      :when (f x)]
                  x)))

(defn pe005
  "Smallest positive number evenly divisible by the numbers 1 to 20."
  [] (* (math/expt 2 4) (math/expt 3 2) 5 7 11 13 17 19))

(defn pe006
  "Difference between the sum of the squares of the first 100 natural numbers
  and the square of the sum."
  [] (let [nat-nums (range 1 101)]
       (- (math/expt (apply + nat-nums) 2)
          (apply + (map #(math/expt % 2) nat-nums)))))

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
            :when (= (+ (math/expt a 2) (math/expt b 2))
                     (math/expt c 2))]
        (* a b c))))

(defn pe010
  "Sum of all primes below 2 million."
  [] (apply + (take-while #(< % 2e6) (pelib/lazy-prime))))

(defn pe011
  "Largest product of 4 adjacent numbers in a given 20x20 grid."
  [] (let [matrix [[ 8  2 22 97 38 15  0 40  0 75  4  5  7 78 52 12 50 77 91  8]
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
  "First triangular number with 500 divisors."
  [] (first
      (filter #(>= (count (pelib/factors %)) 500) (pelib/lazy-triangular))))

(defn pe013
  "First ten digits of the sum of the given 100 50-digit numbers."
  [] (let [v [(bigint 37107287533902102798797998220837590246510135740250)
              (bigint 46376937677490009712648124896970078050417018260538)
              (bigint 74324986199524741059474233309513058123726617309629)
              (bigint 91942213363574161572522430563301811072406154908250)
              (bigint 23067588207539346171171980310421047513778063246676)
              (bigint 89261670696623633820136378418383684178734361726757)
              (bigint 28112879812849979408065481931592621691275889832738)
              (bigint 44274228917432520321923589422876796487670272189318)
              (bigint 47451445736001306439091167216856844588711603153276)
              (bigint 70386486105843025439939619828917593665686757934951)
              (bigint 62176457141856560629502157223196586755079324193331)
              (bigint 64906352462741904929101432445813822663347944758178)
              (bigint 92575867718337217661963751590579239728245598838407)
              (bigint 58203565325359399008402633568948830189458628227828)
              (bigint 80181199384826282014278194139940567587151170094390)
              (bigint 35398664372827112653829987240784473053190104293586)
              (bigint 86515506006295864861532075273371959191420517255829)
              (bigint 71693888707715466499115593487603532921714970056938)
              (bigint 54370070576826684624621495650076471787294438377604)
              (bigint 53282654108756828443191190634694037855217779295145)
              (bigint 36123272525000296071075082563815656710885258350721)
              (bigint 45876576172410976447339110607218265236877223636045)
              (bigint 17423706905851860660448207621209813287860733969412)
              (bigint 81142660418086830619328460811191061556940512689692)
              (bigint 51934325451728388641918047049293215058642563049483)
              (bigint 62467221648435076201727918039944693004732956340691)
              (bigint 15732444386908125794514089057706229429197107928209)
              (bigint 55037687525678773091862540744969844508330393682126)
              (bigint 18336384825330154686196124348767681297534375946515)
              (bigint 80386287592878490201521685554828717201219257766954)
              (bigint 78182833757993103614740356856449095527097864797581)
              (bigint 16726320100436897842553539920931837441497806860984)
              (bigint 48403098129077791799088218795327364475675590848030)
              (bigint 87086987551392711854517078544161852424320693150332)
              (bigint 59959406895756536782107074926966537676326235447210)
              (bigint 69793950679652694742597709739166693763042633987085)
              (bigint 41052684708299085211399427365734116182760315001271)
              (bigint 65378607361501080857009149939512557028198746004375)
              (bigint 35829035317434717326932123578154982629742552737307)
              (bigint 94953759765105305946966067683156574377167401875275)
              (bigint 88902802571733229619176668713819931811048770190271)
              (bigint 25267680276078003013678680992525463401061632866526)
              (bigint 36270218540497705585629946580636237993140746255962)
              (bigint 24074486908231174977792365466257246923322810917141)
              (bigint 91430288197103288597806669760892938638285025333403)
              (bigint 34413065578016127815921815005561868836468420090470)
              (bigint 23053081172816430487623791969842487255036638784583)
              (bigint 11487696932154902810424020138335124462181441773470)
              (bigint 63783299490636259666498587618221225225512486764533)
              (bigint 67720186971698544312419572409913959008952310058822)
              (bigint 95548255300263520781532296796249481641953868218774)
              (bigint 76085327132285723110424803456124867697064507995236)
              (bigint 37774242535411291684276865538926205024910326572967)
              (bigint 23701913275725675285653248258265463092207058596522)
              (bigint 29798860272258331913126375147341994889534765745501)
              (bigint 18495701454879288984856827726077713721403798879715)
              (bigint 38298203783031473527721580348144513491373226651381)
              (bigint 34829543829199918180278916522431027392251122869539)
              (bigint 40957953066405232632538044100059654939159879593635)
              (bigint 29746152185502371307642255121183693803580388584903)
              (bigint 41698116222072977186158236678424689157993532961922)
              (bigint 62467957194401269043877107275048102390895523597457)
              (bigint 23189706772547915061505504953922979530901129967519)
              (bigint 86188088225875314529584099251203829009407770775672)
              (bigint 11306739708304724483816533873502340845647058077308)
              (bigint 82959174767140363198008187129011875491310547126581)
              (bigint 97623331044818386269515456334926366572897563400500)
              (bigint 42846280183517070527831839425882145521227251250327)
              (bigint 55121603546981200581762165212827652751691296897789)
              (bigint 32238195734329339946437501907836945765883352399886)
              (bigint 75506164965184775180738168837861091527357929701337)
              (bigint 62177842752192623401942399639168044983993173312731)
              (bigint 32924185707147349566916674687634660915035914677504)
              (bigint 99518671430235219628894890102423325116913619626622)
              (bigint 73267460800591547471830798392868535206946944540724)
              (bigint 76841822524674417161514036427982273348055556214818)
              (bigint 97142617910342598647204516893989422179826088076852)
              (bigint 87783646182799346313767754307809363333018982642090)
              (bigint 10848802521674670883215120185883543223812876952786)
              (bigint 71329612474782464538636993009049310363619763878039)
              (bigint 62184073572399794223406235393808339651327408011116)
              (bigint 66627891981488087797941876876144230030984490851411)
              (bigint 60661826293682836764744779239180335110989069790714)
              (bigint 85786944089552990653640447425576083659976645795096)
              (bigint 66024396409905389607120198219976047599490197230297)
              (bigint 64913982680032973156037120041377903785566085089252)
              (bigint 16730939319872750275468906903707539413042652315011)
              (bigint 94809377245048795150954100921645863754710598436791)
              (bigint 78639167021187492431995700641917969777599028300699)
              (bigint 15368713711936614952811305876380278410754449733078)
              (bigint 40789923115535562561142322423255033685442488917353)
              (bigint 44889911501440648020369068063960672322193204149535)
              (bigint 41503128880339536053299340368006977710650566631954)
              (bigint 81234880673210146739058568557934581403627822703280)
              (bigint 82616570773948327592232845941706525094512325230608)
              (bigint 22918802058777319719839450180888072429661980811197)
              (bigint 77158542502016545090413245809786882778948721859617)
              (bigint 72107838435069186155435662884062257473692284509516)
              (bigint 20849603980134001723930671666823555245252804609722)
              (bigint 53503534226472524250874054075591789781264330331690)]]
       (->> (apply + v) (pelib/digit-list) (take 10) (pelib/from-digit-list))))

(defn pe014
  "collatz(n) = n/2 (if even) or 3n+1 (if odd)
  Which starting number, under 1 million, will make the longest collatz sequence."
  [] (letfn [(collatz [n] (if (even? n) (quot n 2) (inc (* n 3))))
             (collatz-seq-len [start] (loop [n start
                                             i 0]
                                        (if (= n 1)
                                          i
                                          (recur (collatz n) (inc i)))))]
       (apply max-key collatz-seq-len (range 1 1e6))))

(defn pe015
  "Starting in the top left corner of a 20x20 grid,
  only being able to move right and down,
  how many possible routes are there?"
  [] (let [f40 (pelib/fact 40)
           f20 (pelib/fact 20)]
       (long (/ f40 (* f20 f20)))))

(defn pe016
  "Digit sum of 2^1000."
  [] (long (apply + (pelib/digit-list (math/expt 2 1000)))))

(defn pe017
  "Sum of letters in numbers 1 to 1000."
  [] (let [oneToNine 36
           tenToNineteen 70
           twentyToNintynine (+ (* 10 46) ;prefixes 10 times
                                (* 8 oneToNine) ;1-9 occur 8 times
                                )
           oneToNintynine (+ oneToNine tenToNineteen twentyToNintynine)
           oneHundredTo999 (+ (* oneToNine 100) ;1-9 100 times
                              (* 9 oneToNintynine) ;9 of 1-99
                              (* 7 9) ;9 of "hundred"
                              (* 9 99 10) ;time "hundred and" occur 99*9 times
                              )
           oneThousand 11]
       (+ oneToNintynine oneHundredTo999 oneThousand)))

(defn- merge-rows
  "Helper functions for pe018 and pe067."
  [child parent] (map + parent (map #(apply max %) (partition 2 1 child))))

(defn pe018
  "Find the maximum sum from the top of a given triangle to the bottom."
  [] (let [tri [[75]
                [95 64]
                [17 47 82]
                [18 35 87 10]
                [20  4 82 47 65]
                [19  1 23 75  3 34]
                [88  2 77 73  7 63 67]
                [99 65  4 28  6 16 70 92]
                [41 41 26 56 83 40 80 70 33]
                [41 48 72 33 47 32 37 16 94 29]
                [53 71 44 65 25 43 91 52 97 51 14]
                [70 11 33 28 77 73 17 78 39 68 17 57]
                [91 71 52 38 17 14 91 43 58 50 27 29 48]
                [63 66  4 68 89 53 67 30 73 16 69 87 40 31]
                [ 4 62 98 27 23  9 70 98 73 93 38 53 60  4 23]]]
       (->> (reverse tri)
         (reduce merge-rows)
         (first))))

(defn pe019
  "How many Sundays fall on the first of the month during the 20th century.
  20th century is from Jan 1, 1901 to Dec 31 2000."
  [] (->> (for [year (range 1901 2001)
                month (range 12)]
            (java.util.GregorianCalendar. year month 1))
       (filter (fn [date] (= java.util.GregorianCalendar/SUNDAY
                             (.get date
                                   java.util.GregorianCalendar/DAY_OF_WEEK))))
       (count)))

(defn pe020
  "Digit sum of 100 factorial."
  [] (->> (pelib/fact 100)
       (pelib/digit-list)
       (apply +)
       (long)))

(defn pe021
  "Let d(n) be the sum of proper divisors of n.
  If d(a) = b and d(b) = a and a != b, then a and b are amicable numbers.
  Sum of all amicable numbers under 10000"
  [] (letfn [(d [n] (apply + (pelib/divisors n)))]
       (->> (for [i (range 1e4)
                  j [(d i)]
                  :when (and (not= i j)
                             (= (d j) i))]
              [i j])
         (flatten)
         (distinct)
         (apply +))))

(defn pe022
  "Given a text file of names, what is the total score of the names.
  A name's score the sum of the letter valus times the place.
  A letter value is defined as A=1, B=2, and so on..."
  [] (letfn [(letter-val [letter]
               (inc (- (int (java.lang.Character/toUpperCase letter))
                       (int \A))))
             (name-score [name] (apply + (map letter-val (seq name))))]
       (->> (sort (re-seq #"\w+" (slurp "resources/p022_names.txt")))
         (map name-score)
         (map-indexed (fn [index n] (* (inc index) n)))
         (apply +))))

(defn pe023
  "In an abundant number, the sum of it's proper divisors exceed it.
  Any number over 28123 can be written as a sum of two abundant numbers.
  12 is the smallest abundant number.
  Find the sum of all positive numbers that are not the sum of two abundant numbers."
  [] (letfn [(abundant? [n] (> (apply + (pelib/divisors n)) n))
             (abundant-nums [] (filter abundant? (range 12 28123)))]
       (apply + (remove (set (for [a [(abundant-nums)]
                                   i a
                                   j a]
                               (+ i j)))
                        (range 1 28123)))))

(defn pe024
  "The 1 millionth permutation of the digits 0 to 9."
  [] (pelib/from-digit-list (combo/nth-permutation (vec (range 10)) 999999)))

(defn pe025
  "The first fibonacci number with 1000 digits."
  [] (->> (pelib/lazy-fib 1N 1N)
       (map vector (iterate inc 1))
       (filter (fn [[_ f]] (>= (pelib/digit-count f) 1e3)))
       (ffirst)))

(defn pe026
  "Find 0<d<1000 where 1/d results in the longest repeating cycle of digits.
  Using Fermat's little theorem, 10^n-1 mod d = 0 (I have no idea how this works)"
  [] (letfn [(find-decimal-repeat [d] (loop [period 1]
                                        (if (= 1 (mod (math/expt 10 period) d))
                                          period
                                          (recur (inc period)))))]
       (loop [dseq (reverse (take-while #(< % 1000) (pelib/lazy-prime)))]
         (let [head (first dseq)]
           (if (= (dec head) (find-decimal-repeat head))
             head
             (recur (rest dseq)))))))

(defn pe027
  "For |a|<1000, |b|<1000, find the product of a and b where
  n^2+an+b returns the most amount of consecutive primes."
  [] (letfn [(quad [a b n] (+ (* n n) (* a n) b))
             (gen-quad [a b] (map #(quad a b %) (iterate inc 0)))]
       (->> (for [a (range -999 1000)
                  b (range -999 1000)
                  c [(count (take-while pelib/prime? (gen-quad a b)))]
                  :when (> c 65)]
              [(* a b) c])
         (max-key second)
         (ffirst))))

(defn pe028
  "Starting wiht 1 and moving to the right, clockwise, form a spiral of numbers.
  What is the sum of the numbers on the diagonals for a 1001x1001 spiral?"
  [] (apply + (take 2001 (reductions + 1 (mapcat #(repeat 4 %)
                                                 (iterate (partial + 2) 2))))))

(defn pe029
  "For 2<=a<=100, 2<=b<=100, how many distinct terms does a^b generate?"
  [] (->> (for [a (range 2 101)
                b (range 2 101)]
            (math/expt a b))
       (distinct)
       (count)))

(defn pe030
  "Find the sum of all the numbers that can be written as the sum
  of the fifth power of their digits."
  [] (letfn [(to-the-fifth [n] (math/expt n 5))
             (f [x] (= x (apply + (map to-the-fifth (pelib/digit-list x)))))]
       (apply + (filter f (range 2 2e5)))))

(defn- make-change
  "Returns how many ways you can make change with values in denominations list.
  Helper function pe031 and pe076."
  [denominations target]
  (let [lower (apply min denominations)
        change (promise)]
    (deliver change (memoize (fn [c v]
                               (let [f (first c)]
                                 (if (= f lower)
                                   lower
                                   (apply + (for [n (range 0 (inc (quot v f)))]
                                              (@change (rest c) (- v (* n f))))))))))
    (@change (sort-by num > denominations) target)))

(defn pe031
  "In England, mony comes in denominations of 1p, 2p, 5p, 10p, 20p, 50p, 100p, 200p
  How many ways can you make 2 pounds? (2 pounds is 200 pence)"
  [] (make-change '(1 2 5 10 20 50 100 200) 200))

(defn pe032
  "A pandigital number has all digits, 1 to n, exactly once.
  Find the sum of products where the multiplicand/multplier/product
  are 1 to 9 pandigital."
  [] (->> (for [i (range 2 5000)
                j (range i (/ 9999 i))
                r [(* i j)]
                :when (pelib/pandigital? (Integer/parseInt (str i j r)))]
            r)
       (distinct)
       (apply +)))

(defn pe033
  "Curious fractions are fractions where the digits cancel to get the right reduction.
  There are 4 non-trivial examples where the numerator or denominator.
  Find the product of these fractions and find the denominator."
  [] (->> (for [i (range 1 10)
                d (range 1 i)
                n (range 1 d)
                ;via math: (10n+i)/(10i+d) = n/d
                lside [(/ (+ (* 10 n) i) (+ (* 10 i) d))]
                rside [(/ n d)]
                :when (= lside rside)]
            rside)
       (apply *)
       (denominator)))

(defn pe034
  "A curious number is equal to the sum of the factorial of it's digits.
  Find the sum of all curious numbes."
  [] (apply + (for [i (range 10 45e3)
                    fact-sum [(apply + (map pelib/fact (pelib/digit-list i)))]
                    :when (= i fact-sum)]
                i)))

(defn pe035
  "A circular prime is a prime number where all of it's rotations of its digits.
  How many circular primes are under 1 million?"
  [] (letfn [(rotations [n] (let [xs (pelib/digit-list n)
                                  mag (count xs)]
                              (map pelib/from-digit-list
                                   (take mag (partition mag 1 (cycle xs))))))]
       (->> (take-while #(< % 1e6) (pelib/lazy-prime))
         (map rotations)
         (filter (partial every? pelib/prime?))
         (count))))

(defn pe036
  "Find the sum of all numbers under 1 million that are palindromic
  in base 10 and base 2."
  [] (->> (range 1 1e6)
       (filter (comp pelib/seq-palindrome? #(pelib/digit-list % 10)))
       (filter (comp pelib/seq-palindrome? #(pelib/digit-list % 2)))
       (apply +)))

(defn pe037
  "A trucatable prime is still prime when you remove its digits
  (from the left or from the right). Note 2, 3, 5, and 7 don't count.
  Find the sum of all trucatable primes."
  [] (letfn [(truc-left [x] (let [d (pelib/digit-list x)]
                              (for [i (range (count d))]
                                (pelib/from-digit-list (drop i d)))))
             (truc-right [x] (let [d (pelib/digit-list x)]
                               (for [i (range 1 (inc (count d)))]
                                 (pelib/from-digit-list (take i d)))))]
       (->> (drop 4 (take-while #(< % 75e4) (pelib/lazy-prime)))
         (filter #(every? pelib/prime? (truc-left %)))
         (filter #(every? pelib/prime? (truc-right %)))
         (apply +))))

(defn pe038
  "Find the largest pandigital number that cant be formed by the
  concatednated product of an interger range 1 to n."
  [] (letfn [(mult-cat [n i] (bigint (apply str (map (partial * n)
                                                     (range 1 (inc i))))))]
       (long (apply max (for [n (range 1 1e4)
                              i (range 2 10)
                              prod [(mult-cat n i)]
                              :when (pelib/pandigital? prod)]
                          prod)))))

(defn pe039
  "Find the perimeter of a right triangle with the most combinations of side
  lengths, where the perimeter is under 1000."
  [] (->> (for [p (range 2 1001 2)]
            (list p (count (for [a (range 1 (/ p 2))
                                 b (range 1 (/ p 2))
                                 c [(math/sqrt (+ (math/expt a 2)
                                                  (math/expt b 2)))]
                                 :when (= p (+ a b c))]
                             :validside))))
       (apply max-key second)
       (first)))

(defn pe040
  "Concatenate all the positive integers together. If dn is the nth digit,
  find d1 * d10 * d1e2 * d1e3 * d1e4 * d1e5 * d1e6"
  [] (let [lazy-idf (mapcat pelib/digit-list (iterate inc 0))]
       (apply * (map (partial nth lazy-idf) '(1 10 1e2 1e3 1e4 1e5 1e6)))))

(defn pe041
  "What is the largest n-digit pandigital prime?"
  [] (apply max (for [p (take-while #(< % 8e6) (pelib/lazy-prime))
                      n (range 1 10)
                      :when (pelib/pandigital? 1 n p)]
                  p)))

(defn pe042
  "Given a file of words, how many of the strings are triangular numbers?"
  [] (letfn [(letter-val [letter]
               (inc (- (int (java.lang.Character/toUpperCase letter))
                       (int \A))))
             (word-score [word] (apply + (map letter-val (seq word))))]
       (->> (re-seq #"\w+" (slurp "resources/p042_words.txt"))
        (filter (comp pelib/triangular? word-score))
        (count))))

(defn pe043
  "Find the sum of all 0 to 9 pandigital numbers where:
  -d2d3d4  is divisible by 2
  -d3d4d5  is divisible by 3
  -d4d5d6  is divisible by 5
  -d5d6d7  is divisible by 7
  -d6d7d8  is divisible by 11
  -d7d8d9  is divisible by 13
  -d8d9d10 is divisible by 17
  Note: dn is the nth digit of the number d."
  [] (letfn [(get-nums [number] (map pelib/from-digit-list
                                     (->> (pelib/digit-list number)
                                       (cycle)
                                       (partition 3 1)
                                       (take 8)
                                       (drop 1))))]
       (apply + (for [n (map pelib/from-digit-list
                             (combo/permutations (range 10)))
                      :when (every? identity
                                    (map (comp zero? rem)
                                         (get-nums n)
                                         '(2 3 5 7 11 13 17)))]
                  n))))

(defn pe044
  "Find the pair of pentagonal numbers that difference and sum
  are both pentagonal and minimized."
  [] (let [lazy-pentagonal (map #(/ (- (* 3 % %) %) 2) (iterate inc 1))]
       (first (for [j lazy-pentagonal
                    k (take-while #(< % j) lazy-pentagonal)
                    diff [(- j k)]
                    :when (every? pelib/pentagonal? [diff (+ j k)])]
                diff))))

(defn pe045
  "T(285)=P(165)=H(143)=40755
  Find the next triangular number that is also pentagonal and hexagonal."
  [] (->> (pelib/lazy-triangular)
       (filter pelib/pentagonal?)
       (filter pelib/hexagonal?)
       (drop 2)
       (first)))

(defn pe046
  "Goldbach's conjecture states every odd composite number can be written
  as the sum of a prime and twice a square. This conjecture was false.
  Find the smallest composite that cannot be written as such."
  [] (letfn [(goldbach? [n] (some #(pelib/prime? (- n %))
                                  (take-while #(< % n)
                                              (map #(* 2 % %) (range)))))]
       (->> (iterate inc 3)
         (filter odd?)
         (remove pelib/prime?)
         (remove goldbach?)
         (first))))

(defn pe047
  "Find the first 4 consecutive numbers that have 4 distinct primes."
  [] (letfn [(four-primes? [n] (= 4 (count (filter pelib/prime?
                                                   (pelib/factors n)))))
             (consecutive-sublists [xs] (->> (map-indexed vector xs)
                                          (partition-by #(apply - %))
                                          (map #(map last %))))]
       (->> (filter four-primes? (iterate inc 1))
         (consecutive-sublists)
         (filter #(= (count %) 4))
         (ffirst))))

(defn pe048
  "Find the last ten digits in the serise 1^1+2^2+3^2+...+1000^1000."
  [] (->> (map math/expt (range 1 1001) (range 1 1001))
       (apply +)
       (pelib/digit-list)
       (take-last 10)
       (pelib/from-digit-list)))

(defn pe049
  "1487, 4817, and 8147 are 3330 apart, each prime, and permutations of eachother.
  Find another set of 4-digit numbers and concatenate them."
  [] (->> (take-while #(< % 3339)
                      (drop-while #(< % 1000) (pelib/lazy-prime)))
       (filter #(pelib/prime? (+ % 3330)))
       (filter #(pelib/prime? (+ % 6660)))
       (filter #(= (sort (pelib/digit-list %))
                   (sort (pelib/digit-list (+ % 3330)))))
       (filter #(= (sort (pelib/digit-list %))
                   (sort (pelib/digit-list (+ % 6660)))))
       (remove #{1487 4817 8147})
       (apply #(mapcat pelib/digit-list [% (+ % 3330) (+ % 6660)]))
       (pelib/from-digit-list)))

(defn pe050
  "Which prime below 1 million can be written as the sum of the most consecutive primes?
  (I don't know how this works)"
  [] (let [prime-sums (reductions + (pelib/lazy-prime))
           goal 1e6]
       (loop [c 1]
         (let [bots (reverse (take c prime-sums))
               tops (take c (reverse (take-while #(> goal (- % (last bots)))
                                                 (rest prime-sums))))]
           (if-let [v (some #(if (pelib/prime? %) % nil) (map - tops bots))]
             v
             (recur (inc c)))))))

(defn pe051
  "Find smallest prime which, by replacing any of the digits, results in 8 primes."
  [] (letfn [(duplicate-digits [x] (mapcat (fn [[k v]] (if (> v 1) [k]))
                                           (frequencies
                                            (butlast
                                             (pelib/digit-list x)))))
             (underscores [targets] (for [t targets
                                          tx [(pelib/digit-list t)]
                                          d (duplicate-digits t)
                                          indices [(->> (butlast tx)
                                                     (map-indexed vector)
                                                     (filter (fn [[idx x]] (= x d)))
                                                     (map first))]
                                          powerset (->> (combo/subsets indices)
                                                     (filter (comp (partial < 1)
                                                                   count)))]
                                      (map-indexed (fn [idx x] (if (some (partial = idx)
                                                                         powerset)
                                                                 \_
                                                                 x))
                                                   tx)))]
       (->>
         (filter (fn [[k v]] (= v 8)) (->> (pelib/lazy-prime)
                                        (take-while #(< % 1e6))
                                        (drop-while #(< % 1e5))
                                        (remove (comp empty?
                                                      duplicate-digits))
                                        (underscores)
                                        (frequencies)))
         (ffirst)
         (replace {\_ 1})
         (pelib/from-digit-list))))

(defn pe052
  "Find the smallest x so that x, 2x, 3x, 4x, 5x, & 6x contain the same digits."
  [] (letfn [(dvec [n] (sort (pelib/digit-list n)))
             (same-digits? [x] (= (dvec x)
                                  (dvec (* 2 x))
                                  (dvec (* 3 x))
                                  (dvec (* 4 x))
                                  (dvec (* 5 x))
                                  (dvec (* 6 x))))]
       (first (filter same-digits? (iterate inc 1)))))

(defn pe053
  "How many (repeats are allowed) combinations (n choose r) for 1<=n<=100
  are greater than 1 million."
  [] (letfn [(choose [n r] (/ (pelib/fact n) (* (pelib/fact r)
                                                (pelib/fact (- n r)))))]
       (count (for [n (range 1 101)
                    r (range 1 n)
                    c [(choose n r)]
                    :when (> c 1e6)]
                c))))

(defn pe054
  "Given 100 poker hands in p054_poker.txt, how many rounds did player 1 win."
  [] (let [value {\2 2, \3 3, \4 4, \5 5, \6 6, \7 7, \8 8, \9 9, \T 10, \J 11, \Q 12, \K 13, \A 14}
           suits {\S :spades, \C :clubs, \D :diamonds, \H :hearts}]
       (letfn [(check-royalflush [hand] (if (and (= (map first hand) '(10 11 12 13 14))
                                                 (apply = (map second hand)))
                                          14
                                          0))
               (check-straightflush [hand] (let [c (ffirst hand)]
                                             (if (and (= (map first hand)
                                                         (range c (+ 5 c)))
                                                      (apply = (map second hand)))
                                               (first (last hand))
                                               0)))
               (check-4kind [hand] (let [[c freq] (first (filter (comp #{4} val) (frequencies (map first hand))))]
                                     (if (nil? c)
                                       0
                                       c)))
               (check-fullhouse [hand] (if (or (and (apply = (map first (take 2 hand)))
                                                    (apply = (map first (drop 2 hand))))
                                               (and (apply = (map first (take 3 hand)))
                                                    (apply = (map first (drop 3 hand)))))
                                         (first (last hand))
                                         0))
               (check-flush [hand] (if (apply = (map second hand))
                                     (first (last hand))
                                     0))
               (check-straight [hand] (let [c (ffirst hand)]
                                        (if (= (map first hand)
                                               (range c (+ 5 c)))
                                          (first (last hand))
                                          0)))
               (check-3kind [hand] (let [[c freq] (first (filter (comp #{3} val) (frequencies (map first hand))))]
                                     (if (nil? c)
                                       0
                                       c)))
               (check-2pair [hand] (let [pairs (filter (comp #{2} val) (frequencies (map first hand)))]
                                     (if (= 2 (count pairs))
                                       (first (last pairs))
                                       0)))
               (check-pair [hand] (let [pairs (filter (comp #{2} val) (frequencies (map first hand)))]
                                    (if (empty? pairs)
                                      0
                                      (first (last pairs)))))
               (high-card [hand] (first (last hand)))
               (player1-won? [p1 p2] (let [fvec [check-royalflush
                                                 check-straightflush
                                                 check-4kind
                                                 check-fullhouse
                                                 check-flush
                                                 check-straight
                                                 check-3kind
                                                 check-2pair
                                                 check-pair
                                                 high-card]]
                                       (loop [i 0] (let [p1score ((fvec i) p1)
                                                         p2score ((fvec i) p2)]
                                                     (cond
                                                       (< p1score p2score) false
                                                       (> p1score p2score) true
                                                       :else (recur (inc i)))))))]
         (count (for [round (string/split (slurp "resources/p054_poker.txt") #"\n")
                      :let [[c0 c1 c2 c3 c4 c5 c6 c7 c8 c9] (string/split round #" ")
                            player1 (sort-by first
                                             (map #(vector (value (first %))
                                                           (suits (second %)))
                                                  [c0 c1 c2 c3 c4]))
                            player2 (sort-by first
                                             (map #(vector (value (first %))
                                                           (suits (second %)))
                                                  [c5 c6 c7 c8 c9]))]
                      :when (player1-won? player1 player2)]
                  :win)))))

(defn pe055
  "A Lychrel number is a number that will not be palindromic when it is reversed
  and added to itself. How many Lychrel numbers are below 10 thousand?"
  [] (letfn [(lychrel-test-seq [n]
                               (rest
                                (iterate #(+ % (pelib/from-digit-list
                                                (reverse (pelib/digit-list %))))
                                         (bigint n))))
             (lychrel? [n] (not-any? (comp pelib/seq-palindrome?
                                           pelib/digit-list)
                                     (take 50 (lychrel-test-seq n))))]
       (count (filter lychrel? (range 1 1e4)))))

(defn pe056
  "Let a,b < 100, what is the maximum digit sum of a^b?"
  [] (->> (for [a (range 1 100), b (range 1 100)] (math/expt a b))
       (map (comp (partial apply +) pelib/digit-list))
       (apply max)
       (long)))

(defn pe057
  "sqrt(2) = 1 + 1/(2 + 1/(2 + 1/(2 + ...)))
  Expansions: 1 + 1/2 = 3/2, 1 + 1/(2 + 1/2) = 7/5, etc.
  In the first 1000 expansions, how many fractions contain
  a numerator with more digits than it's denominator?"
  [] (count (filter #(> (pelib/digit-count (numerator %))
                        (pelib/digit-count (denominator %)))
                    (take 1e3 (map inc (iterate #(/ 1 (+ 2 %)) 1/2))))))

(defn pe067
  "Find the maximum sum from the top of a given triangle to the bottom.
  Same as problem 18, but with a larger try given in a file."
  [] (let [tri (map (fn [line] (map #(Integer/parseInt %) (re-seq #"\d+" line)))
                    (string/split (slurp "resources/p067_triangle.txt") #"\n"))]
       (->> (reverse tri)
         (reduce merge-rows)
         (first))))

(defn pe069
  "Euler's Totient function, phi(n) returns numbers that are less then n and
  relatively prime to n. For what n < 1 million is n/phi(n) maximum."
  [] (last (take-while #(< %  1e6) (reductions * (pelib/lazy-prime)))))

(defn pe076
  "How many distinct ways can you count to 100 with at least 2 positive integers."
  [] (make-change (range 1 100) 100))

(defn pe089
  "Given a text file of improper roman numerals.
  Find the number of characters saved by replacing them with proper numberals."
  [] (let [roman {"I"  1
                  "IV" 4
                  "V"  5
                  "IX" 9
                  "X"  10
                  "XL" 40
                  "L"  50
                  "XC" 90
                  "C"  100
                  "CD" 400
                  "D"  500
                  "CM" 900
                  "M"  1000}
           text (string/split (slurp "resources/p089_roman.txt") #"\n")
           numbers (for [t text]
                     (apply +
                       (map roman
                            (re-seq #"I[VX]|X[LC]|C[DM]|I|V|X|L|C|D|M" t))))
           nneg? (comp not neg?)
           num->rn (fn [n] (loop [n n, s ""]
                             (cond
                               (nneg? (- n 1000)) (recur (- n 1000) (str s "M"))
                               (nneg? (- n 900)) (recur (- n 900) (str s "CM"))
                               (nneg? (- n 500)) (recur (- n 500) (str s "D"))
                               (nneg? (- n 400)) (recur (- n 400) (str s "CD"))
                               (nneg? (- n 100)) (recur (- n 100) (str s "C"))
                               (nneg? (- n 90)) (recur (- n 90) (str s "XC"))
                               (nneg? (- n 50)) (recur (- n 50) (str s "L"))
                               (nneg? (- n 40)) (recur (- n 40) (str s "XL"))
                               (nneg? (- n 10)) (recur (- n 10) (str s "X"))
                               (nneg? (- n 9)) (recur (- n 9) (str s "IX"))
                               (nneg? (- n 5)) (recur (- n 5) (str s "V"))
                               (nneg? (- n 4)) (recur (- n 4) (str s "IV"))
                               (nneg? (- n 1)) (recur (- n 1) (str s "I"))
                               :else s)))]
       (apply + (map (fn [t n] (- (count t) (count (num->rn n))))
                     text
                     numbers))))

(defn pe092
  "A chain is made by adding the square of the digits in a number.
  These numbers converge on either 1 or 89.
  How many starting number below ten million arrive at 89?"
  [] (letfn [(f [n] (if (or (= 1 n) (= 89 n))
                      n
                      (recur (apply + (map #(* % %) (pelib/digit-list n))))))]
       (count (for [i (range 1 1e7)
                    :when (= 89 (f i))]
                89))))
