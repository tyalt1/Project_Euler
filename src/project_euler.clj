;; By Tyler Alterio

(require
  '[project_euler_lib :as pelib]
  '[clojure.string :only (split)]
  '[clojure.set :only (difference)]
  '[contrib.math.combinatorics :as combo :only (permutations)]
  '[contrib.math.numeric-tower :as numeric])

(defn print-pe-solution
  "A function for printing the solution to a Project Euler problem.
  Gets function from number, assuming fucntion is called pe??? where ??? is 3-digit problem number with leading zeros.
  If the funtion does not exsist then it will print 'no solution'."
  [problem-number]
  (let [solution-func (try (eval (symbol (format "pe%03d" problem-number)))
                           (catch RuntimeException e (fn [] "no solution")))]
    (time (println "Project Euler problem" problem-number ":" (solution-func)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;Prob 1 from Project Euler
;;The sum of the numbers that are multiples of 3 or 5 and less than 1000.
(defn pe001 []
  (->> (range 1 1000) (filter (fn [x] (or (= 0 (mod x 3)) (= 0 (mod x 5))))) (reduce +)))


;;Prob 2 from Project Euler
;;The sum of even Fibonacci numbers below 4 million.
(defn pe002 []
  (->> (pelib/lazy-fib) (take-while (fn [num] (< num 4000000))) (filter even?) (reduce +)))


;;Prob 3 from Project Euler
;;Find the highest prime factor of 600851475143.
(defn pe003 []
  (->> (pelib/factors 600851475143) (filter pelib/prime?) (apply max)))


;;Prob 4 from Project Euler
;;Find the plandromic number of 2 3-digit numbers
(defn pe004 []
  (let [digit-list-palindrome? (fn [digits] (loop [d digits]
                                              (cond
                                                (< (count d) 2) true
                                                (= (first d) (last d)) (recur (drop-last (drop 1 d)))
                                                :else false)))]
    (->> (for [i (range 100 1000), j (range 100 1000), :let [x (* i j)], :when (digit-list-palindrome? (pelib/digit-list x))] x) (apply max))))


;;Prob 5 from Project Euler
;;What is the smallest number that is divisible by the numbers 1 to 20?
(defn pe005 []
  (let [divisible-1to20? (fn [n] (let [div (fn [d] (= 0 (mod n d)))]
                                   (and (div 11) (div 12) (div 13) (div 14) (div 15)
                                        (div 16) (div 17) (div 18) (div 19) (div 20))))]
    (apply min (for [i (range 100000000 1000000000), :when (divisible-1to20? i)] i))))


;;Prob 6 from Project Euler
;;Difference between squares and the square of the sum of natural numbers 1 to 100.
(defn pe006 [] (let [nat-nums (range 1 101)]
                 (int (-
                        (Math/pow (->> nat-nums (reduce +)) 2)
                        (->> nat-nums (map (fn [x] (Math/pow x 2))) (reduce +))))))


;;Prob 7 from Project Euler
;;Find the 10,001th prime.
(defn pe007 []
  (->> (pelib/lazy-primes) (take 10001) (last) (println)))


;;Prob 8 from Project Euler
;;Given a 1000 digit number, find the largest product possible using 13 adjacent numbers.
;;Given number: 7316717653133062491922511967442657474235534919493496983520312774506326239578318016984801869478851843858615607891129494954595017379583319528532088055111254069874715852386305071569329096329522744304355766896648950445244523161731856403098711121722383113622298934233803081353362766142828064444866452387493035890729629049156044077239071381051585930796086670172427121883998797908792274921901699720888093776657273330010533678812202354218097512545405947522435258490771167055601360483958644670632441572215539753697817977846174064955149290862569321978468622482839722413756570560574902614079729686524145351004748216637048440319989000889524345065854122758866688116427171479924442928230863465674813919123162824586178664583591245665294765456828489128831426076900422421902267105562632111110937054421750694165896040807198403850962455444362981230987879927244284909188845801561660979191338754992005240636899125607176060588611646710940507754100225698315520005593572972571636269561882670428252483600823257530420752963450
(defn pe008 []
  (loop [x (pelib/digit-list 7316717653133062491922511967442657474235534919493496983520312774506326239578318016984801869478851843858615607891129494954595017379583319528532088055111254069874715852386305071569329096329522744304355766896648950445244523161731856403098711121722383113622298934233803081353362766142828064444866452387493035890729629049156044077239071381051585930796086670172427121883998797908792274921901699720888093776657273330010533678812202354218097512545405947522435258490771167055601360483958644670632441572215539753697817977846174064955149290862569321978468622482839722413756570560574902614079729686524145351004748216637048440319989000889524345065854122758866688116427171479924442928230863465674813919123162824586178664583591245665294765456828489128831426076900422421902267105562632111110937054421750694165896040807198403850962455444362981230987879927244284909188845801561660979191338754992005240636899125607176060588611646710940507754100225698315520005593572972571636269561882670428252483600823257530420752963450)
         maximum 0]
    (if (< (count x) 13)
      maximum
      (let [product (->> x (take 13) (apply *))]
        (if (> product maximum)
          (recur (drop 1 x) product)
          (recur (drop 1 x) maximum))))))


;;Prob 9 from Project Euler
;;Ingetegers a<b<c, where a^2+b^2=c^2, find a+b+c=1000, return the product of the 3.
(defn pe009 []
  (for [a (range 1 1000), b (range 1 1000), c (range 1 1000)
        :when (= (+ (Math/pow a 2) (Math/pow b 2)) (Math/pow c 2))
        :when (< a b c)
        :when (= (+ a b c) 1000)]
    (* a b c)))


;;Prob 10 from Project Euler
;;Sum of prime numbers below 2 million.
(defn pe010 []
  (->> (pelib/lazy-primes) (take-while (fn [x] (< x 2000000))) (reduce +)))


;;Prob 11 from Project Euler
;;Given a maxtrix, find the largest 4-number orthogonal/diagonal product.
(defn pe011 []
  (let* [maxtrix (vector
                   (vector  8  2 22 97 38 15  0 40  0 75  4  5  7 78 52 12 50 77 91  8)
                   (vector 49 49 99 40 17 81 18 57 60 87 17 40 98 43 69 48  4 56 62  0)
                   (vector 81 49 31 73 55 79 14 29 93 71 40 67 53 88 30  3 49 13 36 65)
                   (vector 52 70 95 23  4 60 11 42 69 24 68 56  1 32 56 71 37  2 36 91)
                   (vector 22 31 16 71 51 67 63 89 41 92 36 54 22 40 40 28 66 33 13 80)
                   (vector 24 47 32 60 99  3 45  2 44 75 33 53 78 36 84 20 35 17 12 50)
                   (vector 32 98 81 28 64 23 67 10 26 38 40 67 59 54 70 66 18 38 64 70)
                   (vector 67 26 20 68  2 62 12 20 95 63 94 39 63  8 40 91 66 49 94 21)
                   (vector 24 55 58  5 66 73 99 26 97 17 78 78 96 83 14 88 34 89 63 72)
                   (vector 21 36 23  9 75  0 76 44 20 45 35 14  0 61 33 97 34 31 33 95)
                   (vector 78 17 53 28 22 75 31 67 15 94  3 80  4 62 16 14  9 53 56 92)
                   (vector 16 39  5 42 96 35 31 47 55 58 88 24  0 17 54 24 36 29 85 57)
                   (vector 86 56  0 48 35 71 89  7  5 44 44 37 44 60 21 58 51 54 17 58)
                   (vector 19 80 81 68  5 94 47 69 28 73 92 13 86 52 17 77  4 89 55 40)
                   (vector  4 52  8 83 97 35 99 16  7 97 57 32 16 26 26 79 33 27 98 66)
                   (vector 88 36 68 87 57 62 20 72  3 46 33 67 46 55 12 32 63 93 53 69)
                   (vector  4 42 16 73 38 25 39 11 24 94 72 18  8 46 29 32 40 62 76 36)
                   (vector 20 69 36 41 72 30 23 88 34 62 99 69 82 67 59 85 74  4 36 16)
                   (vector 20 73 35 29 78 31 90  1 74 31 49 71 48 86 81 16 23 57  5 54)
                   (vector  1 70 54 71 83 51 54 69 16 92 33 48 61 43 52  1 89 19 67 48))
         vec-access (fn [x y] ((maxtrix y) x))
         up         (fn [x y] (try (* (vec-access x y) (vec-access x (- y 1)) (vec-access x (- y 2)) (vec-access x (- y 3)))                   (catch IndexOutOfBoundsException e 0)))
         down       (fn [x y] (try (* (vec-access x y) (vec-access x (+ y 1)) (vec-access x (+ y 2)) (vec-access x (+ y 3)))                   (catch IndexOutOfBoundsException e 0)))
         right      (fn [x y] (try (* (vec-access x y) (vec-access (+ x 1) y) (vec-access (+ x 2) y) (vec-access (+ x 3) y))                   (catch IndexOutOfBoundsException e 0)))
         left       (fn [x y] (try (* (vec-access x y) (vec-access (- x 1) y) (vec-access (- x 2) y) (vec-access (- x 3) y))                   (catch IndexOutOfBoundsException e 0)))
         up-right   (fn [x y] (try (* (vec-access x y) (vec-access (+ x 1) (- y 1)) (vec-access (+ x 2) (- y 2)) (vec-access (+ x 3) (- y 3))) (catch IndexOutOfBoundsException e 0)))
         up-left    (fn [x y] (try (* (vec-access x y) (vec-access (- x 1) (- y 1)) (vec-access (- x 2) (- y 2)) (vec-access (- x 3) (- y 3))) (catch IndexOutOfBoundsException e 0)))
         down-right (fn [x y] (try (* (vec-access x y) (vec-access (+ x 1) (+ y 1)) (vec-access (+ x 2) (+ y 2)) (vec-access (+ x 3) (+ y 3))) (catch IndexOutOfBoundsException e 0)))
         down-left  (fn [x y] (try (* (vec-access x y) (vec-access (- x 1) (+ y 1)) (vec-access (- x 2) (+ y 2)) (vec-access (- x 3) (+ y 3))) (catch IndexOutOfBoundsException e 0)))
         ]
    (->>
      (for [i (range 20), j (range 20)] (list (up i j) (down i j) (right i j) (left i j) (up-right i j) (up-left i j) (down-right i j) (down-left i j)))
      (flatten)
      (apply max))))


;;Problem 12 from Project Euler
;;Find the triangle number that has 500 divisors (# of factors - 1(itself))
;;A triangle number is the sum of the numbers before it: 7th = 1+2+3+4+5+6+7=28
(defn pe012 []
  (->> (pelib/lazy-triangular) (take 15000) (filter (fn [x] (> (dec (count (pelib/factors x))) 500))) (apply min)))


;;Problem 13 from Project Euler
;;Find the first ten deciamls of the sum of these numbers.
(defn pe013 []
  (let
    [x (vector
         (bigint 37107287533902102798797998220837590246510135740250)
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
         (bigint 53503534226472524250874054075591789781264330331690))]
    (->> (apply + x) (pelib/digit-list) (take 10) (pelib/from-digit-list))))


;;Problem 14 from Project Euler
;;Give a sequence (if (even? n) (next n/2) (next 3n+1))
;;Which starting number, under one million, produces the longest chain.
(defn pe014 []
  (let [collatz-seq (fn [start-num] (loop [n start-num, so-far []]
                                      (if (= n 1)
                                        (conj so-far 1)
                                        (if (even? n)
                                          (recur (/ n 2) (conj so-far n))
                                          (recur (inc (* 3 n)) (conj so-far n))))))]
    (->> (range 1 1000001)
      (map (fn [x] (list x (count (collatz-seq x)))))
      (sort (fn [x y] (> (second x) (second y))))
      (first)
      (first))))


;;Problem 15 from Project Euler
;;Given a 20x20 grid, what are the maximum amount of paths.
(defn pe015 []
  (/ (pelib/factorial (bigint 40)) (* (pelib/factorial (bigint 20)) (pelib/factorial (bigint 20)))))


;;Problem 16 from Project Euler
;;Sum of the digits of 2^1000.
(defn pe016 []
  (->> (numeric/expt (bigint 2) 1000) (pelib/digit-list) (apply +) (long)))


;;Problem 17 from Project Euler
;;Sum of the string lengths of the numbers 1 to 1000.
(defn pe017 []
  (let* [oneToNine 36
         tenToNinteen 70
         twentyToNintynine (+
                             (* 10 46) ;prefixs 10 times
                             (* 8 oneToNine)) ;1-9 occuring 8 times

         oneToNintynine (+ oneToNine tenToNinteen twentyToNintynine) ;What we have so far.

         oneHundredTo999 (+
                         (* oneToNine 100);1-9 100 times
                         (* 9 oneToNintynine);9 of 1-99
                         (* 7 9);9 of "hundred"
                         (* 9 99 10);time "hundred and" occur 99*9 times
                         )]
  (+ oneToNintynine oneHundredTo999 11)))


;;Problem 18
;;Find the path in a tree that yields the larget sum.
(defn merge-rows [child-vec parent-vec]
  (map + parent-vec (map #(apply max %) (partition 2 1 child-vec))))

(defn pe018 []
  (let [triangle [ [75]
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
    (->> triangle (reverse) (reduce merge-rows) (first))))


;;Problem 19
;;How many Sundays fell on the first of the month during he 20th century
;;20th century is from Jan/1/1901 to Dec/31/2000)
;;Jan/1/1900 was a Monday
(defn pe019 []
  (->>
    (for [year (range 1901 2001), month (range 12)] (java.util.GregorianCalendar. year month 1))
    (filter (fn [date] (= (.get date java.util.GregorianCalendar/DAY_OF_WEEK) java.util.GregorianCalendar/SUNDAY)))
    (count)))


;;Problem 20
;;Find the sum of the digits of 100 factorial
(defn pe020 []
  (->> (bigint 100) (pelib/factorial) (pelib/digit-list) (apply +)))


;;Problem 21
;;Let d(n) be the sum of the divisors of n.
;;If d(a)=b and d(b)=a and a != b, then a and b are amicable pairs.
(defn pe021 []
  (let [d #(apply + (butlast (sort (pelib/factors %))))]
    (->>
      (for [i (range 1 10000), :let [j (d i)], :when (and (not= i j) (= (d j) i))] [i j])
      (flatten)
      (set)
      (apply +))))


;;Problem 22
;;Given a list of names in a file (src/p022_names.txt)
;;Each name is a sum of the letters, where A is 1, B is 2...
;;What is the sum of the name values times their position
(defn pe022 []
  (let [letter-val #(- (inc (int (java.lang.Character/toUpperCase %))) (int \A))
        names (map #(subs % 1 (dec (count %))) (clojure.string/split (slurp "assets/p022_names.txt") #","))]
    (->> names
      (sort )
      (map #(seq (char-array %)) )
      (map #(apply + (map letter-val %)) )
      (map * (range 1 (inc (count names))) )
      (apply + ))))


;;Problem 23
;;An abundant number, n, is where the sum of its proper divisors is equal to n.
;;28123 is th largest number that can be represented as a sum of two abundant numbers.
;;Find the sum of all the positive integers that can be repesented as the sum of two abundant numbers.
(defn abundant? [n]
  (> (apply + (pelib/divisors n)) n))

(defn lazy-abundant
  ([] (lazy-abundant 12))
  ([n] (loop [i n] (if (abundant? i)
                     (cons i (lazy-seq (lazy-abundant (inc i))))
                     (recur (inc i))))))

(defn pe023 []
  (let [abundant-num (take-while #(< % 28123) (lazy-abundant))]
    (apply +
      (clojure.set/difference
        (set (range 1 28123))
        (set (for [i abundant-num, j abundant-num] (+ i j)))))))


;;Problem 24
;;Find the millionth permutation of the digits 0 to 9.
(defn pe024 []
  (pelib/from-digit-list
    (nth (combo/permutations [0 1 2 3 4 5 6 7 8 9]) 999999)))


;;Problem 25
;;Find the first Fibonacci number with 1000-digits
(defn pe025 []
  (let [upper-limit 10000]
    (->> (pelib/lazy-fib (bigint 1) (bigint 1))
      (take upper-limit)
      (map #(list %1 %2) (range 1 (inc upper-limit)))
      (filter #(>= (count (str (second %))) 1000))
      (first)
      (first))))


;;Problem 26
;;Find d, the positive integer under 1000 where 1/d contains the longest cycle of digits.
;;Using Fermat's little theorem, 10^n-1 mod d = 0 (I have no idea how this works)
(defn pe026 []
  (let [find-decimal-repeat (fn [d] (loop [period 1]
                                      (if (= 1 (int (mod (numeric/expt 10 period) d)))
                                        period
                                        (recur (inc period)))))]
    (loop [d-seq (reverse (take-while #(< % 1000) (pelib/lazy-primes)))]
      (if (== (dec (first d-seq)) (find-decimal-repeat (first d-seq)))
        (first d-seq)
        (recur (rest d-seq))))))


;;Problem 27
;;For ints a, b where their abs value is less then 1000.
;;Find the product of a and b where the function n^2+an+b yeilds the most primes.
(defn pe027 []
  (let [quad (fn [a b n] (+ (* n n) (* a n) b))
        consecutive-primes (fn [a b] (loop [n 0, xs []]
                                       (let [ans (quad a b n)]
                                         (if (pelib/prime? ans)
                                             (recur (inc n) (conj xs ans))
                                             xs))))]
    (->>
      (for [a (range -999 1000)
            b (range -999 1000)
            :let [c (count (consecutive-primes a b))]
            :when (> c 65)]
        (list (* a b) c))
      (sort-by second >)
      (first)
      (first))))


;;Problem 28
;;Given a spiral of numbers that is 1001x1001, find the sum of the diagnals.
;; Eaxample spiral of 5x5:
; 21 22 23 24 25
; 20  7  8  9 10
; 19  6  1  2 11
; 18  5  4  3 12
; 17 16 15 14 13 The sum of the diagnals of this spiral is 101.
(defn pe028 []
  (reduce + 1 (for [n (range 3 1002 2)] (+ (* 4 n n) (* -1 6 n) 6))))


;;Problem 29
;;Find all the unique solutions to a^b where 2<=a<=100 and 2<=b<=100
(defn pe029 []
  (->> (for [a (range 2 101), b (range 2 101)] (numeric/expt a b))
    (flatten)
    (set)
    (count)))


;;Project 30
;;Find the sum of all numbers that can be represented as the sum of (to the fifth power) of it's digits.
(defn pe030 []
  (let [f (fn [x] (apply + (map #(numeric/expt % 5) (pelib/digit-list x))))]
    (apply + (for [n (range 2 1000000), :when (= n (f n))] n))))


;;Problem 31
;;In England there are 8 types of pence coins: 1p, 2p, 5p, 10p, 20p, 50p, 100p, 200p.
;;How many ways can you make 2 pounds, or 200 pence? (Using any number of coins.)
(defn change [c v]
  (let [f (first c)]
    (if (= f 1)
        1
        (reduce + (for [n (range 0 (inc (quot v f)))]
                       (change (rest c) (- v (* n f))))))))

(defn pe031 []
  (let [target 200
        coins [200 100 50 20 10 5 2 1]]
    (change coins target)))


;;Problem 32
;;A pandigital number is a number that makes use of all 1 to n digits. 12345 is pandigital where n is 5.
;;The product 7254 and be written as 39*186=7254, the digits of these are pandigital (ust 1 to 9)
;;Find the sum of all products where their multiplicand/multiplier/product is pandigital.
(defn pe032 []
  (let [pandigital? (fn [lst] (let [s (count (flatten lst))
                                    dv (sort (flatten lst))]
                                (and (= s 9) (apply distinct? dv) (= 1 (first dv)) (= 9 (last dv)))))]
    (->> (for [i (range 2 5000)
               j (range i (/ 9999 i))
               :let [r (* i j)]
               :when (pandigital? (list (pelib/digit-list i)
                                        (pelib/digit-list j)
                                        (pelib/digit-list r)))]
           r)
      (set)
      (apply +))))


;;Problem 67
;;Same as problem 18, but with a larger tree. Stored in "src/p067_triangle.txt".
(defn pe067 []
  (let [triangle (map
                   (fn [x] (map #(Integer/parseInt %) (re-seq #"\d+" x)))
                   (clojure.string/split (slurp "assets/p067_triangle.txt") #"\n"))]
    ;uses merge-rows function used in pe018
    (->> triangle (reverse) (reduce merge-rows) (first))))
