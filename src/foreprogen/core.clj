(ns foreprogen.core)

(defn last-element
  [x]
  (if (= (count x) 1)
    (first x) (last-element (rest x))))

(defn penultimate-element
  [x]
  (if (= (count x) 2)
    (first x)
    (penultimate-element (rest x))))

(defn a-nil-key
  [x y]
  (if (find y x)
    (if (= nil (val (find y x)))
      true
      false)
    false))

(defn nth-element
  [list x]
  (loop [lst list i 0]
    (if (= i x)
      (first lst)
      (recur (rest lst) (inc i)))))
      
(defn count-a-sequence
  [list]
  (loop [lst list i 0]
    (if (empty? lst)
      i
      (recur (rest lst) (inc i)))))

(defn factorial-fun
  [x]
  (loop [hasil 1 i 1]
    (if (= x (dec i))
      hasil
      (recur (* hasil i) (inc i)))))

(defn perfect-numbers
  [x]
  (loop [hasil 0 i 1]
    (cond
      (= hasil x) true
      (> hasil x) false
      :else (recur (+ hasil i) (inc i)))))

(defn dot-product 
  [x y]
  (let [a (* (first x) (first y)) b (* (second x) (second y)) c (* (last x) (last y))]
    (+ a b c)))

(defn maximum-value
  [& x]
  (last (sort x)))

(defn get-the-caps
  [x]
  (clojure.string/join "" (re-seq #"[A-Z]" x)))

(defn split-a-sequence
  [x y]
  [(take x y) (take-last (- (count y) x) y)])

(defn fibonacci-sequence
  [x]
  (take x (map first (iterate (fn [[a b]] [b (+' a b)]) [1 1]))))

(defn tribonacci
  [x]
  (take x (map first (iterate (fn [[a b c]] [b c (+' a b c)]) [0 1 1]))))

(defn akar-persamaan-kuadrat
  [[a b c]]
  (let [det (Math/sqrt (- (* b b) (* 4 a c)))]
    (if (= (int (/ (+ (- b) det) (* 2 a))) 0)
      (hash-set)
      (hash-set (int (/ (+ (- b) det) (* 2 a)))
                (int (/ (- (- b) det) (* 2 a)))))))

(defn interpose-a-seq
  [x y]
  (drop-last (interleave y (repeat x))))

(defn duplicate-a-sequence
  [x]
  (interleave x x))

(defn implement-range
  [x y]
  (loop [siu [] i x]
    (if (= i y)
      siu
      (recur (conj siu i) (inc i)))))

(defn map-construction
  [x y]
  (apply array-map (interleave x y)))

(defn interleave-two-seqs
  [x y]
  (loop [lst [] a x b y]
    (if (or (empty? a) (empty? b))
      (flatten lst)
      (recur (conj lst [(first a) (first b)]) (rest a) (rest b)))))

(defn set-intersection
  [x y]
  (set 
    (for [a x
          b y
          :when (= a b)]
      a)))

(defn cartesian-product
  [x y]
  (set
    (for [a x
          b y]
      [a b])))

(defn indexing-sequences
  [x]
  (loop [lst [] a x b (take (count a) (range))]
    (if (empty? a)
      lst
      (recur (conj lst [(first a) (first b)]) (rest a) (rest b)))))

(defn product-digits
  [& n]
  (->> (apply * n)
       (str)
       (mapv #(Integer/parseInt (str %)))))

(defn read-a-binary-number
  [x]
  (loop [hasil [] biner (reverse (mapv #(Integer/parseInt (str %)) x)) i 1]
    (if (empty? biner)
      (apply + hasil)
      (recur (conj hasil (* (first biner) i)) (rest biner) (* i 2)))))

(defn intoCamelCase
  [s]
  (loop [kata s hasil []]
    (cond
      (empty? kata) (apply str hasil)
      (#{\-} (first kata)) (recur (rest (rest kata)) (conj hasil (.toUpperCase (str (second kata)))))
      :else (recur (rest kata) (conj hasil (first kata)))))) 

(defn word-sorting
  [s]
  (-> (apply str (butlast s))
      (clojure.string/split #" ")
      (->> (sort-by clojure.string/lower-case))))

(defn flipping-out
  [f]
  #(f %2 %1))

(defn filter-perfect-squares [x]
  (loop [angka (clojure.string/split x #",") hasil []]
    (cond 
      (empty? angka) (clojure.string/join "," hasil)
      (#{"1" "4" "9" "16" "25" "36" "49" "64" "81"} (first angka)) (recur (rest angka) (conj hasil (first angka)))
      :else (recur (rest angka) hasil)))) 

(defn replicate-a-sequence
  [x y]
  (loop [lst [] col x i 1]
    (cond
      (empty? col) lst
      (> i y) (recur lst (rest col) (- i y))
      :else (recur (conj lst (first col)) col (inc i)))))

(defn drop-every-nth-item
  [x y]
  (loop [lst [] col x i 1]
    (cond
      (empty? col) lst
      (= 0 (mod i y)) (recur lst (rest col) (inc i))
      :else (recur (conj lst (first col)) (rest col) (inc i)))))

(defn comparisons
  [f x y]
  (cond
    (f x y) :lt
    (f y x) :gt
    :else :eq))
