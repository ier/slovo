(ns slovo.core
  (:require
   [clojure.string :refer [trim blank? capitalize split]]))

(def ^:private hundreds
  ["сто"
   "двести"
   "триста"
   "четыреста"
   "пятьсот"
   "шестьсот"
   "семьсот"
   "восемьсот"
   "девятьсот"])

(def ^:private tens
  ["двадцать"
   "тридцать"
   "сорок"
   "пятьдесят"
   "шестьдесят"
   "семьдесят"
   "восемьдесят"
   "девяносто"])

(def ^:private units
  ["три"
   "четыре"
   "пять"
   "шесть"
   "семь"
   "восемь"
   "девять"
   "десять"
   "одиннадцать"
   "двенадцать"
   "тринадцать"
   "четырнадцать"
   "пятнадцать"
   "шестнадцать"
   "семнадцать"
   "восемнадцать"
   "девятнадцать"])

(def ^:private sections
  ["тысяч"
   "миллион"
   "миллиард"
   "триллион"
   "квадриллион"
   "квинтилион"
   "секстиллион"
   "септилион"
   "октилион"
   "нониллион"
   "дециллион"])

(defn- vec->str
  [v]
  (->> (remove blank? v)
       (interpose " ")
       (apply str)))

(defn- parse-parts
  [value]
  (let [number (->> value trim Integer/parseInt)]
    [number
     (mod number 10)
     (quot (mod number 100) 10)
     (quot number 100)]))

(defn- translate-to-text
  [[index value] feminine-numeral-mode]
  (let [[number u t h] (parse-parts value)]
    (if (and (pos? u) (zero? t) (zero? h) (zero? index))
      ;; смотрим разряд единиц...
      (cond
        (= u 1) (if feminine-numeral-mode "одна" "один")
        (= u 2) (if feminine-numeral-mode "две" "два")
        (<= 3 u 9) (units (- u 3)))
      ;; смотрим разряды выше единицы
      (let [h* (when (pos? h) (hundreds (dec h)))
            s (when (pos? index) (sections (dec index)))]
        (if (= t 1)
          ;; смотрим первый десяток...
          (let [res (vec->str [h* (units (- (mod number 100) 3)) s])]
            (if (> index 1) (str res "ов") res))
          ;; смотрим десятки выше первого...
          (let [t* (when (>= t 2) (tens (- t 2)))
                res (case u
                      1 (if (= index 1)
                          (vec->str [h* t* "одна"])
                          (vec->str [h* t* (if feminine-numeral-mode "одна" "один")]))
                      2 (if (= index 1)
                          (vec->str [h* t* "две"])
                          (vec->str [h* t* (if feminine-numeral-mode "две" "два")]))
                      (vec->str [h* t* (when (<= 3 u 9) (units (- u 3)))]))
                result (vec->str [res s])]
            (cond
              (= u 1)
              (if (= index 1) (str result "а") result)

              (and (>= u 2) (<= u 4))
              (if (= index 1)
                (str result "и")
                (if (> index 1) (str result "а") result))

              (or (<= 3 u 9) (= u 0))
              (if (> index 1) (str result "ов") result))))))))

(defn- categories
  "Получить пронумерованные разряды единиц, тысяч, миллионов etc."
  [number]
  (->> number
       biginteger
       (format "%36d")
       (partition 3)
       reverse
       (map #(apply str %))
       (remove blank?)
       (map-indexed vector)))

(defn- in-words
  ([number]
   (in-words number false))
  ([number feminine-numeral-mode]
   (if (zero? number)
     "ноль"
     (->> number
          categories
          (map #(translate-to-text % feminine-numeral-mode))
          reverse
          (interpose " ")
          (reduce str)
          trim))))

(defn words
  [number]
  (->> number
       in-words
       capitalize))

(defn- rubles
  [input]
  (if (zero? input)
    "рублей"
    (if (-> input (mod 100) (quot 10) (= 1))
      "рублей"
      (let [units (mod input 10)]
        (cond
          (= 1 units) "рубль"
          (contains? #{2 3 4} units) "рубля"
          :else "рублей")))))

(defn- parse-money
  [number]
  (cond
    (integer? number)
    {:whole number
     :fractional 0}

    (float? number)
    (let [value (->> number double (format "%.3f") str)
          parts (split value #"\.")
          whole (->> parts first Integer/parseInt)
          scnd (second parts)
          len (-> scnd count (min 2))
          fractional (-> scnd (subs 0 len) Integer/parseInt)]
      {:whole whole
       :fractional fractional})))

(defn- kopecks
  [input]
  (let [tens (int (/ input 10))
        units (- input (* tens 10))]
    (if (or (and (> tens 1) (zero? units))
            (and (= tens 1) (pos? units)))
      "копеек"
      (cond
        (= 1 units) "копейка"
        (contains? #{2 3 4} units) "копейки"
        (contains? #{0 5 6 7 8 9} units) "копеек"))))

(defn money
  [number]
  (let [{:keys [whole fractional]} (parse-money number)
        whole-words (words whole)
        ruble (rubles whole)
        fractional-words (in-words fractional true)
        kopeck (kopecks fractional)]
    (str whole-words " " ruble " " fractional-words " " kopeck)))
