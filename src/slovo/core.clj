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
  (->> v
       (filter #(not (blank? %)))
       (interpose " ")
       (apply str)))

(defn- translate-to-text
  [value index feminine-numeral-mode]
  (let [number (->> value trim Integer/parseInt)
        u (mod number 10)
        t (quot (mod number 100) 10)
        h (quot number 100)]
    (cond
      (and (pos? u) (zero? t) (zero? h) (zero? index))
      (cond
        (= u 1) (if feminine-numeral-mode "одна" "один")
        (= u 2) (if feminine-numeral-mode "две" "два")
        (and (>= u 3) (<= u 9)) (units (- u 3)))

      :else
      (let [h* (when (pos? h) (hundreds (dec h)))
            s (when (pos? index) (sections (dec index)))]
        (cond
          (= t 1)
          (let [t* (units (- (mod number 100) 3))
                res (vec->str [h* t* s])]
            (if (> index 1)
              (str res "ов")
              res))

          :else
          (let [t* (when (>= t 2) (tens (- t 2)))
                u* (when (and (>= u 3) (<= u 9)) (units (- u 3)))
                res (cond
                      (= u 1)
                      (if (= index 1)
                        (vec->str [h* t* "одна"])
                        (vec->str [h* t* (if feminine-numeral-mode "одна" "один")]))

                      (= u 2)
                      (if (= index 1)
                        (vec->str [h* t* "две"])
                        (vec->str [h* t* (if feminine-numeral-mode "две" "два")]))

                      (or (and (>= u 3) (<= u 9)) (= u 0))
                      (vec->str [h* t* u*]))
                result (vec->str [res s])]
            (cond
              (= u 1)
              (if (= index 1)
                (str result "а")
                result)

              (and (>= u 2) (<= u 4))
              (if (= index 1)
                (str result "и")
                (if (> index 1)
                  (str result "а")
                  result))

              (or (and (>= u 5) (<= u 9)) (= u 0))
              (if (> index 1)
                (str result "ов")
                result))))))))

(defn- categories
  "Получить пронумерованные разряды единиц, тысяч, миллионов etc."
  [number]
  (filter
   #(not (blank? (second %)))
   (map-indexed
    (fn [idx idm] [idx idm])
    (map
     #(apply str %)
     (->> number
          biginteger
          (format "%36d")
          (partition 3)
          reverse)))))

(defn- in-words
  ([number]
   (in-words number false))
  ([number feminine-numeral-mode]
   (if (zero? number)
     "ноль"
     (->> number
          categories
          (map #(translate-to-text (second %) (first %) feminine-numeral-mode))
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
    (let [units (mod input 10)
          tens (-> input (mod 100) (quot 10))]
      (if (= tens 1)
        "рублей"
        (cond
          (= 1 units) "рубль"
          (contains? #{2 3 4} units) "рубля"
          :else "рублей")))))

(defn- parse-money
  [number]
  (cond
    (integer? number) {:whole number
                       :fractional 0}
    (float? number) (let [value (->> number double (format "%.3f"))
                          parts (-> value str (split #"\."))
                          whole (->> parts first Integer/parseInt)
                          scnd (second parts)
                          len (min (count scnd) 2)
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
        feminine-numeral-mode? true
        fractional-words (in-words fractional feminine-numeral-mode?)
        kopeck (kopecks fractional)]
    (str whole-words " " ruble " " fractional-words " " kopeck)))
