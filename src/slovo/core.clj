(ns slovo.core
  (:require
   [clojure.string :refer [trim blank? capitalize]])
  (:gen-class))


(def ^:private hundreds ["сто"
                         "двести"
                         "триста"
                         "четыреста"
                         "пятьсот" 
                         "шестьсот"
                         "семьсот"
                         "восемьсот"
                         "девятьсот"])

(def ^:private tens     ["двадцать"
                         "тридцать"
                         "сорок"
                         "пятьдесят"
                         "шестьдесят"
                         "семьдесят"
                         "восемьдесят"
                         "девяносто"])

(def ^:private units    ["три"
                         "четыре"
                         "пять"
                         "шесть"
                         "семь"
                         "восемь"
                         "девять"
                         "десять"
                         "одинадцать"
                         "двенадцать"
                         "тринадцать"
                         "четырнадцать"
                         "пятнадцать"
                         "шестнадцать"
                         "семнадцать"
                         "восемнадцать"
                         "девятнадцать"])

(def ^:private sections ["тысяч"
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


(defn- translate-to-text
  [value index]
  (let [number (->> value trim Integer/parseInt)
        u (mod number 10)
        t (quot (mod number 100) 10)
        h (quot number 100)
        fnx (fn [v]
              (->> v
                   (filter #(not (blank? %)))
                   (interpose " ")
                   (apply str)))]
    (cond
      (and (pos? u) (zero? t) (zero? h) (zero? index))
      (cond
        (= u 1) "один"
        (= u 2) "два"
        (and (>= u 3) (<= u 9)) (units (- u 3)))

      :else
      (let [r0 (when (pos? h) (hundreds (dec h)))
            s (when (pos? index) (sections (dec index)))]
        (cond
          (= t 1)
          (let [r1 (units (- (mod number 100) 3))
                r (fnx [r0 r1 s])]
            (if (> index 1) (str r "ов") r))

          :else
          (let [r1 (when (>= t 2) (tens (- t 2)))
                r2 (when (and (>= u 3) (<= u 9)) (units (- u 3)))
                r (cond
                    (= u 1)
                    (if (= index 1) (fnx [r0 r1 "одна"]) (fnx [r0 r1 "один"]))

                    (= u 2)
                    (if (= index 1) (fnx [r0 r1 "две"]) (fnx [r0 r1 "два"]))

                    (or (and (>= u 3) (<= u 9)) (= u 0))
                    (fnx [r0 r1 r2]))
                rs (fnx [r s])]
            (cond
              (= u 1)
              (if (= index 1) (str rs "а") rs)

              (and (>= u 2) (<= u 4))
              (if (= index 1) (str rs "и") (if (> index 1) (str rs "а") rs))

              (or (and (>= u 5) (<= u 9)) (= u 0))
              (if (> index 1) (str rs "ов") rs))))))))


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
  [number]
  ;; {:pre ...}
  (if (zero? number)
    "ноль"
    (->> number
         categories
         (map #(translate-to-text (second %) (first %)))
         reverse
         (interpose " ")
         (reduce str)
         trim)))


(defn words
  [number]
  (capitalize (in-words number)))


(defn- rubles
  [input]
  (if (zero? input)
    "рублей"
    (let [sub-int (fn [value from to]
                    (Integer/parseInt (subs value from to)))
          value (->> input
                     (format "%03d")
                     reverse
                     (reduce str))
          units (sub-int value 0 1)
          tens (sub-int value 1 2)]
      (if (= tens 1)
        "рублей"
        (cond
          (= 1 units) "рубль"
          (contains? #{2 3 4} units) "рубля"
          :else "рублей")))))


(defn money
  [number]
  (let [words (->> number words)
        rubles (->> number rubles)
        kopecks (in-words 0)]
    (str words " " rubles " " kopecks " копеек")))
