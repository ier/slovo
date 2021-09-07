(ns slovo.core
  (:require
   [clojure.string :refer [trim blank? capitalize]])
  (:gen-class))


(def ^:private hundreds [" сто" " двести" " триста" " четыреста" " пятьсот" " шестьсот" " семьсот" " восемьсот" " девятьсот"])
(def ^:private tens     [" двадцать" " тридцать" " сорок" " пятьдесят" " шестьдесят" " семьдесят" " восемьдесят" " девяносто"])
(def ^:private units    [" три" " четыре" " пять" " шесть" " семь" " восемь" " девять" " десять" " одинадцать" " двенадцать" " тринадцать" " четырнадцать" " пятнадцать" " шестнадцать" " семнадцать" " восемнадцать" " девятнадцать"])
(def ^:private sections [" тысяч" " миллион" " миллиард" " триллион" " квадриллион" " квинтилион" " секстиллион" " септилион" " октилион" " нониллион" " дециллион"])


(defn- short-number
  [num section]
  (let [number (Integer/parseInt num)
        h (quot number 100)            ;; сотни числом
        t (quot (mod number 100) 10)   ;; десятки числом
        r0 (hundreds (dec h))          ;; сотни прописью
        s (when (pos? section)
            (sections (dec section)))] ;; разряд прописью
    (if (= t 1)
      (let [r1 (units (- (mod number 100) 3))
            r (str r0 r1 s)]
        (if (> section 1)
          (str r "ов")
          r))
      (let [u (mod number 10)          ;; единицы числом
            r1 (tens (- t 2))
            r2 (units (- u 3))
            r (cond
                (= u 1) (if (= section 1) (str r0 r1 " одна") (str r0 r1 " один"))
                (= u 2) (if (= section 1) (str r0 r1 " две") (str r0 r1 " два"))
                (or (and (>= u 3) (<= u 9)) (= u 0)) (str r0 r1 r2))
            rs (str r s)]
        (cond
          (= u 1) (if (= section 1) (str rs "а") rs)
          (and (>= u 2) (<= u 4)) (if (= section 1) (str rs "и") (if (> section 1) (str rs "а") rs))
          (or (and (>= u 5) (<= u 9)) (= u 0)) (if (> section 1) (str rs "ов") rs))))))


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
          (format "%d")
          (partition 3)
          reverse)))))


(defn- in-words
  [number]
  (if (zero? number)
    "ноль"
    (->> number
         categories
         (map #(short-number (second %) (first %)))
         reverse
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
