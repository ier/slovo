(ns slovo.core-test
  (:require [clojure.test :refer [deftest testing is]]
            [slovo.core :refer [money words]]))


#_(deftest wrong-input
  ;nil
  ;""
  )


(deftest ru-number-in-words-test
  ;[input 1234]
  ;[input 12345]
  ;[input 123456]
  ;[input 1234567]
  ;[input 12345678]
  ;[input 123456789]
  ;[input 1234567890]
  ;[input 12345678901]
  ;[input 987654321010]
  ;[input 245124123142152172120]

  (testing "zero to string"
    (let [input 0
          expected "Ноль"]
      (is (= expected (words input)))))

  (testing "1 to 9"
    (let [inputs-to-expected {1 "Один"
                              2 "Два"
                              3 "Три"
                              4 "Четыре"
                              5 "Пять"
                              6 "Шесть"
                              7 "Семь"
                              8 "Восемь"
                              9 "Девять"}]
      (doseq [[input expected] inputs-to-expected]
        (is (= expected (words input))))))

  (testing "10 to 20"
    (let [inputs-to-expected {10 "Десять"
                              11 "Одинадцать"
                              22 "Двадцать два"
                              33 "Тридцать три"
                              44 "Сорок четыре"
                              55 "Пятьдесят пять"
                              66 "Шестьдесят шесть"
                              77 "Семьдесят семь"
                              88 "Восемьдесят восемь"
                              99 "Девяносто девять"}]
      (doseq [[input expected] inputs-to-expected]
        (is (= expected (words input))))))

  (testing "100 to 999"
    (let [inputs-to-expected {100 "Сто"
                              111 "Сто одинадцать"
                              222 "Двести двадцать два"
                              333 "Триста тридцать три"
                              444 "Четыреста сорок четыре"
                              555 "Пятьсот пятьдесят пять"
                              666 "Шестьсот шестьдесят шесть"
                              777 "Семьсот семьдесят семь"
                              888 "Восемьсот восемьдесят восемь"
                              999 "Девятьсот девяносто девять"}]
      (doseq [[input expected] inputs-to-expected]
        (is (= expected (words input))))))


  (testing "1000"
    (let [inputs-to-expected {1000 "Одна тысяча"}]
      (doseq [[input expected] inputs-to-expected]
        (is (= expected (words input))))))


  (testing "big number to string"
    (let [input 123456789987
          expected "Сто двадцать три миллиарда четыреста пятьдесят шесть миллионов семьсот восемьдесят девять тысяч девятьсот восемьдесят семь"]
      (is (= expected (words input))))))


#_(deftrest negative-values
  ;-1
  )


(deftest ru-money-in-words-test
  (testing "zero money to string"
    (let [input 0
          expected "Ноль рублей ноль копеек"]
      (is (= expected (money input)))))

  (testing "money to string"
    (let [input 231525
          expected "Двести тридцать одна тысяча пятьсот двадцать пять рублей ноль копеек"]
      (is (= expected (money input)))))

  (testing "zero kopecks 1"
    (let [input 00.00
          expected "Ноль рублей ноль копеек"]
      (is (= expected (money input)))))

  (testing "zero kopecks 2"
    (let [input 0.00
          expected "Ноль рублей ноль копеек"]
      (is (= expected (money input)))))

  (testing "zero kopecks 3"
    (let [input 0.0
          expected "Ноль рублей ноль копеек"]
      (is (= expected (money input)))))

  (testing "kopecks"
    (let [inputs-to-expected {1.00 "Один рубль ноль копеек"
                              7.0 "Семь рублей ноль копеек"

                              7.01 "Семь рублей одна копейка"
                              7.02 "Семь рублей две копейки"
                              7.03 "Семь рублей три копейки"
                              7.04 "Семь рублей четыре копейки"
                              7.05 "Семь рублей пять копеек"
                              7.06 "Семь рублей шесть копеек"
                              7.07 "Семь рублей семь копеек"
                              7.08 "Семь рублей восемь копеек"
                              7.09 "Семь рублей девять копеек"

                              28.000 "Двадцать восемь рублей ноль копеек"
                              37.99 "Тридцать семь рублей девяносто девять копеек"
                              46.990 "Сорок шесть рублей девяносто девять копеек"
                              55.994 "Пятьдесят пять рублей девяносто девять копеек"
                              64.999 "Шестьдесят четыре рубля девяносто девять копеек"}]
      (doseq [[input expected] inputs-to-expected]
        (is (= expected (money input))))))

  (testing "kopecks 2"
    (let [inputs-to-expected {7.10 "Семь рублей десять копеек" ;; TODO: fix me
                              7.11 "Семь рублей одинадцать копеек"
                              7.12 "Семь рублей двенадцать копеек"
                              7.13 "Семь рублей тринадцать копеек"
                              7.14 "Семь рублей четырнадцать копеек"
                              7.15 "Семь рублей пятнадцать копеек"
                              7.16 "Семь рублей шестнадцать копеек"
                              7.17 "Семь рублей семнадцать копеек"
                              7.18 "Семь рублей восемнадцать копеек"
                              7.19 "Семь рублей девятнадцать копеек"
                              7.20 "Семь рублей двадцать копеек" ;; TODO: fix me
                              7.21 "Семь рублей двадцать одна копейка"
                              7.22 "Семь рублей двадцать две копейки"
                              7.23 "Семь рублей двадцать три копейки"
                              7.24 "Семь рублей двадцать четыре копейки"
                              7.25 "Семь рублей двадцать пять копеек"
                              7.26 "Семь рублей двадцать шесть копеек"
                              7.27 "Семь рублей двадцать семь копеек"
                              7.28 "Семь рублей двадцать восемь копеек"
                              7.29 "Семь рублей двадцать девять копеек"
                              
                              7.3 "Семь рублей тридцать копеек"
                              7.4 "Семь рублей сорок копеек"
                              7.5 "Семь рублей пятьдесят копеек"
                              7.6 "Семь рублей шестьдесят копеек"
                              7.7 "Семь рублей семьдесят копеек"
                              7.8 "Семь рублей восемьдесят копеек"
                              7.9 "Семь рублей девяносто копеек"}]
      (doseq [[input expected] inputs-to-expected]
        (is (= expected (money input)))))))
