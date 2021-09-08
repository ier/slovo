(ns slovo.core-test
  (:require [clojure.test :refer [deftest testing is]]
            [slovo.core :refer [money words]]))


#_(deftest wrong-input
  ;nil
  ;""
  )


(deftest ru-number-in-words-test
  ;[input 30]
  ;[input 193]
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
                              }]
      (doseq [[input expected] inputs-to-expected]
        (is (= expected (words input))))))

  #_(comment 100 "Сто" 1000 "Одна тысяча")
  
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
      (is (= expected (money input))))))

