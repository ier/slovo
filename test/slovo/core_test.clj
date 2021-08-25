(ns slovo.core-test
  (:require [clojure.test :refer [deftest testing is]]
            [slovo.core :refer [money words]]))


#_(deftest wrong-input
  ;nil
  ;""
  ;0
  ;-1
  )


(deftest ru-number-in-words-test  
  ;[input 9]
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

  (testing "zero rubles to string"
    (let [input 0
          expected "Ноль"]
      (is (= expected (words input)))))

  (testing "big number to string"
    (let [input 123456789987
          expected "Сто двадцать три миллиарда четыреста пятьдесят шесть миллионов семьсот восемьдесят девять тысяч девятьсот восемьдесят семь"]
      (is (= expected (words input))))))


(deftest ru-money-in-words-test
  (testing "zero money to string"
    (let [input 0
          expected "Ноль рублей ноль копеек"]
      (is (= expected (money input)))))

  (testing "money to string"
    (let [input 231525
          expected "Двести тридцать одна тысяча пятьсот двадцать пять рублей ноль копеек"]
      (is (= expected (money input))))))
