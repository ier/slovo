(defproject slovo "0.1.0-SNAPSHOT"
  :description "Represent numbers and money in Russian words"
  :url "https://github.com/ier/slovo"
  :license {:name "MIT Licence"
            :url "https://github.com/ier/slovo/blob/main/LICENSE"}
  :dependencies [[org.clojure/clojure "1.10.1"]]
  :main ^:skip-aot slovo.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all
                       :jvm-opts ["-Dclojure.compiler.direct-linking=true"]}})
