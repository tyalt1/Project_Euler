(defproject project_euler "1.0.0-SNAPSHOT"
  :description "Solution to Project Euler problems."
  :dependencies [[org.clojure/clojure "1.7.0"]
                 [org.clojure/math.numeric-tower "0.0.4"]
                 [org.clojure/math.combinatorics "0.1.1"]]
  :license {:name "Eclipse Public License v1.0"
            :url "http://www.eclipse.org/legal/epl-v10.html"
            :distribution :repo
            :comments "same as Clojure"}
  :repl-options {:init-ns project-euler.solutions}
  :main project-euler.core)
