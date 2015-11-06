(ns project-euler.core
  (:require project-euler.solutions))

(defn run-problems [& problem_numbers]
  (letfn [(solution [n] (try
                          (eval (symbol (format "project-euler.solutions/pe%03d"
                                                n)))
                          (catch RuntimeException e (fn [] "no solution"))))]
    (println (apply str (repeat 14 "="))
             "BEGIN"
             (apply str (repeat 14 "=")))
    (doseq [n problem_numbers]
      (println "Project Euler" n ":" (time ((solution n))))
      (println (apply str (repeat 35 "="))))))

(defn -main
  "Solution Runner"
  [& args] (apply run-problems (map #(Integer/parseInt %) args)))
