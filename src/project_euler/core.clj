(ns project-euler.core
  (:require project-euler.solutions))

(defn run-problems [& problem_numbers]
  (in-ns 'project-euler.solutions)
  (letfn [(solution [n] (try (eval (symbol (format "pe%03d" n)))
                          (catch RuntimeException e (fn [] "no solution"))))
          (run-solution [n] (time ((solution n))))
          (print-begin [] (println (apply str (repeat 14 "="))
                                   "BEGIN"
                                   (apply str (repeat 14 "="))))
          (print-break [] (println (apply str (repeat 35 "="))))]
    (do (print-begin)
      (doseq [n problem_numbers]
        (println "Project Euler" n ":" (run-solution n))
        (print-break)))))

(defn -main
  "Solution Runner"
  [& args] (apply run-problems (map #(Integer/parseInt %) args)))
