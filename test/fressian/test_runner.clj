(ns fressian.test-runner
  (:require [clojure.test :as t]
            [fressian.core-test]
            [fressian.lz4-test]))

(defn -main
  "Runs the JVM test suite for `clj-fressian`."
  [& _args]
  (let [res (t/run-tests 'fressian.core-test 'fressian.lz4-test)]
    (when (pos? (+ (:fail res 0) (:error res 0)))
      (System/exit 1))))
