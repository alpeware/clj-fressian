(ns fressian.cljs-runner
  (:require [cljs.build.api :as b]
            [clojure.java.io :as io]
            [clojure.java.shell :as sh]))

(defn -main
  "Compiles and runs the Node.js ClojureScript test suite for `clj-fressian`."
  [& _args]
  (let [out-dir "target/cljs-test"
        out-file (str out-dir "/test-main.js")
        runner-src (io/file out-dir "fressian" "node_runner.cljs")]
    (.mkdirs (.getParentFile runner-src))
    (spit runner-src
          (pr-str
           '(ns fressian.node-runner
              (:require [cljs.test :refer-macros [run-tests]]
                        [fressian.core-test]
                        [fressian.lz4-test]))))
    (spit runner-src
          "\n(defmethod cljs.test/report [:cljs.test/default :end-run-tests] [m]\n  (when-not (cljs.test/successful? m)\n    (js/process.exit 1)))\n(defn -main [& _]\n  (run-tests 'fressian.core-test 'fressian.lz4-test))\n(set! *main-cli-fn* -main)\n"
          :append true)
    (b/build (b/inputs "src" "test" out-dir)
             {:main 'fressian.node-runner
              :output-to out-file
              :output-dir (str out-dir "/out")
              :target :nodejs
              :optimizations :none})
    (let [{:keys [exit out err]} (sh/sh "node" out-file)]
      (when (seq out) (print out) (flush))
      (when (seq err) (binding [*out* *err*] (print err) (flush)))
      (when-not (zero? exit)
        (System/exit exit)))))
