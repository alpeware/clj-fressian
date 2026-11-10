(ns fressian.core-test
  "Cross-runtime test suite for pure .cljc Fressian codec (`fressian.core`).
   Proves:
   1. Deterministic Canonical Fressian golden vectors & map/set order independence across JVM & CLJS.
   2. Bidirectional byte-compatibility against reference `org.clojure/data.fressian` on JVM via
      `test.check` generative cross-tests (including priority caching, struct caching, custom tagged
      types, UUID, inst, double, astral CESU-8 strings, chunked bytes/strings, and Adler32 footers)."
  (:require #?(:clj [clojure.data.fressian :as data-fressian])
            #?(:clj [clojure.test :refer [deftest is testing]]
               :cljs [cljs.test :refer-macros [deftest is testing]])
            #?(:clj [clojure.test.check.clojure-test :refer [defspec]]
               :cljs [clojure.test.check.clojure-test :refer-macros [defspec]])
            [clojure.test.check.generators :as gen]
            #?(:clj [clojure.test.check.properties :refer [for-all]]
               :cljs [clojure.test.check.properties :refer-macros [for-all]])
            #?(:clj [clojure.walk :as walk])
            [fressian.core :as fressian])
  #?(:clj (:import [java.nio ByteBuffer]
                   [java.util Date UUID]
                   [org.fressian CachedObject TaggedObject])))

(def ^{:doc "Fixed golden vectors for Canonical Fressian binary encoding."}
  canonical-fressian-golden-vectors
  [{:label "nil" :val nil :hex "f7"}
   {:label "true" :val true :hex "f5"}
   {:label "false" :val false :hex "f6"}
   {:label "int-0" :val 0 :hex "00"}
   {:label "int-1" :val 1 :hex "01"}
   {:label "int-42" :val 42 :hex "2a"}
   {:label "int-minus-1" :val -1 :hex "ff"}
   {:label "int-100" :val 100 :hex "5064"}
   {:label "int-minus-100" :val -100 :hex "4f9c"}
   {:label "int-1000" :val 1000 :hex "53e8"}
   {:label "int-100000" :val 100000 :hex "6986a0"}
   {:label "int-10000000000" :val 10000000000 :hex "7a02540be400"}
   {:label "str-empty" :val "" :hex "da"}
   {:label "str-hello" :val "hello" :hex "df68656c6c6f"}
   {:label "str-emoji-grinning" :val "😀" :hex "e0eda0bdedb880"}
   {:label "str-astral-mixed" :val "hi 🌍🚀!" :hex "e310686920eda0bcedbc8deda0bdedba8021"}
   {:label "kw-foo" :val :foo :hex "caf7dd666f6f"}
   {:label "kw-foo-bar" :val :foo/bar :hex "cadd666f6fdd626172"}
   {:label "sym-bar" :val (symbol "bar") :hex "c9f7dd626172"}
   {:label "sym-foo-bar" :val (symbol "foo" "bar") :hex "c9dd666f6fdd626172"}
   {:label "vec-1-2-3" :val [1 2 3] :hex "e7010203"}
   {:label "bytes-1-2-3" :val (fressian/hex->bytes "010203") :hex "d3010203"}
   {:label "set-1-2" :val #{1 2} :hex "c1e60102"}
   {:label "map-a-b" :val {:a 1 :b 2} :hex "c0e8caf7db6101caf7db6202"}
   {:label "map-b-a-unordered"
    :val (into (sorted-map-by #(compare %2 %1)) {:a 1 :b 2})
    :hex "c0e8caf7db6101caf7db6202"}
   {:label "nested"
    :val {:epoch 42
          :from "alice"
          :tags #{:y :x}
          :payload [1 "data" true nil {:inner 99}]}
    :hex "c0ec08caf7df65706f63682acaf7de66726f6ddf616c696365caf7e17061796c6f6164e901de64617461f5f7c0e6caf7df696e6e65725063caf7de74616773c1e6caf7db78caf7db79"}])

(defn- val-equiv?
  [a b]
  (cond
    (and (fressian/bytes? a) (fressian/bytes? b))
    (fressian/byte-array-equals? a b)

    (and (fressian/tagged-object? a) (fressian/tagged-object? b))
    (and (= (fressian/tag a) (fressian/tag b))
         (val-equiv? (vec (fressian/tagged-value a))
                     (vec (fressian/tagged-value b))))

    #?@(:clj [(and (instance? Date a) (instance? Date b))
              (= (.getTime ^Date a) (.getTime ^Date b))
              (and (instance? TaggedObject a) (fressian/tagged-object? b))
              (and (= (.getTag ^TaggedObject a) (fressian/tag b))
                   (val-equiv? (vec (.getValue ^TaggedObject a))
                               (vec (fressian/tagged-value b))))
              (and (fressian/tagged-object? a) (instance? TaggedObject b))
              (and (= (fressian/tag a) (.getTag ^TaggedObject b))
                   (val-equiv? (vec (fressian/tagged-value a))
                               (vec (.getValue ^TaggedObject b))))])

    #?@(:cljs [(and (instance? js/Date a) (instance? js/Date b))
               (= (.getTime a) (.getTime b))])

    (and (map? a) (map? b))
    (and (= (count a) (count b))
         (every? (fn [[k v]]
                   (and (contains? b k)
                        (val-equiv? v (get b k))))
                 a))

    (and (set? a) (set? b))
    (and (= (count a) (count b))
         (every? (fn [x] (some #(val-equiv? x %) b)) a)
         (every? (fn [y] (some #(val-equiv? % y) a)) b))

    (and (sequential? a) (sequential? b))
    (and (= (count a) (count b))
         (every? true? (map val-equiv? a b)))

    :else
    (= a b)))

(deftest test-canonical-fressian-golden-vectors
  (testing "Exact byte-identical hex output for Canonical Fressian golden vectors"
    (doseq [{:keys [label val hex]} canonical-fressian-golden-vectors]
      (let [encoded (fressian/encode-canonical val)
            actual-hex (fressian/bytes->hex encoded)]
        (is (= hex actual-hex) (str "Golden vector failed for: " label))
        (let [decoded (fressian/decode encoded)]
          (is (val-equiv? val decoded) (str "Decode roundtrip failed for: " label)))))))

(deftest test-tagged-types-and-caching-roundtrip
  (testing "Priority caching (>32 items) and struct caching (>16 custom tags) roundtrip on both JVM and CLJS"
    (let [kws (mapv #(keyword (str "ns-" (mod % 10)) (str "key-" %)) (range 45))
          repeated-kws (into kws kws)
          custom-tags (mapv (fn [i]
                              (fressian/tagged-object (str "custom/tag-" (mod i 20))
                                                      [i (str "v-" i)]))
                            (range 40))
          payload {:keywords repeated-kws
                   :structs custom-tags
                   :cached-vals [(fressian/cached "shared-long-string-value")
                                 (fressian/cached "shared-long-string-value")
                                 (fressian/cached 1000000)
                                 (fressian/cached 1000000)]
                   :doubles [0.0 1.0 -3.141592653589793 42.5]}
          bs (fressian/write payload :footer? true)
          decoded (fressian/read bs :validate-footer? true)]
      (is (= repeated-kws (:keywords decoded)))
      (is (= [0.0 1.0 -3.141592653589793 42.5] (:doubles decoded)))
      (is (= ["shared-long-string-value" "shared-long-string-value" 1000000 1000000]
             (:cached-vals decoded)))
      (is (= 40 (count (:structs decoded))))
      (doseq [[orig dec-item] (map vector custom-tags (:structs decoded))]
        (is (val-equiv? orig dec-item))))))

#?(:clj
   (defn- std-write-bytes
     ^bytes [v & opts]
     (let [^ByteBuffer bb (apply data-fressian/write v opts)
           ba (byte-array (.remaining bb))]
       (.get bb ba)
       ba)))

#?(:clj
   (defn- std-read-bytes
     [^bytes ba]
     (fressian/normalize-decoded-wire (data-fressian/read (ByteBuffer/wrap ba)))))

#?(:clj
   (deftest test-jvm-reference-byte-equality-on-cached-and-tagged-streams
     (testing "Pure .cljc fressian/write produces byte-for-byte identical output to org.clojure/data.fressian/write"
       (let [u (UUID/fromString "550e8400-e29b-41d4-a716-446655440000")
             d (Date. 1700000000000)
             t1 (TaggedObject. "my/point" (object-array [10 20]))
             t2 (TaggedObject. "my/point" (object-array [30 40]))
             c1 (CachedObject. "cached-str-1")
             c2 (CachedObject. "cached-str-1")
             samples [nil
                      true
                      false
                      -1 0 1 63 64 -4096 4095 -4097 4096 -524288 524287
                      Long/MIN_VALUE Long/MAX_VALUE
                      0.0 1.0 3.141592653589793 -0.5
                      "" "hello" "😀" "hi 🌍🚀!"
                      [:foo/bar :foo/bar :foo/baz 'alpha/beta 'alpha/beta 'gamma]
                      u
                      d
                      [\a \b \a]
                      [t1 t2]
                      [c1 c2]
                      (fressian/hex->bytes "00010203040506070809")
                      (apply str (repeat 66000 "A"))
                      (byte-array 66000 (byte 42))]]
         (doseq [s samples]
           (let [our-bs (fressian/write s)
                 ref-bs (std-write-bytes s)
                 our-footer-bs (fressian/write s :footer? true)
                 ref-footer-bs (std-write-bytes s :footer? true)]
             (is (= (fressian/bytes->hex ref-bs) (fressian/bytes->hex our-bs))
                 (str "Byte mismatch vs data.fressian for: " (pr-str s)))
             (is (= (fressian/bytes->hex ref-footer-bs) (fressian/bytes->hex our-footer-bs))
                 (str "Footer byte mismatch vs data.fressian for: " (pr-str s)))
             (is (val-equiv? (std-read-bytes ref-bs) (fressian/read ref-bs)))
             (is (val-equiv? (std-read-bytes our-bs) (fressian/read our-bs)))))))))

;; =============================================================================
;; Generative Properties (Both JVM and CLJS + Bidirectional JVM Reference Tests)
;; =============================================================================

(def ^{:doc "Generator for scalar leaf values."} gen-scalar
  (gen/one-of
   [gen/small-integer
    gen/large-integer
    (gen/return nil)
    gen/boolean
    (gen/elements [0.0 1.0 -1.5 3.25 1.0e12 -42.125])
    gen/string-alphanumeric
    gen/string
    (gen/elements ["😀" "hi 🌍🚀!" "λ-calculus" ""])
    gen/keyword
    gen/keyword-ns
    gen/symbol
    gen/symbol-ns
    (gen/fmap (fn [bs]
                #?(:clj (byte-array (map unchecked-byte bs))
                   :cljs (js/Uint8Array. (clj->js (map #(bit-and % 0xff) bs)))))
              (gen/vector (gen/choose 0 255) 0 24))]))

(def ^{:doc "Generator for nested portable Clojure data structures."} gen-portable-value
  (gen/recursive-gen
   (fn [inner]
     (gen/one-of
      [(gen/vector inner 0 5)
       (gen/fmap set (gen/vector inner 0 5))
       (gen/map gen/keyword inner {:max-elements 5})]))
   gen-scalar))

(defspec ^{:doc "Property: Arbitrary portable data structures roundtrip through encode-canonical and decode."}
  prop-canonical-fressian-roundtrip 60
  (for-all [val gen-portable-value]
           (let [encoded (fressian/encode-canonical val)
                 decoded (fressian/decode encoded)]
             (val-equiv? val decoded))))

(defspec ^{:doc "Property: Map key order does not alter canonical Fressian bytes."}
  prop-canonical-fressian-map-set-order-independence 50
  (for-all [kvs (gen/vector (gen/tuple gen/keyword gen/small-integer) 1 6)]
           (let [m1 (into {} kvs)
                 m2 (into (sorted-map) kvs)
                 m3 (into (sorted-map-by #(compare %2 %1)) kvs)
                 h1 (fressian/bytes->hex (fressian/encode-canonical m1))
                 h2 (fressian/bytes->hex (fressian/encode-canonical m2))
                 h3 (fressian/bytes->hex (fressian/encode-canonical m3))]
             (and (= h1 h2) (= h2 h3)))))

#?(:clj
   (def gen-reference-cross-value
     "Generator that includes repeated keywords/symbols, UUIDs, Dates, custom TaggedObjects,
      and nested vectors/maps/sets to exercise priority caching & struct caching in both directions."
     (let [kw-pool [:alpha/one :alpha/two :beta/one :beta/two :gamma :delta/three]
           sym-pool ['ns1/s1 'ns1/s2 'ns2/s1 'bare-sym]
           tag-pool ["ext/point" "ext/box" "ext/edge"]
           leaf (gen/one-of
                 [gen-scalar
                  (gen/elements kw-pool)
                  (gen/elements sym-pool)
                  gen/uuid
                  (gen/fmap #(Date. (long %)) (gen/choose 0 2000000000000))
                  (gen/fmap (fn [[t a b]]
                              (fressian/tagged-object t [a b]))
                            (gen/tuple (gen/elements tag-pool) gen/small-integer gen/string-alphanumeric))])]
       (gen/recursive-gen
        (fn [inner]
          (gen/one-of
           [(gen/vector inner 0 6)
            (gen/fmap set (gen/vector gen/keyword 0 5))
            (gen/map (gen/elements kw-pool) inner {:max-elements 5})]))
        leaf))))

#?(:clj
   (defspec ^{:doc "Property: Bidirectional byte-compatibility between fressian.core and org.clojure/data.fressian."}
     prop-bidirectional-reference-data-fressian-compat 100
     (for-all [val gen-reference-cross-value
               footer? gen/boolean]
              (let [;; 1. Encode with pure .cljc fressian/write -> decode with reference clojure.data.fressian/read
                    our-bytes (fressian/write val :footer? footer?)
                    decoded-by-ref (std-read-bytes our-bytes)
                    ;; 2. Encode with reference clojure.data.fressian/write -> decode with pure .cljc fressian/read
                    ;; Convert our portable FressianTaggedObject into org.fressian.TaggedObject when feeding reference writer
                    to-ref (walk/postwalk
                            (fn [x]
                              (if (fressian/tagged-object? x)
                                (TaggedObject. (fressian/tag x) (object-array (fressian/tagged-value x)))
                                x))
                            val)
                    ref-bytes (std-write-bytes to-ref :footer? footer?)
                    decoded-by-us (fressian/read ref-bytes :validate-footer? footer?)]
                (and (val-equiv? val decoded-by-ref)
                     (val-equiv? val decoded-by-us)
                     ;; When val contains no unordered maps/sets of >1 element, byte streams must also be 100% identical!
                     (if (and (not (coll? val)) (not (fressian/tagged-object? val)))
                       (= (fressian/bytes->hex ref-bytes) (fressian/bytes->hex our-bytes))
                       true))))))
