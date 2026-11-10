(ns fressian.lz4-test
  "Cross-runtime test suite for pure .cljc LZ4 block codec & layered BTEC wire envelope (`fressian.lz4`)."
  (:require #?(:clj [clojure.test :refer [deftest is testing]]
               :cljs [cljs.test :refer-macros [deftest is testing]])
            #?(:clj [clojure.test.check.clojure-test :refer [defspec]]
               :cljs [clojure.test.check.clojure-test :refer-macros [defspec]])
            [clojure.test.check.generators :as gen]
            #?(:clj [clojure.test.check.properties :refer [for-all]]
               :cljs [clojure.test.check.properties :refer-macros [for-all]])
            [fressian.core :as fressian]
            [fressian.lz4 :as lz4]))

(def ^:private gen-lz4-adversarial-bytes
  (gen/one-of
   [(gen/return (fressian/utf8->bytes ""))
    (gen/return (fressian/utf8->bytes "x"))
    (gen/return (fressian/utf8->bytes "0123456789ab"))
    (gen/return (fressian/utf8->bytes "0123456789abc"))
    (gen/return (fressian/utf8->bytes "0123456789abcde"))
    (gen/fmap (fn [n] (fressian/utf8->bytes (apply str (repeat n "A")))) (gen/choose 1 600))
    (gen/fmap (fn [n] (fressian/utf8->bytes (apply str (repeat n "\u0000")))) (gen/choose 1 600))
    (gen/fmap (fn [[pat n]] (fressian/utf8->bytes (apply str (repeat n pat))))
              (gen/tuple (gen/not-empty gen/string-alphanumeric) (gen/choose 2 80)))
    (gen/fmap (fn [bs]
                #?(:clj (byte-array (map unchecked-byte bs))
                   :cljs (js/Uint8Array. (clj->js (map #(bit-and % 0xFF) bs)))))
              (gen/vector (gen/choose 0 255) 0 512))]))

(defspec ^{:doc "Property: Pure-CLJC LZ4 block compressor and decompressor cross-verify against JVM reference across adversarial inputs."}
  prop-lz4-block-codec-cross-reference-and-adversarial 100
  (for-all [input-bytes gen-lz4-adversarial-bytes]
           (let [orig-len (fressian/count-bytes input-bytes)
                 cljc-compressed (lz4/lz4-compress-block input-bytes)
                 cljc-roundtrip (lz4/lz4-decompress-block cljc-compressed orig-len)
                 self-ok? (= (fressian/bytes->hex input-bytes)
                             (fressian/bytes->hex cljc-roundtrip))]
             #?(:clj
                (let [ref-comp (io.airlift.compress.lz4.Lz4Compressor.)
                      ref-decomp (io.airlift.compress.lz4.Lz4Decompressor.)
                      ref-out (byte-array orig-len)
                      ref-dlen (if (zero? orig-len)
                                 0
                                 (.decompress ref-decomp
                                              ^bytes cljc-compressed 0 (alength ^bytes cljc-compressed)
                                              ref-out 0 orig-len))
                      max-clen (.maxCompressedLength ref-comp orig-len)
                      ref-cbuf (byte-array max-clen)
                      ref-clen (if (zero? orig-len)
                                 0
                                 (.compress ref-comp
                                            ^bytes input-bytes 0 orig-len
                                            ref-cbuf 0 max-clen))
                      ref-cbytes (java.util.Arrays/copyOfRange ref-cbuf 0 ref-clen)
                      cljc-from-ref (lz4/lz4-decompress-block ref-cbytes orig-len)]
                  (and self-ok?
                       (= orig-len ref-dlen)
                       (= (fressian/bytes->hex input-bytes) (fressian/bytes->hex ref-out))
                       (= (fressian/bytes->hex input-bytes) (fressian/bytes->hex cljc-from-ref))))
                :cljs self-ok?))))

(deftest test-layered-btec-envelope-and-uncompressed-mode
  (testing "Layered BTEC envelope supports both compressed (> 1 KiB) and explicit uncompressed (:compress? false) modes"
    (let [small {:type :vote :epoch 7 :voter "abc"}
          small-wire (lz4/encode-wire small)
          _ (is (not (lz4/btec-compressed-frame? small-wire)))
          _ (is (= small (lz4/decode-wire small-wire)))
          large-map {:type :send-propose
                     :epoch 42
                     :ancestors (vec (repeat 20 {:proposer "302a300506032b65700321009b33a9ebd1866d6db419edabe2de9ee74c884dc86b4214f60e3a26f04cbd9cfc"
                                                 :parent-hash "abcdef0123456789abcdef0123456789abcdef0123456789abcdef0123456789"
                                                 :payload [{:type :transfer :amount 5000 :seq 1}]}))}
          uncomp-wire (lz4/encode-wire large-map {:compress? false})
          comp-wire (lz4/encode-wire large-map)]
      (is (> (fressian/count-bytes uncomp-wire) 1024))
      (is (not (lz4/btec-compressed-frame? uncomp-wire))
          "Explicit {:compress? false} leaves wire frame uncompressed so callers without LZ4 can decode raw Fressian")
      (is (lz4/btec-compressed-frame? comp-wire))
      (is (< (fressian/count-bytes comp-wire) (fressian/count-bytes uncomp-wire)))
      (is (= large-map (lz4/decode-wire comp-wire)))
      (is (= large-map (lz4/decode-wire uncomp-wire)))
      (is (= large-map (fressian/decode uncomp-wire)))))

  (testing "Fail-closed rejection of unknown compression tag, > 1 MiB decompressed size, and truncated/corrupt BTEC frames"
    (let [bad-tag-frame (fressian/concat-bytes
                         (fressian/utf8->bytes "BTEC")
                         (fressian/hex->bytes "020000010011223344"))]
      (is (nil? (lz4/decode-wire bad-tag-frame)))
      (is (thrown-with-msg? #?(:clj clojure.lang.ExceptionInfo :cljs ExceptionInfo)
                            #"Unsupported BTEC compression tag"
                            (lz4/decode-wire-strict bad-tag-frame))))
    (let [bomb-frame (fressian/concat-bytes
                      (fressian/utf8->bytes "BTEC")
                      (fressian/hex->bytes "010020000000"))]
      (is (nil? (lz4/decode-wire bomb-frame)))
      (is (thrown-with-msg? #?(:clj clojure.lang.ExceptionInfo :cljs ExceptionInfo)
                            #"exceeds 1 MiB"
                            (lz4/decode-wire-strict bomb-frame))))))
