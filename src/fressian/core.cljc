(ns fressian.core
  "Pure, zero-dependency .cljc Fressian codec for Clojure (JVM) and ClojureScript (Node/Browser).
   Provides:
   1. Deterministic Canonical Fressian Mode (`encode-canonical`):
      - Total canonical ordering (`canonical-compare`) across map keys and set elements.
      - Minimal compact integer widths and CESU-8 string encoding.
   2. Standard Fressian Mode (`write` / `read` / `decode`):
      - Byte-compatible with `org.clojure/data.fressian` in both directions.
      - Priority caching (`PUT_PRIORITY_CACHE`, `PRIORITY_CACHE_PACKED`, `GET_PRIORITY_CACHE`).
      - Struct caching (`STRUCTTYPE`, `STRUCT_CACHE_PACKED`, `STRUCT`) and custom tagged types
        (`tagged-object`, `:write-handlers`, `:read-handlers`).
      - Full extended type support (`UUID`, `inst`, `URI`, `regex`, `bigint`, `bigdec`, `char`,
        `double` `0.0`/`1.0`/IEEE-754, `float`, chunked `bytes` and CESU-8 `string`s, closed/open
        lists, `RESET_CACHES`, and Adler32 `FOOTER` generation & validation)."
  (:refer-clojure :exclude [bytes? read])
  (:require [clojure.string :as str]))

;; =============================================================================
;; 1. Portable Byte & String Utilities
;; =============================================================================

(defn bytes?
  "Returns true if x is a raw byte array (byte[] on JVM, Uint8Array on JS)."
  [x]
  #?(:clj  (clojure.core/bytes? x)
     :cljs (instance? js/Uint8Array x)))

(defn count-bytes
  "Returns the length in bytes of x."
  ^long [x]
  #?(:clj  (alength ^bytes x)
     :cljs (.-length x)))

(defn u8
  "Returns unsigned byte (0..255) at index idx."
  ^long [ba ^long idx]
  #?(:clj  (bit-and (long (aget ^bytes ba (int idx))) 0xff)
     :cljs (aget ba idx)))

(defn aset-u8!
  "Sets unsigned byte (0..255) at index idx."
  [ba ^long idx ^long v]
  #?(:clj  (aset ^bytes ba (int idx) (unchecked-byte v))
     :cljs (aset ba idx (bit-and v 0xff))))

(defn utf8->bytes
  "Encodes a string to UTF-8 bytes."
  ^bytes [^String s]
  #?(:clj  (.getBytes (str s) "UTF-8")
     :cljs (.encode (js/TextEncoder.) (str s))))

(defn bytes->utf8
  "Decodes UTF-8 bytes to a string."
  [ba]
  #?(:clj  (String. ^bytes ba "UTF-8")
     :cljs (.decode (js/TextDecoder.) ba)))

(defn string->cesu8-bytes
  "Encodes a string as CESU-8 bytes (UTF-16 code units encoded in 1-3 bytes each).
   Guarantees byte-level compatibility with org.clojure/data.fressian on astral-plane characters."
  [s]
  #?(:clj
     (let [s (str s)
           len (.length ^String s)
           bos (java.io.ByteArrayOutputStream.)]
       (dotimes [i len]
         (let [c (int (.charAt ^String s i))]
           (cond
             (<= c 0x007f)
             (.write bos (unchecked-byte c))

             (<= c 0x07ff)
             (do
               (.write bos (unchecked-byte (bit-or 0xc0 (bit-shift-right c 6))))
               (.write bos (unchecked-byte (bit-or 0x80 (bit-and c 0x3f)))))

             :else
             (do
               (.write bos (unchecked-byte (bit-or 0xe0 (bit-shift-right c 12))))
               (.write bos (unchecked-byte (bit-or 0x80 (bit-and (bit-shift-right c 6) 0x3f))))
               (.write bos (unchecked-byte (bit-or 0x80 (bit-and c 0x3f))))))))
       (.toByteArray bos))
     :cljs
     (let [s (str s)
           len (.-length s)
           out (js/Array.)]
       (dotimes [i len]
         (let [c (.charCodeAt s i)]
           (cond
             (<= c 0x007f)
             (.push out c)

             (<= c 0x07ff)
             (do
               (.push out (bit-or 0xc0 (bit-shift-right c 6)))
               (.push out (bit-or 0x80 (bit-and c 0x3f))))

             :else
             (do
               (.push out (bit-or 0xe0 (bit-shift-right c 12)))
               (.push out (bit-or 0x80 (bit-and (bit-shift-right c 6) 0x3f)))
               (.push out (bit-or 0x80 (bit-and c 0x3f)))))))
       (js/Uint8Array. out))))

(defn cesu8-bytes->string
  "Decodes CESU-8 bytes into a string, with permissive 4-byte UTF-8 fallback."
  [ba]
  #?(:clj
     (let [len (alength ^bytes ba)
           sb (StringBuilder.)]
       (loop [i 0]
         (when (< i len)
           (let [b0 (bit-and (aget ^bytes ba i) 0xff)]
             (cond
               (< b0 0x80)
               (do (.append sb (char b0)) (recur (inc i)))

               (< b0 0xe0)
               (let [b1 (bit-and (aget ^bytes ba (inc i)) 0xff)
                     c (bit-or (bit-shift-left (bit-and b0 0x1f) 6) (bit-and b1 0x3f))]
                 (.append sb (char c))
                 (recur (+ i 2)))

               (< b0 0xf0)
               (let [b1 (bit-and (aget ^bytes ba (inc i)) 0xff)
                     b2 (bit-and (aget ^bytes ba (+ i 2)) 0xff)
                     c (bit-or (bit-shift-left (bit-and b0 0x0f) 12)
                               (bit-shift-left (bit-and b1 0x3f) 6)
                               (bit-and b2 0x3f))]
                 (.append sb (char c))
                 (recur (+ i 3)))

               (< b0 0xf8)
               (let [b1 (bit-and (aget ^bytes ba (inc i)) 0xff)
                     b2 (bit-and (aget ^bytes ba (+ i 2)) 0xff)
                     b3 (bit-and (aget ^bytes ba (+ i 3)) 0xff)
                     cp (bit-or (bit-shift-left (bit-and b0 0x07) 18)
                                (bit-shift-left (bit-and b1 0x3f) 12)
                                (bit-shift-left (bit-and b2 0x3f) 6)
                                (bit-and b3 0x3f))
                     adj (- cp 0x10000)
                     hi (+ 0xd800 (bit-shift-right adj 10))
                     lo (+ 0xdc00 (bit-and adj 0x3ff))]
                 (.append sb (char hi))
                 (.append sb (char lo))
                 (recur (+ i 4)))

               :else
               (recur (inc i))))))
       (.toString sb))
     :cljs
     (let [len (.-length ba)
           chars (js/Array.)]
       (loop [i 0]
         (when (< i len)
           (let [b0 (aget ba i)]
             (cond
               (< b0 0x80)
               (do (.push chars b0) (recur (inc i)))

               (< b0 0xe0)
               (let [b1 (aget ba (inc i))
                     c (bit-or (bit-shift-left (bit-and b0 0x1f) 6) (bit-and b1 0x3f))]
                 (.push chars c)
                 (recur (+ i 2)))

               (< b0 0xf0)
               (let [b1 (aget ba (+ i 1))
                     b2 (aget ba (+ i 2))
                     c (bit-or (bit-shift-left (bit-and b0 0x0f) 12)
                               (bit-shift-left (bit-and b1 0x3f) 6)
                               (bit-and b2 0x3f))]
                 (.push chars c)
                 (recur (+ i 3)))

               (< b0 0xf8)
               (let [b1 (aget ba (+ i 1))
                     b2 (aget ba (+ i 2))
                     b3 (aget ba (+ i 3))
                     cp (bit-or (bit-shift-left (bit-and b0 0x07) 18)
                                (bit-shift-left (bit-and b1 0x3f) 12)
                                (bit-shift-left (bit-and b2 0x3f) 6)
                                (bit-and b3 0x3f))
                     adj (- cp 0x10000)
                     hi (+ 0xd800 (bit-shift-right adj 10))
                     lo (+ 0xdc00 (bit-and adj 0x3ff))]
                 (.push chars hi)
                 (.push chars lo)
                 (recur (+ i 4)))

               :else
               (recur (inc i))))))
       (let [chunk-size 4096
             num-chunks (js/Math.ceil (/ (.-length chars) chunk-size))
             result (js/Array.)]
         (dotimes [c num-chunks]
           (let [sub (.slice chars (* c chunk-size) (* (inc c) chunk-size))]
             (.push result (.apply (.-fromCharCode js/String) nil sub))))
         (.join result "")))))

(defn byte-array-equals?
  "Checks byte equality between two byte arrays."
  [a b]
  (if (or (nil? a) (nil? b))
    (identical? a b)
    #?(:clj  (and (bytes? a) (bytes? b) (java.util.Arrays/equals ^bytes a ^bytes b))
       :cljs (and (bytes? a)
                  (bytes? b)
                  (= (.-length a) (.-length b))
                  (loop [i 0]
                    (if (= i (.-length a))
                      true
                      (if (= (aget a i) (aget b i))
                        (recur (inc i))
                        false)))))))

(defn concat-bytes
  "Concatenates byte arrays / Uint8Arrays into a single byte array."
  ([] #?(:clj (byte-array 0) :cljs (js/Uint8Array. 0)))
  ([a] a)
  ([a b]
   #?(:clj  (let [la (alength ^bytes a)
                  lb (alength ^bytes b)
                  out (byte-array (+ la lb))]
              (System/arraycopy a 0 out 0 la)
              (System/arraycopy b 0 out la lb)
              out)
      :cljs (let [la (.-length a)
                  lb (.-length b)
                  out (js/Uint8Array. (+ la lb))]
              (.set out a 0)
              (.set out b la)
              out)))
  ([a b & more]
   (reduce concat-bytes (concat-bytes a b) more)))

(defn bytes->hex
  "Converts a byte array / Uint8Array to a lowercase hex string."
  [ba]
  (if (nil? ba)
    ""
    #?(:clj  (let [sb (StringBuilder. (* 2 (alength ^bytes ba)))]
               (doseq [b ba]
                 (.append sb (String/format "%02x" (to-array [(bit-and b 0xff)]))))
               (.toString sb))
       :cljs (let [len (.-length ba)
                   parts #js []]
               (dotimes [i len]
                 (let [hex (.toString (aget ba i) 16)]
                   (.push parts (if (< (.-length hex) 2) (str "0" hex) hex))))
               (.join parts "")))))

(defn hex->bytes
  "Converts a hex string to a byte array / Uint8Array."
  [s]
  (let [s (str s)
        len (count s)]
    (when (odd? len)
      #?(:clj (throw (IllegalArgumentException. (str "Hex string must have an even length: " s)))
         :cljs (throw (js/Error. (str "Hex string must have an even length: " s)))))
    #?(:clj  (let [data (byte-array (quot len 2))]
               (loop [i 0]
                 (if (< i len)
                   (let [high (Character/digit (.charAt s i) 16)
                         low  (Character/digit (.charAt s (inc i)) 16)]
                     (when (or (= high -1) (= low -1))
                       (throw (IllegalArgumentException. (str "Invalid hex character in: " s))))
                     (aset data (quot i 2)
                           (unchecked-byte (+ (bit-shift-left high 4) low)))
                     (recur (+ i 2)))
                   data)))
       :cljs (let [out (js/Uint8Array. (quot len 2))]
               (loop [i 0]
                 (if (< i len)
                   (let [sub (.substring s i (+ i 2))]
                     (when-not (re-matches #"^[0-9a-fA-F]{2}$" sub)
                       (throw (js/Error. (str "Invalid hex character in: " s))))
                     (aset out (quot i 2) (js/parseInt sub 16))
                     (recur (+ i 2)))
                   out))))))

(defn byte-array-compare
  "Lexicographical unsigned comparison of two byte arrays."
  [a b]
  (let [len-a (count-bytes a)
        len-b (count-bytes b)
        min-len (min len-a len-b)]
    (loop [i 0]
      (if (< i min-len)
        (let [va (u8 a i)
              vb (u8 b i)
              diff (- va vb)]
          (if (zero? diff)
            (recur (inc i))
            diff))
        (- len-a len-b)))))

;; =============================================================================
;; 2. Portable TaggedObject & CachedObject Wrappers
;; =============================================================================

#_{:clj-kondo/ignore [:missing-docstring]}
(defrecord FressianTaggedObject [tag value meta])

(defn tagged-object
  "Constructs a portable Fressian tagged object with `tag` (string) and `value` (vector/array of components)."
  ([tag value]
   (->FressianTaggedObject (str tag) (vec value) nil))
  ([tag value meta-map]
   (->FressianTaggedObject (str tag) (vec value) meta-map)))

(defn- jvm-fressian-tagged-object?
  [o]
  #?(:clj  (and (some? o) (= "org.fressian.TaggedObject" (.getName (class o))))
     :cljs (boolean (and false o))))

(defn tagged-object?
  "Returns true if `o` is a Fressian tagged object (portable `FressianTaggedObject` or JVM `org.fressian.TaggedObject`)."
  [o]
  (or (instance? FressianTaggedObject o)
      (jvm-fressian-tagged-object? o)))

(defn tag
  "Returns the string/object tag of a tagged object, or nil."
  [o]
  (cond
    (instance? FressianTaggedObject o) (:tag o)
    #?@(:clj [(jvm-fressian-tagged-object? o)
              (let [m (.getMethod (class o) "getTag" (into-array Class []))]
                (.invoke m o (into-array Object [])))])
    :else nil))

(defn tagged-value
  "Returns the component values of a tagged object, or nil."
  [o]
  (cond
    (instance? FressianTaggedObject o) (:value o)
    #?@(:clj [(jvm-fressian-tagged-object? o)
              (let [m (.getMethod (class o) "getValue" (into-array Class []))]
                (vec (.invoke m o (into-array Object []))))])
    :else nil))

#_{:clj-kondo/ignore [:missing-docstring]}
(defrecord FressianCachedObject [value])

(defn cached
  "Wraps `val` so that standard Fressian `write` records it in the priority cache (`PUT_PRIORITY_CACHE`)."
  [val]
  (->FressianCachedObject val))

(defn- jvm-fressian-cached-object?
  [o]
  #?(:clj  (and (some? o) (= "org.fressian.CachedObject" (.getName (class o))))
     :cljs (boolean (and false o))))

(defn cached?
  "Returns true if `o` is a priority-cache wrapper (`FressianCachedObject` or JVM `org.fressian.CachedObject`)."
  [o]
  (or (instance? FressianCachedObject o)
      (jvm-fressian-cached-object? o)))

(defn cached-value
  "Unwraps the underlying value from a `cached` wrapper."
  [o]
  (cond
    (instance? FressianCachedObject o) (:value o)
    #?@(:clj [(jvm-fressian-cached-object? o)
              (let [m (.getMethod (class o) "getObjectToCache" (into-array Class []))]
                (.invoke m o (into-array Object [])))])
    :else o))

;; =============================================================================
;; 3. Deterministic Canonical Ordering
;; =============================================================================

(defn- canonical-type-rank
  [x]
  (cond
    (nil? x)       0
    (boolean? x)   1
    (number? x)    2
    (string? x)    3
    (keyword? x)   4
    (symbol? x)    5
    (bytes? x)     6
    (vector? x)    7
    (set? x)       8
    (map? x)       9
    :else          10))

(defn canonical-compare
  "Deterministic total ordering for portable Clojure data structures.
   Guarantees byte-identical iteration order across JVM and CLJS for maps and sets."
  [a b]
  (let [ra (canonical-type-rank a)
        rb (canonical-type-rank b)]
    (if (not= ra rb)
      (compare ra rb)
      (case ra
        0 0
        1 (compare (if a 1 0) (if b 1 0))
        2 (compare a b)
        3 (compare (str a) (str b))
        4 (let [c (compare (or (namespace a) "") (or (namespace b) ""))]
            (if (zero? c)
              (compare (name a) (name b))
              c))
        5 (let [c (compare (or (namespace a) "") (or (namespace b) ""))]
            (if (zero? c)
              (compare (name a) (name b))
              c))
        6 (byte-array-compare a b)
        7 (let [ca (count a) cb (count b)
                cdiff (compare ca cb)]
            (if (not= 0 cdiff)
              cdiff
              (loop [i 0]
                (if (= i ca)
                  0
                  (let [c (canonical-compare (nth a i) (nth b i))]
                    (if (not= 0 c)
                      c
                      (recur (inc i))))))))
        8 (let [ca (count a) cb (count b)
                cdiff (compare ca cb)]
            (if (not= 0 cdiff)
              cdiff
              (let [sa (sort canonical-compare a)
                    sb (sort canonical-compare b)]
                (canonical-compare (vec sa) (vec sb)))))
        9 (let [ca (count a) cb (count b)
                cdiff (compare ca cb)]
            (if (not= 0 cdiff)
              cdiff
              (let [ska (sort canonical-compare (keys a))
                    skb (sort canonical-compare (keys b))
                    key-diff (canonical-compare (vec ska) (vec skb))]
                (if (not= 0 key-diff)
                  key-diff
                  (loop [ks ska]
                    (if-let [k (first ks)]
                      (let [c (canonical-compare (get a k) (get b k))]
                        (if (not= 0 c)
                          c
                          (recur (rest ks))))
                      0))))))
        (compare (str a) (str b))))))

;; =============================================================================
;; 4. Fressian Byte Codes (Complete Spec)
;; =============================================================================

(def ^{:doc "Fressian PRIORITY_CACHE_PACKED_START byte code."} ^:const tag-priority-cache-packed-start 128)
(def ^{:doc "Fressian PRIORITY_CACHE_PACKED_END byte code."} ^:const tag-priority-cache-packed-end 160)
(def ^{:doc "Fressian STRUCT_CACHE_PACKED_START byte code."} ^:const tag-struct-cache-packed-start 160)
(def ^{:doc "Fressian STRUCT_CACHE_PACKED_END byte code."} ^:const tag-struct-cache-packed-end 176)
(def ^{:doc "Fressian LONG_ARRAY byte code."} ^:const tag-long-array 176)
(def ^{:doc "Fressian DOUBLE_ARRAY byte code."} ^:const tag-double-array 177)
(def ^{:doc "Fressian BOOLEAN_ARRAY byte code."} ^:const tag-boolean-array 178)
(def ^{:doc "Fressian INT_ARRAY byte code."} ^:const tag-int-array 179)
(def ^{:doc "Fressian FLOAT_ARRAY byte code."} ^:const tag-float-array 180)
(def ^{:doc "Fressian OBJECT_ARRAY byte code."} ^:const tag-object-array 181)
(def ^{:doc "Fressian MAP byte code."} ^:const tag-map 192)
(def ^{:doc "Fressian SET byte code."} ^:const tag-set 193)
(def ^{:doc "Fressian UUID byte code."} ^:const tag-uuid 195)
(def ^{:doc "Fressian REGEX byte code."} ^:const tag-regex 196)
(def ^{:doc "Fressian URI byte code."} ^:const tag-uri 197)
(def ^{:doc "Fressian BIGINT byte code."} ^:const tag-bigint 198)
(def ^{:doc "Fressian BIGDEC byte code."} ^:const tag-bigdec 199)
(def ^{:doc "Fressian INST byte code."} ^:const tag-inst 200)
(def ^{:doc "Fressian SYM byte code."} ^:const tag-sym 201)
(def ^{:doc "Fressian KEY byte code."} ^:const tag-key 202)
(def ^{:doc "Fressian GET_PRIORITY_CACHE byte code."} ^:const tag-get-priority-cache 204)
(def ^{:doc "Fressian PUT_PRIORITY_CACHE byte code."} ^:const tag-put-priority-cache 205)
(def ^{:doc "Fressian PRECACHE byte code."} ^:const tag-precache 206)
(def ^{:doc "Fressian FOOTER byte code."} ^:const tag-footer 207)
(def ^{:doc "Fressian BYTES_PACKED_LENGTH_START byte code."} ^:const tag-bytes-packed-length-start 208)
(def ^{:doc "Fressian BYTES_PACKED_LENGTH_END byte code."} ^:const tag-bytes-packed-length-end 216)
(def ^{:doc "Fressian BYTES_CHUNK byte code."} ^:const tag-bytes-chunk 216)
(def ^{:doc "Fressian BYTES byte code."} ^:const tag-bytes 217)
(def ^{:doc "Fressian STRING_PACKED_LENGTH_START byte code."} ^:const tag-string-packed-length-start 218)
(def ^{:doc "Fressian STRING_PACKED_LENGTH_END byte code."} ^:const tag-string-packed-length-end 226)
(def ^{:doc "Fressian STRING_CHUNK byte code."} ^:const tag-string-chunk 226)
(def ^{:doc "Fressian STRING byte code."} ^:const tag-string 227)
(def ^{:doc "Fressian LIST_PACKED_LENGTH_START byte code."} ^:const tag-list-packed-length-start 228)
(def ^{:doc "Fressian LIST_PACKED_LENGTH_END byte code."} ^:const tag-list-packed-length-end 236)
(def ^{:doc "Fressian LIST byte code."} ^:const tag-list 236)
(def ^{:doc "Fressian BEGIN_CLOSED_LIST byte code."} ^:const tag-begin-closed-list 237)
(def ^{:doc "Fressian BEGIN_OPEN_LIST byte code."} ^:const tag-begin-open-list 238)
(def ^{:doc "Fressian STRUCTTYPE byte code."} ^:const tag-structtype 239)
(def ^{:doc "Fressian STRUCT byte code."} ^:const tag-struct 240)
(def ^{:doc "Fressian META byte code."} ^:const tag-meta 241)
(def ^{:doc "Fressian ANY byte code."} ^:const tag-any 244)
(def ^{:doc "Fressian TRUE byte code."} ^:const tag-true 245)
(def ^{:doc "Fressian FALSE byte code."} ^:const tag-false 246)
(def ^{:doc "Fressian NULL byte code."} ^:const tag-null 247)
(def ^{:doc "Fressian INT byte code."} ^:const tag-int 248)
(def ^{:doc "Fressian FLOAT byte code."} ^:const tag-float 249)
(def ^{:doc "Fressian DOUBLE byte code."} ^:const tag-double 250)
(def ^{:doc "Fressian DOUBLE_0 byte code."} ^:const tag-double-0 251)
(def ^{:doc "Fressian DOUBLE_1 byte code."} ^:const tag-double-1 252)
(def ^{:doc "Fressian END_COLLECTION byte code."} ^:const tag-end-collection 253)
(def ^{:doc "Fressian RESET_CACHES byte code."} ^:const tag-reset-caches 254)

(def ^:private tag-to-code
  {"map" tag-map
   "set" tag-set
   "uuid" tag-uuid
   "regex" tag-regex
   "uri" tag-uri
   "bigint" tag-bigint
   "bigdec" tag-bigdec
   "inst" tag-inst
   "sym" tag-sym
   "key" tag-key
   "long[]" tag-long-array
   "double[]" tag-double-array
   "boolean[]" tag-boolean-array
   "int[]" tag-int-array
   "float[]" tag-float-array
   "Object[]" tag-object-array})

;; =============================================================================
;; 5. Byte Writer & Adler32 Checksum
;; =============================================================================

(defprotocol IByteWriter
  (write-raw-byte [this b])
  (write-raw-bytes [this ba])
  (bytes-written [this])
  (get-result-bytes [this]))

#?(:clj
   (deftype JVMByteWriter [^java.io.ByteArrayOutputStream baos]
     IByteWriter
     (write-raw-byte [_ b]
       (.write baos (unchecked-int (bit-and b 0xff))))
     (write-raw-bytes [_ ba]
       (.write baos ^bytes ba 0 (alength ^bytes ba)))
     (bytes-written [_]
       (.size baos))
     (get-result-bytes [_]
       (.toByteArray baos))))

#?(:cljs
   (deftype JSByteWriter [arr]
     IByteWriter
     (write-raw-byte [_ b]
       (.push arr (bit-and b 0xff)))
     (write-raw-bytes [_ ba]
       (let [len (.-length ba)]
         (dotimes [i len]
           (.push arr (aget ba i)))))
     (bytes-written [_]
       (.-length arr))
     (get-result-bytes [_]
       (js/Uint8Array. arr))))

(defn make-byte-writer
  "Creates a platform-appropriate `IByteWriter` instance (`JVMByteWriter` or `JSByteWriter`)."
  []
  #?(:clj  (JVMByteWriter. (java.io.ByteArrayOutputStream. 128))
     :cljs (JSByteWriter. #js [])))

(defn adler32-bytes
  "Computes standard Adler-32 checksum over `ba[start..end)` (pure .cljc)."
  ([ba]
   (adler32-bytes ba 0 (count-bytes ba)))
  ([ba ^long start ^long end]
   (loop [i start
          a 1
          b 0]
     (if (< i end)
       (let [a' (rem (+ a (u8 ba i)) 65521)
             b' (rem (+ b a') 65521)]
         (recur (inc i) a' b'))
       (bit-or (bit-shift-left b 16) a)))))

(defn- write-raw-int32
  [w ^long v]
  (write-raw-byte w (bit-and (unsigned-bit-shift-right v 24) 0xff))
  (write-raw-byte w (bit-and (unsigned-bit-shift-right v 16) 0xff))
  (write-raw-byte w (bit-and (unsigned-bit-shift-right v 8) 0xff))
  (write-raw-byte w (bit-and v 0xff)))

(defn- write-raw-float
  [w ^double f]
  #?(:clj
     (let [bits (Float/floatToIntBits (float f))]
       (write-raw-int32 w bits))
     :cljs
     (let [buf (js/ArrayBuffer. 4)
           dv (js/DataView. buf)]
       (.setFloat32 dv 0 f false)
       (dotimes [i 4]
         (write-raw-byte w (.getUint8 dv i))))))

(defn- write-raw-double
  [w ^double d]
  #?(:clj
     (let [bits (Double/doubleToRawLongBits d)]
       (write-raw-byte w (bit-and (unsigned-bit-shift-right bits 56) 0xff))
       (write-raw-byte w (bit-and (unsigned-bit-shift-right bits 48) 0xff))
       (write-raw-byte w (bit-and (unsigned-bit-shift-right bits 40) 0xff))
       (write-raw-byte w (bit-and (unsigned-bit-shift-right bits 32) 0xff))
       (write-raw-byte w (bit-and (unsigned-bit-shift-right bits 24) 0xff))
       (write-raw-byte w (bit-and (unsigned-bit-shift-right bits 16) 0xff))
       (write-raw-byte w (bit-and (unsigned-bit-shift-right bits 8) 0xff))
       (write-raw-byte w (bit-and bits 0xff)))
     :cljs
     (let [buf (js/ArrayBuffer. 8)
           dv (js/DataView. buf)]
       (.setFloat64 dv 0 d false)
       (dotimes [i 8]
         (write-raw-byte w (.getUint8 dv i))))))

(defn- shift-right-helper
  [n shift]
  #?(:clj  (bit-shift-right n shift)
     :cljs (let [b (bit-shift-right (js/BigInt n) (js/BigInt shift))]
             (js/Number (js/BigInt.asIntN 64 b)))))

(defn write-fressian-int
  "Writes integer `n` using Fressian's compact 1..9 byte width encoding."
  [w n]
  (cond
    (= n -1)
    (write-raw-byte w 255)

    (<= 0 n 63)
    (write-raw-byte w (unchecked-int n))

    (<= -4096 n 4095)
    (do
      (write-raw-byte w (+ 80 (unchecked-int (shift-right-helper n 8))))
      (write-raw-byte w (bit-and (unchecked-int n) 0xff)))

    (<= -524288 n 524287)
    (do
      (write-raw-byte w (+ 104 (unchecked-int (shift-right-helper n 16))))
      (write-raw-byte w (bit-and (unchecked-int (shift-right-helper n 8)) 0xff))
      (write-raw-byte w (bit-and (unchecked-int n) 0xff)))

    (<= -33554432 n 33554431)
    (do
      (write-raw-byte w (+ 114 (unchecked-int (shift-right-helper n 24))))
      (write-raw-byte w (bit-and (unchecked-int (shift-right-helper n 16)) 0xff))
      (write-raw-byte w (bit-and (unchecked-int (shift-right-helper n 8)) 0xff))
      (write-raw-byte w (bit-and (unchecked-int n) 0xff)))

    (<= -8589934592 n 8589934591)
    (do
      (write-raw-byte w (+ 118 (unchecked-int (shift-right-helper n 32))))
      (write-raw-byte w (bit-and (unchecked-int (shift-right-helper n 24)) 0xff))
      (write-raw-byte w (bit-and (unchecked-int (shift-right-helper n 16)) 0xff))
      (write-raw-byte w (bit-and (unchecked-int (shift-right-helper n 8)) 0xff))
      (write-raw-byte w (bit-and (unchecked-int n) 0xff)))

    (<= -2199023255552 n 2199023255551)
    (do
      (write-raw-byte w (+ 122 (unchecked-int (shift-right-helper n 40))))
      (write-raw-byte w (bit-and (unchecked-int (shift-right-helper n 32)) 0xff))
      (write-raw-byte w (bit-and (unchecked-int (shift-right-helper n 24)) 0xff))
      (write-raw-byte w (bit-and (unchecked-int (shift-right-helper n 16)) 0xff))
      (write-raw-byte w (bit-and (unchecked-int (shift-right-helper n 8)) 0xff))
      (write-raw-byte w (bit-and (unchecked-int n) 0xff)))

    (<= -562949953421312 n 562949953421311)
    (do
      (write-raw-byte w (+ 126 (unchecked-int (shift-right-helper n 48))))
      (write-raw-byte w (bit-and (unchecked-int (shift-right-helper n 40)) 0xff))
      (write-raw-byte w (bit-and (unchecked-int (shift-right-helper n 32)) 0xff))
      (write-raw-byte w (bit-and (unchecked-int (shift-right-helper n 24)) 0xff))
      (write-raw-byte w (bit-and (unchecked-int (shift-right-helper n 16)) 0xff))
      (write-raw-byte w (bit-and (unchecked-int (shift-right-helper n 8)) 0xff))
      (write-raw-byte w (bit-and (unchecked-int n) 0xff)))

    :else
    (do
      (write-raw-byte w tag-int)
      (write-raw-byte w (bit-and (unchecked-int (shift-right-helper n 56)) 0xff))
      (write-raw-byte w (bit-and (unchecked-int (shift-right-helper n 48)) 0xff))
      (write-raw-byte w (bit-and (unchecked-int (shift-right-helper n 40)) 0xff))
      (write-raw-byte w (bit-and (unchecked-int (shift-right-helper n 32)) 0xff))
      (write-raw-byte w (bit-and (unchecked-int (shift-right-helper n 24)) 0xff))
      (write-raw-byte w (bit-and (unchecked-int (shift-right-helper n 16)) 0xff))
      (write-raw-byte w (bit-and (unchecked-int (shift-right-helper n 8)) 0xff))
      (write-raw-byte w (bit-and (unchecked-int n) 0xff)))))

(defn- write-fressian-bytes
  [w ba]
  (let [total (count-bytes ba)]
    (cond
      (< total 8)
      (do
        (write-raw-byte w (+ tag-bytes-packed-length-start total))
        (write-raw-bytes w ba))

      (<= total 65535)
      (do
        (write-raw-byte w tag-bytes)
        (write-fressian-int w total)
        (write-raw-bytes w ba))

      :else
      (loop [offset 0
             rem-len total]
        (if (> rem-len 65535)
          (let [chunk #?(:clj (java.util.Arrays/copyOfRange ^bytes ba (int offset) (int (+ offset 65535)))
                         :cljs (.subarray ba offset (+ offset 65535)))]
            (write-raw-byte w tag-bytes-chunk)
            (write-fressian-int w 65535)
            (write-raw-bytes w chunk)
            (recur (+ offset 65535) (- rem-len 65535)))
          (let [chunk #?(:clj (java.util.Arrays/copyOfRange ^bytes ba (int offset) (int (+ offset rem-len)))
                         :cljs (.subarray ba offset (+ offset rem-len)))]
            (write-raw-byte w tag-bytes)
            (write-fressian-int w rem-len)
            (write-raw-bytes w chunk)))))))

(defn- cesu8-char-size
  ^long [^long c]
  (cond
    (<= c 0x007f) 1
    (<= c 0x07ff) 2
    :else         3))

(defn- write-fressian-string
  [w s]
  (let [s (str s)
        char-len #?(:clj (.length ^String s) :cljs (.-length s))]
    (if (zero? char-len)
      (write-raw-byte w tag-string-packed-length-start)
      (loop [char-pos 0]
        (let [cw (make-byte-writer)
              next-pos (loop [i char-pos
                              bytes-written 0]
                         (if (>= i char-len)
                           i
                           (let [c (long #?(:clj (int (.charAt ^String s (int i)))
                                            :cljs (.charCodeAt s i)))
                                 sz (cesu8-char-size c)]
                             (if (> (+ bytes-written sz) 65536)
                               i
                               (do
                                 (cond
                                   (= sz 1)
                                   (write-raw-byte cw c)

                                   (= sz 2)
                                   (do
                                     (write-raw-byte cw (bit-or 0xc0 (bit-shift-right c 6)))
                                     (write-raw-byte cw (bit-or 0x80 (bit-and c 0x3f))))

                                   :else
                                   (do
                                     (write-raw-byte cw (bit-or 0xe0 (bit-shift-right c 12)))
                                     (write-raw-byte cw (bit-or 0x80 (bit-and (bit-shift-right c 6) 0x3f)))
                                     (write-raw-byte cw (bit-or 0x80 (bit-and c 0x3f)))))
                                 (recur (inc i) (+ bytes-written sz)))))))
              chunk-bytes (get-result-bytes cw)
              chunk-len (count-bytes chunk-bytes)]
          (cond
            (< chunk-len 8)
            (write-raw-byte w (+ tag-string-packed-length-start chunk-len))

            (= next-pos char-len)
            (do
              (write-raw-byte w tag-string)
              (write-fressian-int w chunk-len))

            :else
            (do
              (write-raw-byte w tag-string-chunk)
              (write-fressian-int w chunk-len)))
          (write-raw-bytes w chunk-bytes)
          (when (< next-pos char-len)
            (recur next-pos)))))))

(defn- uuid->bytes
  [u]
  (let [s (str/replace (str u) "-" "")]
    (hex->bytes s)))

(defn- bytes->uuid
  [ba]
  (let [h (bytes->hex ba)
        s (str (subs h 0 8) "-"
               (subs h 8 12) "-"
               (subs h 12 16) "-"
               (subs h 16 20) "-"
               (subs h 20 32))]
    #?(:clj  (java.util.UUID/fromString s)
       :cljs (uuid s))))

(defn- ms->inst
  [^long ms]
  #?(:clj  (java.util.Date. ms)
     :cljs (js/Date. ms)))

(defn- float32?
  [x]
  #?(:clj  (instance? Float x)
     :cljs (boolean (and false x))))

;; =============================================================================
;; 6. Standard Fressian Writer (Priority Cache + Struct Cache + Footer)
;; =============================================================================

(defn- bigint-or-biginteger?
  [x]
  #?(:clj  (or (instance? clojure.lang.BigInt x)
               (instance? java.math.BigInteger x))
     :cljs (boolean (and false x))))

(defn- should-skip-cache?
  "Matches `org.fressian.FressianWriter/shouldSkipCache`."
  [x]
  (cond
    (or (nil? x) (boolean? x)) true
    (bigint-or-biginteger? x) false
    (integer? x) (<= -4096 x 4095)
    (string? x) (zero? (count x))
    (float? x) (or (== x 0.0) (== x 1.0))
    :else false))

(declare write-standard-object!)

(defn- write-tag!
  [w state-atom tag-name component-count]
  (if-let [code (get tag-to-code tag-name)]
    (write-raw-byte w code)
    (let [sc (:struct-cache @state-atom)]
      (if-let [idx (get sc tag-name)]
        (if (< idx 16)
          (write-raw-byte w (+ tag-struct-cache-packed-start idx))
          (do
            (write-raw-byte w tag-struct)
            (write-fressian-int w idx)))
        (let [new-idx (count sc)]
          (swap! state-atom assoc-in [:struct-cache tag-name] new-idx)
          (write-raw-byte w tag-structtype)
          (write-standard-object! w state-atom tag-name false)
          (write-fressian-int w component-count))))))

(defn- write-list-elements!
  [w state-atom coll]
  (let [v (vec coll)
        cnt (count v)]
    (if (< cnt 8)
      (write-raw-byte w (+ tag-list-packed-length-start cnt))
      (do
        (write-raw-byte w tag-list)
        (write-fressian-int w cnt)))
    (doseq [item v]
      (write-standard-object! w state-atom item false))))

(defn- write-named!
  [w state-atom tag-str s]
  (write-tag! w state-atom tag-str 2)
  (write-standard-object! w state-atom (namespace s) true)
  (write-standard-object! w state-atom (name s) true))

(defn- write-unwrapped-value!
  [w state-atom val]
  (let [custom-handlers (:write-handlers @state-atom)]
    (cond
      (nil? val)
      (write-raw-byte w tag-null)

      (true? val)
      (write-raw-byte w tag-true)

      (false? val)
      (write-raw-byte w tag-false)

      #?@(:clj [(or (instance? clojure.lang.BigInt val) (instance? java.math.BigInteger val))
                (let [^java.math.BigInteger bi (if (instance? clojure.lang.BigInt val)
                                                 (.toBigInteger ^clojure.lang.BigInt val)
                                                 val)]
                  (write-tag! w state-atom "bigint" 1)
                  (write-fressian-bytes w (.toByteArray bi)))])

      (integer? val)
      (write-fressian-int w val)

      (float32? val)
      (write-raw-float w (double val))

      (float? val)
      (let [d (double val)]
        (cond
          ;; Match FressianWriter: +0.0 (not -0.0) uses DOUBLE_0
          (and (== d 0.0) (pos? (/ 1.0 (if (zero? d) (+ d 0.0) d)))
               #?(:clj (zero? (Double/doubleToRawLongBits d))
                  :cljs (pos? (/ 1.0 d))))
          (write-raw-byte w tag-double-0)

          (== d 1.0)
          (write-raw-byte w tag-double-1)

          :else
          (do
            (write-raw-byte w tag-double)
            (write-raw-double w d))))

      (string? val)
      (write-fressian-string w val)

      (bytes? val)
      (write-fressian-bytes w val)

      (keyword? val)
      (write-named! w state-atom "key" val)

      (symbol? val)
      (write-named! w state-atom "sym" val)

      #?@(:clj [(char? val)
                (do
                  (write-tag! w state-atom "char" 1)
                  (write-fressian-int w (int val)))

                (ratio? val)
                (let [^clojure.lang.Ratio r val]
                  (write-tag! w state-atom "ratio" 2)
                  (write-standard-object! w state-atom (.numerator r) false)
                  (write-standard-object! w state-atom (.denominator r) false))

                (decimal? val)
                (let [^java.math.BigDecimal bd val]
                  (write-tag! w state-atom "bigdec" 2)
                  (write-fressian-bytes w (.toByteArray (.unscaledValue bd)))
                  (write-fressian-int w (.scale bd)))

                (instance? java.net.URI val)
                (do
                  (write-tag! w state-atom "uri" 1)
                  (write-fressian-string w (.toString ^java.net.URI val)))

                (instance? java.util.regex.Pattern val)
                (do
                  (write-tag! w state-atom "regex" 1)
                  (write-fressian-string w (.pattern ^java.util.regex.Pattern val)))])

      (uuid? val)
      (do
        (write-tag! w state-atom "uuid" 1)
        (write-fressian-bytes w (uuid->bytes val)))

      (inst? val)
      (do
        (write-tag! w state-atom "inst" 1)
        (write-fressian-int w (inst-ms val)))

      (tagged-object? val)
      (let [t (tag val)
            comps (vec (tagged-value val))]
        (write-tag! w state-atom t (count comps))
        (doseq [c comps]
          (write-standard-object! w state-atom c false)))

      (and custom-handlers (contains? custom-handlers (type val)))
      (let [{:keys [tag fields write-fn]} (get custom-handlers (type val))]
        (if write-fn
          (write-fn w state-atom val)
          (let [comps (vec (fields val))]
            (write-tag! w state-atom tag (count comps))
            (doseq [c comps]
              (write-standard-object! w state-atom c false)))))

      (record? val)
      (let [rname #?(:clj (-> val class .getName symbol)
                     :cljs (symbol (pr-str (type val))))]
        (write-tag! w state-atom "record" 2)
        (write-standard-object! w state-atom rname true)
        (write-tag! w state-atom "map" 1)
        (write-raw-byte w tag-begin-closed-list)
        (doseq [[k v] val]
          (write-standard-object! w state-atom k true)
          (write-standard-object! w state-atom v false))
        (write-raw-byte w tag-end-collection))

      (set? val)
      (do
        (write-tag! w state-atom "set" 1)
        (write-list-elements! w state-atom val))

      (map? val)
      (let [flat-kvs (mapcat (fn [[k v]] [k v]) val)]
        (write-tag! w state-atom "map" 1)
        (write-list-elements! w state-atom flat-kvs))

      (sequential? val)
      (write-list-elements! w state-atom val)

      :else
      (throw (ex-info (str "Unsupported Fressian type: " (type val))
                      {:val val})))))

(defn- write-standard-object!
  [w state-atom raw-val cache?]
  (let [wrapped? (cached? raw-val)
        val (if wrapped? (cached-value raw-val) raw-val)
        do-cache? (and (or cache? wrapped?) (not (should-skip-cache? val)))]
    (if-not do-cache?
      (write-unwrapped-value! w state-atom val)
      (let [pc (:priority-cache @state-atom)]
        (if-let [idx (get pc val)]
          (if (< idx 32)
            (write-raw-byte w (+ tag-priority-cache-packed-start idx))
            (do
              (write-raw-byte w tag-get-priority-cache)
              (write-fressian-int w idx)))
          (let [new-idx (count pc)]
            (swap! state-atom assoc-in [:priority-cache val] new-idx)
            (write-raw-byte w tag-put-priority-cache)
            (write-unwrapped-value! w state-atom val)))))))

(defn- write-footer!
  [w]
  (let [payload-len (bytes-written w)]
    (write-raw-int32 w -808464433) ;; 0xCFCFCFCF (207 207 207 207)
    (write-raw-int32 w payload-len)
    (let [pre-checksum-bytes (get-result-bytes w)
          csum (adler32-bytes pre-checksum-bytes 0 (count-bytes pre-checksum-bytes))]
      (write-raw-int32 w csum))))

(defn write
  "Encodes `val` into standard Fressian bytes (with keyword/symbol priority caching and struct caching),
   producing output byte-compatible with `org.clojure/data.fressian/write`.
   Options:
   - `:footer? true` — appends standard 12-byte Fressian footer (`0xCFCFCFCF` + 4-byte length + 4-byte Adler32).
   - `:write-handlers` — map of `type -> {:tag \"...\" :fields (fn [v] [...])}`."
  [val & {:keys [footer? write-handlers]}]
  (let [w (make-byte-writer)
        state-atom (atom {:priority-cache {}
                          :struct-cache {}
                          :write-handlers write-handlers})]
    (write-standard-object! w state-atom val false)
    (when footer?
      (write-footer! w))
    (get-result-bytes w)))

;; =============================================================================
;; 7. Deterministic Canonical Fressian Encoder (`encode-canonical`)
;; =============================================================================

(declare write-canonical-value!)

(defn- write-canonical-vector!
  [w v]
  (let [cnt (count v)]
    (if (< cnt 8)
      (write-raw-byte w (+ tag-list-packed-length-start cnt))
      (do
        (write-raw-byte w tag-list)
        (write-fressian-int w cnt)))
    (doseq [item v]
      (write-canonical-value! w item))))

(defn- write-canonical-value!
  [w val]
  (cond
    (nil? val)
    (write-raw-byte w tag-null)

    (true? val)
    (write-raw-byte w tag-true)

    (false? val)
    (write-raw-byte w tag-false)

    #?@(:clj [(or (instance? clojure.lang.BigInt val) (instance? java.math.BigInteger val))
              (let [^java.math.BigInteger bi (if (instance? clojure.lang.BigInt val)
                                               (.toBigInteger ^clojure.lang.BigInt val)
                                               val)]
                (write-raw-byte w tag-bigint)
                (write-fressian-bytes w (.toByteArray bi)))

              (decimal? val)
              (let [^java.math.BigDecimal bd val]
                (write-raw-byte w tag-bigdec)
                (write-fressian-bytes w (.toByteArray (.unscaledValue bd)))
                (write-fressian-int w (.scale bd)))

              (ratio? val)
              (let [^clojure.lang.Ratio r val]
                (write-raw-byte w tag-structtype)
                (write-fressian-string w "ratio")
                (write-fressian-int w 2)
                (write-canonical-value! w (.numerator r))
                (write-canonical-value! w (.denominator r)))

              (char? val)
              (do
                (write-raw-byte w tag-structtype)
                (write-fressian-string w "char")
                (write-fressian-int w 1)
                (write-fressian-int w (int val)))

              (instance? java.net.URI val)
              (do
                (write-raw-byte w tag-uri)
                (write-fressian-string w (.toString ^java.net.URI val)))

              (instance? java.util.regex.Pattern val)
              (do
                (write-raw-byte w tag-regex)
                (write-fressian-string w (.pattern ^java.util.regex.Pattern val)))])

    (integer? val)
    (write-fressian-int w val)

    (float? val)
    (let [d (double val)]
      (cond
        (and (== d 0.0)
             #?(:clj (zero? (Double/doubleToRawLongBits d))
                :cljs (pos? (/ 1.0 d))))
        (write-raw-byte w tag-double-0)

        (== d 1.0)
        (write-raw-byte w tag-double-1)

        :else
        (do
          (write-raw-byte w tag-double)
          (write-raw-double w d))))

    (bytes? val)
    (write-fressian-bytes w val)

    (string? val)
    (write-fressian-string w val)

    (keyword? val)
    (do
      (write-raw-byte w tag-key)
      (if-let [ns (namespace val)]
        (write-fressian-string w ns)
        (write-raw-byte w tag-null))
      (write-fressian-string w (name val)))

    (symbol? val)
    (do
      (write-raw-byte w tag-sym)
      (if-let [ns (namespace val)]
        (write-fressian-string w ns)
        (write-raw-byte w tag-null))
      (write-fressian-string w (name val)))

    (uuid? val)
    (do
      (write-raw-byte w tag-uuid)
      (write-fressian-bytes w (uuid->bytes val)))

    (inst? val)
    (do
      (write-raw-byte w tag-inst)
      (write-fressian-int w (inst-ms val)))

    (tagged-object? val)
    (let [t (str (tag val))
          comps (vec (tagged-value val))]
      (write-raw-byte w tag-structtype)
      (write-fressian-string w t)
      (write-fressian-int w (count comps))
      (doseq [c comps]
        (write-canonical-value! w c)))

    (set? val)
    (let [sorted-items (vec (sort canonical-compare val))]
      (write-raw-byte w tag-set)
      (write-canonical-vector! w sorted-items))

    (map? val)
    (let [sorted-ks (sort canonical-compare (keys val))
          flat-items (vec (mapcat (fn [k] [k (get val k)]) sorted-ks))]
      (write-raw-byte w tag-map)
      (write-canonical-vector! w flat-items))

    (sequential? val)
    (write-canonical-vector! w (vec val))

    :else
    (throw (ex-info (str "Unsupported canonical Fressian type: " (type val))
                    {:val val}))))

(defn encode-canonical
  "Encodes a portable data structure into deterministic Canonical Fressian bytes.
   Returns byte[] on JVM, Uint8Array on ClojureScript.
   Guarantees:
   - Map keys deterministically sorted via `canonical-compare`.
   - Set elements deterministically sorted via `canonical-compare`.
   - Minimal canonical compact integer width.
   - Standard Fressian byte-code conformance readable by any Fressian reader."
  [val]
  (let [w (make-byte-writer)]
    (write-canonical-value! w val)
    (get-result-bytes w)))

;; =============================================================================
;; 8. Fressian Reader / Decoder (Priority Cache + Struct Cache + Footer Validation)
;; =============================================================================

(defprotocol IByteReader
  (read-raw-byte [this])
  (read-slice [this n])
  (reader-pos [this])
  (reader-len [this]))

#?(:clj
   (deftype JVMByteReader [^bytes ba ^:unsynchronized-mutable ^int idx ^int len]
     IByteReader
     (read-raw-byte [_]
       (if (< idx len)
         (let [b (bit-and (aget ba idx) 0xff)]
           (set! idx (unchecked-inc-int idx))
           b)
         (throw (java.io.EOFException. "Unexpected EOF in Fressian stream"))))
     (read-slice [_ n]
       (let [n-int (int n)
             end (unchecked-add-int idx n-int)]
         (if (<= end len)
           (let [res (java.util.Arrays/copyOfRange ba idx end)]
             (set! idx end)
             res)
           (throw (java.io.EOFException. "Unexpected EOF in Fressian stream")))))
     (reader-pos [_] idx)
     (reader-len [_] len)))

#?(:cljs
   (deftype JSByteReader [ba ^:mutable idx len]
     IByteReader
     (read-raw-byte [_]
       (if (< idx len)
         (let [b (aget ba idx)]
           (set! idx (inc idx))
           b)
         (throw (ex-info "Unexpected EOF in Fressian stream" {}))))
     (read-slice [_ n]
       (let [end (+ idx n)]
         (if (<= end len)
           (let [res (.subarray ba idx end)]
             (set! idx end)
             res)
           (throw (ex-info "Unexpected EOF in Fressian stream" {})))))
     (reader-pos [_] idx)
     (reader-len [_] len)))

(defn make-byte-reader
  "Creates a platform-appropriate `IByteReader` instance over `ba`."
  [ba]
  #?(:clj  (JVMByteReader. ba 0 (alength ^bytes ba))
     :cljs (JSByteReader. ba 0 (.-length ba))))

(defn- read-raw-float
  [rdr]
  #?(:clj
     (let [b0 (read-raw-byte rdr)
           b1 (read-raw-byte rdr)
           b2 (read-raw-byte rdr)
           b3 (read-raw-byte rdr)
           bits (unchecked-int (bit-or (bit-shift-left b0 24)
                                       (bit-shift-left b1 16)
                                       (bit-shift-left b2 8)
                                       b3))]
       (double (Float/intBitsToFloat bits)))
     :cljs
     (let [bs (read-slice rdr 4)
           dv (js/DataView. (.-buffer bs) (.-byteOffset bs) 4)]
       (.getFloat32 dv 0 false))))

(defn- read-raw-double
  [rdr]
  #?(:clj
     (let [b0 (long (read-raw-byte rdr))
           b1 (long (read-raw-byte rdr))
           b2 (long (read-raw-byte rdr))
           b3 (long (read-raw-byte rdr))
           b4 (long (read-raw-byte rdr))
           b5 (long (read-raw-byte rdr))
           b6 (long (read-raw-byte rdr))
           b7 (long (read-raw-byte rdr))
           bits (bit-or (bit-shift-left b0 56)
                        (bit-shift-left b1 48)
                        (bit-shift-left b2 40)
                        (bit-shift-left b3 32)
                        (bit-shift-left b4 24)
                        (bit-shift-left b5 16)
                        (bit-shift-left b6 8)
                        b7)]
       (Double/longBitsToDouble bits))
     :cljs
     (let [bs (read-slice rdr 8)
           dv (js/DataView. (.-buffer bs) (.-byteOffset bs) 8)]
       (.getFloat64 dv 0 false))))

(defn- read-packed-int
  [rdr code]
  (cond
    (= code 255) -1
    (<= 0 code 63) code

    (<= 64 code 95)
    (let [b0 (- code 80)
          b1 (read-raw-byte rdr)]
      (+ (bit-shift-left b0 8) b1))

    (<= 96 code 111)
    (let [b0 (- code 104)
          b1 (read-raw-byte rdr)
          b2 (read-raw-byte rdr)]
      (+ (bit-shift-left b0 16) (bit-shift-left b1 8) b2))

    (<= 112 code 115)
    (let [b0 (- code 114)
          b1 (read-raw-byte rdr)
          b2 (read-raw-byte rdr)
          b3 (read-raw-byte rdr)]
      (+ (bit-shift-left b0 24) (bit-shift-left b1 16) (bit-shift-left b2 8) b3))

    (<= 116 code 119)
    (let [b0 (- code 118)
          b1 (read-raw-byte rdr)
          b2 (read-raw-byte rdr)
          b3 (read-raw-byte rdr)
          b4 (read-raw-byte rdr)]
      #?(:clj  (+ (bit-shift-left (long b0) 32)
                  (bit-shift-left (long b1) 24)
                  (bit-shift-left (long b2) 16)
                  (bit-shift-left (long b3) 8)
                  (long b4))
         :cljs (let [hi (+ (bit-shift-left b0 8) b1)
                     lo (+ (bit-shift-left b2 16) (bit-shift-left b3 8) b4)]
                 (+ (* hi 0x1000000) lo))))

    (<= 120 code 123)
    (let [b0 (- code 122)
          b1 (read-raw-byte rdr)
          b2 (read-raw-byte rdr)
          b3 (read-raw-byte rdr)
          b4 (read-raw-byte rdr)
          b5 (read-raw-byte rdr)]
      #?(:clj  (+ (bit-shift-left (long b0) 40)
                  (bit-shift-left (long b1) 32)
                  (bit-shift-left (long b2) 24)
                  (bit-shift-left (long b3) 16)
                  (bit-shift-left (long b4) 8)
                  (long b5))
         :cljs (let [hi (+ (bit-shift-left b0 16) (bit-shift-left b1 8) b2)
                     lo (+ (bit-shift-left b3 16) (bit-shift-left b4 8) b5)]
                 (+ (* hi 0x1000000) lo))))

    (<= 124 code 127)
    (let [b0 (- code 126)
          b1 (read-raw-byte rdr)
          b2 (read-raw-byte rdr)
          b3 (read-raw-byte rdr)
          b4 (read-raw-byte rdr)
          b5 (read-raw-byte rdr)
          b6 (read-raw-byte rdr)]
      #?(:clj  (+ (bit-shift-left (long b0) 48)
                  (bit-shift-left (long b1) 40)
                  (bit-shift-left (long b2) 32)
                  (bit-shift-left (long b3) 24)
                  (bit-shift-left (long b4) 16)
                  (bit-shift-left (long b5) 8)
                  (long b6))
         :cljs (let [b (js/BigInt.asIntN 64
                                         (bit-or
                                          (bit-shift-left (js/BigInt b0) (js/BigInt 48))
                                          (bit-shift-left (js/BigInt b1) (js/BigInt 40))
                                          (bit-shift-left (js/BigInt b2) (js/BigInt 32))
                                          (bit-shift-left (js/BigInt b3) (js/BigInt 24))
                                          (bit-shift-left (js/BigInt b4) (js/BigInt 16))
                                          (bit-shift-left (js/BigInt b5) (js/BigInt 8))
                                          (js/BigInt b6)))]
                 (js/Number b))))

    (= code tag-int)
    (let [b0 (read-raw-byte rdr)
          b1 (read-raw-byte rdr)
          b2 (read-raw-byte rdr)
          b3 (read-raw-byte rdr)
          b4 (read-raw-byte rdr)
          b5 (read-raw-byte rdr)
          b6 (read-raw-byte rdr)
          b7 (read-raw-byte rdr)]
      #?(:clj  (+ (bit-shift-left (long b0) 56)
                  (bit-shift-left (long b1) 48)
                  (bit-shift-left (long b2) 40)
                  (bit-shift-left (long b3) 32)
                  (bit-shift-left (long b4) 24)
                  (bit-shift-left (long b5) 16)
                  (bit-shift-left (long b6) 8)
                  (long b7))
         :cljs (let [b (js/BigInt.asIntN 64
                                         (bit-or
                                          (bit-shift-left (js/BigInt b0) (js/BigInt 56))
                                          (bit-shift-left (js/BigInt b1) (js/BigInt 48))
                                          (bit-shift-left (js/BigInt b2) (js/BigInt 40))
                                          (bit-shift-left (js/BigInt b3) (js/BigInt 32))
                                          (bit-shift-left (js/BigInt b4) (js/BigInt 24))
                                          (bit-shift-left (js/BigInt b5) (js/BigInt 16))
                                          (bit-shift-left (js/BigInt b6) (js/BigInt 8))
                                          (js/BigInt b7)))]
                 (js/Number b))))

    :else
    (throw (ex-info (str "Expected integer code, got: " code) {:code code}))))

(declare decode-value-by-code)

(defn- decode-value
  [rdr state-atom]
  (let [code (read-raw-byte rdr)]
    (decode-value-by-code rdr state-atom code)))

(defn- decode-count
  [rdr state-atom]
  (long (decode-value rdr state-atom)))

(defn- handle-struct-read
  [rdr state-atom struct-tag field-count]
  (let [read-handlers (:read-handlers @state-atom)
        tag-str (str struct-tag)]
    (cond
      (and read-handlers (contains? read-handlers struct-tag))
      (let [h (get read-handlers struct-tag)
            fields (mapv (fn [_] (decode-value rdr state-atom)) (range field-count))]
        (h fields))

      (= tag-str "char")
      (let [code (long (decode-value rdr state-atom))]
        #?(:clj (char code) :cljs (.fromCharCode js/String code)))

      (= tag-str "byte")
      (let [v (long (decode-value rdr state-atom))]
        #?(:clj (byte v) :cljs v))

      #?@(:clj [(= tag-str "ratio")
                (let [num (biginteger (decode-value rdr state-atom))
                      den (biginteger (decode-value rdr state-atom))]
                  (clojure.lang.Ratio. num den))])

      (= tag-str "record")
      (let [rname (decode-value rdr state-atom)
            rmap (decode-value rdr state-atom)]
        #?(:clj
           (let [comps (str/split (str rname) #"\.")
                 factory-sym (symbol (->> (butlast comps)
                                          (map #(str/replace % "_" "-"))
                                          (str/join "."))
                                     (str "map->" (last comps)))]
             (if-let [rcons (try (resolve factory-sym) (catch Exception _ nil))]
               (rcons rmap)
               (tagged-object "record" [rname rmap])))
           :cljs
           (tagged-object "record" [rname rmap])))

      (= tag-str "map")
      (let [kvs (decode-value rdr state-atom)]
        (into {} (map vec (partition 2 kvs))))

      (= tag-str "set")
      (set (decode-value rdr state-atom))

      (= tag-str "key")
      (let [ns (decode-value rdr state-atom)
            nm (decode-value rdr state-atom)]
        (if ns (keyword ns nm) (keyword nm)))

      (= tag-str "sym")
      (let [ns (decode-value rdr state-atom)
            nm (decode-value rdr state-atom)]
        (if ns (symbol ns nm) (symbol nm)))

      :else
      (let [fields (mapv (fn [_] (decode-value rdr state-atom)) (range field-count))]
        (tagged-object struct-tag fields)))))

(defn- decode-value-by-code
  [rdr state-atom code]
  (cond
    (= code tag-null) nil
    (= code tag-true) true
    (= code tag-false) false

    (or (<= 0 code 63)
        (= code 255)
        (<= 64 code 127)
        (= code tag-int))
    (read-packed-int rdr code)

    (= code tag-float)
    (read-raw-float rdr)

    (= code tag-double)
    (read-raw-double rdr)

    (= code tag-double-0)
    0.0

    (= code tag-double-1)
    1.0

    (<= tag-bytes-packed-length-start code (dec tag-bytes-packed-length-end))
    (read-slice rdr (- code tag-bytes-packed-length-start))

    (= code tag-bytes-chunk)
    (let [initial-len (decode-count rdr state-atom)
          initial-chunk (read-slice rdr initial-len)]
      (loop [chunks [initial-chunk]]
        (let [next-code (read-raw-byte rdr)]
          (cond
            (= next-code tag-bytes-chunk)
            (let [len (decode-count rdr state-atom)
                  chunk (read-slice rdr len)]
              (recur (conj chunks chunk)))

            (<= tag-bytes-packed-length-start next-code (dec tag-bytes-packed-length-end))
            (let [len (- next-code tag-bytes-packed-length-start)
                  chunk (read-slice rdr len)]
              (apply concat-bytes (conj chunks chunk)))

            (= next-code tag-bytes)
            (let [len (decode-count rdr state-atom)
                  chunk (read-slice rdr len)]
              (apply concat-bytes (conj chunks chunk)))

            :else
            (throw (ex-info (str "Unexpected code in chunked bytes: " next-code) {:code next-code}))))))

    (= code tag-bytes)
    (let [len (decode-count rdr state-atom)]
      (read-slice rdr len))

    (<= tag-string-packed-length-start code (dec tag-string-packed-length-end))
    (let [len (- code tag-string-packed-length-start)
          cesu8 (read-slice rdr len)]
      (cesu8-bytes->string cesu8))

    (= code tag-string-chunk)
    (let [initial-len (decode-count rdr state-atom)
          initial-chunk (read-slice rdr initial-len)]
      (loop [chunks [initial-chunk]]
        (let [next-code (read-raw-byte rdr)]
          (cond
            (= next-code tag-string-chunk)
            (let [len (decode-count rdr state-atom)
                  chunk (read-slice rdr len)]
              (recur (conj chunks chunk)))

            (<= tag-string-packed-length-start next-code (dec tag-string-packed-length-end))
            (let [len (- next-code tag-string-packed-length-start)
                  chunk (read-slice rdr len)]
              (cesu8-bytes->string (apply concat-bytes (conj chunks chunk))))

            (= next-code tag-string)
            (let [len (decode-count rdr state-atom)
                  chunk (read-slice rdr len)]
              (cesu8-bytes->string (apply concat-bytes (conj chunks chunk))))

            :else
            (throw (ex-info (str "Unexpected code in chunked string: " next-code) {:code next-code}))))))

    (= code tag-string)
    (let [len (decode-count rdr state-atom)
          cesu8 (read-slice rdr len)]
      (cesu8-bytes->string cesu8))

    (= code tag-key)
    (let [ns (decode-value rdr state-atom)
          nm (decode-value rdr state-atom)]
      (if ns (keyword ns nm) (keyword nm)))

    (= code tag-sym)
    (let [ns (decode-value rdr state-atom)
          nm (decode-value rdr state-atom)]
      (if ns (symbol ns nm) (symbol nm)))

    (= code tag-uuid)
    (let [bs (decode-value rdr state-atom)]
      (bytes->uuid bs))

    (= code tag-inst)
    (let [ms (long (decode-value rdr state-atom))]
      (ms->inst ms))

    (= code tag-uri)
    (let [s (decode-value rdr state-atom)]
      #?(:clj  (java.net.URI. (str s))
         :cljs (tagged-object "uri" [s])))

    (= code tag-regex)
    (let [pat (decode-value rdr state-atom)]
      (re-pattern (str pat)))

    (= code tag-bigint)
    (let [bs (decode-value rdr state-atom)]
      #?(:clj  (bigint (java.math.BigInteger. ^bytes bs))
         :cljs (tagged-object "bigint" [bs])))

    (= code tag-bigdec)
    (let [bs (decode-value rdr state-atom)
          scale (int (decode-value rdr state-atom))]
      #?(:clj  (java.math.BigDecimal. (java.math.BigInteger. ^bytes bs) scale)
         :cljs (tagged-object "bigdec" [bs scale])))

    (<= tag-list-packed-length-start code (dec tag-list-packed-length-end))
    (let [cnt (- code tag-list-packed-length-start)]
      (loop [i 0 acc (transient [])]
        (if (< i cnt)
          (recur (inc i) (conj! acc (decode-value rdr state-atom)))
          (persistent! acc))))

    (= code tag-list)
    (let [cnt (decode-count rdr state-atom)]
      (loop [i 0 acc (transient [])]
        (if (< i cnt)
          (recur (inc i) (conj! acc (decode-value rdr state-atom)))
          (persistent! acc))))

    (= code tag-begin-closed-list)
    (loop [acc (transient [])]
      (let [next-code (read-raw-byte rdr)]
        (if (= next-code tag-end-collection)
          (persistent! acc)
          (recur (conj! acc (decode-value-by-code rdr state-atom next-code))))))

    (= code tag-begin-open-list)
    (loop [acc (transient [])]
      (if (>= (reader-pos rdr) (reader-len rdr))
        (persistent! acc)
        (let [next-code (read-raw-byte rdr)]
          (if (= next-code tag-end-collection)
            (persistent! acc)
            (recur (conj! acc (decode-value-by-code rdr state-atom next-code)))))))

    (contains? #{tag-long-array tag-double-array tag-boolean-array tag-int-array tag-float-array tag-object-array} code)
    (let [cnt (decode-count rdr state-atom)]
      (loop [i 0 acc (transient [])]
        (if (< i cnt)
          (recur (inc i) (conj! acc (decode-value rdr state-atom)))
          (persistent! acc))))

    (= code tag-set)
    (set (decode-value rdr state-atom))

    (= code tag-map)
    (let [items (decode-value rdr state-atom)]
      (into {} (map vec (partition 2 items))))

    ;; Priority Cache: reserve slot BEFORE reading child object (matches FressianReader.readAndCacheObject)
    (= code tag-put-priority-cache)
    (let [slot (count (:priority-cache @state-atom))
          _ (swap! state-atom update :priority-cache conj ::under-construction)
          obj (decode-value rdr state-atom)]
      (swap! state-atom assoc-in [:priority-cache slot] obj)
      obj)

    (<= tag-priority-cache-packed-start code (dec tag-priority-cache-packed-end))
    (let [idx (- code tag-priority-cache-packed-start)]
      (get (:priority-cache @state-atom) idx))

    (= code tag-get-priority-cache)
    (let [idx (decode-count rdr state-atom)]
      (get (:priority-cache @state-atom) idx))

    ;; Struct Cache: STRUCTTYPE (239), STRUCT_CACHE_PACKED (160..175), STRUCT (240)
    (= code tag-structtype)
    (let [struct-tag (decode-value rdr state-atom)
          fields (decode-count rdr state-atom)]
      (swap! state-atom update :struct-cache conj {:tag struct-tag :fields fields})
      (handle-struct-read rdr state-atom struct-tag fields))

    (<= tag-struct-cache-packed-start code (dec tag-struct-cache-packed-end))
    (let [idx (- code tag-struct-cache-packed-start)
          {:keys [tag fields]} (get (:struct-cache @state-atom) idx)]
      (handle-struct-read rdr state-atom tag fields))

    (= code tag-struct)
    (let [idx (decode-count rdr state-atom)
          {:keys [tag fields]} (get (:struct-cache @state-atom) idx)]
      (handle-struct-read rdr state-atom tag fields))

    (= code tag-reset-caches)
    (do
      (swap! state-atom assoc :priority-cache [] :struct-cache [])
      (decode-value rdr state-atom))

    :else
    (throw (ex-info (str "Unrecognized Fressian byte code: " code) {:code code}))))

(defn- validate-footer-bytes!
  [ba ^long footer-start]
  (let [total (count-bytes ba)]
    (when (< (- total footer-start) 12)
      (throw (ex-info "Truncated Fressian footer" {:footer-start footer-start :total total})))
    (let [m0 (u8 ba footer-start)
          m1 (u8 ba (+ footer-start 1))
          m2 (u8 ba (+ footer-start 2))
          m3 (u8 ba (+ footer-start 3))]
      (when-not (and (= m0 207) (= m1 207) (= m2 207) (= m3 207))
        (throw (ex-info "Invalid Fressian footer magic" {:magic [m0 m1 m2 m3]})))
      (let [claimed-len (bit-or (bit-shift-left (u8 ba (+ footer-start 4)) 24)
                                (bit-shift-left (u8 ba (+ footer-start 5)) 16)
                                (bit-shift-left (u8 ba (+ footer-start 6)) 8)
                                (u8 ba (+ footer-start 7)))
            claimed-csum (bit-or (bit-shift-left (u8 ba (+ footer-start 8)) 24)
                                 (bit-shift-left (u8 ba (+ footer-start 9)) 16)
                                 (bit-shift-left (u8 ba (+ footer-start 10)) 8)
                                 (u8 ba (+ footer-start 11)))
            actual-csum (adler32-bytes ba 0 (+ footer-start 8))]
        (when (not= claimed-len footer-start)
          (throw (ex-info "Fressian footer length mismatch"
                          {:expected footer-start :claimed claimed-len})))
        (when (not= claimed-csum actual-csum)
          (throw (ex-info "Fressian footer Adler32 checksum mismatch"
                          {:expected actual-csum :claimed claimed-csum})))))))

(defn read
  "Reads a Fressian value from byte array `ba` (`byte[]` on JVM, `Uint8Array` on CLJS).
   Options:
   - `:validate-footer? true` — validates trailing 12-byte Fressian footer (`0xCFCFCFCF` + length + Adler32).
   - `:read-handlers` — map of `tag -> (fn [fields-vec] ...)`."
  [ba & {:keys [validate-footer? read-handlers]}]
  (let [rdr (make-byte-reader ba)
        state-atom (atom {:priority-cache []
                          :struct-cache []
                          :read-handlers read-handlers})
        val (decode-value rdr state-atom)]
    (when validate-footer?
      (validate-footer-bytes! ba (reader-pos rdr)))
    val))

(defn decode
  "Decodes a Fressian byte array into Clojure persistent data structures."
  [ba]
  (read ba))

;; =============================================================================
;; 9. Wire Normalization Helpers
;; =============================================================================

(defn normalize-for-wire
  "Recursively pre-normalizes Clojure data structures for wire encoding:
   converts defrecords and maps into standard persistent maps,
   sequential collections/lazy seqs into persistent vectors, and
   JVM Byte/Short boxed primitives into Long."
  [x]
  (cond
    #?@(:clj [(or (instance? Byte x) (instance? Short x))
              (long x)])

    (tagged-object? x)
    x

    (cached? x)
    (cached (normalize-for-wire (cached-value x)))

    (record? x)
    (reduce-kv (fn [m k v]
                 (assoc m (normalize-for-wire k) (normalize-for-wire v)))
               {}
               x)

    (map? x)
    (reduce-kv (fn [m k v]
                 (assoc m (normalize-for-wire k) (normalize-for-wire v)))
               {}
               x)

    (vector? x)
    (mapv normalize-for-wire x)

    (set? x)
    (into #{} (map normalize-for-wire) x)

    (sequential? x)
    (mapv normalize-for-wire x)

    :else x))

#?(:clj
   (defn normalize-decoded-wire
     "Recursively normalizes Java collection types decoded by standard Fressian into
      standard Clojure persistent data structures."
     [x]
     (cond
       (tagged-object? x)
       (tagged-object (tag x) (mapv normalize-decoded-wire (tagged-value x)))

       (record? x)
       (reduce-kv (fn [r k v]
                    (assoc r k (normalize-decoded-wire v)))
                  x
                  x)

       (instance? java.util.Map x)
       (reduce (fn [m ^java.util.Map$Entry e]
                 (assoc m
                        (normalize-decoded-wire (.getKey e))
                        (normalize-decoded-wire (.getValue e))))
               {}
               (.entrySet ^java.util.Map x))

       (instance? java.util.List x)
       (mapv normalize-decoded-wire x)

       (instance? java.util.Set x)
       (into #{} (map normalize-decoded-wire) x)

       (sequential? x)
       (mapv normalize-decoded-wire x)

       :else x))
   :cljs
   (defn normalize-decoded-wire
     "Normalizes collections into standard Clojure persistent data structures."
     [x]
     (cond
       (tagged-object? x)
       (tagged-object (tag x) (mapv normalize-decoded-wire (tagged-value x)))

       (map? x)
       (reduce-kv (fn [m k v]
                    (assoc m (normalize-decoded-wire k) (normalize-decoded-wire v)))
                  {}
                  x)

       (vector? x)
       (mapv normalize-decoded-wire x)

       (set? x)
       (into #{} (map normalize-decoded-wire) x)

       (sequential? x)
       (mapv normalize-decoded-wire x)

       :else x)))
