(ns fressian.lz4
  "Pure-CLJC raw LZ4 block codec and layered BTEC wire envelope (`fressian.lz4`).
   Layered cleanly above `fressian.core` so callers on JVM, Node.js, or Browser can
   choose compressed (`:compress? true`, default for frames > 1 KiB) or uncompressed
   (`:compress? false`) wire encoding."
  (:require [fressian.core :as fressian]))

(def wire-compression-threshold-bytes
  "Minimum uncompressed Fressian payload byte length (> 1 KiB) before attempting LZ4 compression."
  1024)

(def max-wire-decompressed-bytes
  "Maximum allowed uncompressed payload size (1 MiB) when decoding a BTEC compressed frame."
  1048576)

(def btec-tag-lz4
  "Algorithm selector byte for raw LZ4 block compression in a BTEC frame."
  1)

(defn- write-lz4-run-length
  [w ^long len]
  (loop [rem-len len]
    (if (>= rem-len 255)
      (do
        (fressian/write-raw-byte w 255)
        (recur (- rem-len 255)))
      (fressian/write-raw-byte w rem-len))))

(defn- lz4-hash4
  ^long [ba ^long pos]
  (let [v (bit-or (fressian/u8 ba pos)
                  (bit-shift-left (fressian/u8 ba (+ pos 1)) 8)
                  (bit-shift-left (fressian/u8 ba (+ pos 2)) 16)
                  (bit-shift-left (fressian/u8 ba (+ pos 3)) 24))
        prod #?(:clj  (unchecked-multiply-int (unchecked-int v) -1640531535)
                :cljs (js/Math.imul v -1640531535))]
    (bit-and (unsigned-bit-shift-right prod 20) 0xfff)))

(defn lz4-compress-block
  "Pure-CLJC raw LZ4 block compressor compatible with standard LZ4 block decoders
   (including io.airlift.compress.lz4.Lz4Decompressor).
   Enforces standard LZ4 block safety margins (MIN_MATCH=4, LAST_LITERAL_SIZE=5, MF_LIMIT=12)."
  [input-bytes]
  (let [in-len (fressian/count-bytes input-bytes)]
    (if (zero? in-len)
      #?(:clj (byte-array 0) :cljs (js/Uint8Array. 0))
      (let [w (fressian/make-byte-writer)
            match-limit (- in-len 5)
            mf-limit (- in-len 12)
            htable #?(:clj  (int-array 4096 -1)
                      :cljs (let [a (js/Int32Array. 4096)] (.fill a -1) a))]
        (loop [anchor 0
               pos 0]
          (if (> pos mf-limit)
            (let [lit-len (- in-len anchor)]
              (fressian/write-raw-byte w (bit-shift-left (min lit-len 15) 4))
              (when (>= lit-len 15)
                (write-lz4-run-length w (- lit-len 15)))
              (loop [i anchor]
                (when (< i in-len)
                  (fressian/write-raw-byte w (fressian/u8 input-bytes i))
                  (recur (inc i))))
              (fressian/get-result-bytes w))
            (let [h (lz4-hash4 input-bytes pos)
                  ref-pos (long (aget htable h))
                  _ (aset htable h (int pos))
                  offset (- pos ref-pos)
                  match? (and (>= ref-pos 0)
                              (pos? offset)
                              (<= offset 65535)
                              (= (fressian/u8 input-bytes ref-pos) (fressian/u8 input-bytes pos))
                              (= (fressian/u8 input-bytes (+ ref-pos 1)) (fressian/u8 input-bytes (+ pos 1)))
                              (= (fressian/u8 input-bytes (+ ref-pos 2)) (fressian/u8 input-bytes (+ pos 2)))
                              (= (fressian/u8 input-bytes (+ ref-pos 3)) (fressian/u8 input-bytes (+ pos 3))))]
              (if-not match?
                (recur anchor (inc pos))
                (let [mlen (loop [m 4]
                             (if (and (< (+ pos m) match-limit)
                                      (= (fressian/u8 input-bytes (+ ref-pos m))
                                         (fressian/u8 input-bytes (+ pos m))))
                               (recur (inc m))
                               m))
                      lit-len (- pos anchor)
                      m-extra (- mlen 4)
                      token (bit-or (bit-shift-left (min lit-len 15) 4)
                                    (min m-extra 15))]
                  (fressian/write-raw-byte w token)
                  (when (>= lit-len 15)
                    (write-lz4-run-length w (- lit-len 15)))
                  (loop [i anchor]
                    (when (< i pos)
                      (fressian/write-raw-byte w (fressian/u8 input-bytes i))
                      (recur (inc i))))
                  (fressian/write-raw-byte w (bit-and offset 0xff))
                  (fressian/write-raw-byte w (bit-and (unsigned-bit-shift-right offset 8) 0xff))
                  (when (>= m-extra 15)
                    (write-lz4-run-length w (- m-extra 15)))
                  (let [next-pos (+ pos mlen)]
                    (recur next-pos next-pos)))))))))))

(defn lz4-decompress-block
  "Pure-CLJC raw LZ4 block decompressor with strict bounds checking.
   Throws ExceptionInfo with :reason :corrupt-compressed-frame on any malformed input."
  ([compressed-bytes orig-len]
   (lz4-decompress-block compressed-bytes 0 (fressian/count-bytes compressed-bytes) orig-len))
  ([compressed-bytes start-idx end-idx orig-len]
   (let [orig-len (long orig-len)
         start-idx (long start-idx)
         end-idx (long end-idx)
         in-len (- end-idx start-idx)]
     (cond
       (or (neg? orig-len) (> orig-len max-wire-decompressed-bytes))
       (throw (ex-info (str "BTEC decompressed size exceeds 1 MiB limit: " orig-len)
                       {:reason :decompressed-size-exceeded :orig-len orig-len}))

       (zero? orig-len)
       (if (zero? in-len)
         #?(:clj (byte-array 0) :cljs (js/Uint8Array. 0))
         (throw (ex-info "Corrupt LZ4 block: non-empty input for zero-length output"
                         {:reason :corrupt-compressed-frame})))

       (<= in-len 0)
       (throw (ex-info "Corrupt LZ4 block: empty input for non-zero output"
                       {:reason :corrupt-compressed-frame}))

       :else
       (let [out #?(:clj (byte-array orig-len) :cljs (js/Uint8Array. orig-len))]
         (loop [in-pos start-idx
                out-pos 0]
           (if (>= in-pos end-idx)
             (if (= out-pos orig-len)
               out
               (throw (ex-info "Corrupt LZ4 block: unexpected end of stream"
                               {:reason :corrupt-compressed-frame})))
             (let [token (fressian/u8 compressed-bytes in-pos)
                   in-pos-1 (inc in-pos)
                   lit-base (unsigned-bit-shift-right token 4)
                   [lit-len in-pos-2] (if (= lit-base 15)
                                        (loop [acc 15
                                               p in-pos-1]
                                          (when (>= p end-idx)
                                            (throw (ex-info "Corrupt LZ4 block: truncated literal length"
                                                            {:reason :corrupt-compressed-frame})))
                                          (let [b (fressian/u8 compressed-bytes p)
                                                next-acc (+ acc b)]
                                            (when (> next-acc orig-len)
                                              (throw (ex-info "Corrupt LZ4 block: literal length overflow"
                                                              {:reason :corrupt-compressed-frame})))
                                            (if (= b 255)
                                              (recur next-acc (inc p))
                                              [next-acc (inc p)])))
                                        [lit-base in-pos-1])
                   lit-end-in (+ in-pos-2 lit-len)
                   lit-end-out (+ out-pos lit-len)]
               (when (or (> lit-end-in end-idx) (> lit-end-out orig-len))
                 (throw (ex-info "Corrupt LZ4 block: literal copy out of bounds"
                                 {:reason :corrupt-compressed-frame})))
               (loop [i 0]
                 (when (< i lit-len)
                   (fressian/aset-u8! out (+ out-pos i) (fressian/u8 compressed-bytes (+ in-pos-2 i)))
                   (recur (inc i))))
               (if (= lit-end-in end-idx)
                 (if (= lit-end-out orig-len)
                   out
                   (throw (ex-info "Corrupt LZ4 block: output size mismatch after final literals"
                                   {:reason :corrupt-compressed-frame})))
                 (do
                   (when (> (+ lit-end-in 2) end-idx)
                     (throw (ex-info "Corrupt LZ4 block: truncated match offset"
                                     {:reason :corrupt-compressed-frame})))
                   (let [offset (bit-or (fressian/u8 compressed-bytes lit-end-in)
                                        (bit-shift-left (fressian/u8 compressed-bytes (inc lit-end-in)) 8))
                         in-pos-3 (+ lit-end-in 2)]
                     (when (or (<= offset 0) (> offset lit-end-out))
                       (throw (ex-info "Corrupt LZ4 block: invalid match offset"
                                       {:reason :corrupt-compressed-frame :offset offset})))
                     (let [m-base (bit-and token 0x0f)
                           [match-len in-pos-4] (if (= m-base 15)
                                                  (loop [acc 19
                                                         p in-pos-3]
                                                    (when (>= p end-idx)
                                                      (throw (ex-info "Corrupt LZ4 block: truncated match length"
                                                                      {:reason :corrupt-compressed-frame})))
                                                    (let [b (fressian/u8 compressed-bytes p)
                                                          next-acc (+ acc b)]
                                                      (when (> (+ lit-end-out next-acc) orig-len)
                                                        (throw (ex-info "Corrupt LZ4 block: match length overflow"
                                                                        {:reason :corrupt-compressed-frame})))
                                                      (if (= b 255)
                                                        (recur next-acc (inc p))
                                                        [next-acc (inc p)])))
                                                  [(+ m-base 4) in-pos-3])
                           match-end-out (+ lit-end-out match-len)
                           match-src (- lit-end-out offset)]
                       (when (> match-end-out orig-len)
                         (throw (ex-info "Corrupt LZ4 block: match copy overrun"
                                         {:reason :corrupt-compressed-frame})))
                       (loop [i 0]
                         (when (< i match-len)
                           (fressian/aset-u8! out (+ lit-end-out i) (fressian/u8 out (+ match-src i)))
                           (recur (inc i))))
                       (recur in-pos-4 match-end-out)))))))))))))

(defn btec-compressed-frame?
  "Returns true if ba is a byte array of length >= 4 starting with the 'BTEC' magic header (0x42 0x54 0x45 0x43)."
  [ba]
  (and (fressian/bytes? ba)
       (>= (fressian/count-bytes ba) 4)
       (= (fressian/u8 ba 0) 0x42)
       (= (fressian/u8 ba 1) 0x54)
       (= (fressian/u8 ba 2) 0x45)
       (= (fressian/u8 ba 3) 0x43)))

(defn wrap-btec-lz4-frame
  "Wraps `compressed-bytes` in a 9-byte BTEC LZ4 header (`BTEC` + tag 0x01 + 4-byte BE original length)."
  [compressed-bytes ^long orig-len]
  (let [c-len (fressian/count-bytes compressed-bytes)
        total-len (+ 9 c-len)
        out #?(:clj (byte-array total-len) :cljs (js/Uint8Array. total-len))]
    (fressian/aset-u8! out 0 0x42)
    (fressian/aset-u8! out 1 0x54)
    (fressian/aset-u8! out 2 0x45)
    (fressian/aset-u8! out 3 0x43)
    (fressian/aset-u8! out 4 btec-tag-lz4)
    (fressian/aset-u8! out 5 (bit-and (unsigned-bit-shift-right orig-len 24) 0xff))
    (fressian/aset-u8! out 6 (bit-and (unsigned-bit-shift-right orig-len 16) 0xff))
    (fressian/aset-u8! out 7 (bit-and (unsigned-bit-shift-right orig-len 8) 0xff))
    (fressian/aset-u8! out 8 (bit-and orig-len 0xff))
    (loop [i 0]
      (when (< i c-len)
        (fressian/aset-u8! out (+ 9 i) (fressian/u8 compressed-bytes i))
        (recur (inc i))))
    out))

(defn unwrap-btec-frame
  "Strictly validates and decompresses a BTEC-prefixed frame.
   Fails closed by throwing ExceptionInfo on truncated header (< 9 bytes),
   unknown algorithm tag (!= 0x01), > 1 MiB claimed size, or corrupt LZ4 stream."
  [ba]
  (let [total-len (fressian/count-bytes ba)]
    (when (< total-len 9)
      (throw (ex-info "Truncated BTEC compressed frame"
                      {:reason :corrupt-compressed-frame :length total-len})))
    (let [tag (fressian/u8 ba 4)]
      (when (not= tag btec-tag-lz4)
        (throw (ex-info (str "Unsupported BTEC compression tag: " tag)
                        {:reason :unknown-compression-tag :tag tag})))
      (let [b5 (fressian/u8 ba 5)
            b6 (fressian/u8 ba 6)
            b7 (fressian/u8 ba 7)
            b8 (fressian/u8 ba 8)
            orig-len (bit-or (bit-shift-left b5 24)
                             (bit-shift-left b6 16)
                             (bit-shift-left b7 8)
                             b8)]
        (when (or (neg? orig-len) (> orig-len max-wire-decompressed-bytes))
          (throw (ex-info (str "BTEC decompressed size exceeds 1 MiB limit: " orig-len)
                          {:reason :decompressed-size-exceeded :orig-len orig-len})))
        (lz4-decompress-block ba 9 total-len orig-len)))))

(defn encode-wire-uncompressed
  "Encodes `val` into uncompressed Fressian wire bytes (using standard priority-cached Fressian `write`)."
  [val]
  (let [norm (fressian/normalize-for-wire val)]
    (fressian/write norm)))

(defn encode-wire
  "Encodes `val` into wire bytes using Fressian, applying synchronous LZ4 block compression
   ('BTEC' + tag 0x01 + 4-byte BE original length) when `:compress?` is true (default),
   the uncompressed Fressian payload exceeds `:threshold-bytes` (1024), and the compressed frame
   is strictly smaller than the raw Fressian bytes.
   Pass `{:compress? false}` to force uncompressed Fressian output."
  ([val]
   (encode-wire val nil))
  ([val {:keys [compress? threshold-bytes]
         :or {compress? true
              threshold-bytes wire-compression-threshold-bytes}}]
   (let [raw-bytes (encode-wire-uncompressed val)
         raw-len (fressian/count-bytes raw-bytes)]
     (if (and compress? (> raw-len (long threshold-bytes)))
       (let [compressed (lz4-compress-block raw-bytes)
             comp-len (fressian/count-bytes compressed)]
         (if (< (+ 9 comp-len) raw-len)
           (wrap-btec-lz4-frame compressed raw-len)
           raw-bytes))
       raw-bytes))))

(defn decode-wire-strict
  "Strictly decodes a raw or BTEC-compressed Fressian wire frame, throwing ExceptionInfo on
   BTEC framing/decompression errors."
  [ba]
  (when (and (fressian/bytes? ba) (pos? (fressian/count-bytes ba)))
    (let [payload-bytes (if (btec-compressed-frame? ba)
                          (unwrap-btec-frame ba)
                          ba)]
      (fressian/normalize-decoded-wire (fressian/read payload-bytes)))))

(defn decode-wire
  "Decodes a raw or BTEC-compressed Fressian wire frame into Clojure persistent data structures,
   failing closed (`nil`) on any malformed or tampered input."
  [ba]
  (try
    (decode-wire-strict ba)
    (catch #?(:clj Exception :cljs :default) _
      nil)))
