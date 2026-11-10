# clj-fressian

Pure, zero-dependency `.cljc` Fressian serialization and layered LZ4 block compression library for Clojure (JVM) and ClojureScript (Node.js & Browser).

## Features

- **`fressian.core` — Pure `.cljc` Fressian Codec**:
  - **Standard Fressian Mode (`write` / `read` / `decode`)**: Byte-compatible with `org.clojure/data.fressian` in both directions, including priority caching (`PUT_PRIORITY_CACHE`, `PRIORITY_CACHE_PACKED`, `GET_PRIORITY_CACHE`), struct caching (`STRUCTTYPE`, `STRUCT_CACHE_PACKED`, `STRUCT`), custom tagged types (`tagged-object`, `:write-handlers`, `:read-handlers`), `UUID`, `inst`, `URI`, `regex`, `bigint`, `bigdec`, `char`, `double` (`0.0`, `1.0`, IEEE-754), `float`, chunked `bytes` and CESU-8 `string`s, closed/open lists, `RESET_CACHES`, and Adler32 `FOOTER` validation.
  - **Deterministic Canonical Fressian Mode (`encode-canonical`)**: Total canonical ordering (`canonical-compare`) across map keys and set elements for cryptographic signing and content-addressed hashing (`Blob` / `Pointer` CIDs) across JVM and ClojureScript.
  - **Zero Production Dependencies**: `org.clojure/data.fressian` is used strictly in `:test` to prove bidirectional byte-compatibility via `clojure.test.check` generative cross-tests.
- **`fressian.lz4` — Pure `.cljc` LZ4 Block Codec & Layered `BTEC` Wire Envelope**:
  - Pure `.cljc` raw LZ4 block compressor (`lz4-compress-block`) and bounded fail-closed decompressor (`lz4-decompress-block`), cross-verified against `io.airlift.compress.lz4`.
  - Optional layered `BTEC` wire envelope (`encode-wire` / `decode-wire` / `decode-wire-strict`) with `:compress? false` support so callers on any runtime can operate compressed or uncompressed.

## Verification

```bash
clojure -M:format
clojure -M:lint
clojure -M:test
clojure -M:test-cljs
```
