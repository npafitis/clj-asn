(ns clj-asn.utils
  (:require [clojure.spec.alpha :as s]
            [clojure.pprint :as pprint]
            [clope.core :as clp])
  (:import (clope.impl Rope)))

(defn bytes->int [bytes]
  "Converts a byte array into an integer."
  (->>
    bytes
    (map (partial format "%02x"))
    (apply (partial str "0x"))
    read-string))

(defn int-to-byte-array [value]
  {:pre [(s/valid? integer? value)]}
  (.toByteArray (BigInteger. (pr-str value))))

(defn print-binary [number]
  (pprint/cl-format nil "2r~8,'0',B" number))

(defn wrap-int [value]
  (clp/wrap (if (>= value 128)
              (int-to-byte-array value)
              (byte-array [(byte value)]))))

(defn rope? [x]
  (instance? Rope x))

(defn in-bounds-pred [bs]
  {:pre [(s/valid? bytes? bs)]}
  (fn [b]
    (< b (count bs))))

(defn log2 [x]
  (/ (Math/log x) (Math/log 2)))

(defn length-octets [x]
  (int (Math/ceil (/ (Math/ceil (log2 x)) 8))))