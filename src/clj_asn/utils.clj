(ns clj-asn.utils
  (:require [clojure.spec.alpha :as spec]
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
  {:pre [(spec/valid? integer? value)]}
  (.toByteArray (BigInteger. (pr-str value))))

(defn print-binary [number]
  (pprint/cl-format nil "2r~8,'0',B" number))

(defn wrap-int [value]
  (clp/wrap (byte-array [(byte value)])))

(defn rope? [x]
  (instance? Rope x))