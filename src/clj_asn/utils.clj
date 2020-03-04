(ns clj-asn.utils
  (:require [clojure.spec.alpha :as s]
            [clope.core :as clp]
            [byte-streams :as bstr])
  (:import (clope.impl Rope)))

(def boolean-value
  {false 0
   true  1})

(defn num->byte-array [value]
  {:pre [(s/valid? integer? value)]}
  (.toByteArray (BigInteger. (pr-str value))))

(def byte-array-type (type (byte-array [])))

(bstr/def-conversion [Number byte-array-type]
                     [x options]
                     (num->byte-array x))

(bstr/def-conversion [Boolean byte-array-type]
                     [x options]
                     (-> x
                         (boolean-value)
                         (num->byte-array)))

(defn wrap [value]
  (clp/wrap (bstr/to-byte-array value)))

(defn rope? [x]
  (instance? Rope x))

(defn in-bounds-pred [bs]
  {:pre [(s/valid? bytes? bs)]}
  (fn [b]
    (< b (count bs))))