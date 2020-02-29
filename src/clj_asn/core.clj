(ns clj-asn.core
  (:require [clope.core :as clp]
            [clj-asn.utils :as utils]
            [clojure.spec.alpha :as spec])
  (:import (clope.impl Rope)))

(declare create-input-stream)

(def tag
  {::end-of-content    2r00000000
   ::boolean           2r00000001
   ::integer           2r00000010
   ::bit-sting         2r00000011
   ::octet-string      2r00000100
   ::null              2r00000101
   ::object-identifier 2r00000110
   ::object-descriptor 2r00000111
   ::external          2r00001000
   ::real              2r00001001
   ::enumerated        2r00001010
   ::embedded-pdv      2r00001011
   ::utf8-string       2r00001100
   ::relative-oid      2r00001101
   ::time              2r00001110
   ::sequence          2r00010000
   ::set               2r00010001
   ::numeric-string    2r00010010
   ::printable-string  2r00010011
   ::t61-string        2r00010100
   ::videotex-string   2r00010101
   ::ia5:string        2r00010110
   ::utc-time          2r00010111
   ::generalized-time  2r00011000
   ::graphic-string    2r00011001
   ::visible-string    2r00011010
   ::general-string    2r00011011
   ::universal-string  2r00011100
   ::character-string  2r00011101
   ::bmp-string        2r00011110
   ::date              2r0001111100011111
   ::time-of-day       2r0001111100100000
   ::date-time         2r0001111100100001
   ::duration          2r0001111100100010
   ::oid-iri           2r0001111100100011
   ::relative-oid-iri  2r0001111100100100})

(def tag-encoding-map
  {::end-of-content    ::primitive
   ::boolean           ::primitive
   ::integer           ::primitive
   ::bit-sting         ::both
   ::octet-string      ::both
   ::null              ::primitive
   ::object-identifier ::primitive
   ::object-descriptor ::both
   ::external          ::constructed
   ::real              ::primitive
   ::enumerated        ::primitive
   ::embedded-pdv      ::constructed
   ::utf8-string       ::both
   ::relative-oid      ::primitive
   ::time              ::primitive
   ::sequence          ::constructed
   ::set               ::constructed
   ::numeric-string    ::both
   ::printable-string  ::both
   ::t61-string        ::both
   ::videotex-string   ::both
   ::ia5:string        ::both
   ::utc-time          ::both
   ::generalized-time  ::both
   ::graphic-string    ::both
   ::visible-string    ::both
   ::general-string    ::both
   ::universal-string  ::both
   ::character-string  ::both
   ::bmp-string        ::both
   ::date              ::primitive
   ::time-of-day       ::primitive
   ::date-time         ::primitive
   ::duration          ::primitive
   ::oid-iri           ::primitive
   ::relative-oid-iri  ::primitive})

(def tag-class
  {::universal        2r00000000
   ::application      2r01000000
   ::context-specific 2r10000000
   ::private          2r11000000})

(def form
  {::both        -1
   ::primitive   2r00000000
   ::constructed 2r01000000})

(def boolean-value
  {false 0
   true  1})

(defn byte-size [number]
  (count (utils/int-to-byte-array number)))

(defn shift-octet [number t]
  (if (> t 30)
    (bit-shift-left number 8)
    number))

(defprotocol IAsnOutputStream
  (write-boolean [this value])
  (write-integer [this value])
  (write-bit-string [this value] [this value form])
  (write-octet-string [this value] [this value form])
  (write-null [this value])
  (write-object-identifier [this value])
  (write-object-descriptor [this value] [this value form])
  (write-external [this value])
  (write-real [this value])
  (write-enumerated [this value])
  (write-embedded-pdv [this value])
  (write-utf8-string [this value] [this value form])
  (write-relative-oid [this value])
  (write-time [this value])
  (write-sequence [this value])
  (write-set [this value])
  (write-numeric-string [this value] [this value form])
  (write-printable-string [this value] [this value form])
  (write-t61-string [this value] [this value form])
  (write-videotex-string [this value] [this value form])
  (write-ia5-string [this value] [this value form])
  (write-utc-time [this value] [this value form])
  (write-generalized-time [this value] [this value form])
  (write-graphic-string [this value] [this value form])
  (write-universal-string [this value] [this value form])
  (write-character-string [this value] [this value form])
  (write-bmp-string [this value] [this value form])
  (write-date [this value])
  (write-time-of-day [this value])
  (write-date-time [this value])
  (write-duration [this value])
  (write-oid-iri [this value])
  (write-relative-oid-iri [this value])
  (write-tag [this t tc form])
  (write-length [this length])
  (write [this value] [this bytes off size])
  )

(defrecord AsnOutputStream [rope]
  IAsnOutputStream
  (write-boolean [^AsnOutputStream this
                  value]
    {:pre [(spec/valid? boolean? value)]}
    (-> this
        (write-tag ::universal ::primitive ::boolean)))

  (write-integer [^AsnOutputStream this
                  value]
    (-> this
        (write-tag ::universal ::primitive ::integer)))

  (write-bit-string [^AsnOutputStream this
                     value]
    (write-bit-string this value ::primitive))

  (write-bit-string [^AsnOutputStream this
                     value form]
    (-> this
        (write-tag ::universal form ::bit-sting)))

  (write-octet-string [^AsnOutputStream this
                       value]
    (write-octet-string this value ::primitive))

  (write-octet-string [^AsnOutputStream this
                       value form]
    (-> this
        (write-tag ::universal form ::octet-string)))

  (write-null [^AsnOutputStream this
               value]
    (-> this
        (write-tag ::universal ::primitive ::null)))

  (write-object-identifier [^AsnOutputStream this
                            value]
    (-> this
        (write-tag ::universal ::primitive ::object-identifier)))

  (write-object-descriptor [^AsnOutputStream this
                            value]
    (write-object-descriptor this value ::primitive))

  (write-object-descriptor [^AsnOutputStream this
                            value form]
    (-> this
        (write-tag ::universal form ::object-descriptor)))

  (write-external [^AsnOutputStream this
                   value]
    (-> this
        (write-tag ::universal ::primitive ::external)))

  (write-real [^AsnOutputStream this
               value]
    (-> this
        (write-tag ::universal ::primitive ::real)))

  (write-enumerated [^AsnOutputStream this
                     value]
    (-> this
        (write-tag ::universal ::primitive ::enumerated)))

  (write-embedded-pdv [^AsnOutputStream this
                       value]
    (-> this
        (write-tag ::universal ::primitive ::embedded-pdv)))

  (write-utf8-string [^AsnOutputStream this
                      value]
    (write-utf8-string this value ::primitive))

  (write-utf8-string [^AsnOutputStream this
                      value form]
    (-> this
        (write-tag ::universal form ::utf8-string)))

  (write-relative-oid [^AsnOutputStream this
                       value]
    (-> this
        (write-tag ::universal ::primitive ::relative-oid)))

  (write-time [^AsnOutputStream this
               value]
    (-> this
        (write-tag ::universal ::primitive ::time)))

  (write-sequence [^AsnOutputStream this
                   value]
    (-> this
        (write-tag ::universal ::primitive ::sequence)))

  (write-set [^AsnOutputStream this
              value]
    (-> this
        (write-tag ::universal ::primitive ::set)))

  (write-numeric-string [^AsnOutputStream this
                         value]
    (write-numeric-string this value ::primitive))

  (write-numeric-string [^AsnOutputStream this
                         value form]
    (-> this
        (write-tag ::universal form ::numeric-string)))

  (write-printable-string [^AsnOutputStream this
                           value]
    (write-printable-string this value ::primitive))

  (write-printable-string [^AsnOutputStream this
                           value form]
    (-> this
        (write-tag ::universal form ::printable-string)))

  (write-t61-string [^AsnOutputStream this
                     value]
    (write-t61-string this value ::primitive))

  (write-t61-string [^AsnOutputStream this
                     value form]
    (-> this
        (write-tag ::universal form ::t61-string)))

  (write-videotex-string [^AsnOutputStream this
                          value]
    (write-videotex-string this value ::primitive))

  (write-videotex-string [^AsnOutputStream this
                          value form]
    (-> this
        (write-tag ::universal form ::videotex-string)))

  (write-ia5-string [^AsnOutputStream this
                     value]
    (write-ia5-string this value ::primitive))

  (write-ia5-string [^AsnOutputStream this
                     value form]
    (-> this
        (write-tag ::universal form ::ia5-string)))

  (write-utc-time [^AsnOutputStream this
                   value]
    (write-utc-time this value ::primitive))

  (write-utc-time [^AsnOutputStream this
                   value form]
    (-> this
        (write-tag ::universal form ::utc-time)))

  (write-generalized-time [^AsnOutputStream this
                           value]
    (write-generalized-time this value ::primitive))

  (write-generalized-time [^AsnOutputStream this
                           value form]
    (-> this
        (write-tag ::universal form ::generalized-time)))

  (write-graphic-string [^AsnOutputStream this
                         value]
    (write-graphic-string this value ::primitive))

  (write-graphic-string [^AsnOutputStream this
                         value form]
    (-> this
        (write-tag ::universal form ::graphic-string)))

  (write-universal-string [^AsnOutputStream this
                           value]
    (write-universal-string this value ::primitive))

  (write-universal-string [^AsnOutputStream this
                           value form]
    (-> this
        (write-tag ::universal form ::universal-string)))

  (write-character-string [^AsnOutputStream this
                           value]
    (write-character-string this value ::primitive))

  (write-character-string [^AsnOutputStream this
                           value form]
    (-> this
        (write-tag ::universal form ::character-string)))

  (write-bmp-string [^AsnOutputStream this
                     value]
    (write-bmp-string this value ::primitive))

  (write-bmp-string [^AsnOutputStream this
                     value form]
    (-> this
        (write-tag ::universal form ::bmp-string)))

  (write-date [^AsnOutputStream this
               value]
    (-> this
        (write-tag ::universal ::primitive ::date)))

  (write-time-of-day [^AsnOutputStream this
                      value]
    (-> this
        (write-tag ::universal ::primitive ::time-of-day)))

  (write-date-time [^AsnOutputStream this
                    value]
    (-> this
        (write-tag ::universal ::primitive ::date-time)))

  (write-duration [^AsnOutputStream this
                   value]
    (-> this
        (write-tag ::universal ::primitive ::duration)))

  (write-oid-iri [^AsnOutputStream this
                  value]
    (-> this
        (write-tag ::universal ::primitive ::oid-iri)))

  (write-relative-oid-iri [^AsnOutputStream this
                           value]
    (-> this
        (write-tag ::universal ::primitive ::relative-oid-iri)))

  (write-tag [^AsnOutputStream this
              tc f t]
    (let [tag-encoding (tag t)
          tag-class-encoding (shift-octet (tag-class tc) tag-encoding)
          form-encoding (shift-octet (form f) tag-encoding)
          rope (clp/wrap
                 (utils/int-to-byte-array
                   (bit-or tag-encoding
                           tag-class-encoding
                           form-encoding)))]
      (write this (if (< tag-encoding 30)
                    (clp/subr rope (dec (clp/size rope)))
                    (clp/subr rope (dec (dec (clp/size rope))))))))

  (write-length [^AsnOutputStream this
                 length]
    {:pre [(spec/valid? integer? length)]}
    (if (= length ::indefinite-length)
      (write this 0x80)                                     ;write 0x80
      (if (< length 0x80)
        (write this length)
        this)                                               ;; write length bigger than a byte
      ))

  (write [^AsnOutputStream this value]
    {:pre [(or (spec/valid? integer? value)
               (spec/valid? bytes? value)
               (spec/valid? string? value)
               (spec/valid? boolean? value)
               (spec/valid? utils/rope? value))]}
    (create-input-stream this (cond (integer? value) (utils/wrap-int value)
                                    (bytes? value) (clp/wrap value)
                                    (string? value) (clp/wrap (.getBytes value))
                                    (boolean? value) (utils/wrap-int (boolean-value value))
                                    (utils/rope? value) value))))


(defn create-input-stream
  ([]
   (->AsnOutputStream nil))
  ([^AsnOutputStream output-stream ^Rope rope]
   (->AsnOutputStream (clp/join (:rope output-stream) rope))))
