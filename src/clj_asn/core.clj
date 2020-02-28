(ns clj-asn.core
  (:require [clope.core :as clp]))

(def tag
  {::end-of-content    0
   ::boolean           1
   ::integer           2
   ::bit-sting         3
   ::octet-string      4
   ::null              5
   ::object-identifier 6
   ::object-descriptor 7
   ::external          8
   ::real              9
   ::enumerated        10
   ::embedded-pdv      11
   ::utf8-string       12
   ::relative-oid      13
   ::time              14
   ::sequence          16
   ::set               17
   ::numeric-string    18
   ::printable-string  19
   ::t61-string        20
   ::videotex-string   21
   ::ia5:string        22
   ::utc-time          23
   ::generalized-time  24
   ::graphic-string    25
   ::visible-string    26
   ::general-string    27
   ::universal-string  28
   ::character-string  29
   ::bmp-string        30
   ::date              31
   ::time-of-day       32
   ::date-time         33
   ::duration          34
   ::oid-iri           35
   ::relative-oid-iri  36})

(def tag-encoding
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
  {::universal        0
   ::application      1
   ::context-specific 2
   ::private          3})

(def value-encoding
  {::both        -1
   ::primitive   0
   ::constructed 1})

(defprotocol IAsnInputStream
  (write-boolean [this value])
  (write-integer [this value])
  (write-bit-string [this value] [this value primitive?])
  (write-octet-string [this value] [this value primitive?])
  (write-null [this value])
  (write-object-identifier [this value])
  (write-object-descriptor [this value] [this value primitive?])
  (write-external [this value])
  (write-real [this value])
  (write-enumerated [this value])
  (write-embedded-pdv [this value])
  (write-utf8-string [this value] [this value primitive?])
  (write-relative-oid [this value])
  (write-time [this value])
  (write-sequence [this value])
  (write-set [this value])
  (write-numeric-string [this value] [this value primitive?])
  (write-printable-string [this value] [this value primitive?])
  (write-t61-string [this value] [this value primitive?])
  (write-videotex-string [this value] [this value primitive?])
  (write-ia5-string [this value] [this value primitive?])
  (write-utc-time [this value] [this value primitive?])
  (write-generalized-time [this value] [this value primitive?])
  (write-graphic-string [this value] [this value primitive?])
  (write-universal-string [this value] [this value primitive?])
  (write-character-string [this value] [this value primitive?])
  (write-bmp-string [this value] [this value primitive?])
  (write-date [this value])
  (write-time-of-day [this value])
  (write-date-time [this value])
  (write-duration [this value])
  (write-oid-iri [this value])
  (write-relative-oid-iri [this value])

  (write-tag [this t tc primitive?])

  )

(defrecord AsnInputStream [rope]
  IAsnInputStream
  (write-boolean [this value] nil)

  (write-integer [this value] nil)

  (write-bit-string [this value]
    (write-bit-string this value true))

  (write-bit-string [this value primitive?] nil)

  (write-octet-string [this value]
    (write-octet-string this value true))

  (write-octet-string [this value primitive?] nil)

  (write-null [this value] nil)

  (write-object-identifier [this value] nil)

  (write-object-descriptor [this value]
    (write-object-descriptor this value true))

  (write-object-descriptor [this value primitive?] nil)

  (write-external [this value] nil)

  (write-real [this value] nil)

  (write-enumerated [this value] nil)

  (write-embedded-pdv [this value] nil)

  (write-utf8-string [this value]
    (write-utf8-string this value true))

  (write-utf8-string [this value primitive?] nil)

  (write-relative-oid [this value] nil)

  (write-time [this value] nil)

  (write-sequence [this value] nil)

  (write-set [this value] nil)

  (write-numeric-string [this value]
    (write-numeric-string this value true))

  (write-numeric-string [this value primitive?] nil)

  (write-printable-string [this value]
    (write-printable-string this value true))

  (write-printable-string [this value primitive?] nil)

  (write-t61-string [this value]
    (write-t61-string this value true))

  (write-t61-string [this value primitive?] nil)

  (write-videotex-string [this value]
    (write-videotex-string this value true))

  (write-videotex-string [this value primitive?] nil)

  (write-ia5-string [this value]
    (write-ia5-string this value true))

  (write-ia5-string [this value primitive?] nil)

  (write-utc-time [this value]
    (write-utc-time this value true))

  (write-utc-time [this value primitive?] nil)

  (write-generalized-time [this value]
    (write-generalized-time this value true))

  (write-generalized-time [this value primitive?] nil)

  (write-graphic-string [this value]
    (write-graphic-string this value true))

  (write-graphic-string [this value primitive?] nil)

  (write-universal-string [this value]
    (write-universal-string this value true))

  (write-universal-string [this value primitive?] nil)

  (write-character-string [this value]
    (write-character-string this value true))

  (write-character-string [this value primitive?] nil)

  (write-bmp-string [this value]
    (write-bmp-string this value true))

  (write-bmp-string [this value primitive?] nil)

  (write-date [this value] nil)

  (write-time-of-day [this value] nil)

  (write-date-time [this value] nil)

  (write-duration [this value] nil)

  (write-oid-iri [this value] nil)

  (write-relative-oid-iri [this value] nil)

  (write-tag [this t tc primitive?]
    (let [rope (:rope this)
          tag-rope (clp/wrap ())])))


(defn -main [& args]
  (prn "Hello World"))
