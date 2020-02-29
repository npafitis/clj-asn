(ns clj-asn.buffer-spec
  (:require [octet.core :as octbuf]
            [octet.spec :as octspec]
            [octet.spec.basic :as octtype]))

(def tag-buffer-spec (octspec/spec ::tag-class octtype/byte))

(def int-buffer-spec (octspec/spec octtype/int32))

(def bool-buffer-spec (octspec/spec octtype/bool))


