(in-package :cl-stun)

(defvar *attribute-types*
  '((#x0000 . :reserved)
    (#x0001 . :mapped-address)
    (#x0002 . :reserved)     ; was RESPONSE-ADDRESS prior to [RFC5389]
    (#x0003 . :reserved)     ; was CHANGE-REQUEST prior to [RFC5389]
    (#x0004 . :reserved)     ; was SOURCE-ADDRESS prior to [RFC5389]
    (#x0005 . :reserved)     ; was CHANGED-ADDRESS prior to [RFC5389]
    (#x0006 . :username)
    (#x0007 . :reserved)	     ; was PASSWORD prior to [RFC5389]
    (#x0008 . :message-integrity)
    (#x0009 . :error-code)
    (#x000A . :unknown-attributes)
    (#x000B . :reserved)       ; was REFLECTED-FROM prior to [RFC5389]
    (#x0014 . :realm)
    (#x0015 . :nonce)
    (#x0020 . :xor-mapped-address)
    (#x8022 . :software)
    (#x8023 . :alternate-server)
    (#x8028 . :fingerprint)
    (#x001C . :message-integrity-sha256)
    (#x001D . :password-algorithm)
    (#x001E . :userhash)
    (#x8002 . :password-algorithms)
    (#x8003 . :alternate-domain)))

(defparameter *attribute-type-values*
  '((:mapped-address
     :family
     :port
     :address)
    (:xor-mapped-address
     :family
     :x-port
     :x-address)))

(defvar *address-families*
  '((#x01 . :ip4)
    (#x02 . :ip6)))

(defstruct stun-attribute
  "structure for a stun message attribute")
