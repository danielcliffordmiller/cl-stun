(in-package :cl-stun)

;; this file contains some constants shared across the project
;; but I am aiming to just keep this the place where offset
;; constants are stored

(defconstant +message-type-offset+ 0)

(defconstant +message-length-offset+ 2)

(defconstant +magic-cookie-offset+ 4)
(defconstant +magic-cookie-end+ 8)

(defconstant +transaction-id-offset+ 8)
(defconstant +transaction-id-end+ 20)

(defconstant +message-header-size+ 20)

;; type and length fields are four bytes long
(defconstant +tlv-header-size+ 4)

(defconstant +tlv-type-offset+ 0)
(defconstant +tlv-length-offset+ 2)

(defconstant +crc32-size+ 4)

(defconstant +sha1-size+ 20)

(defconstant +sha256-size+ 32)
