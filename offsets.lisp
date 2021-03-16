(in-package :cl-stun)

;; this file contains some constants shared across the project
;; but I am aiming to just keep this the place where offset
;; constants are stored

(defvar *message-type-offset* 0)

(defvar *message-length-offset* 2)

(defvar *magic-cookie-offset* 4)
(defvar *magic-cookie-end* 8)

(defvar *transaction-id-offset* 8)
(defvar *transaction-id-end* 20)

(defvar *message-header-size* 20)

(defvar *tlv-header-size* 4) ;; type and length fields are four bytes long

(defvar *tlv-type-offset* 0)
(defvar *tlv-length-offset* 2)
