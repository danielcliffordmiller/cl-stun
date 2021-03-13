(in-package :common-lisp)

(defpackage cl-stun
  (:use
   :common-lisp)
  (:export
   #:decompose-message-type
   #:compose-message-type))

(defpackage cl-stun.test
  (:use
   :common-lisp
   :cl-stun
   :fiveam))
