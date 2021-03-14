(in-package :common-lisp)

(defpackage cl-stun
  (:use
   :common-lisp
   :ironclad
   :usocket
   :flexi-streams)
  (:export
   #:decompose-message-type
   #:compose-message-type
   #:make-stun-message))

(defpackage cl-stun.test
  (:use
   :common-lisp
   :cl-stun
   :fiveam))
