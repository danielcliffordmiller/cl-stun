(in-package :common-lisp)

(defpackage cl-stun
  (:use
   :common-lisp
   :ironclad
   :flexi-streams)
  (:export
   #:decompose-message-type
   #:compose-message-type))

(defpackage cl-stun.test
  (:use
   :common-lisp
   :cl-stun
   :fiveam))
