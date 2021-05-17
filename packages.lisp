(in-package :common-lisp)

(defpackage cl-stun
  (:use
   :alexandria
   :common-lisp
   :ironclad
   :usocket
   :flexi-streams)
  (:export
   #:make-stun-message
   #:stun-message-attributes

   #:looks-like-stun-message

   #:stun
   #:stun-send
   #:stun-receive
   #:get-mapped-address

   #:encode-message
   #:decode-message
   #:encode-attribute
   #:decode-attribute

   #:bytes

   #:*default-software-attribute*

   #:tlv-type
   #:tlv-length
   #:tlv-value

   #:bind-request)
  (:shadow
   #:xor))
