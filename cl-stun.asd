(in-package :asdf)
(defsystem "cl-stun"
  :description "library to implement the STUN protocol as defined in RFC8489"
  :author "Dan Miller <danielcliffordmiller@gmail.com>"
  :depends-on (:usocket :alexandria :ironclad)
  :serial t
  :components ((:file "packages")
	       (:file "cl-stun"))
  :in-order-to ((test-op (test-op "cl-stun/test"))))

(defsystem "cl-stun/test"
  :description "tests for the cl-stun library"
  :author "Dan Miller <danielcliffordmiller@gmail.com>"
  :depends-on (:cl-stun :fiveam)
  :components ((:module "t"
			:components ((:file "tests")))))
