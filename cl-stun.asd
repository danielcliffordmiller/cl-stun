(in-package :asdf)
(defsystem "cl-stun"
  :description "library to implement the STUN protocol as defined in RFC8489"
  :author "Dan Miller <danielcliffordmiller@gmail.com>"
  :version "0.0.9"
  :depends-on (:usocket :alexandria :ironclad :flexi-streams)
  :serial t
  :components ((:file "packages")
	       (:file "utils")
	       (:file "constants")
	       (:file "cl-stun")
	       (:file "attributes")
	       (:file "samples"))
  :in-order-to ((test-op (test-op "cl-stun/test"))))

(defsystem "cl-stun/test"
  :description "tests for the cl-stun library"
  :author "Dan Miller <danielcliffordmiller@gmail.com>"
  :depends-on (:cl-stun
               :fiveam
               :flexi-streams)
  :components ((:module "t"
		:components ((:file "tests"))))
  :perform (test-op (o s)
                    (uiop:symbol-call :fiveam '#:run!
				      (uiop:find-symbol* '#:test-suite
							 :cl-stun.test))))
