(in-package :cl)

(defpackage cl-stun.test
  (:use
   :common-lisp
   :cl-stun
   :ironclad
   :fiveam))

(in-package :cl-stun.test)

(def-suite test-suite
  :description "this is the main test suite for cl-stun")

(in-suite test-suite)

(test sample-test
  "sample test to get us off the ground"
  (is (= 2 2)))

(test decompose-message-type
  "happy path for method-type decomposition"
  (is (equal '(:binding :request) (decompose-message-type #x0001))))

(test compose-message-type
  "happy path for message-type composition"
  (is (equal #x0001 (compose-message-type :binding :request))))

(test looks-like-stun-message
  "does it look like a stun message"
  (is-true (cl-stun::looks-like-stun-message
	    (cl-stun::stun-message-seq (make-stun-message)))))

(test ip-address-parsing
  "test that we can determine different sorts of ip addresses"
  (is (equalp
       (cl-stun::parse-ip-addr #(127 0 0 1)) '(#(127 0 0 1) :ip4)))
  (is (equalp
       (cl-stun::parse-ip-addr "192.168.1.136") '(#(192 168 1 136) :ip4)))
  (is (equalp
       (cl-stun::parse-ip-addr
	#(8 0 0 0 0 0 0 0 0 0 0 0 0 0 0 4))
       '(#(8 0 0 0 0 0 0 0 0 0 0 0 0 0 0 4) :ip6)))
  (is (equalp (cl-stun::parse-ip-addr "::a:208")
	      '(#(0 0 0 0 0 0 0 0 0 0 0 0 0 10 2 8) :ip6))))

(test mapped-address
  "test the attribute creation of a mapped attribute"
  (let* ((ip-address #(192 168 1 101))
	 (port 80)
	 (buf-res (cl-stun::attribute-seq
		   '() :mapped-address (list
					ip-address
					port))))
    ;; check length reported is 8 bytes
    (is (= (ub16ref/be buf-res 2) 8))
    ;; check address family is ip4
    (is (eql (cdr (assoc (elt buf-res 5) cl-stun::*address-families*))
	     :ip4))
    ;; port is port in
    (is (= (ub16ref/be buf-res 6) port))
    (is (equalp (subseq buf-res 8) ip-address))))
