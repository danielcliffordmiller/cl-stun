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
