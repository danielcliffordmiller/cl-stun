(in-package :cl)

(defpackage cl-stun.test
  (:use
   :common-lisp
   :cl-stun
   :cl-stun.samples
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
       '(#(127 0 0 1) :ip4) (cl-stun::parse-ip-addr #(127 0 0 1))))
  (is (equalp
       '(#(192 168 1 136) :ip4) (cl-stun::parse-ip-addr "192.168.1.136")))
  (is (equalp
       '(#(8 0 0 0 0 0 0 0 0 0 0 0 0 0 0 4) :ip6)
       (cl-stun::parse-ip-addr
	#(8 0 0 0 0 0 0 0 0 0 0 0 0 0 0 4))))
  (is (equalp '(#(0 0 0 0 0 0 0 0 0 0 0 0 0 10 2 8) :ip6)
	      (cl-stun::parse-ip-addr "::a:208"))))

(test mapped-address
  "test the attribute creation of a mapped attribute"
  (let* ((ip-address #(192 168 1 101))
	 (port 80)
	 (buf-res (cl-stun::encode-attribute
		   '() :mapped-address (list
					ip-address
					port))))
    ;; check length reported is 8 bytes
    (is (= 8 (ub16ref/be buf-res 2)))
    ;; check address family is ip4
    (is (eql :ip4
	     (cdr (assoc (elt buf-res 5) cl-stun::*address-families*))))
    ;; port is port in
    (is (= port (ub16ref/be buf-res 6)))
    (is (equalp ip-address (subseq buf-res 8)))))

(test next-word-boundary
  "tests to round up lengths on 32-bit boundaries"
  (is (= #b0100 (cl-stun::next-word-boundary #b0100)))
  (is (= #b1000 (cl-stun::next-word-boundary #b0101)))
  (is (= #b1000 (cl-stun::next-word-boundary #b0110)))
  (is (= #b1000 (cl-stun::next-word-boundary #b0111)))
  (is (= #b1000 (cl-stun::next-word-boundary #b1000)))
  (is (= #b1100 (cl-stun::next-word-boundary #b1001))))

(test decode-mapped-address-attribute
  "tests the decoding of a mapped address attribute"
  (let* ((in-data (bytes 0 1 0 80 192 168 1 102))
	 (out-data (cl-stun::decode-attribute
		    :mapped-address in-data nil nil)))
    (is (equalp #(192 168 1 102) (first out-data)))
    (is (= 80 (second out-data)))))

(test decode-error-code-attribute
  "tests the decoding of an error-code attribute"
  (let* ((in-data (bytes #x00 #x00 #x04 #x14
			 #x55 #x6E #x6B #x6E
			 #x6F #x77 #x6E #x20
			 #x41 #x74 #x74 #x72
			 #x69 #x62 #x75 #x74
			 #x65))
	 (out-data (cl-stun::decode-attribute
		    :error-code in-data nil nil)))
    (is (= 420 (first out-data)))
    (is (string=
	 (second out-data)
	 "Unknown Attribute"))))

(test decode-unknown-attributes-attribute
  "read the function title, gosh"
  (let* ((in-data (bytes #x00 #x03))
	 (out-data (cl-stun::decode-attribute
		    :unknown-attributes in-data nil nil)))
    (is (equalp '(3) out-data))))

(test decode-software-attribute
  (let ((test-string "foobar"))
    (is (string= test-string (cl-stun::decode-attribute
			      :software (flexi-streams:string-to-octets
					 test-string)
			      nil nil)))))

;; exampled pulled from the samples package
(test decode-xor-mapped-address-attribute
  (let* ((in-data (bytes #x00 #x01 #xFA #xDA
			 #xE1 #xBA #xA5 #x58))
	 (out-data (cl-stun::decode-attribute
		    :xor-mapped-address
		    in-data
		    *res-xor-mapped*
		    nil)))
    (is (= 56264 (first out-data)))
    (is (equalp #(192 168 1 26) (second out-data)))))
