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
