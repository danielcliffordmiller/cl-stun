(in-package :cl-stun)

;; here are found those functions which are useful, but not
;; specifically related to logic of the stun protcol itself

(defun next-word-boundary (n)
  "takes a length and rounds up to the nearest multiple of four"
  (logandc2 (+ n #b11) #b11))

(defun parse-ip-addr (addr)
  "goal here take in vector or string representation of address return only vecters and type"
  (etypecase addr
    ((vector t 4) (list addr :ip4))
    ((vector t 16) (list addr :ip6))
    (string (if (find #\. addr)
		(list (dotted-quad-to-vector-quad addr) :ip4)
		(list (ipv6-host-to-vector addr) :ip6)))))

(defmacro bytes (&rest bytes)
  "takes bunch of numbers (bytes) and returns an octet sequence"
  (with-gensyms (m-bytes)
    `(let ((,m-bytes ',bytes))
       (make-array (list (length ,m-bytes))
		   :element-type '(unsigned-byte 8)
		   :initial-contents ,m-bytes))))
