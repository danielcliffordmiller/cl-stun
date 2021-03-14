(in-package :cl-stun)

(defvar *attribute-types*
  '((#x0000 . :reserved)
    (#x0001 . :mapped-address)
    (#x0002 . :reserved)     ; was RESPONSE-ADDRESS prior to [RFC5389]
    (#x0003 . :reserved)     ; was CHANGE-REQUEST prior to [RFC5389]
    (#x0004 . :reserved)     ; was SOURCE-ADDRESS prior to [RFC5389]
    (#x0005 . :reserved)     ; was CHANGED-ADDRESS prior to [RFC5389]
    (#x0006 . :username)
    (#x0007 . :reserved)	     ; was PASSWORD prior to [RFC5389]
    (#x0008 . :message-integrity)
    (#x0009 . :error-code)
    (#x000A . :unknown-attributes)
    (#x000B . :reserved)       ; was REFLECTED-FROM prior to [RFC5389]
    (#x0014 . :realm)
    (#x0015 . :nonce)
    (#x0020 . :xor-mapped-address)
    (#x8022 . :software)
    (#x8023 . :alternate-server)
    (#x8028 . :fingerprint)
    (#x001C . :message-integrity-sha256)
    (#x001D . :password-algorithm)
    (#x001E . :userhash)
    (#x8002 . :password-algorithms)
    (#x8003 . :alternate-domain)))

(defparameter *attribute-type-values*
  '((:mapped-address
     :family
     :port
     :address)
    (:xor-mapped-address
     :family
     :x-port
     :x-address)))

(defvar *address-families*
  '((#x01 . :ip4)
    (#x02 . :ip6)))

(defstruct stun-attribute
  "structure for a stun message attribute")

(defmacro with-tlv-buffer ((buffer-name attribute-type length) &body body)
  (with-gensyms (attribute-code)
   `(let ((,buffer-name
	    (make-array (list ,length) :element-type '(unsigned-byte 8)))
	  (,attribute-code
	    (car (rassoc ,attribute-type *attribute-types*))))
      (setf (ub16ref/be ,buffer-name 0) ,attribute-code)
      (setf (ub16ref/be ,buffer-name 2) (- (length ,buffer-name) 8))
      ,@body
      ,buffer-name)))

(defgeneric attribute-seq (octets type args)
  (:documentation "This is a mechanism by which the different attributes are turned to sequences of octets"))

(defmethod attribute-seq (octets (type (eql :mapped-address)) args)
  "render mapped address attribute"
  (declare (ignore octets))
  (assert (eql type :mapped-address))
  (destructuring-bind (ip-addr port) args
    (destructuring-bind (addr family) (parse-ip-addr ip-addr)
      (let ((buffer-size (case family (:ip4 12) (:ip6 24))))
	(with-tlv-buffer (buffer type buffer-size)
	  (setf (elt buffer 5) (car (rassoc family *address-families*)))
	  (setf (ub16ref/be buffer 6) port)
	  (setf (subseq buffer 8) addr))))))

(defun parse-ip-addr (addr)
  "goal here take in vector or string representation of address return only vecters and type"
  (etypecase addr
    ((vector t 4) (list addr :ip4))
    ((vector t 16) (list addr :ip6))
    (string (if (find #\. addr)
		(list (dotted-quad-to-vector-quad addr) :ip4)
		(list (ipv6-host-to-vector addr) :ip6)))))
