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

(defvar *address-families*
  '((#x01 . :ip4)
    (#x02 . :ip6)))

(defstruct stun-attribute
  "structure for a stun message attribute")

(defun tlv-type (buffer)
  (ub16ref/be buffer +tlv-type-offset+))

(defun (setf tlv-type) (value buffer)
  (setf (ub16ref/be buffer +tlv-type-offset+) value))

(defun tlv-length (buffer)
  (ub16ref/be buffer +tlv-length-offset+))

(defun (setf tlv-length) (value buffer)
  (setf (ub16ref/be buffer +tlv-length-offset+) value))

(defmacro with-tlv-buffer ((buffer-name attribute-type length) &body body)
  "Macro to help create encoded attributes.

`length` should be the length of the value part of the attribute
(in other words, not including the length of the type and length fields)"
  (with-gensyms (attribute-code)
   `(let ((,buffer-name
	    (make-array (list (next-word-boundary
			       (+ ,length +tlv-header-size+)))
			:element-type '(unsigned-byte 8)))
	  (,attribute-code
	    (car (rassoc ,attribute-type *attribute-types*))))
      (setf (tlv-type ,buffer-name) ,attribute-code)
      (setf (tlv-length ,buffer-name) ,length)
      ,@body
      ,buffer-name)))

(defgeneric encode-attribute (octets type args)
  (:documentation "This is a mechanism by which the different attributes are turned to sequences of octets"))

(defmethod encode-attribute (octets (type (eql :mapped-address)) args)
  "render mapped address attribute"
  (declare (ignore octets))
  (assert (eql type :mapped-address))
  (destructuring-bind (ip-addr port) args
    (destructuring-bind (addr family) (parse-ip-addr ip-addr)
      (let ((buffer-size (case family (:ip4 8) (:ip6 20))))
	(with-tlv-buffer (buffer type buffer-size)
	  (setf (elt buffer 5) (car (rassoc family *address-families*)))
	  (setf (ub16ref/be buffer 6) port)
	  (setf (subseq buffer 8) addr))))))

(defgeneric decode-attribute (type octets message offset)
  (:documentation "Generic for decoding the attributes. Should be differentiated based on the type. Octets is the exact value of the attribute to decode, message is the full message octet buffer and offset is the place at which this attribute was found in the message buffer."))

(defmethod decode-attribute
    ((type (eql :mapped-address)) octets message offset)
  "decode mapped-address"
  (declare (ignore message offset))
  (list
   (subseq octets 4)
   (ub16ref/be octets 2)))

(defmethod decode-attribute
    ((type (eql :error-code)) octets message offset)
  "decode error-code"
  (declare (ignore message offset))
  (list (+ (* (elt octets 2) 100)
	   (elt octets 3))
	(octets-to-string (subseq octets 4)
			  :external-format :utf8)))

(defmethod decode-attribute
    ((type (eql :unknown-attributes)) octets message offset)
  "decode unknown-attributes"
  (declare (ignore message offset))
  (loop :for i :below (length octets):by 2
	:collect (ub16ref/be octets i)))

(defmethod decode-attribute
    ((type (eql :software)) octets message offset)
  "decode software attribute"
  (declare (ignore message offset))
  (octets-to-string octets :external-format :utf8))

(defmethod decode-attribute
    ((type (eql :xor-mapped-address)) octets message offset)
  "decode xor-mapped address attribute"
  (list
   (loop :with dest = (subseq octets 4)
	 :for i :below (length dest)
	 :do (setf
	      (elt dest i)
	      (logxor
	       (elt dest i)
	       (elt message (+ *magic-cookie-offset* i))))
	 :finally (return dest))
   (logxor (ub16ref/be message *magic-cookie-offset*)
	   (ub16ref/be octets 2))))

(defun process-tlv (octets message offset)
  ;; takes in a buffer that consists of a tlv and will
  ;; look up the attribute and try to decode it
  (let ((attr (cdr (assoc (tlv-type octets)
			  *attribute-types*))))
    (if attr
	(cons attr
	      (decode-attribute
	       attr
	       (subseq octets
		       +tlv-header-size+
		       (+ +tlv-header-size+
			  (tlv-length octets)))
	       message
	       offset))
	;; else attribute is unknown ; this case needs to be handled
	;; (may be handle via the decode-attribute multi-method)
	)))
