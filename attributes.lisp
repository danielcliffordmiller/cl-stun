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

(defvar *fingerprint-xor* #x5354554E)

(defun tlv-type (buffer &optional (offset 0))
  (ub16ref/be buffer (+ offset +tlv-type-offset+)))

(defun (setf tlv-type) (value buffer &optional (offset 0))
  (setf (ub16ref/be buffer (+ offset +tlv-type-offset+)) value))

(defun tlv-length (buffer &optional (offset 0))
  (ub16ref/be buffer (+ offset +tlv-length-offset+)))

(defun (setf tlv-length) (value buffer &optional (offset 0))
  (setf (ub16ref/be buffer (+ offset +tlv-length-offset+)) value))

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

(defgeneric encode-attribute (type octets args)
  (:documentation "This is a mechanism by which the different attributes are turned to sequences of octets"))

(defmethod encode-attribute ((type (eql :mapped-address)) octets args)
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

(defmethod encode-attribute ((type (eql :software)) octets args)
  "render software attribute"
  (declare (ignore octets))
  (let ((data (string-to-octets args)))
    (with-tlv-buffer (buffer type (length data))
      (setf (subseq buffer +tlv-header-size+) data))))

(defmethod encode-attribute ((type (eql :fingerprint)) octets args)
  (declare (ignore args)) ;; not sure this is the best way to handle this
  (incf (message-length (car octets))
	(+ 4 ; CRC32 size
	   +tlv-header-size+))
  (with-tlv-buffer (buffer type 4)
    (let ((digest (make-digest :crc32)))
      (dolist (seq octets)
	(update-digest digest seq))
      (setf (ub32ref/be buffer +tlv-header-size+)
	    (logxor *fingerprint-xor*
		    (ub32ref/be (produce-digest digest) 0))))))

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

;; TODO: value should not be more than 763 bytes when decoding
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
	       (elt message (+ +magic-cookie-offset+ i))))
	 :finally (return dest))
   (logxor (ub16ref/be message +magic-cookie-offset+)
	   (ub16ref/be octets 2))))

;; TODO: value should not be more than 763 bytes when decoding
(defmethod decode-attribute
    ((type (eql :username)) octets message offset)
  "decode username attrbute"
  (declare (ignore message offset))
  (octets-to-string octets :external-format :utf8))

(defmethod decode-attribute
    ((type (eql :userhash)) octets message offset)
  "\"decode\" userhash"
  (declare (ignore message offset))
  octets)

;; TODO: value should not be more than 763 bytes when decoding
(defmethod decode-attribute
    ((type (eql :realm)) octets message offset)
  "decode realm attrbute"
  (declare (ignore message offset))
  (octets-to-string octets :external-format :utf8))

(defmethod decode-attribute
    ((type (eql :alternate-server)) octets message offset)
  "decode alternate-server"
  ;; same as mapped address
  (declare (ignore message offset))
  (list
   (subseq octets 4)
   (ub16ref/be octets 2)))

(defmethod decode-attrbute
    ((type (eql :alternate-domain)) octets message offset)
  "decode alternate-domain"
  ;; TODO: check that dns name is less than 255 chars
  (declare (ignore message offset))
  (octets-to-string octets :external-format :us-ascii))

(defun scan-for-attributes (buffer)
  "given a message buffer, returns an a-list for the offsets and attribute type codes"
  (labels
      ((scan-helper (offset acc)
	 (if (= offset
		(+ (message-length buffer)
		   +message-header-size+))
	     (nreverse acc)
	     (let ((tlv-end-offset
		     (+ offset
			+tlv-header-size+
			(tlv-length buffer offset))))
	       (scan-helper
		(next-word-boundary tlv-end-offset)
		(cons
		 (list (tlv-type buffer offset)
		       offset
		       tlv-end-offset)
		 acc))))))
    (scan-helper +message-header-size+ nil)))

(defun process-tlv (octets message offset)
  ;; takes in a buffer that consists of a tlv and will
  ;; look up the attribute and try to decode it
  (let ((attr (cdr (assoc (tlv-type octets)
			  *attribute-types*)))
	(value-octets (subseq octets
			      +tlv-header-size+
			      (+ +tlv-header-size+
				 (tlv-length octets)))))
    (if (and attr
	     (not (eql attr :reserved)))
	(cons attr
	      (decode-attribute attr value-octets message offset))
	(cons (tlv-type octets) value-octets)
	;; else attribute is unknown ; this case needs to be handled
	;; (may be handle via the decode-attribute multi-method)
	)))

(defun opaque-string (string)
  "implements the unicode opaque string profile"
  ;; TODO: have an actual implementation
  string)

(declaim (inline requiredp))
(defun requiredp (attribute-type-code)
  (zerop (logandc1 #x7FFF attribute-type-code)))
