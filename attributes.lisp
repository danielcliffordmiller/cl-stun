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

(defvar *password-algorithms*
  '((#x00 . :reserved)
    (#x01 . :md5)
    (#x02 . :sha256)))

(defvar *default-software-attribute*
  (concatenate 'string
               "cl-stun "
               (asdf:component-version
                (asdf:find-system :cl-stun))))

(defun tlv-type (buffer &optional (offset 0))
  (ub16ref/be buffer (+ offset +tlv-type-offset+)))

(defun (setf tlv-type) (value buffer &optional (offset 0))
  (setf (ub16ref/be buffer (+ offset +tlv-type-offset+)) value))

(defun tlv-length (buffer &optional (offset 0))
  (ub16ref/be buffer (+ offset +tlv-length-offset+)))

(defun (setf tlv-length) (value buffer &optional (offset 0))
  (setf (ub16ref/be buffer (+ offset +tlv-length-offset+)) value))

(defun tlv-value (buffer &optional (offset 0))
  (subseq buffer
          (+ offset +tlv-header-size+)
          (+ offset +tlv-header-size+ (tlv-length buffer offset))))

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
      (setf (tlv-type ,buffer-name) (or ,attribute-code ,attribute-type))
      (setf (tlv-length ,buffer-name) ,length)
      ,@body
      ,buffer-name)))

(defmacro set-message-length-from-octets (buffer octets size)
  "helper macro for those encoders that set the message length for some calculations"
  `(setf (message-length ,buffer)
         (- (+ (reduce #'+ ,octets :key #'length :initial-value 0)
               ,size
               +tlv-header-size+)
            +message-header-size+)))

(defgeneric encode-attribute (type l-octets args)
  (:documentation "This is a mechanism by which the different attributes are turned to sequences of octets"))

(defmethod encode-attribute (type l-octets args)
  (let ((data (car args))
        (accepted-type '(simple-array (unsigned-byte 8))))
    (if (not (typep data accepted-type))
        (error 'type-error :datum data :expected-type accepted-type))
    (with-tlv-buffer (buffer type (length data))
      (setf (subseq buffer +tlv-header-size+)
            data))))

(defmethod encode-attribute ((type (eql :mapped-address)) l-octets args)
  "render mapped address attribute"
  (declare (ignore l-octets))
  (assert (eql type :mapped-address))
  (destructuring-bind (ip-addr port) args
    (destructuring-bind (addr family) (parse-ip-addr ip-addr)
      (with-tlv-buffer (buffer type (+ +tlv-header-size+
				       (length addr)))
	(setf (elt buffer 5) (car (rassoc family *address-families*)))
	(setf (ub16ref/be buffer 6) port)
	(setf (subseq buffer 8) addr)))))

(defmethod encode-attribute ((type (eql :xor-mapped-address)) l-octets args)
  "render mapped address attribute"
  (destructuring-bind (ip-addr port) args
    (destructuring-bind (addr family) (parse-ip-addr ip-addr)
      (with-tlv-buffer (buffer type (+ +tlv-header-size+
				       (length addr)))
	(setf (elt buffer 5) (car (rassoc family *address-families*)))
	(setf (ub16ref/be buffer 6)
	      (logxor (ub16ref/be *magic-cookie* 0)
		      port))
	(dotimes (i (length addr))
	  (setf (elt buffer (+ +tlv-header-size+ 4 i))
		(logxor (elt addr i)
			(elt (car l-octets)
			     (+ +magic-cookie-offset+ i)))))))))

(defmethod encode-attribute ((type (eql :software)) l-octets args)
  "render software attribute"
  (declare (ignore l-octets))
  (let ((data (string-to-octets (or (car args) *default-software-attribute*))))
    (with-tlv-buffer (buffer type (length data))
      (setf (subseq buffer +tlv-header-size+) data))))

(defmethod encode-attribute ((type (eql :fingerprint)) l-octets args)
  (declare (ignore args)) ;; not sure this is the best way to handle this
  (set-message-length-from-octets (car l-octets) l-octets 4)
  (with-tlv-buffer (buffer type 4)
    (let ((digest (make-digest :crc32)))
      (dolist (seq l-octets)
	(update-digest digest seq))
      (setf (ub32ref/be buffer +tlv-header-size+)
	    (logxor *fingerprint-xor*
		    (octets-to-integer (produce-digest digest)))))))

(defmethod encode-attribute ((type (eql :username)) l-octets args)
  (declare (ignore l-octets))
  (let ((value (string-to-octets (opaque-string (car args)))))
    ;; TODO: check length of username
    (with-tlv-buffer (buffer type (length value))
      (setf (subseq buffer +tlv-header-size+) value))))

;; TODO: add support for algorithm parameters
(defmethod encode-attribute ((type (eql :password-algorithm)) l-octets args)
  (declare (ignore l-octets))
  (let ((pa-code (car (rassoc (car args) *password-algorithms*))))
    (with-tlv-buffer (buffer :password-algorithm 4)
      (setf (ub16ref/be buffer +tlv-header-size+) pa-code))))

(defmethod encode-attribute ((type (eql :realm)) l-octets args)
  (declare (ignore l-octets))
  (let ((value (string-to-octets (car args))))
    (with-tlv-buffer (buffer type (length value))
      (setf (subseq buffer +tlv-header-size+) value))))

;; TODO: add support for algorithm parameters
(defmethod encode-attribute ((type (eql :password-algorithms)) l-octets args)
  (declare (ignore l-octets))
  (let* ((pa-codes (mapcar #'(lambda (pa)
			       (car (rassoc pa *password-algorithms*)))
			   args))
	 (num-algorithms (length pa-codes)))
    (with-tlv-buffer (buffer :password-algorithms (* 4 num-algorithms))
      (loop :for i :below num-algorithms
	    :do (setf (ub16ref/be buffer (+ (* i 4)
					    +tlv-header-size+))
		      (elt pa-codes i))))))

(defmethod encode-attribute ((type (eql :message-integrity)) l-octets args)
  (set-message-length-from-octets (car l-octets) l-octets 20)
  (with-tlv-buffer (buffer type 20)
    (let* ((key (cond
                  ((typep (car args) '(simple-array (unsigned-byte 8)))
                   (car args))
                  ((= 1 (length args)) (string-to-octets (opaque-string (car args))))
                  ((= 3 (length args))
                   (digest-sequence
                    :md5
                    (string-to-octets
                     (format nil "~@{~a~^:~}"
                             (first args)
                             (opaque-string (second args))
                             (opaque-string (third args))))))))
           (hmac (make-hmac key :sha1)))
      (dolist (seq l-octets)
        (update-hmac hmac seq))
      (let ((digest (hmac-digest hmac)))
        (setf (subseq buffer +tlv-header-size+) digest)))))

(defgeneric decode-attribute (type octets message offset)
  (:documentation "Generic for decoding the attributes. Should be differentiated based on the type. Octets is the exact value of the attribute to decode, message is the full message octet buffer and offset is the place at which this attribute was found in the message buffer."))

(defmethod decode-attribute (type octets message offset)
  "catch all decoder"
  octets)

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
  (loop :for i :below (length octets) :by 2
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

(defmethod decode-attribute
    ((type (eql :alternate-domain)) octets message offset)
  "decode alternate-domain"
  ;; TODO: check that dns name is less than 255 chars
  (declare (ignore message offset))
  (octets-to-string octets :external-format :us-ascii))

(defmethod decode-attribute
    ((type (eql :fingerprint)) octets message offset)
  "decode fingerprint"
  (= (logxor
      *fingerprint-xor*
      (octets-to-integer (digest-sequence :crc32 (subseq message 0 offset))))
     (octets-to-integer octets)))

(defun scan-for-attributes (buffer)
  "given a message buffer, returns an a-list for the offsets and attribute type codes"
  (labels
      ((scan-helper (offset acc)
	 (if (= offset
		(+ (message-length buffer)
		   +message-header-size+))
	     (nreverse acc)
	     (scan-helper
	      (next-word-boundary
               (+ offset
		  +tlv-header-size+
		  (tlv-length buffer offset)))
	      (cons
	       (list (lookup-attribute-type (tlv-type buffer offset))
		     offset)
	       acc)))))
    (scan-helper +message-header-size+ nil)))

(defun lookup-attribute-type (code)
  "lookup attribute and return the keyword if recognized, else return the code unchanged"
  (let ((assoc (assoc code *attribute-types*)))
    (if assoc
        (if (eql (cdr assoc) :reserved)
            code
            (cdr assoc))
        code)))

(defun opaque-string (string)
  "implements the unicode opaque string profile"
  ;; TODO: have an actual implementation
  string)

(declaim (inline requiredp))
(defun requiredp (attribute-type-code)
  (zerop (logandc1 #x7FFF attribute-type-code)))
