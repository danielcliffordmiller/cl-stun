(in-package :cl-stun)

(defvar *magic-cookie* (bytes #x21 #x12 #xA4 #x42))

;;; these masks are defined as a list of byte-specs concerning
;;; the bits of interest for unpacking
(defvar *class-type-mask*
  (list (byte 1 8)
	(byte 1 4)))
(defvar *method-type-mask*
  (list (byte 5 9)
	(byte 3 5)
	(byte 4 0)))

(defvar *class-types*
  '((#b00 . :request)
    (#b01 . :indication)
    (#b10 . :success-response)
    (#b11 . :error)))

(defvar *method-types*
  '((#b000000000001 . :binding)))

(defstruct stun-message
  "structure for a stun message"
  (transaction-id
   (generate-transaction-id)
   :type (vector (unsigned-byte 8) 12)
   :read-only t)
  (method-type
   :binding
   :type keyword
   :read-only t)
  (class-type
   :request
   :type keyword
   :read-only t)
  (attributes
   '()
   :type list
   :read-only t))

(defun field-extract (packed mask)
  "taking in an interger and a mask, return the value with our mask"
  (loop	:with acc = 0 :and position = 0
	:for byte-spec
	  :across (sort (concatenate 'vector mask) #'< :key #'byte-position)
	:do (setf (ldb (byte (byte-size byte-spec) position) acc)
		  (ldb byte-spec packed))
	:do (incf position (byte-size byte-spec))
	:finally (return acc)))

(defun field-inject (new-value mask dest)
  "taking in a value and returning the packed value based on a mask"
  (loop :with acc = dest :and position = 0
	:for byte-spec
	  :across (sort (concatenate 'vector mask) #'< :key #'byte-position)
	:do (setf (ldb byte-spec acc)
		  (ldb (byte (byte-size byte-spec) position) new-value))
	:do (incf position (byte-size byte-spec))
	:finally (return acc)))

(defun decode-message-type (message-type)
  "decode message-type into method-type and class-type"
  (let ((class-value (field-extract message-type *class-type-mask*))
	(method-value (field-extract message-type *method-type-mask*)))
    (list
     (cdr (assoc method-value *method-types*))
     (cdr (assoc class-value *class-types*)))))

(defun encode-message-type (method-type class-type)
  (let ((class-value (car (rassoc class-type *class-types*)))
	(method-value (car (rassoc method-type *method-types*))))
    (field-inject method-value *method-type-mask*
		  (field-inject class-value *class-type-mask* 0))))

(defun generate-transaction-id ()
  "return a cryptographically secure sequence of 96 bits (12 bytes)"
  (random-data 12))

(defun looks-like-stun-message (byte-sequence)
  "function to tell if a packet looks like a stun message"
  ;; there are four ways to tell, according to the spec:
  ;; 1: The first two bits of the message should be 0b00
  (and (zerop (ldb (byte 2 6) (elt byte-sequence 0)))
       ;; 2: Bytes 5-8 should comprise the magic cookie.
       (equalp (magic-cookie byte-sequence) *magic-cookie*)
       ;; 3: The last two bits of the message length field should be 0x00
       (zerop (ldb (byte 2 0) (elt byte-sequence 3)))
       ;; 4: (optionally) a FIGNERPRINT attribute
       (let ((last-attr (lastcar (scan-for-attributes byte-sequence))))
	 (if (eql (car last-attr) :fingerprint)
	     (decode-attribute
	      :fingerprint
	      (subseq byte-sequence
		      (+ (second last-attr) +tlv-header-size+)
		      (third last-attr))
	      byte-sequence
	      (second last-attr))
	     t))))

(defun encode-message (stun-message)
  "turn a stun-message into a sequence of bytes"
  (declare (type stun-message stun-message))
  (let ((header (make-array (list +message-header-size+)
			    :element-type '(unsigned-byte 8)))
	(message-type
	  (encode-message-type
	   (stun-message-method-type stun-message)
	   (stun-message-class-type stun-message))))
    ;; set message-type
    (setf (message-type header) message-type)
    ;; length is calculated later
    ;; now magic cookie
    (setf (magic-cookie header) *magic-cookie*)
    ;; set transaction-id
    (setf (transaction-id header)
	  (stun-message-transaction-id stun-message))

    ;; now the attributes
    (loop :with buffers = (list header)
	  :for attribute :in (stun-message-attributes stun-message)
	  :do (appendf buffers
		       (list
                        (apply #'encode-attribute
                               (if (listp attribute)
                                   (list (car attribute)
					  buffers
					  (cdr attribute))
                                   (list attribute
                                         buffers
                                         nil)))))
	  :finally
	     (return
	       (let* ((result (join-sequences buffers)))
		 (setf (message-length result)
		       (- (length result)
			  +message-header-size+))
		 result)))))

(defun join-sequences (seqs)
  "utility to join sequences"
  (let* ((size (reduce #'+ seqs :key #'length :initial-value 0))
	 (buffer (make-array (list size) :element-type '(unsigned-byte 8))))
    (loop :for seq :in seqs
	  :and offset = 0 :then (+ offset (length seq))
	  :do (setf (subseq buffer offset) seq))
    buffer))

(defun decode-message (buffer)
  (destructuring-bind (method-type class-type)
      (decode-message-type (message-type buffer))
    (make-stun-message
     :transaction-id (transaction-id buffer)
     :method-type method-type
     :class-type class-type
     :attributes
     (mapcar
      #'(lambda (tlv-data)
          (cons
           (car tlv-data)
	   (ensure-list (decode-attribute
                         (car tlv-data)
                         (tlv-value buffer (second tlv-data))
	                 buffer
	                 (second tlv-data)))))
      (scan-for-attributes buffer)))))

;; buffer utils

(defun message-type (buffer)
  (declare (type (vector (unsigned-byte 8)) buffer))
  (ub16ref/be buffer +message-type-offset+))

(defun (setf message-type) (value buffer)
  (declare (type (vector (unsigned-byte 8)) buffer)
	   (type (unsigned-byte 14) value))
  (setf (ub16ref/be buffer +message-type-offset+) value))

(defun message-length (buffer)
  (declare (type (simple-array (unsigned-byte 8)) buffer))
  (ub16ref/be buffer +message-length-offset+))

(defun (setf message-length) (value buffer)
  (declare (type (vector (unsigned-byte 8)) buffer)
	   (type (unsigned-byte 16) value))
  (setf (ub16ref/be buffer +message-length-offset+) value))

(defun magic-cookie (buffer)
  (declare (type (vector (unsigned-byte 8)) buffer))
  (subseq buffer
	  +magic-cookie-offset+
	  +magic-cookie-end+))

(defun (setf magic-cookie) (value buffer)
  (declare (type (vector (unsigned-byte 8)) buffer)
	   (type vector value))
  (setf (subseq buffer
		+magic-cookie-offset+
		+magic-cookie-end+) value))

(defun transaction-id (buffer)
  (declare (type (vector (unsigned-byte 8)) buffer))
  (subseq buffer
	  +transaction-id-offset+
	  +transaction-id-end+))

(defun (setf transaction-id) (value buffer)
  (declare (type (vector (unsigned-byte 8)) buffer)
	   (type (vector (unsigned-byte 8)) value))
  (setf (subseq buffer
		+transaction-id-offset+
		+transaction-id-end+) value))

(defun stun (&optional (attributes nil) (method-type :binding) (class-type :request))
  (let ((message (make-stun-message :method-type method-type
				    :class-type class-type
				    :attributes attributes)))
    (values
     (encode-message message)
     (stun-message-transaction-id message))))

(defun stun-send (socket &rest rest)
  (multiple-value-bind (message transaction-id) (apply #'stun rest)
    (values
     (socket-send socket message nil)
     transaction-id)))

(let ((stun-buffer (make-array '(578) :element-type '(unsigned-byte 8))))
  (defun stun-receive (socket)
    (socket-receive socket stun-buffer nil)
    (decode-message stun-buffer)))

(defun get-mapped-address (stun-message)
  (let ((attributes (stun-message-attributes stun-message)))
    (if-let (xor-mapped (assoc :xor-mapped-address attributes))
      (cdr xor-mapped)
      (cdr (assoc :mapped-address attributes)))))

;;;; just for fun!

(defvar *buffer*
  (make-array '(576)
	      :element-type '(unsigned-byte 8)))

(defun bind-request (host &optional (port 3478))
  (let ((s (socket-connect host port :protocol :datagram)))
    (socket-send s (encode-message (make-stun-message
					 :attributes '((:fingerprint t))
					 )) nil)
    (socket-receive s *buffer* nil)
    (socket-close s)
    (decode-message *buffer*)))

(defun print-buffer (&optional (buffer *buffer*))
  (loop :for i :below (+ +message-header-size+
			 (message-length buffer)) :by 4
	:do (format t "~{#x~2,'0X ~}~%"
		    (concatenate 'list (subseq buffer i (+ i 4)))))
  buffer)

(defun run-simple-server (&optional (port 3478))
  (let ((socket (socket-connect nil nil :protocol :datagram :local-port port)))
    (unwind-protect
     (loop
       (multiple-value-bind (buffer length request-addr request-port)
           (socket-receive socket *buffer* nil)
         (declare (ignore buffer length))
         (let ((message (decode-message *buffer*)))
           (socket-send socket
                        (encode-message
                         (make-stun-message :transaction-id (stun-message-transaction-id message)
                                            :class-type :success-response
                                            :attributes `((:xor-mapped-address ,request-addr ,request-port))))
                        nil
                        :host request-addr
                        :port request-port))))
      (socket-close socket))))
