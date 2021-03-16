(in-package :cl-stun)

(defvar *magic-cookie* #(#x21 #x12 #xA4 #x42))

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
       ;; TODO: check the fingerprint attribute
       ))

(defun encode-stun-message (stun-message)
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
    ;; TODO: write out the attributes
    header))

(defun decode-message (message)
  (destructuring-bind (method-type class-type)
      (decode-message-type (message-type message))
    (make-stun-message
     :transaction-id (transaction-id message)
     :method-type method-type
     :class-type class-type
     :attributes
     (loop :with message-length = (+ (message-length message)
                                     +message-header-size+)
	   :for offset = +message-header-size+ :then next
	   :for next = (+ (next-word-boundary
			   (ub16ref/be message (+ offset 2)))
			  +tlv-header-size+
			  offset)
	   :collect (process-tlv (subseq message offset next) message offset)
	   :until (>= next message-length)))))

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

;;;; just for fun!

(defvar *a*
  (make-array '(576)
	      :element-type '(unsigned-byte 8)))

(defun bind-request (host &optional (port 3478))
  (let ((s (socket-connect host port :protocol :datagram)))
    (socket-send s (encode-stun-message (make-stun-message)) nil)
    (socket-receive s *a* nil)
    (socket-close s)
    (decode-message *a*)))

(defun print-buffer ()
  (loop :for i :below (+ +message-header-size+
			 (message-length *a*)) :by 4
	:do (format t "~{#x~2,'0X ~}~%"
		    (concatenate 'list (subseq *a* i (+ i 4))))))
