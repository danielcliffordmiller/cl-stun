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

(defun decompose-message-type (message-type)
  "decompose-message-type into method-type and class-type"
  (let ((class-value (field-extract message-type *class-type-mask*))
	(method-value (field-extract message-type *method-type-mask*)))
    (list
     (cdr (assoc method-value *method-types*))
     (cdr (assoc class-value *class-types*)))))

(defun compose-message-type (method-type class-type)
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
       (equalp (subseq byte-sequence 4 8) *magic-cookie*)
       ;; 3: The last two bits of the message length field should be 0x00
       (zerop (ldb (byte 2 0) (elt byte-sequence 3)))
       ;; 4: (optionally) a FIGNERPRINT attribute
       ;; TODO: check the fingerprint attribute
       ))

(defun stun-message-seq (stun-message)
  "turn a stun-message into a sequence of bytes"
  (declare (type stun-message stun-message))
  (let ((header (make-array '(20) :element-type '(unsigned-byte 8)))
	(message-type
	  (compose-message-type
	   (stun-message-method-type stun-message)
	   (stun-message-class-type stun-message))))
    ;; set message-type
    (setf (ub16ref/be header 0) message-type)
    ;; length is calculated later
    ;; now magic cookie
    (setf (subseq header 4) *magic-cookie*)
    ;; set transaction-id
    (setf (subseq header 8) (stun-message-transaction-id stun-message))
    ;; TODO: write out the attributes
    header))
