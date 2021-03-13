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
;;0x2112A442

