(in-package :cl-stun)

(defvar *turn-methods*
  '((#x003 . :allocate)                 ; only request/response
    (#x004 . :refresh)                  ; only request/response
    (#x006 . :send)                     ; only indication
    (#x007 . :data)                     ; only indication
    (#x008 . :create-permission)        ; only request/response
    (#x009 . :channel-bind)))           ; only request/response

(defvar *turn-attributes*
  '((#x000C . :channel-number)
    (#x000D . :lifetime)
    (#x0010 . :reserved)                ; (was BANDWIDTH)
    (#x0012 . :xor-peer-address)
    (#x0013 . :data)
    (#x0016 . :xor-relayed-address)
    (#x0017 . :requested-address-family)
    (#x0018 . :even-port)
    (#x0019 . :requested-transport)
    (#x001A . :dont-fragment)
    (#x0021 . :reserved)                ; (was TIMER-VAL)
    (#x0022 . :reservation-token)

    (#x8000 . :additional-address-family)
    (#x8001 . :address-error-code)
    (#x8004 . :icmp)))

(defvar *turn-is-loaded* nil)

(defun load-turn-methods ()
  (when (not *turn-is-loaded*)
    (write-line "loading turn methods and attributes...")
    (appendf *method-types* *turn-methods*)
    (appendf *attribute-types* *turn-attributes*)
    (setf *turn-is-loaded* t)))

;; this was lifted from attributes.lisp just for testing
(defmethod encode-attribute ((type (eql :xor-peer-address)) l-octets args)
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

(defmethod decode-attribute ((type (eql :xor-peer-address)) octets message offset)
  (decode-attribute :xor-mapped-address octets message offset))

(defun realm (message) (cadr (assoc :realm (stun-message-attributes message))))
(defun nonce (message) (cadr (assoc :nonce (stun-message-attributes message))))

(defun do-things ()
  (socket-send *socket* (encode-message (make-stun-message :method-type :allocate)) nil)
  (socket-receive *socket* *buffer* nil)
  (let ((message (decode-message *buffer*)))
    (write-line (realm message))
    (socket-send *socket*
                 (encode-message
                  (make-stun-message :method-type :allocate
                                     :attributes `((:nonce ,(nonce message))
                                                   (:username ,*username*)
                                                   (:requested-transport ,(bytes 17 0 0 0))
                                                   (:realm ,(realm message))
                                                   (:message-integrity ,*username* ,(realm message) ,*password*))))
                 nil)
    (socket-receive *socket* *buffer* nil))
  (decode-message *buffer*))

(defmethod decode-attribute ((type (eql :xor-relayed-address)) octets message offset)
  (decode-attribute :xor-mapped-address octets message offset))

