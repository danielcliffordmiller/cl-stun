
(in-package :cl)

(defpackage cl-stun.samples
  (:use :cl :cl-stun)
  (:export #:*error* #:*res-mapped* #:*res-xor-mapped*))

(in-package :cl-stun.samples)

;; this sample response was taken off the wire of a stun server
;; barfing on a classic stun request (the request used the
;; deprecated "CHANGE-REQUEST" attribute)
(defvar *error*
  (bytes
   #x01 #x11 #x00 #x38  #x01 #xFC #x89 #x31  #xD7 #x8B #x28 #x48
   #xD4 #xA9 #x39 #x28  #x64 #xCF #x42 #x4C  #x80 #x22 #x00 #x0F
   #x50 #x31 #x20 #x53  #x54 #x55 #x4E #x20  #x6C #x69 #x62 #x72
   #x61 #x72 #x79 #x00  #x00 #x09 #x00 #x15  #x00 #x00 #x04 #x14
   #x55 #x6E #x6B #x6E  #x6F #x77 #x6E #x20  #x41 #x74 #x74 #x72
   #x69 #x62 #x75 #x74  #x65 #x00 #x00 #x00  #x00 #x0A #x00 #x02
   #x00 #x03 #x00 #x00))

(defvar *res-mapped*
  (bytes
   #x01 #x01 #x00 #x20  #x00 #x00 #x00 #x00  #x4D #x19 #x84 #xC5
   #xFD #xE7 #x3D #x1D  #x8A #x71 #x33 #x7A  #x80 #x22 #x00 #x0F
   #x50 #x31 #x20 #x53  #x54 #x55 #x4E #x20  #x6C #x69 #x62 #x72
   #x61 #x72 #x79 #x00  #x00 #x01 #x00 #x08  #x00 #x01 #xCD #x92
   #xC0 #xA8 #x01 #x1A))

(defvar *res-xor-mapped*
  (bytes
   #x01 #x01 #x00 #x20  #x21 #x12 #xA4 #x42  #x5A #xA8 #xEE #xC5
   #xEA #x90 #x27 #x3e  #x56 #xE6 #xC5 #xAD  #x80 #x22 #x00 #x0F
   #x50 #x31 #x20 #x53  #x54 #x55 #x4E #x20  #x6C #x69 #x62 #x72
   #x61 #x72 #x79 #x00  #x00 #x20 #x00 #x08  #x00 #x01 #xFA #xDA
   #xE1 #xBA #xA5 #x58))
