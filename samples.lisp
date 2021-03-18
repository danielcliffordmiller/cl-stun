
(in-package :cl)

(defpackage cl-stun.samples
  (:use :cl :cl-stun)
  (:export
   #:*error*
   #:*res-mapped*
   #:*res-xor-mapped*
   #:*callwithus-res*
   #:*counterpath-res*
   #:*dcalling-res*))

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

(defvar *callwithus-res*
  (bytes
   #x01 #x01 #x00 #x44    #x21 #x12 #xA4 #x42 
   #xE2 #xEF #x80 #x22    #x68 #xB6 #xE3 #xA7 
   #x91 #x68 #x59 #x42    #x00 #x01 #x00 #x08 
   #x00 #x01 #xF0 #x9B    #xAE #x14 #x93 #xC1 
   #x00 #x04 #x00 #x08    #x00 #x01 #x0D #x96 
   #xC0 #x5F #x11 #x3E    #x00 #x05 #x00 #x08 
   #x00 #x01 #x0D #x97    #x9E #x45 #x1B #xA6 
   #x80 #x20 #x00 #x08    #x00 #x01 #xD1 #x89 
   #x8F #x06 #x37 #x83    #x80 #x22 #x00 #x10 
   #x56 #x6F #x76 #x69    #x64 #x61 #x2E #x6F 
   #x72 #x67 #x20 #x30    #x2E #x39 #x36 #x00))

(defvar *counterpath-res*
  (bytes
   #x01 #x01 #x00 #x48    #x21 #x12 #xA4 #x42 
   #xAD #xEE #x49 #xDE    #xDB #x50 #x56 #xEB 
   #x3D #x78 #x05 #xF3    #x00 #x01 #x00 #x08 
   #x00 #x01 #xE3 #xA4    #xAE #x14 #x93 #xC1 
   #x00 #x04 #x00 #x08    #x00 #x01 #x0D #x96 
   #xD8 #x5D #xF6 #x12    #x00 #x05 #x00 #x08 
   #x00 #x01 #x0D #x97    #xD8 #x5D #xF6 #x0F 
   #x80 #x20 #x00 #x08    #x00 #x01 #xC2 #xB6 
   #x8F #x06 #x37 #x83    #x80 #x22 #x00 #x14 
   #x56 #x6F #x76 #x69    #x64 #x61 #x2E #x6F 
   #x72 #x67 #x20 #x30    #x2E #x39 #x38 #x2D 
   #x43 #x50 #x43 #x00))

(defvar *dcalling-res*
  (bytes
   #x01 #x01 #x00 #x50    #x21 #x12 #xA4 #x42 
   #x21 #x5C #xC6 #x59    #xF2 #x55 #x0C #x1C 
   #xAF #xE0 #x84 #x9A    #x00 #x20 #x00 #x08 
   #x00 #x01 #xD6 #x7D    #x8F #x06 #x37 #x83 
   #x00 #x01 #x00 #x08    #x00 #x01 #xF7 #x6F 
   #xAE #x14 #x93 #xC1    #x80 #x2B #x00 #x08 
   #x00 #x01 #x0D #x96    #x2D #x0F #x66 #x1F 
   #x80 #x2C #x00 #x08    #x00 #x01 #x0D #x97 
   #x2D #x0F #x66 #x1F    #x80 #x22 #x00 #x1A 
   #x43 #x6F #x74 #x75    #x72 #x6E #x2D #x34 
   #x2E #x35 #x2E #x31    #x2E #x31 #x20 #x27 
   #x64 #x61 #x6E #x20    #x45 #x69 #x64 #x65 
   #x72 #x27 #x41 #x31))
