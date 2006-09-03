;;; ---------------------------------------------------------------------------- ;;;
;;;  LISCPL - Lisp Player Client - proxy--std.lisp                               ;;;
;;;  Copyright (C) 2006, Armin Mueller                                           ;;;
;;;                                                                              ;;;
;;;  This program is free software; you can redistribute it and/or modify        ;;;
;;;  it under the terms of the GNU General Public License as published by        ;;;
;;;  the Free Software Foundation; either version 2 of the License, or           ;;;
;;;  (at your option) any later version.                                         ;;;
;;;                                                                              ;;;
;;;  This program is distributed in the hope that it will be useful,             ;;;
;;;  but WITHOUT ANY WARRANTY; without even the implied warranty of              ;;;
;;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the               ;;;
;;;  GNU General Public License for more details.                                ;;;
;;;                                                                              ;;;
;;;  You should have received a copy of the GNU General Public License           ;;;
;;;  along with this program; if not, write to the Free Software                 ;;;
;;;  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA   ;;;
;;; ---------------------------------------------------------------------------- ;;;

;;;
;;; http://cybertiggyr.com/gene/lizard/xdr.lisp
;;; Copyright (c) 2003, 2005 Gene Michael Stover.  All rights reserved.
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU Lesser General Public License as
;;; published by the Free Software Foundation; either version 2.1 of the
;;; License, or (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU Lesser General Public
;;; License along with this program; if not, write to the Free Software
;;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
;;; USA
;;;

(in-package "LISPCL")

;; *****************************************************************************
;; ** xdr stream                                                              **
;; *****************************************************************************

; xdr stream
(defstruct xdr-stream
  is-good                               ; stream status, once bad, always bad
  read-octet-fn                         ; return next octet or self
  write-octet-fn                        ; write an octet or return self
  sync-fn)                              ; flush input & output buffers

; make wrapper xdr stream
(defun make-wrapper-xdr-stream (binary-stream)
  "Given a Common Lisp stream that can read & write octets,
   return a new XDR stream which reads & writes that Common Lisp stream."
  (assert (input-stream-p binary-stream))
  (assert (output-stream-p binary-stream))
  (assert (equal (stream-element-type binary-stream) '(unsigned-byte 8)))
  (let ( (xdrs (make-xdr-stream)) )
    (setf (xdr-stream-read-octet-fn xdrs) #'(lambda () (read-byte binary-stream nil xdrs)))
    (setf (xdr-stream-write-octet-fn xdrs)
          #'(lambda (octet)
              (declare (type (unsigned-byte 8) octet))
              (write-byte octet binary-stream)))
    (setf (xdr-stream-sync-fn xdrs) #'(lambda () (force-output binary-stream) nil))
    (setf (xdr-stream-is-good xdrs) T)
    xdrs))

;; *****************************************************************************
;; ** basics                                                                  **
;; *****************************************************************************

; xdr read octect
(defun xdr-read-octet (xdrs)
  "Return next octet or XDRS on error"
  (declare (type xdr-stream xdrs))
  (cond ( (not (xdr-stream-is-good xdrs))
          xdrs ) ; prior error
        ( (null (xdr-stream-read-octet-fn xdrs))
          ;; No reader function.  Mark an error.
          (setf (xdr-stream-is-good xdrs) nil)
          xdrs )
        ( T
          (let ( (octet (funcall (xdr-stream-read-octet-fn xdrs))) )
            (when (eq octet xdrs)
              ;; There must have been some error, so ensure that the XDR stream's state ain't good.
              (setf (xdr-stream-is-good xdrs) nil))
              octet))))

; xdr write octet
(defun xdr-write-octet (octet xdrs)
  "Write octet. Return XDRS on error."
  (declare (type integer octet) (type xdr-stream xdrs))
  (cond ( (not (xdr-stream-is-good xdrs))
          xdrs ) ; prior error
        ( (null (xdr-stream-write-octet-fn xdrs))
          ;; No writer function.  Mark an error.
          (setf (xdr-stream-is-good xdrs) nil)
          xdrs )
        ( (eq (funcall (xdr-stream-write-octet-fn xdrs) octet) xdrs)
          ;; Tried to write, encountered error.
          (setf (xdr-stream-is-good xdrs) nil)
          xdrs )
        ( T
          octet )))                     ; success

; xdr read word
(defun xdr-read-word (xdrs &key (type 'vector))
  "Return vector or list of the next 4 octets or nil. The keyword argument
   TYPE may be VECTOR or LIST."
  (declare (type xdr-stream xdrs))
  (let ( (lst (list (xdr-read-octet xdrs) (xdr-read-octet xdrs)
                    (xdr-read-octet xdrs) (xdr-read-octet xdrs))) )
    (cond ( (member xdrs lst)
            xdrs)      ; error occurred somewhere
          ( (eq type 'vector)
            ;; Caller requested a vector
            (make-array 4 :adjustable nil :element-type 'integer :fill-pointer nil :initial-contents lst) )
          ( T
            lst ))))                    ; return a list by default

; xdr write word
(defun xdr-write-word (word xdrs)
  "Word is a vector of 4 integers.  Write each (mod 256).  Return xdrs on
   success, nil on error."
  (declare (type (or list array) word) (type xdr-stream xdrs))
  (assert (xdr-stream-write-octet-fn xdrs))
  (if (or (eq (xdr-write-octet (elt word 0) xdrs) xdrs)
          (eq (xdr-write-octet (elt word 1) xdrs) xdrs)
          (eq (xdr-write-octet (elt word 2) xdrs) xdrs)
          (eq (xdr-write-octet (elt word 3) xdrs) xdrs))
    xdrs                              ; error somewhere
    word))

; xdr read fixed opaque
(defun xdr-read-fixed-opaque (length xdrs &key (type 'vector))
  "Read LENGTH octets from XDRS, returning them in a list or a vector,
   as you request with the :TYPE keyword argument. Defaults to vector.
   Returns XDRS on error."
  (declare (type integer length) (type xdr-stream xdrs) (type symbol type))
  (assert (>= length 0))
  (assert (member type '(vector list)))
  ;; Collect all the octets in one fell swoop.  This is a compact piece
  ;; of code, which is why I chose it.  When all works well, it's as
  ;; efficient as any other, I'm sure.  If there is an error, it will
  ;; call XDR-READ-OCTET possibly many times after the error, but it
  ;; will do no harm because the first failed XDR-READ-OCTET would have
  ;; set XDRS's IS-GOOD to false, so it would not attempt to read any
  ;; more.  So in the erroneous case, we waste some time, but no serious
  ;; harm will be done.  And in the non-erroneous case, it's great.
  (let ( (lst (loop for ii from 1 to length collect (xdr-read-octet xdrs))) )
    ;; Now we have the octets in LST.
    ;; Read & discard to the end of the XDR word. In the erroneous case,
    ;; this will waste some time, but do no harm.
    (dotimes (ii (mod (- 4 (mod length 4)) 4))
      (xdr-read-octet xdrs))
    ;; Figure out what to return.  Three cases: error, vector, or list.
    (cond ( (member xdrs lst)
            xdrs )      ; error
          ( T
            (coerce lst type) ))))

; xdr write fixed opaque
(defun xdr-write-fixed-opaque (seq length xdrs)
  "Write LENGTH octets of opaque data from sequence SEQ.  In XDR, opaque
   is always a vector of octets, but this function is forgiving about the
   thing it writes.  The thing may be any sequence accessible with ELT, &
   every element must be writable as an octet.  Length of SEQ must be at least
   LENGTH, & it must hold non-negative integers which will be written
   with XDR-WRITE-OCTET.  Return XDRS on error.  Return anything other than
   XDRS on success."
  (declare (type sequence seq) (type integer length) (type xdr-stream xdrs))
  (assert (every #'(lambda (x) (and (integerp x) (<= 0 x 255))) seq))
  (assert (>= length 0))
  (assert (>= (length seq) length))
  (typecase seq
    (list (do ( (ii 0 (1+ ii))
                (octets seq (rest octets)) )
            ((or (>= ii length)
                 (eq (xdr-write-octet (first octets) xdrs) xdrs)))))
    (otherwise
      (loop for i from 1 to length
        do (xdr-write-octet (elt seq i) xdrs))))
  ;; Pad the final block with zeros
  (dotimes (ii (mod (- 4 (mod length 4)) 4))
    (xdr-write-octet 0 xdrs))
  (if (xdr-stream-is-good xdrs)
    seq
    xdrs))

;; *****************************************************************************
;; ** player special: int8, uint8, char, unsigned char                        **
;; *****************************************************************************

; xdr read player unsigned byte
(defun xdr-read-player-uint8 (xdrs)
  "return an unsigned byte (encoded for player)"
  (xdr-read-octet xdrs))

; xdr write player unsigned byte
(defun xdr-write-player-uint8 (uint xdrs)
  "write an unsigned byte (encoded for player)"
  (xdr-write-octet uint xdrs))

; xdr read player byte
(defun xdr-read-player-int8 (xdrs)
  "Read & return a 8-bit, signed byte from the XDR-encoded stream. (encoded for player)"
  (declare (type xdr-stream xdrs))
  (let ( (uint (xdr-read-player-uint8 xdrs)) )
    (if (>= uint #x080)
      (- uint #x100)
      uint)))

; xdr write player byte
(defun xdr-write-player-int8 (int xdrs)
  "Write the signed byte (mod (expt 2 8)) to the XDR stream. (encoded for player)"
  (let ( (is-minus (minusp int)) )
    ; As long as our integer is way, way too negative ...
    (loop while (minusp int)
      do (incf int #x080))
    (when is-minus
      ; Map the negative value to the high positive range
      (incf int #x080))
    (xdr-write-player-uint8 int xdrs)))

; xdr read player char
(defun xdr-read-player-char (xdrs)
  ; I hate exceptions that are thrown when you try to convert a number
  ; to a character & the number isn't in the character set.  So we
  ; use ignore-errors to prevent the exception.  Bonus: If there is
  ; an error, we get nil, which is what we want to return on error.
  (or (ignore-errors (code-char (xdr-read-player-uint8 xdrs)))
      xdrs))

; xdr write player char
(defun xdr-write-player-char (char xdrs)
  (xdr-write-player-uint8 (char-int char) xdrs))

;; *****************************************************************************
;; ** bool                                                                    **
;; *****************************************************************************

; xdr read bool
(defun xdr-read-bool (xdrs)
  (not (zerop (xdr-read-uint xdrs))))

; xdr write bool
(defun xdr-write-bool (bool xdrs)
  (xdr-write-uint (if bool 1 0) xdrs))

;; *****************************************************************************
;; ** character                                                               **
;; *****************************************************************************

; xdr read char
(defun xdr-read-char (xdrs)
  ; I hate exceptions that are thrown when you try to convert a number
  ; to a character & the number isn't in the character set.  So we
  ; use ignore-errors to prevent the exception.  Bonus: If there is
  ; an error, we get nil, which is what we want to return on error.
  (or (ignore-errors (code-char (xdr-read-uint xdrs)))
      xdrs))

; xdr write char
(defun xdr-write-char (char xdrs)
  (xdr-write-uint (char-int char) xdrs))

;; *****************************************************************************
;; ** string                                                                  **
;; *****************************************************************************

; xdr read string
(defun xdr-read-string (xdrs)
  (coerce
    (loop for octet across (xdr-read-fixed-opaque (xdr-read-uint xdrs) xdrs)
      collect (code-char octet))
    'string))

; xdr write string
(defun xdr-write-string (string xdrs)
  (if (eq (xdr-write-uint (length string) xdrs) xdrs)
    xdrs                              ; error
    (xdr-write-fixed-opaque
      (loop for char across string
        collect (char-code char))
      (length string)
      xdrs)))

;; *****************************************************************************
;; ** integer                                                                 **
;; *****************************************************************************

; xdr read var uint
(defun xdr-read-var-uint (length xdrs)
  "Reconstruct & return an unsigned integer of LENGTH octets."
  (let ( (lst (xdr-read-fixed-opaque length xdrs :type 'list)) )
    (if (eq lst xdrs)
      xdrs                            ; error
      ;; Now we sum the octets.  Due to the SHIFT, it's easiest
      ;; to start with the little end, so we must reverse LST
      ;; before we iterate over it.  Note the initializer for LST2.
      (do ( (lst2 (nreverse lst) (rest lst2))
            (ii 0 (1+ ii))
            (shift 0 (+ 8 shift))
            (sum 0 (+ (ash (first lst2) shift) sum)) )
        ((>= ii length) sum)))))

; xdr write var uint
(defun xdr-write-var-uint (uint length xdrs)
  "Write a 32-bit, XDR-encoded, unsigned integer to the strm."
  (xdr-write-fixed-opaque
    (do ( (ii 0 (1+ ii))
          (shift 0 (- shift 8))
          (lst () (cons (boole boole-and (ash uint shift) #x0FF) lst)) )
      ((>= ii length) lst))
    length
    xdrs))

; xdr read unsigned integer
(defun xdr-read-uint (xdrs)
  "Reads & returns a 32-bit, unsigned integer from the XDR-encoded stream."
  (xdr-read-var-uint 4 xdrs))

; xdr write unsigned integer
(defun xdr-write-uint (uint xdrs)
  (xdr-write-var-uint uint 4 xdrs))

; xdr read unsigned hyper integer
(defun xdr-read-uhyper (xdrs)
  "Read & return the next unsigned hyper integer"
  (xdr-read-var-uint 8 xdrs))

; xdr write unsigned hyper integer
(defun xdr-write-uhyper (uhyper xdrs)
  (xdr-write-var-uint uhyper 8 xdrs))

; xdr read integer
(defun xdr-read-int (xdrs)
  "Read & return a 32-bit, signed integer from the XDR-encoded stream."
  (declare (type xdr-stream xdrs))
  (let ( (uint (xdr-read-uint xdrs)) )
    (if (>= uint #x080000000)
      (- uint #x100000000)
      uint)))

; xdr write integer
(defun xdr-write-int (int xdrs)
  "Write the signed integer (mod (expt 2 31)) to the XDR stream."
  (let ( (is-minus (minusp int)) )
    ;; As long as our integer is way, way too negative ...
    (loop while (minusp int)
      do (incf int #x080000000))
    (when is-minus
      ;; Map the negative value to the high positive range
      (incf int #x080000000))
    (xdr-write-uint int xdrs)))

; xdr read hyper
(defun xdr-read-hyper (xdrs)
  "Read & return a 64-bit, signed integer from the XDR-encoded stream."
  (declare (type xdr-stream xdrs))
  (let ( (uint (xdr-read-uint xdrs)) )
    (if (>= uint #x0800000000000000)
      (- uint #x1000000000000000)
      uint)))

; xdr write hyper
(defun xdr-write-hyper (hyper xdrs)
  "Write the signed integer (mod (expt 2 63)) to the XDR stream."
  (let ( (is-minus (minusp hyper)) )
    ;; As long as our integer is way, way too negative ...
    (loop while (minusp hyper)
      do (incf hyper #x0800000000000000))
    (when is-minus
      ;; Map the negative value to the high positive range
      (incf hyper #x0800000000000000))
    (xdr-write-uhyper hyper xdrs)))

;; *****************************************************************************
;; ** float and double                                                        **
;; *****************************************************************************

;;; There is a bug in the floating point stuff.  It mostly works, but I
;;; notice that if you decode 1.1, then encode the result, you get
;;; 1.099999.  Other numbers I tested worked correctly.

; xdr read float
(defun xdr-read-float (xdrs)
  (labels ( (xdr-encode-float (sign exponent fraction)
              (if (and (zerop sign) (zerop exponent) (zerop fraction))
                0.0    ; correct ???
                (* (expt -1 sign)
                   (handler-case (expt 2 (- exponent 127.0d0))
                     (floating-point-underflow () 0.0))
                   (+ 1 (float (/ fraction #x800000) 1.0d0))))) )
    (let*( (a4 (xdr-read-word xdrs))
           (sign (if (>= (aref a4 0) #x080) 1 0))
           (exponent (+ (* (mod (aref a4 0) #x080) 2) (if (>= (aref a4 1) #x080) 1 0)))
           (fraction (+ (* (mod (aref a4 1) #x080)  #x010000)
                        (*      (aref a4 2)           #x0100)
                                (aref a4 3))) )
      (declare (type (simple-array integer (4)) a4))
      (float (xdr-encode-float sign exponent fraction) 1.0))))

; xdr write float
(defun xdr-write-float (float xdrs)
  (labels ( (xdr-decode-float (float)
              "Return three values: sign, exponent, & fraction."
              (multiple-value-bind (significand exponent sign)
                                   (decode-float float)
                (declare (ignore sign))
                (values
                  (if (minusp float) 1 0)            ; XDR's sign
                  (mod (+ exponent 126) 256)
                  (floor (* (second (multiple-value-list (floor (* 2.0 significand)))) #x800000))))) )
    (if (zerop float)
      (xdr-write-word (list 0 0 0 0) xdrs)
      (multiple-value-bind (sign exponent fraction)
                           (xdr-decode-float float)
        (xdr-write-word
          (list
            (+ (* sign 128) (mod (floor (/ exponent 2)) 128))
            (+ (* (mod exponent 2) 128) (mod (floor (/ fraction #x10000)) 128))
            (mod (floor (/ fraction #x100)) 256)
            (mod fraction 256))
          xdrs)))))

; xdr read double
(defun xdr-read-double (xdrs)
  (labels ( (xdr-encode-double (sign exponent fraction)
              (cond ( ; zero
                      (and (zerop sign) (zerop exponent) (zerop fraction))
                      0.0 ) ; correct ???
                    ( ; nan
                      (and (zerop sign) (= exponent 2047) (= fraction 2251799813685248))
                      :nan ) ; correct ???
                    ( ; all other cases
                      T
                      (* (expt -1 sign)
                         (handler-case (expt 2 (- exponent 1023.0d0))
                           (floating-point-underflow () 0.0))
                         (+ 1 (float (/ fraction #x10000000000000) 1.0d0))) ))) )
    (let*( (a4 (xdr-read-word xdrs))
           (b4 (xdr-read-word xdrs))
           (sign (if (>= (aref a4 0) #x080) 1 0))
           (exponent (+ (* (mod (aref a4 0) #x080) #x10) (truncate (/ (aref a4 1) #x010))))
           (fraction (+ (* (mod (aref a4 1) #x010)  #x1000000000000)
                        (*      (aref a4 2)           #x10000000000)
                        (*      (aref a4 3)             #x100000000)
                        (*      (aref b4 0)               #x1000000)
                        (*      (aref b4 1)                 #x10000)
                        (*      (aref b4 2)                   #x100)
                                (aref b4 3))) )
      (declare (type (simple-array integer (4)) a4 b4))
      (xdr-encode-double sign exponent fraction))))



; xdr write double
(defun xdr-write-double (double xdrs)
  (labels ( (xdr-decode-double (double)
              "Return three values: sign, exponent, & fraction."
              (multiple-value-bind (significand exponent sign)
                                   (decode-float double)
                (declare (ignore sign))
                (values
                  (if (minusp double) 1 0)            ; XDR's sign
                  (mod (+ exponent 1022) 2048)
                  (floor (* (second (multiple-value-list (floor (* 2.0 significand)))) #x10000000000000))))) )
    (if (zerop double)
      (progn
        (xdr-write-word (list 0 0 0 0) xdrs)
        (xdr-write-word (list 0 0 0 0) xdrs))
      (multiple-value-bind (sign exponent fraction)
                           (xdr-decode-double double)
        (xdr-write-word
          (list
            (+ (* sign 128) (mod (floor (/ exponent #x10)) 128))
            (+ (* (mod exponent #x10) #x10) (mod (floor (/ fraction #x1000000000000)) #x10))
            (mod (floor (/ fraction      #x10000000000)) #x100)
            (mod (floor (/ fraction        #x100000000)) #x100))
          xdrs)
        (xdr-write-word
          (list
            (mod (floor (/ fraction          #x1000000)) #x100)
            (mod (floor (/ fraction            #x10000)) #x100)
            (mod (floor (/ fraction              #x100)) #x100)
            (mod           fraction                      #x100))
          xdrs)))))

;; *****************************************************************************
;; ** arrays                                                                  **
;; *****************************************************************************

; xdr read fixed array
(defun xdr-read-fixed-array (array read-fun length xdrs &key (pad-bytes nil))
  "Read an array of exactly LENGTH elements.  READ-FUN gives read function for each element."
  (declare (type sequence array) (type integer length) (type xdr-stream xdrs))
  ; read
  (dotimes (ii length)
    (setf (elt array ii) (funcall read-fun (elt array ii) xdrs)))
  ; read padding bytes
  (when pad-bytes
    (dotimes (ii (mod (- 4 (mod length 4)) 4))
      (xdr-read-octet xdrs)))
  array)

; xdr write fixed array
(defun xdr-write-fixed-array (array write-fun length xdrs &key (pad-bytes nil))
  "Write the first LENGTH elements of sequence.  Length of V must be at
   least LENGTH.  Each element is write with WRITE-FUN."
  (declare (type sequence array) (type integer length) (type xdr-stream xdrs))
  ; write
  (dotimes (ii length)
    (funcall write-fun (elt array ii) xdrs))
  ; Pad the final block with zeros
  (when pad-bytes
    (dotimes (ii (mod (- 4 (mod length 4)) 4))
      (xdr-write-octet 0 xdrs))))

; xdr read var array
(defun xdr-read-var-array (array read-fun xdrs &key (pad-bytes nil))
  (declare (type sequence array) (type xdr-stream xdrs))
  (xdr-read-fixed-array array read-fun (xdr-read-uint xdrs) xdrs :pad-bytes pad-bytes))

; xdr write var array
(defun xdr-write-var-array (array write-fun length xdrs &key (pad-bytes nil))
  (declare (type sequence array) (type integer length) (type xdr-stream xdrs))
  (xdr-write-uint length xdrs)
  (xdr-write-fixed-array array write-fun length xdrs :pad-bytes pad-bytes))

