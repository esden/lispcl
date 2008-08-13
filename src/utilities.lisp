;;; ---------------------------------------------------------------------------- ;;;
;;;  LISCPL - Lisp Player Client - utilities.lisp                                ;;;
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

(in-package "LISPCL")

;; *****************************************************************************
;; * loggers                                                                   *
;; * =======                                                                   *
;; *                                                                           *
;; * Print log informations                                                    *
;; *                                                                           *
;; * add:                                                                      *
;; *   (add-logger 'lispcl-init-close :stream '*standard-output*)              *
;; * log-msg:                                                                  *
;; *   (log-msg lispcl-init-close "closing player connection~%")               *
;; *                                                                           *
;; * call add-logger with quoted name and log-msg without quoted name          *
;; *****************************************************************************

(let ( (loggers* nil) )

  (labels ( (get-symbol (name)
              (intern (string-upcase (format nil "~a" name)) "KEYWORD")) )

    ; get-loggers
    (defun get-loggers ()
      "return current list of loggers"
      loggers*)

    ; add-logger
    (defun add-logger (name &key (stream '*standard-output*))
      "add a new logger with given name and write output to stream"
      (let*( (symbol (get-symbol name))
             (cell (assoc symbol loggers*)) )
        (cond ( cell
                (rplacd cell stream) )
              ( T
                (push (cons symbol stream) loggers*) ))))

    ; remove logger
    (defun remove-logger (name)
      "remove a logger with the given name"
      (setf loggers* (remove (get-symbol name) loggers* :key #'first)))

    ; log-msg (evaluates logger name at compile time)
    (defmacro log-msg (name control-string &rest args)
      "write a log-message for logger name"
      (let*( (symbol (get-symbol name))
             (cell (assoc symbol loggers*)) )
        (when cell
          `(format ,(cdr cell) ,control-string ,@args))))

    ; log-msg-fun (evaluates logger name at run time)
    (defun log-msg-fun (name control-string &rest args)
      "write a log-message for logger name"
      (let*( (symbol (get-symbol name))
             (cell (assoc symbol loggers*)) )
        (when cell
          (apply #'format (symbol-value (cdr cell)) control-string args))))

  )
)

;; *****************************************************************************
;; ** error and warning handling                                              **
;; *****************************************************************************

; throw error
(defmacro throw-error (source datum &rest arguments)
  `(error (concatenate 'string "[" ,source "] " ,datum) ,@arguments))

; throw warning
(defmacro throw-warn (source datum &rest arguments)
  `(warn (concatenate 'string "[" ,source "] " ,datum) ,@arguments))

;; *********************************************************************************
;; ***** orientation operations                                                *****
;; *********************************************************************************

; quaternion->euler
(defun quaternion->euler (qu qx qy qz)
  "convert a quaternion to euler angles"
  (values
    (atan (* 2 (+ (* qy qz) (* qu qx))) (+ (* qu qu) (* -1 qx qx) (* -1 qy qy) (* qz qz)))
    (asin (max -1.0 (min 1.0 (* -2 (- (* qx qz) (* qu qy))))))
    (atan (* 2 (+ (* qx qy) (* qu qz))) (+ (* qu qu) (* qx qx) (* -1 qy qy) (* -1 qz qz)))))

; euler->quaternion
(defun euler->quaternion (ax ay az)
  "convert euler angles to a quaternion"
  (let ( (phi (* ax 0.5))
         (the (* ay 0.5))
         (psi (* az 0.5)) )
    (declare (type (single-float -10.0 10.0) phi the psi))
    (values
      (+ (* (cos phi) (cos the) (cos psi)) (* (sin phi) (sin the) (sin psi)))
      (- (* (sin phi) (cos the) (cos psi)) (* (cos phi) (sin the) (sin psi)))
      (+ (* (cos phi) (sin the) (cos psi)) (* (sin phi) (cos the) (sin psi)))
      (- (* (cos phi) (cos the) (sin psi)) (* (sin phi) (sin the) (cos psi))))))

;frame->quaternion
(defun frame->quaternion (approach orientation)
  "convert a frame (given by it's z- and y-vector) to a quaternion: (approach orientation) -> (qu qx qy qz)"
  (labels ((crossproduct (ax ay az bx by bz)
             (values
              (- (* ay bz) (* az by))
              (- (* az bx) (* ax bz))
              (- (* ax by) (* ay bx))))
           (matrix->quaternion (r11 r12 r13 r21 r22 r23 r31 r32 r33)
             (let ((trace (+ r11 r22 r33)))
               (cond ((> trace 0)
                       (let ((s (/ 0.5 (sqrt (+ 1.0 trace)))))
                        (values
                         (/ 0.25 s)
                         (* (- r32 r23) s)
                         (* (- r13 r31) s)
                         (* (- r21 r12) s))))
                     ((and (> r11 r22) (> r11 r33))
                       (let ((s (/ 0.5 (sqrt (- (+ 1.0 r11) r22 r33)))))
                         (values
                          (* (- r32 r23) s)
                          (/ 0.25 s)
                          (* (+ r12 r21) s)
                          (* (+ r13 r31) s))))
                     ((> r22 r33)
                      (let ((s (/ 0.5 (sqrt (- (+ 1.0 r22) r11 r33)))))
                        (values
                         (* (- r13 r31) s)
                         (* (+ r12 r21) s)
                         (/ 0.25 s)
                         (* (+ r23 r32) s))))
                     (t
                      (let ((s (/ 0.5 (sqrt (- (+ 1.0 r33) r11 r22)))))
                        (values
                         (* (- r21 r12) s)
                         (* (+ r13 r31) s)
                         (* (+ r23 r32) s)
                         (/ 0.25 s))))))))        
    (let ((ax (player-px approach))
          (ay (player-py approach))
          (az (player-pz approach))
          (ox (player-px orientation))
          (oy (player-py orientation))
          (oz (player-pz orientation)))
      (multiple-value-bind (nx ny nz) (crossproduct ox oy oz ax ay az)
        (matrix->quaternion nx ox ax ny oy ay nz oz az)))))

;quaternion->frame
(defun quaternion->frame (qu qx qy qz)
  (values
   (make-instance 'player-point-3d :px (* 2 (+ (* qx qz) (* qu qy)))
                                   :py (* 2 (- (* qy qz) (* qu qx)))
                                   :pz (- (+ (* qu qu) (* qz qz)) (* qx qx) (* qy qy)))
   (make-instance 'player-point-3d :px (* 2 (- (* qx qy) (* qu qz)))
                                   :py (- (+ (* qu qu) (* qy qy)) (* qx qx) (* qz qz))
                                   :pz (* 2 (+ (* qy qz) (* qu qx))))))


;; *****************************************************************************
;; ** other functions                                                         **
;; *****************************************************************************

; map-pairs
(defun map-pairs (fun lst &key (combination #'cons))
  (labels ( (key-fun (ls)
              (cond ( (null ls)
                      nil )
                    ( T
                      (value-fun (first ls) (rest ls)) )))
            (value-fun (key ls)
              (cond ( (null ls)
                      (throw-warn "LISPCL:UTILITIES" "Uneven parameter list in map-pairs") )
                    ( T
                      (funcall combination (funcall fun key (first ls)) (key-fun (rest ls))) ))) )
    (key-fun lst)))

; add-hash-entries
(defun add-hash-entries (ht &rest key-value-pairs)
  (map-pairs #'(lambda (key value) (setf (gethash key ht) value)) key-value-pairs)
  ht)

; nil-t->0-1
(defun nil-t->0-1 (val)
  (cond ( (null val)
          0.0 )
        ( (numberp val)
          val )
        ( T
          1.0 )))

; nil-t->0-1-int
(defun nil-t->0-1-int (val)
  (round (nil-t->0-1 val)))

