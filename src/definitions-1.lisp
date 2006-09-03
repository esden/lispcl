;;; ---------------------------------------------------------------------------- ;;;
;;;  LISCPL - Lisp Player Client - definitions-1.lisp                            ;;;
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
;; ** loggers                                                                 **
;; *****************************************************************************

;(add-logger 'lispcl-init-close :stream '*standard-output*)
;(add-logger 'lispcl-read-message :stream '*standard-output*)
;(add-logger 'lispcl-send-message :stream '*standard-output*)

;; *****************************************************************************
;; ** global constants                                                        **
;; *****************************************************************************

; host and port
(defparameter *player-default-host* "127.0.0.1" "default host of player server")
(defparameter *player-default-port* 6665 "default port of player server")

;; *****************************************************************************
;; ** global variables                                                        **
;; *****************************************************************************

; thread creation function
(defvar *thread-creation-fun* #'make-thread)

;; *****************************************************************************
;; ** auxiliary functions                                                     **
;; *****************************************************************************

; interface code to string
(defun interface-code->string (no)
  "converts a device given as number into a string"
  (cond ( (= no *player-null-code*) *player-null-string* )
        ( (= no *player-actarray-code*) *player-actarray-string* )
        ( (= no *player-aio-code*) *player-aio-string* )
        ( (= no *player-audiodsp-code*) *player-audiodsp-string* )
        ( (= no *player-audiomixer-code*) *player-audiomixer-string* )
        ( (= no *player-blinkenlight-code*) *player-blinkenlight-string* )
        ( (= no *player-blobfinder-code*) *player-blobfinder-string* )
        ( (= no *player-bumper-code*) *player-bumper-string* )
        ( (= no *player-camera-code*) *player-camera-string* )
        ( (= no *player-dio-code*) *player-dio-string* )
        ( (= no *player-gripper-code*) *player-gripper-string* )
        ( (= no *player-fiducial-code*) *player-fiducial-string* )
        ( (= no *player-gps-code*) *player-gps-string* )
        ( (= no *player-graphics2d-code*) *player-graphics2d-string* )
        ( (= no *player-graphics3d-code*) *player-graphics3d-string* )
        ( (= no *player-ir-code*) *player-ir-string* )
        ( (= no *player-joystick-code*) *player-joystick-string* )
        ( (= no *player-limb-code*) *player-limb-string* )
        ( (= no *player-laser-code*) *player-laser-string* )
        ( (= no *player-localize-code*) *player-localize-string* )
        ( (= no *player-log-code*) *player-log-string* )
        ( (= no *player-map-code*) *player-map-string* )
        ( (= no *player-mcom-code*) *player-mcom-string* )
        ( (= no *player-opaque-code*) *player-opaque-string* )
        ( (= no *player-planner-code*) *player-planner-string* )
        ( (= no *player-player-code*) *player-player-string* )
        ( (= no *player-position1d-code*) *player-position1d-string* )
        ( (= no *player-position2d-code*) *player-position2d-string* )
        ( (= no *player-position3d-code*) *player-position3d-string* )
        ( (= no *player-power-code*) *player-power-string* )
        ( (= no *player-ptz-code*) *player-ptz-string* )
        ( (= no *player-rfid-code*) *player-rfid-string* )
        ( (= no *player-simulation-code*) *player-simulation-string* )
        ( (= no *player-sonar-code*) *player-sonar-string* )
        ( (= no *player-sound-code*) *player-sound-string* )
        ( (= no *player-speech-code*) *player-speech-string* )
        ( (= no *player-speech-recognition-code*) *player-speech-recognition-string* )
        ( (= no *player-waveform-code*) *player-waveform-string* )
        ( (= no *player-wifi-code*) *player-wifi-string* )
        ( (= no *player-wsn-code*) *player-wsn-string* )
        ( T nil )))

; access-code to string
(defun access-code->string (no)
  "converts a device given as number into a string"
  (cond ( (= no *player-open-mode*) "open" )
        ( (= no *player-close-mode*) "closed" )
        ( (= no *player-error-mode*) "error" )
        ( T "unknown" )))

;; *****************************************************************************
;; ** player standard object                                                  **
;; *****************************************************************************

; player-standard-object-indentation
(defvar *player-standard-object-indentation* "")

; class player-standard-object
(defclass player-standard-object (standard-object) ())

; print-object for player-standard-object
(defmethod print-object ((pc player-standard-object) stream)
  #+(or allegro cmu sbcl clisp)
  (format stream "~a"
    (string-downcase
      (let ( (*player-standard-object-indentation* (concatenate 'string *player-standard-object-indentation* "  ")) )
        (declare (special *player-standard-object-indentation*))
        (format nil "#<~a ~{~{~%~a:~a ~a~}~}>"
          (type-of pc)
          (mapcar #'(lambda (slot)
                      (let ( (slot-name #+(or allegro clisp) (clos:slot-definition-name slot)
                                        #+cmu (pcl:slot-definition-name slot)
                                        #+sbcl (sb-pcl:slot-definition-name slot)) )
                        (when (slot-boundp pc slot-name)
                          (list *player-standard-object-indentation* slot-name (slot-value pc slot-name)))))
                  #+(or allegro clisp) (clos:class-slots (find-class (type-of pc)))
                  #+cmu (pcl:class-slots (find-class (type-of pc)))
                  #+sbcl (sb-pcl:class-slots (find-class (type-of pc))))))))
  #-(or allegro cmu sbcl clisp)
  (call-next-method pc stream))

