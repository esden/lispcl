;;; ---------------------------------------------------------------------------- ;;;
;;;  LISCPL - Lisp Player Client - lispcl.asd                                    ;;;
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

;; *****************************************************************************
;; ** package definition                                                      **
;; *****************************************************************************

(defpackage "LISP-PLAYER-CLIENT-LIBRARY"
  (:documentation "Lisp player client library")
  (:nicknames "LISPCL")
  (:use "COMMON-LISP" "COMMON-LISP-USER")
  (:export
    ; constants
    ; definitions-1
    "PLAYER-STANDARD-OBJECT"
    ; utiltities
    "NIL-T->0-1"
    ; threads
    "WAIT-FOR-INPUT"
    ; xdr
    "MAKE-XDR-STREAM" "XDR-READ" "XDR-WRITE"
    ; player-client
    "PLAYER-CLIENT" "TIMESTAMP" "CONNECTION" "DISCONNECT" "GET-PLAYER-PROXY" "GET-PROXY" "PULL-MODE-P"
    "READ-MESSAGE" "READ-DATA"
    ; player-proxy
    "PLAYER-PROXY"
    "GET-DEVICE-LIST" "GET-DRIVER-INFO" "REQUEST-DEVICE-ACCESS" "REQUEST-DATA" "SET-DATAMODE" "SET-REPLACE-RULE"
    ; std-proxy
    "GET-DATA"
    ; actarray-proxy
    "SET-POSITION" "SET-VELOCITY" "SET-CURRENT"
    "SET-MOTOR-STATE" "SET-BRAKES-STATE" "SET-VELOCITY-PARAM" "SET-ACCELERATION-PARAM"
    ; blobfinder-proxy
    "SET-TRACKING-COLOR" "SET-IMAGER-PARAMS"
    ; laser-proxy
    "GET-GEOMETRY" "SET-POWER-STATE"
    ; limb-proxy
    "SET-HOME" "SET-POSE" "SET-POSE-EULER" "SET-POSE-PAO"
    ; planner-proxy
    "SET-GOAL"
    "GET-WAYPOINTS" "SET-ENABLED"
    ; position2d-proxy
    "SET-VELOCITY" "SET-POSE"
    "GET-GEOMETRY" "SET-MOTOR-STATE" "SET-VELOCITY-MODE" "SET-POSITION-MODE"
    "RESET-ODOMETRY" "SET-ODOMETRY"
    "SET-VELOCITY-PID-PARAMS" "SET-POSITION-PID-PARAMS" "SET-VELOCITY-PROFILE-PARAMS"
    ; position3d-proxy
    "SET-VELOCITY" "SET-POSE"
    "GET-GEOMETRY" "SET-MOTOR-STATE"
    ; ptz-proxy
    "SET-PTZ"
    "GET-GEOMETRY" "SET-CONTROL-MODE"
    ; sonar-proxy
    "GET-GEOMETRY" "SET-POWER-STATE"))

(in-package "LISPCL")

; lispcl version
; for stable releases the string is set by the script make-release
(defparameter *lispcl-version* "svn-version")

;; *****************************************************************************
;; ** asdf definitions                                                        **
;; *****************************************************************************

(in-package "ASDF")

;; lispcl source file
; class definition
(defclass lispcl-source-file (cl-source-file) ())

; source file type
(defmethod source-file-type ((c lispcl-source-file) (s module)) "lisp")

; get filename of output file
(defmethod output-files ((operation compile-op) (c lispcl-source-file))
  (if (find-package "CL-PPCRE")
    (list (make-pathname
            :defaults (compile-file-pathname
                        (funcall (intern "REGEX-REPLACE" (find-package "CL-PPCRE"))
                          "^(.*/)src(/.*)$"
                          (namestring (component-pathname c))
                          (list 0 "bin" 1)))))
    (call-next-method operation c)))


;; execute file

; class definition
(defclass run-make (static-file) ())

; operation-done-p
(defmethod operation-done-p ((o compile-op) (c run-make))
  (when (string= (subseq lispcl::*lispcl-version* 0 3) "svn")
    (let ( (path (directory-namestring (component-pathname c))) )
      #+allegro (excl:run-shell-command (vector "make" "make" "--quiet" "-C" path) :output *standard-output*)
      #+cmu (ext:run-program "make" (list "--quiet" "-C" path) :output *standard-output*)
      #+sbcl (sb-ext:run-program "/usr/bin/make" (list "--quiet" "-C" path) :output *standard-output*)
      #+clisp (ext:run-program "make" :arguments (list "--quiet" "-C" path))
      #-(or allegro cmu sbcl clisp)
        (format T "Can't run 'make' from lisp. Please run make from the command line in ~a~%" path)))
  T)

;; *****************************************************************************
;; ** lisp player client library                                              **
;; *****************************************************************************

(in-package "LISPCL")

; asdf definition
(asdf:defsystem lispcl
  :name "Lisp player client"
  :author "Armin Mueller <armin-mueller@users.sourceforge.net>"
  :version "svn-version"
  :maintainer "Armin Mueller <armin-mueller@users.sourceforge.net>"
  :licence "GPL v2 or higher"
  :description "Lisp player client"
  :long-description "Lisp player client for the Player Project"

  :components
  ( (:module "src"
      :default-component-class asdf::lispcl-source-file
      :components
      ( (:run-make "make")
        (:file "utilities")
        (:file "threads" :depends-on ("utilities"))
        (:file "constants-player" :depends-on ("make"))
        (:file "definitions-1" :depends-on ("threads" "constants-player"))
        (:file "xdr" :depends-on ("threads"))
        (:file "xdr-player" :depends-on ("definitions-1" "xdr"))
        (:file "definitions-2" :depends-on ("xdr-player"))
        (:file "player-client" :depends-on ("definitions-2"))
        (:file "proxy--base" :depends-on ("player-client"))
        (:file "proxy-player" :depends-on ("proxy--base"))
        (:file "proxy--std" :depends-on ("proxy-player"))
        (:file "proxies-player" :depends-on ("proxy--std"))
        (:file "proxy-actarray" :depends-on ("proxies-player"))
        (:file "proxy-blobfinder" :depends-on ("proxies-player"))
        (:file "proxy-laser" :depends-on ("proxies-player"))
        (:file "proxy-limb" :depends-on ("proxies-player"))
        (:file "proxy-planner" :depends-on ("proxies-player"))
        (:file "proxy-position2d" :depends-on ("proxies-player"))
        (:file "proxy-position3d" :depends-on ("proxies-player"))
        (:file "proxy-ptz" :depends-on ("proxies-player"))
        (:file "proxy-sonar" :depends-on ("proxies-player")) )) ))

