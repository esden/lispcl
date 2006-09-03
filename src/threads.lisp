;;; ---------------------------------------------------------------------------- ;;;
;;;  LISCPL - Lisp Player Client - threads.lisp                                  ;;;
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
;; ** threads handling                                                        **
;; *****************************************************************************

; make thread
(defun make-thread (name fun &rest args)
  "Create a new thread and start it"
  #+allegro (apply #'mp:process-run-function name fun args)
  #+cmu (mp:make-process (lambda () (apply fun args)) :name name)
  #-(or allegro cmu) (declare (ignore name fun args))
  #-(or allegro cmu) (throw-error "lispcl:make-thread in threads.lisp"
                                  "function make-thread not implemented for ~a ~a"
                                  (lisp-implementation-type) (lisp-implementation-version)))

; thread yield
(defun thread-yield ()
  "Allow other threades to run"
  #+allegro (mp:process-allow-schedule)
  #+cmu (mp:process-yield)
  #-(or allegro cmu) (throw-error "lispcl:thread-yield in threads.lisp"
                                  "function thread-yield not implemented for ~a ~a"
                                  (lisp-implementation-type) (lisp-implementation-version)))

; thread wait
(defun thread-wait (whostate predicate &rest args)
  "Sleep until predicate becomes true"
  #+allegro (apply #'mp:process-wait whostate predicate args)
  #+cmu (mp:process-wait whostate (lambda () (apply predicate args)))
  #-(or allegro cmu) (declare (ignore whostate predicate args))
  #-(or allegro cmu) (throw-error "lispcl:thread-wait in threads.lisp"
                                  "function thread-wait not implemented for ~a ~a"
                                  (lisp-implementation-type) (lisp-implementation-version)))

; current thread
(defun current-thread ()
  "Return the current thread"
  #+(or allegro cmu) mp:*current-process*
  #-(or allegro cmu) (throw-error "lispcl:current-thread in threads.lisp"
                                  "function current-thread not implemented for ~a ~a"
                                  (lisp-implementation-type) (lisp-implementation-version)))

; kill thread
(defun kill-thread (thread)
  "Kill thread"
  #+allegro (mp:process-kill thread)
  #+cmu (mp:destroy-process thread)
  #-(or allegro cmu) (declare (ignore thread))
  #-(or allegro cmu) (throw-error "lispcl:kill-thread in threads.lisp"
                                  "function kill-thread not implemented for ~a ~a"
                                  (lisp-implementation-type) (lisp-implementation-version)))

; without scheduling
(defmacro without-scheduling (&rest body)
  "Disable interrupts for body"
  #+(or allegro cmu) `(mp:without-scheduling ,@body)
  #-(or allegro cmu) `(progn
                        (throw-warn "lispcl:without-scheduling in threads.lisp"
                                    "macro without-scheduling not implemented for ~a ~a"
                                    (lisp-implementation-type) (lisp-implementation-version))
                        ,@body))

;; *****************************************************************************
;; ** streams handling                                                        **
;; *****************************************************************************

(eval-when (:compile-toplevel :load-toplevel :execute)
  #+cmu (require :gray-streams)
  #+cmu (require :simple-streams)
  #+sbcl (require :sb-bsd-sockets))

; open-binary-connection
(defun open-binary-connection (host port)
  "Open a binary connection to the given host and port"
  #+allegro (socket:make-socket :remote-host host :remote-port port :format :binary)
  #+cmu (system:make-fd-stream
          (extensions:connect-to-inet-socket host port) :input T :output T :element-type '(unsigned-byte 8))
  #+clisp (socket:socket-connect port host :element-type '(unsigned-byte 8))
  #+sbcl (let ( (socket (make-instance 'sb-bsd-sockets:inet-socket :type :stream :protocol :tcp)) )
           (sb-bsd-sockets:socket-connect
             socket (sb-bsd-sockets:host-ent-address (sb-bsd-sockets:get-host-by-name host)) port)
           (sb-bsd-sockets:socket-make-stream
             socket :input T :output T :buffering :none :element-type '(unsigned-byte 8)))
  #-(or allegro cmu clisp sbcl)
    (throw-error "lispcl:open-binary-connection in threads.lisp"
                 "function open-binary-connection not implemented for ~a ~a"
                 (lisp-implementation-type) (lisp-implementation-version)))

; defun wait-for-input
(defun wait-for-input (stream &optional timeout)
  "Wait for input from stream with given timout"
  #+allegro (mp:wait-for-input-available (list stream) :timeout timeout)
  #+cmu (mp:process-wait-until-fd-usable (system:fd-stream-fd stream) :input timeout)
  #-(or allegro cmu) (declare (ignore stream timeout))
  #-(or allegro cmu) (throw-error "lispcl:wait-for-input in threads.lisp"
                                  "function wait-for-input not implemented for ~a ~a"
                                  (lisp-implementation-type) (lisp-implementation-version)))

