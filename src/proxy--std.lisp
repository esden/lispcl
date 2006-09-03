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

(in-package "LISPCL")

;; *****************************************************************************
;; ** proxy                                                                   **
;; *****************************************************************************

;; ** std proxy class                                                         **
(defclass std-proxy (base-proxy)
  ( (access-mode
      :initarg :access-mode
      :reader access-mode
      :initform *player-open-mode*
      :type integer
      :documentation "The current acces mode of the proxy.")
    (driver-name
      :reader driver-name
      :initform ""
      :type string
      :documentation "The name of the underlying player driver.")
    (timestamp
      :reader timestamp
      :initform 0.0
      :type float
      :documentation "Timestamp of the last data message.")
    (data
      :initarg :data
      :reader data
      :initform (make-hash-table)
      :type hash-table
      :documentation "Stores the latest data received for each data type.")
    (data-handlers
      :initarg :data-handlers
      :accessor data-handlers
      :initform (make-hash-table)
      :type hash-table
      :documentation "Stores for each data type a function to be called when new data is received") )
  (:documentation
    "Standard proxy class for all proxy devices except the player device"))

;; ** print object std-proxy                                                  **
(defmethod print-object ((sp std-proxy) stream)
  (format stream "#<proxy ~a:~a access=~a driver=~a>"
    (interface-code->string (player-interf (devaddr sp)))
    (player-index (devaddr sp))
    (access-code->string (access-mode sp))
    (driver-name sp)))

;; ** initialize instance std-proxy                                           **
(defmethod initialize-instance :after ((sp std-proxy) &key (index 0) &allow-other-keys)
  (setf (player-index (devaddr sp)) index)
  (unless (= (access-mode sp) *player-close-mode*)
    (change-access-mode sp (access-mode sp))))

;; *****************************************************************************
;; ** proxy methods                                                           **
;; *****************************************************************************

;; ** get data                                                                **
(defmethod get-data ((sp std-proxy) &optional (type nil))
  (cond ( type
          (gethash type (data sp)) )
        ( T
          (let ( (res nil) )
            (maphash #'(lambda (key value) (push (cons key value) res)) (data sp))
            (reverse res)) )))

;; ** get data handler                                                        **
(defmethod get-data-handler ((sp std-proxy) type)
  (gethash type (data-handlers sp)))

;; ** set data handler                                                        **
(defmethod set-data-handler ((proxy std-proxy) data-code data-handler-fun)
  (setf (gethash data-code (data-handlers proxy)) data-handler-fun))

;; ** disconnect std-proxy from player server                                 **
(defmethod disconnect ((sp std-proxy))
  (unless (or (equal (access-mode sp) *player-close-mode*)
              (equal (access-mode sp) *player-error-mode*))
    (change-access-mode sp *player-close-mode*))
  (log-msg lispcl-init-close "disconnect std-proxy : ~a~%" sp)
  (remove-proxy (player-client sp) sp))

;; ** change-access-mode for std-proxy to player server                       **
(defmethod change-access-mode ((sp std-proxy) (access-mode integer))
  "change current access mode of player server for proxy"
  (let ( (resp (request-device-access
                 (get-player-proxy (player-client sp))
                 (player-interf (devaddr sp))
                 (player-index (devaddr sp))
                 access-mode)) )
    (when resp
      (setf (slot-value sp 'driver-name)
            (subseq (player-driver-name resp) 0 (player-driver-name-count resp)))
      (setf (slot-value sp 'access-mode)
            (player-access resp)))))

