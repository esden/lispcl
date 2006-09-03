;;; ---------------------------------------------------------------------------- ;;;
;;;  LISCPL - Lisp Player Client - definitions-2.lisp                            ;;;
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
;; ** player client                                                           **
;; *****************************************************************************
(defclass player-client ()
  ( (connection
      :type stream
      :documentation "The connection stream to the Player server")
    (xdr-stream
      :type xdr-stream
      :documentation "The connection stream wrapped as xdr-strem")
    (read-thread
      :initform nil
      :documentation "Thread performing read operations in threaded mode")
    (pull-mode
      :type boolean
      :initform nil
      :reader pull-mode-p
      :writer pull-mode
      :documentation "Are we in pull mode?")
    (host
      :initarg :host
      :reader host
      :initform *player-default-host*
      :type string
      :documentation "The host of the Player server to which we are connected.")
    (port
      :initarg :port
      :reader port
      :initform *player-default-port*
      :type integer
      :documentation "The port of the Player server to which we are connected.")
    (banner
      :reader banner
      :initform ""
      :type string
      :documentation "The banner message of the Player server to which we are connected.")
    (timestamp
      :reader timestamp
      :initform 0.0
      :type float
      :documentation "The latest time the server sent a message")
    (proxies
      :initform nil
      :documentation "list of proxies associated with us") )
  (:documentation "One player-client object is used to control each connection to a Player server."))

; print object player-client
(defmethod print-object ((pc player-client) stream)
  (format stream "#<~a ~a:~a ~,3fs~{~%  ~a~}>"
    (banner pc) (host pc) (port pc) (timestamp pc) (slot-value pc 'proxies)))

;; *****************************************************************************
;; ** base proxy class                                                        **
;; *****************************************************************************

;; ** base proxy class                                                        **
(defclass base-proxy ()
  ( (player-client
      :initarg :player-client
      :reader player-client
      :type player-client
      :documentation "The controlling client object.")
    (devaddr
      :initarg :devaddr
      :reader devaddr
      :type player-devaddr
      :documentation "device address")
    (request-status
      :reader request-status
      :initform :none
      :type symbol
      :documentation "Status of current request handling.
                      :none = no request sent;
                      :waiting = request sent, waiting for response
                      :ignoring = request sent, ignoring response
                      :response = response received, waiting for handling response")
    (response-header
      :reader response-header
      :type player-msghdr
      :documentation "last response header received") )
  (:documentation
    "Base Proxy class for all proxies"))

; print object base-proxy
(defmethod print-object ((bp base-proxy) stream)
  (format stream "#<proxy ~a:~a>"
    (interface-code->string (player-interf (devaddr bp))) (player-index (devaddr bp))))

