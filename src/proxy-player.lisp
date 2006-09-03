;;; ---------------------------------------------------------------------------- ;;;
;;;  LISCPL - Lisp Player Client - proxy-player.lisp                             ;;;
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

;; ** player proxy class                                                      **
(defclass player-proxy (base-proxy)
  ( )
  (:default-initargs :devaddr (make-instance 'player-devaddr :interf *player-player-code* :index 0))
  (:documentation "player device proxy"))

;; *****************************************************************************
;; ** commands                                                                **
;; *****************************************************************************

;; *****************************************************************************
;; ** requests                                                                **
;; *****************************************************************************

;; ** get device list                                                         **
(defmethod get-device-list ((pp player-proxy))
  "Get the list of available devices from the player server;"
  (proxy-request pp *player-player-req-devlist* nil
    :response (make-instance 'player-device-devlist)))

;; ** get driver info                                                         **
(defmethod get-driver-info ((pp player-proxy) (interface integer) (index integer))
  "Get driver info for specified interface and index"
  (let ( (reqresp (make-instance 'player-device-driverinfo
                    :addr (make-instance 'player-devaddr :interf interface :index index)
                    :driver-name-count 0)) )
    (proxy-request pp *player-player-req-driverinfo* reqresp :response reqresp)))

;; ** request device access                                                   **
(defmethod request-device-access ((pp player-proxy) (interface integer) (index integer) (access-mode integer))
  "Request access to a device;"
  (let ( (reqresp (make-instance 'player-device-req
                    :addr (make-instance 'player-devaddr :interf interface :index index)
                    :access access-mode
                    :driver-name-count 0)) )
    (proxy-request pp *player-player-req-dev* reqresp :response reqresp)))

;; ** request data                                                            **
(defmethod request-data ((pp player-proxy) &key (wait T))
  "Request a new round of data."
  (proxy-request pp *player-player-req-data* nil :wait wait))

;; ** set data mode                                                           **
(defmethod set-datamode ((pp player-proxy) (mode integer))
  "Set datamode (either pull or push)"
  (when (proxy-request pp *player-player-req-datamode* (make-instance 'player-device-datamode-req :mode mode))
    (setf (slot-value (player-client pp) 'pull-mode) (eq mode *player-datamode-pull*))))

;; ** set replace rule                                                        **
(defmethod set-replace-rule ((pp player-proxy) (interface integer) (index integer) (type integer) (subtype integer) replace)
  "Set a replace rule"
  (proxy-request pp *player-player-req-add-replace-rule*
    (make-instance 'player-add-replace-rule-req
      :interf interface
      :index index
      :type type
      :subtype subtype
      :replace (nil-t->0-1-int replace))))

