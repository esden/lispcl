;;; ---------------------------------------------------------------------------- ;;;
;;;  LISCPL - Lisp Player Client - proxy-laser.lisp                              ;;;
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
;; ** commands                                                                **
;; *****************************************************************************

;; *****************************************************************************
;; ** requests                                                                **
;; *****************************************************************************

;; ** get geometry                                                            **
(defmethod get-geometry ((lp laser-proxy))
  "Get geometry"
  (proxy-request lp *player-laser-req-get-geom* nil
    :response (make-instance 'player-laser-geom))
)

;; ** set power state                                                         **
(defmethod set-power-state ((lp laser-proxy) state)
  "set power state"
  (proxy-request lp *player-laser-req-power*
    (make-instance 'player-laser-power-config :state (nil-t->0-1-int state))))

