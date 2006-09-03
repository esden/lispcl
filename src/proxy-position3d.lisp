;;; ---------------------------------------------------------------------------- ;;;
;;;  LISCPL - Lisp Player Client - proxy-position3d.lisp                         ;;;
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

(let ( (vel-cmd (make-instance 'player-position3d-cmd-vel))
       (pos-cmd (make-instance 'player-position3d-cmd-pos)) )
  (labels ( (update-pos-vel (data x y z ax ay az)
              (setf (player-px data) x)
              (setf (player-py data) y)
              (setf (player-pz data) z)
              (setf (player-proll data) ax)
              (setf (player-ppitch data) ay)
              (setf (player-pyaw data) az)) )

    ;; ** set velocity                                                        **
    (defmethod set-velocity ((pp position3d-proxy) &key (x 0.0) (y 0.0) (z 0.0)
                                                        (ax 0.0) (ay 0.0) (az 0.0)
                                                        (state T)
                                                   &allow-other-keys)
      (update-pos-vel (player-vel vel-cmd) x y z ax ay az)
      (setf (player-state vel-cmd) (if state 1 0))
      (proxy-command pp *player-position3d-cmd-set-vel* vel-cmd))

    ;; ** set pose                                                            **
    (defmethod set-pose ((pp position3d-proxy) &key (x 0.0) (y 0.0) (z 0.0)
                                                    (ax 0.0) (ay 0.0) (az 0.0)
                                                    (state T)
                                               &allow-other-keys)
      (update-pos-vel (player-pos pos-cmd) x y z ax ay az)
      (setf (player-state pos-cmd) (if state 1 0))
      (proxy-command pp *player-position3d-cmd-set-pos* pos-cmd))

  )
)

;; *****************************************************************************
;; ** requests                                                                **
;; *****************************************************************************

;; ** get geometry                                                            **
(defmethod get-geometry ((pp position3d-proxy))
  "Get geometry"
  (proxy-request pp *player-position3d-get-geom* nil
    :response (make-instance 'player-position3d-geom)))

;; ** set motor state                                                         **
(defmethod set-motor-state ((pp position3d-proxy) state &key &allow-other-keys)
  "Set motor state"
  (proxy-request pp *player-position3d-motor-power*
    (make-instance 'player-position3d-power-config :state (nil-t->0-1-int state))))

