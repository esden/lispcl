;;; ---------------------------------------------------------------------------- ;;;
;;;  LISCPL - Lisp Player Client - proxy-ptz.lisp                                ;;;
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

(let ( (cmd-state (make-instance 'player-ptz-cmd :pan 0.0 :tilt 0.0 :zoom 1.309 :panspeed 0.0 :tiltspeed 0.0)) )

  ;; ** set ptz                                                               **
  (defmethod set-ptz ((ptz ptz-proxy) &key (pan 0.0 pp) (tilt 0.0 tp) (zoom 1.309 zp)
                                           (panspeed 0.0 psp) (tiltspeed 0.0 tsp)
                                      &allow-other-keys)
    (when (and pp (numberp pan))
      (setf (player-pan cmd-state) pan))
    (when (and tp (numberp tilt))
      (setf (player-tilt cmd-state) tilt))
    (when (and zp (numberp zoom))
      (setf (player-zoom cmd-state) zoom))
    (when (and psp (numberp panspeed))
      (setf (player-panspeed cmd-state) panspeed))
    (when (and tsp (numberp tiltspeed))
      (setf (player-tiltspeed cmd-state) tiltspeed))
    (proxy-command ptz *player-ptz-cmd-state* cmd-state))

)

;; *****************************************************************************
;; ** requests                                                                **
;; *****************************************************************************

;; ** get geometry                                                            **
(defmethod get-geometry ((ptz ptz-proxy))
  "Get geometry"
  (proxy-request ptz *player-ptz-req-geom* nil
    :response (make-instance 'player-ptz-geom)))

;; ** set control mode                                                        **
(labels ( (mode->int (mode)
            (cond ( (eq mode :velocity) *player-ptz-velocity-control* )
                  ( (eq mode :position) *player-ptz-position-control* )
                  ( T *player-ptz-position-control* ))) )

  (defmethod set-control-mode ((ptz ptz-proxy) mode)
    "Set control mode"
    (proxy-request ptz *player-ptz-req-control-mode*
      (make-instance 'player-ptz-req-control-mode :mode (mode->int mode))))

)

