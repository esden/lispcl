;;; ---------------------------------------------------------------------------- ;;;
;;;  LISCPL - Lisp Player Client - proxy-position2d.lisp                         ;;;
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

(let ( (vel-cmd (make-instance 'player-position2d-cmd-vel))
       (pos-cmd (make-instance 'player-position2d-cmd-pos)) )
  (labels ( (update-pos-vel (data x y z)
              (setf (player-px data) x)
              (setf (player-py data) y)
              (setf (player-pa data) z)) )

    ;; ** set velocity                                                        **
    (defmethod set-velocity ((pp position2d-proxy) &key (x 0.0) (y 0.0) (az 0.0)
                                                        (state T)
                                                   &allow-other-keys)
      (update-pos-vel (player-vel vel-cmd) x y az)
      (setf (player-state vel-cmd) (if state 1 0))
      (proxy-command pp *player-position2d-cmd-vel* vel-cmd))

    ;; ** set pose                                                            **
    (defmethod set-pose ((pp position2d-proxy) &key (x 0.0) (y 0.0) (az 0.0)
                                                    (vx 0.0) (vy 0.0) (vaz 0.0)
                                                    (state T)
                                               &allow-other-keys)
      (update-pos-vel (player-pos pos-cmd) x y az)
      (update-pos-vel (player-vel pos-cmd) vx vy vaz)
      (setf (player-state pos-cmd) (if state 1 0))
      (proxy-command pp *player-position2d-cmd-pos* pos-cmd))

  )
)

;; *****************************************************************************
;; ** requests                                                                **
;; *****************************************************************************

;; ** get geometry                                                            **
(defmethod get-geometry ((pp position2d-proxy))
  "Get geometry"
  (proxy-request pp *player-position2d-req-get-geom* nil
    :response (make-instance 'player-position2d-geom)))

;; ** set motor state                                                         **
(defmethod set-motor-state ((pp position2d-proxy) state &key &allow-other-keys)
  "Set motor state"
  (proxy-request pp *player-position2d-req-motor-power*
    (make-instance 'player-position2d-power-config :state (nil-t->0-1-int state))))

;; ** set velcocity mode                                                      **
(defmethod set-velocity-mode ((pp position2d-proxy) value &key &allow-other-keys)
  "Set velocity mode"
  (proxy-request pp *player-position2d-req-velocity-mode*
    (make-instance 'player-position2d-velocity-mode-config :value value)))

;; ** set position mode                                                       **
(defmethod set-position-mode ((pp position2d-proxy) state &key &allow-other-keys)
  "Set position mode"
  (proxy-request pp *player-position2d-req-position-mode*
    (make-instance 'player-position2d-position-mode-req :state (nil-t->0-1-int state))))

;; ** reset odometry                                                          **
(defmethod reset-odometry ((pp position2d-proxy) &key &allow-other-keys)
  "Reset odometry"
  (proxy-request pp *player-position2d-req-reset-odom*
    (make-instance 'player-position2d-reset-odom-config)))

;; ** set odometry                                                            **
(defmethod set-odometry ((pp position2d-proxy) &key (x 0.0) (y 0.0) (az 0.0) &allow-other-keys)
  "Set odometry"
  (proxy-request pp *player-position2d-req-set-odom*
    (make-instance 'player-position2d-set-odom-req
      :pose (make-instance 'player-pose :px x :py y :pa az))))

;; ** set velocity pid parameters                                             **
(defmethod set-velocity-pid-params ((pp position2d-proxy) &key (kp 1.0) (ki 1.0) (kd 1.0) &allow-other-keys)
  "Set velocity pid parameters"
  (proxy-request pp *player-position2d-req-speed-pid*
    (make-instance 'player-position2d-speed-pid-req :kp kp :ki ki :kd kd)))

;; ** set position pid parameters                                             **
(defmethod set-position-pid-params ((pp position2d-proxy) &key (kp 1.0) (ki 1.0) (kd 1.0) &allow-other-keys)
  "Set position pid parameters"
  (proxy-request pp *player-position2d-req-position-pid*
    (make-instance 'player-position2d-position-pid-req :kp kp :ki ki :kd kd)))

;; ** set linear velocity profile parameters                                  **
(defmethod set-velocity-profile-params ((pp position2d-proxy) &key (velocity 0.0) (acc 0.0) &allow-other-keys)
  "Set linear velocity profile parameters"
  (proxy-request pp *player-position2d-req-speed-prof*
    (make-instance 'player-position2d-speed-prof-req :speed velocity :acc acc)))

