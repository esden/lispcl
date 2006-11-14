;;; ---------------------------------------------------------------------------- ;;;
;;;  LISCPL - Lisp Player Client - proxy-actarray.lisp                           ;;;
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

(let ( (home-cmd (make-instance 'player-actarray-home-cmd))
       (pos-cmd (make-instance 'player-actarray-position-cmd))
       (multi-pos-cmd (make-instance 'player-actarray-multi-position-cmd :positions-count *player-actarray-num-actuators*))
       (speed-cmd (make-instance 'player-actarray-speed-cmd))
       (multi-speed-cmd (make-instance 'player-actarray-multi-speed-cmd :speeds-count *player-actarray-num-actuators*))
       (current-cmd (make-instance 'player-actarray-current-cmd))
       (multi-current-cmd (make-instance 'player-actarray-multi-current-cmd :currents-count *player-actarray-num-actuators*)) )

  ;; ** home command                                                          **
  (defmethod set-home ((ap actarray-proxy) &key (joint nil) &allow-other-keys)
    (setf (player-joint home-cmd) (if (integerp joint) joint -1))
    (proxy-command ap *player-actarray-home-cmd* home-cmd))

  ;; ** set position                                                          **
  ; single: (set-position ap :value 80deg :joint 0)
  ; multi:  (set-position ap :value '( (0 . 90deg) (2 . 70deg) (3 . 70deg) (4 . 70deg) (5 . 42deg) ))
  (defmethod set-position ((ap actarray-proxy) &key (joint nil) (value nil) &allow-other-keys)
    (cond ( (integerp joint)
            (setf (player-joint pos-cmd) joint)
            (setf (player-position pos-cmd) (nil-t->0-1 value))
            (proxy-command ap *player-actarray-pos-cmd* pos-cmd) )
          ( T
            (loop for ii from 0 to (1- *player-actarray-num-actuators*) do
              (setf (elt (player-positions multi-pos-cmd) ii) (nil-t->0-1 (rest (assoc ii value)))))
            (proxy-command ap *player-actarray-multi-pos-cmd* multi-pos-cmd) )))

  ;; ** set velocity                                                          **
  (defmethod set-velocity ((ap actarray-proxy) &key (joint nil) (value nil) &allow-other-keys)
    (cond ( (integerp joint)
            (setf (player-joint speed-cmd) joint)
            (setf (player-speed speed-cmd) (nil-t->0-1 value))
            (proxy-command ap *player-actarray-speed-cmd* speed-cmd) )
          ( T
            (loop for ii from 0 to (1- *player-actarray-num-actuators*) do
              (setf (elt (player-speeds multi-speed-cmd) ii) (nil-t->0-1 (rest (assoc ii value)))))
            (proxy-command ap *player-actarray-multi-speed-cmd* multi-speed-cmd) )))

  ;; ** set current                                                           **
  (defmethod set-current ((ap actarray-proxy) &key (joint nil) (value nil) &allow-other-keys)
    (cond ( (integerp joint)
            (setf (player-joint current-cmd) joint)
            (setf (player-current current-cmd) (nil-t->0-1 value))
            (proxy-command ap *player-actarray-current-cmd* current-cmd) )
          ( T
            (loop for ii from 0 to (1- *player-actarray-num-actuators*) do
              (setf (elt (player-currents multi-current-cmd) ii) (nil-t->0-1 (rest (assoc ii value)))))
            (proxy-command ap *player-actarray-multi-current-cmd* multi-current-cmd) )))

)

;; *****************************************************************************
;; ** requests                                                                **
;; *****************************************************************************

;; ** set motor state                                                         **
(defmethod set-motor-state ((ap actarray-proxy) state &key &allow-other-keys)
  "Set motor state"
  (proxy-request ap *player-actarray-power-req*
    (make-instance 'player-actarray-power-config :value (nil-t->0-1-int state))))

;; ** set brakes state                                                        **
(defmethod set-brakes-state ((ap actarray-proxy) state &key &allow-other-keys)
  "Set brakes"
  (proxy-request ap *player-actarray-brakes-req*
    (make-instance 'player-actarray-brakes-config :value (nil-t->0-1-int state))))

;; ** set velocity param                                                      **
(defmethod set-velocity-param ((ap actarray-proxy) joint velocity &key &allow-other-keys)
  "Set velocity param"
  (proxy-request ap *player-actarray-speed-req*
    (make-instance 'player-actarray-speed-config :joint joint :speed velocity)))

;; ** set acceleration param                                                  **
(defmethod set-acceleration-param ((ap actarray-proxy) joint acceleration &key &allow-other-keys)
  "Set velocity param"
  (proxy-request ap *player-actarray-accel-req*
    (make-instance 'player-actarray-accel-config :joint joint :accel acceleration)))

