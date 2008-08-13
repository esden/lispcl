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

(let ( (home-cmd (make-instance 'player-limb-home-cmd))
       (pose-cmd (make-instance 'player-limb-setpose-cmd)) )
  (labels ( (update-point3d (data x y z)
              (setf (player-px data) x)
              (setf (player-py data) y)
              (setf (player-pz data) z)) )

    ;; ** home command                                                          **
    (defmethod set-home ((lp limb-proxy) &key &allow-other-keys)
      (proxy-command lp *player-limb-home-cmd* home-cmd))

    ;; ** set pose pao                                                          **
    (defmethod set-pose-pao ((lp limb-proxy) &key (px 0.0) (py 0.0) (pz 0.0)
                                                  (ax 0.0) (ay 0.0) (az 0.0)
                                                  (ox 0.0) (oy 0.0) (oz 0.0)
                                                  &allow-other-keys)
      (update-point3d (player-position pose-cmd) px py pz)
      (update-point3d (player-approach pose-cmd) ax ay az)
      (update-point3d (player-orientation pose-cmd) ox oy oz)
      (proxy-command lp *player-limb-setpose-cmd* pose-cmd))

    ;; ** set pose                                                              **
    (defmethod set-pose ((lp limb-proxy) &key (x 0.0) (y 0.0) (z 0.0)
                                              (qu 0.0) (qx 0.0) (qy 0.0) (qz 0.0)
                                              &allow-other-keys)
      (multiple-value-bind (app ori) (quaternion->frame qu qx qy qz)
        (set-pose-pao lp :px x :py y :pz z :ax (player-px app) :ay (player-py app) :az (player-pz app)
                                           :ox (player-px ori) :oy (player-py ori) :oz (player-pz ori))))

    ;; ** set pose euler                                                        **
    (defmethod set-pose-euler ((lp limb-proxy) &key (x 0.0) (y 0.0) (z 0.0)
                                                    (ax 0.0) (ay 0.0) (az 0.0)
                                              &allow-other-keys)
      (set-pose-pao lp :px x :py y :pz z :ox ax :oy ay :oz az))

  )
)

;; *****************************************************************************
;; ** requests                                                                **
;; *****************************************************************************

