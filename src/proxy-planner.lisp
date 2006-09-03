;;; ---------------------------------------------------------------------------- ;;;
;;;  LISCPL - Lisp Player Client - proxy-planner.lisp                            ;;;
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

(let ( (goal-cmd (make-instance 'player-planner-cmd)) )
  (labels ( (update (data x y az )
              (setf (player-px data) x)
              (setf (player-py data) y)
              (setf (player-pa data) az)) )

    ;; ** set planner goal cmd                                                **
    (defmethod set-goal ((pp planner-proxy) &key (x 0) (y 0) (az 0)
                                            &allow-other-keys)
      (update (player-goal goal-cmd) x y az)
      (proxy-command pp *player-planner-cmd-goal* goal-cmd))
  )
)

;; *****************************************************************************
;; ** requests                                                                **
;; *****************************************************************************

;; ** get waypints                                                            **
(defmethod get-waypoints ((pp planner-proxy))
  "Get waypoints"
  (proxy-request pp *player-planner-req-get-waypoints* nil
    :response (make-instance 'player-planner-waypoints-req)))

;; ** set enabled state                                                       **
(defmethod set-enabled ((pp planner-proxy) enabled)
  "Enable/disable the robot motion"
  (proxy-request pp *player-planner-req-enable*
    (make-instance 'player-planner-enable-req :state (nil-t->0-1-int enabled))))

