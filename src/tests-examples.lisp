;;; ---------------------------------------------------------------------------- ;;;
;;;  LISCPL - Lisp Player Client - tests-examples.lisp                           ;;;
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

(eval-when (:execute :compile-toplevel :load-toplevel)
  (add-logger 'lispcl-test-print-unsupported :stream '*standard-output*)
  (add-logger 'lispcl-test-print-ignored :stream '*standard-output*)
  (add-logger 'lispcl-blob-finder-blob-detected :stream '*standard-output*)
)

;; *****************************************************************************
;; *****************************************************************************
;; ** tests                                                                   **
;; *****************************************************************************
;; *****************************************************************************

;; *****************************************************************************
;; ** common test definitions                                                 **
;; *****************************************************************************

; test player client
(defvar *test-pc* nil)

; test accepted interfaces
(defvar *test-accepted-interfaces* nil)
;  '( ("blobfinder" . t)
;     ("laser" . t)
;     ("planner" . nil)
;     ("position2d" . t)
;     ("position3d" . (0 10))
;     ("sonar" . t) ))

; connect to proxies defined in *test-accepted-interfaces*
(defun connect-to-accepted-proxies ()
  (let*( (resp (get-device-list (get-player-proxy *test-pc*)))
         (devices (player-devices resp)) )
    (loop for ii from 0 to (1- (player-devices-count resp)) do
      (let*( (device (elt devices ii))
             (interface-string (interface-code->string (player-interf device)))
             (interface-accepted-p
               (or (null *test-accepted-interfaces*)
                   (rest (first (member interface-string *test-accepted-interfaces* :test #'string= :key #'first))))) )
        (cond ( (null interface-string)
                (log-msg lispcl-test-print-unsupported
                  "Interface '~a:~a' not supported~%" (player-interf device) (player-index device)) )
              ( (and interface-accepted-p
                     (or (eq interface-accepted-p t)
                         (member (player-index device) interface-accepted-p)))
                (make-instance
                  (intern (string-upcase (concatenate 'string interface-string "-proxy")) (find-package "LISPCL"))
                  :player-client *test-pc*
                  :index (player-index device)) )
              ( T
                (log-msg-fun 'lispcl-test-print-ignored
                  "Interface '~a:~a' ignored~%" interface-string (player-index device)) ))))))

; pc test stop
(defun pc-test-stop ()
  (disconnect *test-pc*))

;; *****************************************************************************
;; ** test player client threaded                                             **
;; *****************************************************************************

; read thread run method
(defmethod pc-test-read-run ((pc player-client))
  "read data from player server"
  (let ( (stream (slot-value pc 'connection))
         (pp (get-player-proxy pc))
         (request-data-status-after-sync-message nil) )
    (loop
      (cond ( (listen stream)
              (when (read-message pc)
                ; got sync message
                (thread-yield)
                ; set sync status
                (setf request-data-status-after-sync-message :received))
              ; if in pull mode request another round of data
              (when (and (pull-mode-p pc)
                         (member request-data-status-after-sync-message '(:received :other-request-pending)))
                (setf request-data-status-after-sync-message (request-data pp :wait nil))) )
            ( T
              (wait-for-input stream 1) )))))

; start player client threaded test
(defun pc-threaded-test-start (&key (host *player-default-host*)
                                    (port *player-default-port*)
                                    (thread-fun #'pc-test-read-run))
  ; connect to server
  (setf *test-pc* (make-instance 'player-client :host host :port port :thread-fun thread-fun))
  ; change to pull mode
  (set-datamode (get-player-proxy *test-pc*) *player-datamode-pull*)
  ; set replace rule for data messages
  (set-replace-rule (get-player-proxy *test-pc*) -1 -1 *player-msgtype-data* -1 T)
  ; connect to proxies
  (connect-to-accepted-proxies)
  ; set data handlers
  ;(set-data-handler (p2dp-get 0) *player-position2d-data-state* #'test-data-handler)
  ; request first round of data
  (request-data (get-player-proxy *test-pc*))
  *test-pc*)

;; *****************************************************************************
;; ** test player client non threaded                                         **
;; *****************************************************************************

; start player client non threaded test
(defun pc-test-start (&key (host *player-default-host*) (port *player-default-port*))
  ; connect to server
  (setf *test-pc* (make-instance 'player-client :host host :port port))
  ; change to pull mode
  (set-datamode (get-player-proxy *test-pc*) *player-datamode-pull*)
  ; set replace rule for data messages
  (set-replace-rule (get-player-proxy *test-pc*) -1 -1 *player-msgtype-data* -1 T)
  ; connect to proxies
  (connect-to-accepted-proxies)
  *test-pc*)

;; *****************************************************************************
;; ** test data handlers                                                      **
;; *****************************************************************************

; test data handler
(defmethod test-data-handler ((proxy std-proxy) (data player-standard-object))
  (format T "~a: ~a~%" (type-of proxy) data))

;; *****************************************************************************
;; *****************************************************************************
;; ** access functions                                                        **
;; *****************************************************************************
;; *****************************************************************************

; get player client object
(defun pc-get ()
  *test-pc*)

; get actarray proxy
(defun ap-get (&optional (index 0))
  (get-proxy *test-pc* *player-actarray-code* index))

; get blobfinder proxy
(defun bp-get (&optional (index 0))
  (get-proxy *test-pc* *player-blobfinder-code* index))

; get laser proxy
(defun lp-get (&optional (index 0))
  (get-proxy *test-pc* *player-laser-code* index))

; get planner proxy
(defun plp-get (&optional (index 0))
  (get-proxy *test-pc* *player-planner-code* index))

; get position2d proxy
(defun p2dp-get (&optional (index 0))
  (get-proxy *test-pc* *player-position2d-code* index))

; get position3d proxy
(defun p3dp-get (&optional (index 0))
  (get-proxy *test-pc* *player-position3d-code* index))

; get ptz proxy
(defun ptzp-get (&optional (index 0))
  (get-proxy *test-pc* *player-ptz-code* index))

; get sonar proxy
(defun sp-get (&optional (index 0))
  (get-proxy *test-pc* *player-sonar-code* index))

;; *****************************************************************************
;; *****************************************************************************
;; ** examples                                                                **
;; *****************************************************************************
;; *****************************************************************************

;; *****************************************************************************
;; ** space wanderer                                                          **
;; *****************************************************************************

;; ** space wanderer                                                          **
(defun space-wanderer (&key (timeout 60.0) (sleep-time 0.1) (threaded nil))
  ; connect to server and create player client
  (let ( (pc (make-instance 'player-client :thread-fun (when threaded #'pc-test-read-run))) )
    ; change to pull mode
    (set-datamode (get-player-proxy pc) *player-datamode-pull*)
    ; set replace rule for data messages (always get the newest data)
    (set-replace-rule (get-player-proxy pc) -1 -1 *player-msgtype-data* -1 T)
    ; connect to position2d proxy
    (make-instance 'position2d-proxy :player-client pc :index 0)
    ; connect to sonar proxy
    (let ( (sp (make-instance 'sonar-proxy :player-client pc :index 0)) )
      ; set sonar data handler
      (set-data-handler sp *player-sonar-data-ranges* #'space-wanderer-sonar-data-handler)
      ; run controller either threaded or non-threaded
      (cond ( threaded
              ; set global variable, so we can use pc-test-stop
              (setf *test-pc* pc)
              ; request first round of data (read thread will continue)
              (request-data (get-player-proxy pc)) )
            ( T
              ; initial timestamps
              (let ( (initial-player-server-timestamp (timestamp pc)) )
                ; main loop
                (loop
                  ; check timeout
                  (when (>= (- (timestamp pc) initial-player-server-timestamp) timeout)
                    (return))
                  ; read a round of data (if new sonar data arrives, the handler will automatically called)
                  (read-data pc)
                  ; sleep the given time
                  (sleep sleep-time))
                ; disconnect (all proxies will also be disconnected)
                (disconnect pc)) )))))

;; ** space wanderer sonar data handler                                       **
(let ( ; define minimum/maximum allowed values for the SONAR sensors
       (sonar-min-value 0.2)
       (sonar-max-value 5.0)
       ; define the threshold (any value under this is considered an obstacle)
       (sonar-threshold 0.5)
       ; define the wheel diameter (~example for a Pioneer 3 robot)
       (wheel-diameter 24.0)
       ; define the default rotational speed in rad/s
       (def-yaw-speed 0.50)
       ; local variables
       sonar-values left-side right-side x-speed yaw-speed )

  (defmethod space-wanderer-sonar-data-handler ((sp sonar-proxy) (sp-data player-sonar-data))
    (let ( (p2dp (get-proxy (player-client sp) *player-position2d-code* 0)) )
      ; read sonar values in interval [sonar-min-value; sonar-max-value]
      (setf sonar-values (map '(vector float)
                              #'(lambda (sv) (min (max sv sonar-min-value) sonar-max-value))
                              (subseq (player-ranges sp-data) 0 (player-ranges-count sp-data))))
      ; calculate averages of left and right side
      (setf left-side  (/ (+ (elt sonar-values 1) (elt sonar-values 2) #| (elt sonar-values 3) |#) 2 #| 3 |# 10)
            right-side (/ (+ (elt sonar-values 6) (elt sonar-values 5) #| (elt sonar-values 4) |#) 2 #| 3 |# 10))
      ; calculate the translational and rotational velocities
      (setf x-speed (* 0.5 (+ left-side right-side))
            yaw-speed (* (- left-side right-side) (/ 180 pi wheel-diameter)))
      ; set velocity
      (cond ( ; if the path is clear on the left OR on the right, use {x,yaw}speed
              (or (and (> (elt sonar-values 1) sonar-threshold)
                       (> (elt sonar-values 2) sonar-threshold)
                       (> (elt sonar-values 3) sonar-threshold))
                  (and (> (elt sonar-values 4) sonar-threshold)
                       (> (elt sonar-values 5) sonar-threshold)
                       (> (elt sonar-values 6) sonar-threshold)))
              (set-velocity p2dp :x x-speed :az yaw-speed) )
            ( ; if we have obstacles in front (both left and right), rotate
              T
              (set-velocity p2dp :az (* (if (< (elt sonar-values 0) (elt sonar-values 7)) -1.0 1.0) def-yaw-speed)) ))))

)

;; *****************************************************************************
;; ** wall follower                                                           **
;; *****************************************************************************

(defun wall-follower (&key (timeout 60.0) (sleep-time 0.1))
  (let ( ; define minimum/maximum allowed values for the SONAR sensors
         (sonar-min-value 0.2)
         (sonar-max-value 5.0)
         ; define the wall threshold
         (min-wall-threshold 0.3)
         (max-wall-threshold 0.4)
         ; define the default translational and rotational speeds
         (def-x-speed 0.2)
         (def-yaw-speed 0.15)
         ; player client, position2d proxy, and sonar proxy
         pc p2dp sp
         ; data references
         sp-data
         ; initial timestamps
         (initial-player-server-timestamp 0.0)
         (sp-timestamp 0.0)
         ; local variables
         sonar-values left-side front-side x-speed yaw-speed )
    ; auxilliary functions
    (labels ( (get-sonar-data ()
                ; sleep the given time
                (sleep sleep-time)
                ; read data
                (read-data pc)
                ; timout ?
                (when (< (- (timestamp pc) initial-player-server-timestamp) timeout)
                  (cond ( ; new sonar data?
                          (> (timestamp sp) sp-timestamp)
                          (setf sp-timestamp (timestamp sp))
                          ; read sonar values in interval [sonar-min-value; sonar-max-value]
                          (setf sonar-values (map '(vector float)
                                                  #'(lambda (sv) (min (max sv sonar-min-value) sonar-max-value))
                                                  (subseq (player-ranges sp-data) 0 (player-ranges-count sp-data))))
                          ; calculate minimal left and front disntance
                          (setf left-side  (min (elt sonar-values 0) (elt sonar-values 1) (elt sonar-values 2))
                                front-side (min (elt sonar-values 3) (elt sonar-values 4)))
                          t )
                        ( ; no new sonar data -> read another round of data
                          T
                          (get-sonar-data) )))) )
      ;;; intialization
      ; connect to server and create player client
      (setf pc (make-instance 'player-client))
      ; change to pull mode
      (set-datamode (get-player-proxy pc) *player-datamode-pull*)
      ; set replace rule for data messages (always get the newest data)
      (set-replace-rule (get-player-proxy pc) -1 -1 *player-msgtype-data* -1 T)
      ; connect to position2d proxy
      (setf p2dp (make-instance 'position2d-proxy :player-client pc :index 0))
      ; connect to sonar proxy and set data reference
      (setf sp (make-instance 'sonar-proxy :player-client pc :index 0))
      (setf sp-data (get-data sp *player-sonar-data-ranges*))
      ; save initial player server timestamp
      (setf initial-player-server-timestamp (timestamp pc))
      ;;; controller
      ;; go to a wall
      ; if the robot is in open space, go ahead until it "sees" the wall
      (loop
        (when (or (not (get-sonar-data))
                  (<= left-side max-wall-threshold)
                  (<= front-side max-wall-threshold))
          (return))
        (set-velocity p2dp :x def-x-speed))
      ; rotate until we get a smaller value in sonar 0
      (let ( (previous-left-side (elt sonar-values 0)) )
        (loop
          (when (or (not (get-sonar-data))
                    (> (elt sonar-values 0) previous-left-side))
            (return))
          (setf previous-left-side (elt sonar-values 0))
          (set-velocity p2dp :az (if (= (min left-side front-side) front-side)
                                   (* -3.0 def-yaw-speed)
                                   (- def-yaw-speed)))))
      ;; follow wall
      (loop
        (unless (get-sonar-data)
          (return))
        (cond ( ; if we're getting too close to the wall with the front side back up a little bit if we're bumping in front
                (< front-side max-wall-threshold)
                (setf x-speed -0.1
                      yaw-speed (* -4.0 def-yaw-speed)) )
              ( ; if we're getting too close to the wall with the left side
                (< left-side min-wall-threshold)
                (setf x-speed (* 0.5 def-x-speed)
                      yaw-speed (- def-yaw-speed)) )
              ( ; if we're getting too far away from the wall with the left side...
                (> left-side max-wall-threshold)
                (setf x-speed (* 0.5 def-x-speed)
                      yaw-speed def-yaw-speed) )
              ( ; by default just move on
                T
                (setf x-speed def-x-speed
                  yaw-speed 0.0) ))
        (set-velocity p2dp :x x-speed :az yaw-speed))
      ; disconnect (all proxies will also be disconnected)
      (disconnect pc))))

;; *****************************************************************************
;; ** blob finder                                                             **
;; *****************************************************************************

;; ** blob finder                                                             **
(defun blob-finder (&key (timeout 60.0) (sleep-time 0.1) (threaded nil))
  ; connect to server and create player client
  (let ( (pc (make-instance 'player-client :thread-fun (when threaded #'pc-test-read-run))) )
    ; change to pull mode
    (set-datamode (get-player-proxy pc) *player-datamode-pull*)
    ; set replace rule for data messages (always get the newest data)
    (set-replace-rule (get-player-proxy pc) -1 -1 *player-msgtype-data* -1 T)
    ; connect to position2d proxy
    (make-instance 'position2d-proxy :player-client pc :index 0)
    ; connect to sonar proxy
    (let ( (sp (make-instance 'sonar-proxy :player-client pc :index 0))
           (bp (make-instance 'blobfinder-proxy :player-client pc :index 0)) )
      ; set data handlers
      (set-data-handler sp *player-sonar-data-ranges* #'space-wanderer-sonar-data-handler)
      (set-data-handler bp *player-blobfinder-data-blobs* #'blob-finder-data-handler)
      ; run controller either threaded or non-threaded
      (cond ( threaded
              ; set global variable, so we can use pc-test-stop
              (setf *test-pc* pc)
              ; request first round of data (read thread will continue)
              (request-data (get-player-proxy pc)) )
            ( T
              ; initial timestamps
              (let ( (initial-player-server-timestamp (timestamp pc)) )
                ; main loop
                (loop
                  ; check timeout
                  (when (>= (- (timestamp pc) initial-player-server-timestamp) timeout)
                    (return))
                  ; read a round of data (data handlers if new sonar or blobfinder data arrives will automatically called)
                  (read-data pc)
                  ; sleep the given time
                  (sleep sleep-time))
                ; disconnect (all proxies will also be disconnected)
                (disconnect pc)) )))))

;; ** blob finder data handler                                                **
(let ( blob )

  (defmethod blob-finder-data-handler ((bp blobfinder-proxy) (bp-data player-blobfinder-data))
    (log-msg lispcl-blob-finder-blob-detected "Blobs detected: ~a~%" (player-blobs-count bp-data))
    (loop for ii from 0 to (1- (player-blobs-count bp-data)) do
      (setf blob (elt (player-blobs bp-data) ii))
      (log-msg lispcl-blob-finder-blob-detected
        "  ~a: area=~a coords=(~a,~a)->(~a,~a) center=(~a,~a)~%"
        (1+ ii) (player-area blob)
        (player-right blob) (player-top blob)
        (player-left blob) (player-bottom blob)
        (player-x blob) (player-y blob))))

)
