;;; ---------------------------------------------------------------------------- ;;;
;;;  LISCPL - Lisp Player Client - player client                                 ;;;
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
;; *****************************************************************************
;; ** player client                                                           **
;; *****************************************************************************
;; *****************************************************************************

;; *****************************************************************************
;; ** connection handling                                                     **
;; *****************************************************************************

;; ** initialize instance player client                                       **
(defmethod initialize-instance :after ((pc player-client)
                                       &key (thread-fun nil) &allow-other-keys)
  "Make a player client and connect it as indicated."
  ; read banner from stream
  (labels ( (read-banner (xdr-stream)
              (coerce
                (mapcar #'code-char (remove 0 (xdr-read-fixed-opaque *player-ident-strlen* xdr-stream :type 'list)))
                'string)) )
    ; set player client values
    (setf (slot-value pc 'connection) (open-binary-connection (host pc) (port pc)))
    (setf (slot-value pc 'xdr-stream) (make-wrapper-xdr-stream (slot-value pc 'connection)))
    (setf (slot-value pc 'banner) (read-banner (slot-value pc 'xdr-stream)))
    ; create player proxy (will be added to slot proxies in player-client automatically
    (make-instance 'player-proxy :player-client pc)
    ; create thread
    (setf (slot-value pc 'read-thread) (when thread-fun
                                         (funcall *thread-creation-fun* "player-client-read-thread" thread-fun pc)))
    nil))

;; ** disconnect (player-client)                                              **
(defmethod disconnect ((pc player-client))
  "disconnect from player server"
  ; wait till no response is pending for player proxy
  (when (and (slot-value pc 'read-thread)
             (member (slot-value (get-player-proxy pc) 'request-status) '(:waiting :ignoring)))
    (thread-wait "waiting for other request to end"
      #'(lambda () (not (member (request-status (get-player-proxy pc)) '(:waiting :ignoring))))))
  ; disconnect proxies
  (mapcar #'disconnect (slot-value pc 'proxies))
  ; kill read thread
  (when (slot-value pc 'read-thread)
    (kill-thread (slot-value pc 'read-thread)))
  ; close stream
  (log-msg lispcl-init-close "closing player connection~%")
  (when (open-stream-p (slot-value pc 'connection))
    (close (slot-value pc 'connection))))

;; ** connected-p (player-client)                                             **
(defmethod connected-p ((pc player-client))
  "check if we are connected to the player server"
  (open-stream-p (slot-value pc 'connection)))

;; *****************************************************************************
;; read and send messages                                                     **
;; *****************************************************************************

;; ** read message                                                            **
(let ( (header (make-instance 'player-msghdr)) )

  (defmethod read-message ((pc player-client))
    "Read message from the server. This method is only intended for direct use in threaded mode."
    ; are we connected?
    (unless (connected-p pc)
      (throw-warn "lispcl:read-message in player-client.lisp" "player-client not connected")
      (return-from read-message))
    ; read message
    (xdr-read header (slot-value pc 'xdr-stream))
    (log-msg lispcl-read-message "header ~a~%" header)
    ; update player client timestamp to server timestamp
    (when (> (- (player-timestamp header) (slot-value pc 'timestamp)) 0)
      (setf (slot-value pc 'timestamp) (player-timestamp header)))
    ; handle message
    (cond (     ; data message
            (or (= (player-type header) *player-msgtype-data*)
                ; response acknowledge message
                (= (player-type header) *player-msgtype-resp-ack*)
                (= (player-type header) *player-msgtype-resp-nack*))
            (let ( (pp (get-proxy pc (player-interf (player-addr header)) (player-index (player-addr header)))) )
              ; is proxy valid
              (when (null pp)
                (throw-warn "lispcl:read-message in player-client.lisp"
                            "received unexpected message (type ~a) from disconnected proxy ~a:~a"
                            (player-type header)
                            (interface-code->string (player-interf (player-addr header)))
                            (player-index (player-addr header)))
                ; read ignored data
                (xdr-read-fixed-opaque (player-size header) (slot-value pc 'xdr-stream))
                (return-from read-message))
              (cond ( ; handle data message
                      (= (player-type header) *player-msgtype-data*)
                      (let ( (data (get-data pp (player-subtype header)))
                             (data-handler (get-data-handler pp (player-subtype header))) )
                        (cond ( data
                                ; update proxy data
                                (xdr-read data (slot-value pc 'xdr-stream))
                                ; update proxy times
                                (setf (slot-value pp 'timestamp) (player-timestamp header))
                                ; call new data handler
                                (when data-handler
                                  (funcall data-handler pp data))
                                ; print info
                                (log-msg lispcl-read-message "data ~a~%" data) )
                              ( T
                                (throw-warn "lispcl:read-message in player-client.lisp"
                                  "received data type ~a, but could not write data to proxy ~a"
                                  (player-subtype header) pp)
                                (xdr-read-fixed-opaque (player-size header) (slot-value pc 'xdr-stream)) ))) )
                    ( ; handle response message
                      T
                      (cond ( (eq (slot-value pp 'request-status) :waiting)
                              ; transfer response to proxy
                              (setf (slot-value pp 'response-header) header)
                              ; update request-status slot
                              (setf (slot-value pp 'request-status) :response)
                              ; in threaded mode let other thread handle response
                              (when (slot-value pc 'read-thread)
                                (thread-yield)) )
                            ( T ; :ignoring
                              ; read ignored data
                              (xdr-read-fixed-opaque (player-size header) (slot-value pc 'xdr-stream))
                              (if (eq (slot-value pp 'request-status) :ignoring)
                                (log-msg lispcl-read-message "resp :ingored~%")
                                (throw-warn "lispcl:read-message in player-client.lisp"
                                  "received response ~a for proxy ~a but message body was ignored."
                                  (player-subtype header) pp))
                              ; reset request-status slot
                              (setf (slot-value pp 'request-status) :none) )) ))) )
          ( ; sync message
            (= (player-type header) *player-msgtype-synch*)
            (log-msg lispcl-read-message "****~%") )
          ( T
            (throw-warn "lispcl:read-message in player-client.lisp"
                        "received unexpected message (type ~a) from proxy ~a:~a"
                        (player-type header)
                        (interface-code->string (player-interf (player-addr header)))
                        (player-index (player-addr header)))
            (return-from read-message) ))
    ; return value
    (= (player-type header) *player-msgtype-synch*))

)

;; ** read data messages                                                      **
(defmethod read-data ((pc player-client))
  "Read message from the server. This method is intended for direct use in non threaded mode."
  (let ( (player-proxy (get-player-proxy pc))
         (sync-message-received nil) )
    ; in pull mode request another round of data
    (when (pull-mode-p pc)
      (request-data (get-player-proxy pc) :wait nil))
    ; in pull mode return after synchronization message, otherwise return after each message
    (loop
      (setf sync-message-received (or (read-message pc) sync-message-received))
      (when (or (not (pull-mode-p pc))
                (and sync-message-received (eq (request-status player-proxy) :none)))
        (return)))))

;; ** send message                                                            **
(let ( (header (make-instance 'player-msghdr)) )

  (defmethod send-message ((pc player-client) (bp base-proxy) type subtype data)
    "Send message (command or request) to the server. This method is not intended for direct use."
    ; are we connected?
    (when (not (connected-p pc))
      (throw-warn "lispcl:send-message in player-client.lisp" "player-client not connected")
      (return-from send-message))
    ; set header
    (setf (player-addr header) (devaddr bp))
    (setf (player-type header) type)
    (setf (player-subtype header) subtype)
    (setf (player-timestamp header) (float (get-universal-time)))
    (setf (player-seq header) 0)
    (setf (player-size header) (if data (get-size data) 0))
    ; print message
    (log-msg lispcl-send-message "send ~a~%     ~a~%" header data)
    ; write header
    (xdr-write header (slot-value pc 'xdr-stream))
    ; write payload
    (when data
      (xdr-write data (slot-value pc 'xdr-stream)))
    (funcall (xdr-stream-sync-fn (slot-value pc 'xdr-stream))))

)

;; ** send a proxy command                                                    **
(defmethod proxy-command ((bp base-proxy) subtype cmd)
  "send the command to player server"
  ; send command
  (send-message (player-client bp) bp *player-msgtype-cmd* subtype cmd))

;; ** send a proxy request and wait for or ignore response                    **
(defmethod proxy-request ((bp base-proxy) subtype req &key (response nil) (wait T))
  "send the request to player server and wait for response if wait == T"
  (cond ( ; in threaded mode wait for other requests to end and set own request status (but only if current-thread != read-thread)
          (slot-value (player-client bp) 'read-thread)
          (cond ( (eq (current-thread) (slot-value (player-client bp) 'read-thread))
                  (without-scheduling
                    (if (member (request-status bp) '(:waiting :ignoring :response))
                      (return-from proxy-request :other-request-pending)
                      (setf (slot-value bp 'request-status) (if wait :waiting :ignoring)))) )
                ( T
                  (thread-wait "waiting for other request to end"
                    #'(lambda () (when (not (member (request-status bp) '(:waiting :ignoring :response)))
                                   (setf (slot-value bp 'request-status) (if wait :waiting :ignoring))
                                   T))) )) )
        ( ; set waiting for response status in non threaded mode
          T
          (setf (slot-value bp 'request-status) (if wait :waiting :ignoring)) ))
  ; send request
  (send-message (player-client bp) bp *player-msgtype-req* subtype req)
  ; wait for response
  (when wait
    (cond ( ; threaded mode
            (slot-value (player-client bp) 'read-thread)
            (thread-wait "waiting for player server response"
              #'(lambda () (eq (request-status bp) :response))) )
          ( ; non threaded mod
            T
            (loop
              (read-message (player-client bp))
              (when (eq (request-status bp) :response)
                (return))) ))
    ; handle response
    (let ( (header (response-header bp)) )
      (prog1
        (cond ( (/= subtype (player-subtype header))
                (throw-warn "lispcl:proxy-request in proxy--base.lisp"
                            "Proxy ~a received wrong response subtype: received ~a waited-for ~a"
                            bp
                            (player-subtype header)
                            subtype) )
              ( (= (player-type header) *player-msgtype-resp-nack*)
                (throw-warn "lispcl:proxy-request in player-client.lisp"
                            "Proxy ~a received negative acknowledgment response message for request subtype ~a~%"
                            bp subtype)
                (when (> (player-size header) 0)
                  (xdr-read-fixed-opaque (player-size header) (slot-value (player-client bp) 'xdr-stream)))
                nil )
              ( T
                ; read payload
                (prog1
                  (cond ( (and response (zerop (player-size header)))
                          nil )
                        ( response
                          (xdr-read response (slot-value (player-client bp) 'xdr-stream)) )
                        ( T
                          (unless (zerop (player-size header))
                            (xdr-read-fixed-opaque (player-size header) (slot-value (player-client bp) 'xdr-stream))
                            (throw-warn "lispcl:proxy-request in proxy--base.lisp"
                                        "response ignored for proxy ~a with request subtype ~a"
                                        bp
                                        subtype))
                          T ))
                  ; print info
                  (log-msg lispcl-read-message "resp ~a~%" response)) ))
        ; set response status
        (setf (slot-value bp 'request-status) :none)))))

;; *****************************************************************************
;; proxy handling                                                             **
;; *****************************************************************************

;; ** add proxy                                                               **
(defmethod add-proxy ((pc player-client) (pp base-proxy))
  "add a proxy"
  (push pp (slot-value pc 'proxies)))

;; ** get proxy                                                               **
(defmethod get-proxy ((pc player-client) (interface integer) (index integer))
  "Get a proxy specified by its device an device-index. Returns the found proxy
   or NIL if there is no such proxy"
  (loop for proxy in (slot-value pc 'proxies)
    when (and (= interface (player-interf (devaddr proxy)))
              (= index (player-index (devaddr proxy))))
    do (return-from get-proxy proxy))
  nil)

;; ** get player proxy                                                       **
(defmethod get-player-proxy ((pc player-client))
  "Get the player proxy"
  (get-proxy pc *player-player-code* 0))

;; ** remove proxy                                                            **
(defmethod remove-proxy ((pc player-client) (pp base-proxy))
  "remove proxy"
  (setf (slot-value pc 'proxies) (delete pp (slot-value pc 'proxies))))

