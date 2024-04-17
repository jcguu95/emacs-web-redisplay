;;; -*- lexical-binding: t; -*-

(require 'websocket)

(defvar *debug-queue* nil)
(defvar *opened-websocket* nil)
(defvar *current-connection-id* nil)

;; NOTE When the connection is closed from the remote, we do not have to close
;; and reopen here. The server should keep listening to the next client
;; connection request. In case you want to quit listening, evaluate the
;; following.
(websocket-server-close my-websocket-server)

;; TODO If connection can't be opened, try closing the previous ones and do it
;; again. If it still gets wrong, a serios error should be signaled.
(setq my-websocket-server
      (websocket-server
       3000
       :host 'local
       :on-open
       (lambda (_websocket)
         (setf *current-connection-id* (format-time-string "%Y-%m-%d %H:%M:%S"))
         (message (format "[%s] Websocket opened." *current-connection-id*))
         (setq *opened-websocket* _websocket))

       :on-message
       (lambda (_websocket frame)
         ;; (push (list 'message frame) *debug-queue*)
         ;; (message (format "\n[Connection %s]:" *current-connection-id*))
         ;; (message (format-time-string "[%Y-%m-%d %H:%M:%S] Received message through websocket:"))
         ;; (message (format "> text: %s" (websocket-frame-text frame)))                ;
         (let ((key (websocket-frame-text frame)))
           ;; (message (format "%S" key))
           ;; FIXME Handle conditions carefully here.
           (setq unread-command-events (listify-key-sequence (kbd key))))
         ;; (websocket-send-text *opened-websocket* "Hello from emacs!")
         (websocket-send-text *opened-websocket* (json-encode-list (process-data (%%peekable-data))))
         ;; (websocket-send-text *opened-websocket* (json-encode-list (window-string-with-all-properties)))
         )

       :on-close
       (lambda (_websocket)
         (websocket-send-text *opened-websocket* "Goodbye.")
         (message (format "\n[Connection %s]:" *current-connection-id*))
         (setf *current-connection-id* nil)
         (message (format-time-string "[%Y-%m-%d %H:%M:%S] Websocket closed."))
         (setq *opened-websocket* nil))))

;;;
