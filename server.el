;;; -*- lexical-binding: t; -*-

(require 'websocket)

(defvar *debug-queue* nil)
(defvar *opened-websocket* nil)
(defvar *current-connection-id* nil)
(defvar my-websocket-server nil)

;; NOTE When the connection is closed from the remote, we do not have to close
;; and reopen here. The server should keep listening to the next client
;; connection request. In case you want to quit listening, evaluate the
;; following.
(ignore-errors (websocket-server-close my-websocket-server))

(defun websocket-redisplay! ()
  "NOTE It is too slow."
  (websocket-send-text
   *opened-websocket*
   (json-encode-list (process-data (%%peekable-data)))))

;; FIXME Experimental and may slow down emacs indefinitely. 
;; (defvar *tmp* pre-redisplay-functions)
;; (push (lambda (window)
;;         (when (websocket-openp *opened-websocket*)
;;           (websocket-redisplay!)))
;;       pre-redisplay-functions)
;; (setf pre-redisplay-functions *tmp*)

;; FIXME Experimental and may slow down emacs indefinitely. 
;; This code takes care of redisplay whenever buffer text is changed.
(defvar *tmp2* after-change-functions)
(push (lambda (_ __ ___) (websocket-redisplay!))
      after-change-functions)
;; (setf after-change-functions *tmp2*)

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
         (websocket-redisplay!)
         ;; (websocket-send-text *opened-websocket* (json-encode-list (window-string-with-all-properties)))
         )

       :on-close
       (lambda (_websocket)
         (condition-case err
             (websocket-send-text *opened-websocket* "Goodbye.")
           (websocket-closed
            (message "Socket closed already. Message dropped.")))
         (message (format "\n[Connection %s]:" *current-connection-id*))
         (setf *current-connection-id* nil)
         (message (format-time-string "[%Y-%m-%d %H:%M:%S] Websocket closed."))
         (setq *opened-websocket* nil))))

;;;
