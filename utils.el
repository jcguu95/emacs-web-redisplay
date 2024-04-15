;; This file is meant to be evaluated by emacsclient.

(defun peekable-string-and-properties ()
  "Return the segment of the buffer string that is peekable in the
current window."
  (let* ((window (car (window-list)))
         (from (window-start window))
         (to   (window-end   window))
         (buffer (window-buffer window)))
    (with-current-buffer buffer
      (let ((string (substring (buffer-string) (1- from))))
        (setf string (substring string 0 (min (- to from) (length string))))
        (let* ((plain-string (substring-no-properties string))
               (properties (object-intervals string)))
          (cons plain-string
                (mapcar (lambda (p)
                          (setf (nth 2 p) (ht<-plist (nth 2 p)))
                          p)
                        properties)))))))

(defun peekable-overlay-properties ()
  (let* ((window (car (window-list)))
         (buffer (window-buffer window)))
    (with-current-buffer buffer
      (mapcar (lambda (overlay)
                (list (overlay-start overlay)
                      (overlay-end overlay)
                      (ht<-plist (overlay-properties overlay))))
              (car (overlay-lists))))))


(defun json-string<-all-peekable ()
  "Serialize the peekable text, its text properties, and the overlay
properties of the current buffer to JSON."
  (let* ((sp (peekable-string-and-properties))
         (text (car sp))
         (text-properties (cdr sp))
         (overlay-properties (peekable-overlay-properties)))
    ;; NOTE I don't think json-*.el is smart enough. So I to break object
    ;; down, jsonize, glue it back.
    (format "{%s, %s, %s}"
            (format "\"text\": %s" (json-encode text))
            (format "\"text-properties\": [%s]"
                    (let ((result "")
                          (xs (mapcar #'json-encode-list text-properties)))
                      (cl-loop for x in xs
                               for n from 1
                               do (setf result (concat result x))
                               do (unless (= n (length xs))
                                    (setf result (concat result ","))))
                      result))
            (format "\"overlay-properties\": [%s]"
                    (let ((result "")
                          (xs (mapcar #'json-encode-list overlay-properties)))
                      (cl-loop for x in xs
                               for n from 1
                               do (setf result (concat result x))
                               do (unless (= n (length xs))
                                    (setf result (concat result ","))))
                      result)))))

;; TODO Test - This should not return any error.
(json-parse-string (json-string<-all-peekable))

;;; Note

;; Invisible Text https://www.gnu.org/software/emacs/manual/html_node/elisp/Invisible-Text.html
;; e.g. buffer-invisibility-spec
;;
;; See the source code of #'org-fold-core-region  -- org-fold may be using this to fold stuff.
;; Notice also that there are at least two possible values of org-fold-core-style: 'overlays and 'text-properties.


;; Temporary Display
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Temporary-Displays.html


;; Truncation
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Truncation.html
