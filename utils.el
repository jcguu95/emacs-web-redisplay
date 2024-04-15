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

;;; Interval Properties Update

(cl-defun %push-interval-properties
    (int-prop int-props &optional (add-method (lambda (new-prop old-prop)
                                                 (append new-prop old-prop))))
  "
An interval property is assumed to be a list of length three: (n
m x), where n < m are integers, and x is a lisp datum. INT-PROP
is of type interval property, whereas INT-PROPS is a list of
inverval properties (.. (n m x) (m k y) (k l z) ..).

This function returns a newly created list of interval
properties. The returned object is obtained from \"pushing\"
INT-PROP into INT-PROPS. For example,

  (%push-interval-properties '(2 7 plist-new)
                             '((1 4 plist-0) (4 6 plist-1) (6 9 plist-2))) =>
    ((1 2 plist-0)
     (2 4 (append plist-new plist-0))
     (4 6 (append plist-new plist-1))
     (6 7 (append plist-new plist-2))
     (7 9 plist-2))
"
  (let ((start (nth 0 int-prop))
        (end (nth 1 int-prop))
        (new-prop (nth 2 int-prop))
        (result nil))
    (cl-loop
     for int-prop- in int-props
     do (let ((start- (nth 0 int-prop-))
              (end- (nth 1 int-prop-))
              (prop- (nth 2 int-prop-)))
          (cond
           ;; ()[   ]
           ((<= end start-)
            (push prop- result))
           ;; [   ]()
           ((>= start end-)
            (push prop- result))
           ;; [ () ]
           ((and (>= start start-)
                 (<= end end-))
            (push (list start- start prop-) result)
            (push (list start end (funcall add-method new-prop prop-)) result)
            (push (list end end- prop-) result))
           ;; ([   ])
           ((and (<= start start-)
                 (>= end end-))
            (push (list start- end- (funcall add-method new-prop prop-)) result))
           ;; ([ ) ]
           ((and (<= start start-)
                 (> end start-)
                 (<= end end-))
            (push (list start- end (funcall add-method new-prop prop-)) result)
            (push (list end end- prop-) result))
           ;; [ ( ])
           ((and (>= start start-)
                 (> end end-))
            (push (list start- start prop-) result)
            (push (list start end- (funcall add-method new-prop prop-)) result)))))
    (reverse result)))

;; test
(cl-assert
 (equal
  (%push-interval-properties
   '(2 7 (:new 999))
   '((1 4 (:old 0)) (4 6 (:old 1)) (6 9 (:old 2)))
   (lambda (new-prop old-prop)
     (append new-prop old-prop)))
  '((1 2 (:old 0))
    (2 4 (:new 999 :old 0))
    (4 6 (:new 999 :old 1))
    (6 7 (:new 999 :old 2))
    (7 9 (:old 2)))))
