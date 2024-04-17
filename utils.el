;;; -*- lexical-binding: t; -*-

;;; THE WHOLE FILE MAY BE DEPRECATED SOON by UTILS2.EL.

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
    (cond
     ((< start end)
      (progn
        (cl-loop
         for int-prop- in int-props
         do (let ((start- (nth 0 int-prop-))
                  (end- (nth 1 int-prop-))
                  (prop- (nth 2 int-prop-)))
              ;; NOTE In the following branches, () denotes INT-PROP, and [] denotes INT-PROPS
              (cond
               ;; ()[   ]
               ((<= end start-)
                (push int-prop- result))
               ;; [   ]()
               ((>= start end-)
                (push int-prop- result))
               ;; [ () ]
               ((and (>= start start-)
                     (<= end end-))
                (unless (= start- start)
                  (push (list start- start prop-) result))
                (unless (= start end)
                  (push (list start end (funcall add-method new-prop prop-)) result))
                (unless (= end end-)
                  (push (list end end- prop-) result)))
               ;; ([   ])
               ((and (<= start start-)
                     (>= end end-))
                (unless (= start- end-)
                  (push (list start- end- (funcall add-method new-prop prop-)) result)))
               ;; ([ ) ]
               ((and (<= start start-)
                     (> end start-)
                     (<= end end-))
                (unless (= start- end)
                  (push (list start- end (funcall add-method new-prop prop-)) result))
                (unless (= end end-)
                  (push (list end end- prop-) result)))
               ;; [ ( ])
               ((and (>= start start-)
                     (> end end-))
                (unless (= start- start)
                  (push (list start- start prop-) result))
                (unless (= start end-)
                  (push (list start end- (funcall add-method new-prop prop-)) result))))))
        (reverse result)))
     ((= start end)
      (setf result int-props)
      result))))

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

(cl-assert
 (equal
  (%push-interval-properties
   '(2 7 :new)
   '((1 4 (:old-0)) (4 6 (:old-1)) (6 9 (:old-2)))
   (lambda (x xs) (push x xs)))
  '((1 2 (:old-0))
    (2 4 (:new :old-0))
    (4 6 (:new :old-1))
    (6 7 (:new :old-2))
    (7 9 (:old-2)))))

(cl-assert
 (equal
  (%push-interval-properties
   '(1 1 :new)
   '((0 1 (:old-0)) (1 3 (:old-1)))
   (lambda (x xs) (push x xs)))
  '((0 1 (:old-0)) (1 3 (:old-1)))))

(cl-assert
 (equal
  (%push-interval-properties
   '(2 3 :new)
   '((0 1 (:old-0)) (1 3 (:old-1)))
   (lambda (x xs) (push x xs)))
  '((0 1 (:old-0)) (1 2 (:old-1)) (2 3 (:new :old-1)))))

(cl-assert
 (equal
  (%push-interval-properties
   '(1 2 :new)
   '((0 1 (:old-0)) (1 3 (:old-1)))
   (lambda (x xs) (push x xs)))
  '((0 1 (:old-0)) (1 2 (:new :old-1)) (2 3 (:old-1)))))

;;;

(defun plist-remove-if (pred plist)
  "Remove all key-value pairs from PLIST if the pair satisfies PRED."
  (let ((result nil)
        (key nil)
        (value nil)
        (collect-flag nil))
    (cl-loop for n from 0 to (1- (length plist))
             for x in plist
             do (if (cl-evenp n)
                    (setf key x)
                  (progn
                    (setf value x)
                    (unless (funcall pred key value)
                      (setf collect-flag t))))
             when collect-flag
             collect key
             when collect-flag
             collect value
             do (setf collect-flag nil))))

(cl-assert
 (equal
  (plist-remove-if (lambda (key value)
                     (cl-evenp (* key value)))
                   `(1 1 2 2 3 3 4 4))
  `(1 1 3 3)))

(cl-assert
 (equal
  (plist-remove-if (lambda (key value)
                     (cl-oddp (* key value)))
                   `(1 1 2 2 3 3 4 4))
  `(2 2 4 4)))

;;;

(defvar relevant-text-properties
  ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Special-Properties.html
  '(face font-lock-face
    ;; fontified
    ;; display
    ;; invisible invisible-isearch
    ;; wrap-prefix line-prefix
    ))

(defvar relevant-overlay-properties
  ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Overlay-Properties.html
  '(priority window
    face
    display
    invisible
    isearch-open-invisible isearch-open-invisible-temporary
    before-string after-string
    line-prefix wrap-prefix))

(defun filter-text-properties (text-properties)
  (plist-remove-if
   (lambda (key value)
     (not (member key relevant-text-properties)))
   text-properties))

(defun filter-overlay-properties (text-properties)
  (plist-remove-if
   (lambda (key value)
     (not (member key relevant-overlay-properties)))
   text-properties))

(cl-defun %peekable-string-and-text-properties (&optional (window (car (window-list))))
  (let* ((buffer (window-buffer window))
         (from (window-start window))
         (to (window-end window))
         (string (buffer-string)))
    (setf string (substring string (1- from)))
    (setf string (substring string 0 (min (- to from) (length string))))
    (with-current-buffer buffer
      (let* ((plain-text (substring-no-properties string))
             (text-properties (object-intervals string)))
        (cl-loop for text-property in text-properties
                 do (setf (nth 2 text-property)
                          (filter-text-properties (nth 2 text-property))))
        (cons plain-text text-properties)))))

(cl-defun %peekable-overlay-properties (&optional (window (car (window-list))))
  (let* ((buffer (window-buffer window))
         (from (window-start window))
         (to (window-end window))
         (overlays (car (overlay-lists))))
    (setf overlays
          (cl-remove-if
           (lambda (overlay)
             (or (> (overlay-start overlay) to)
                 (< (overlay-end overlay) from)))
           overlays))
    (with-current-buffer buffer
      (mapcar (lambda (overlay)
                (list (overlay-start overlay)
                      (overlay-end overlay)
                      (filter-overlay-properties (overlay-properties overlay))))
              overlays))))

(cl-defun window-string-with-all-properties (&optional (window (car (window-list))))
  (let* ((overlays (%peekable-overlay-properties))
         (string (%peekable-string-and-text-properties))
         (plain-text (car string))
         (text-props (cdr string))
         (result))
    (cl-loop for text-prop in text-props
             do (setf (nth 2 text-prop) (list (nth 2 text-prop))))
    (setf result text-props)
    (push result *debug-queue*)
    (cl-loop for overlay in overlays
             for n from 0
             do (setf result (%push-interval-properties
                              overlay result (lambda (x xs) (push x xs)))))
    (cons plain-text result)))

;; TODO Test - This should not return any error.
(json-parse-string
 (json-encode-list
  (window-string-with-all-properties)))

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
