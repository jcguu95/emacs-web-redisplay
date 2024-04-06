;; This file is meant to be evaluated by emacsclient.

(defun peekable-string ()
  "Return the segment of the buffer string that is peekable in the
current window."
  (let* ((window (car (window-list)))
         (from (window-start window))
         (to   (window-end   window))
         (buffer (window-buffer window)))
    (with-current-buffer buffer
      (let ((string (substring (buffer-string) (1- from))))
        (setf string (substring string 0 (min (- to from) (length string))))
        string))))

;; Text properties keys of interest to be piped to electron.
(defvar text-property-keys
  '(face
    ;; fontified
    ;; display                             ; Support this for showing images in electron.
    ))

(defun take-pairs (plist keys)
  "Return a sub-plist consisting of key is a member of keys."
  ;; FIXME This takes O(nn), while it can be done in O(n).
  (let ((result nil))
    (dotimes (n (/ (length plist) 2))
      (when (member (nth (* n 2) plist) keys)
        (push (nth (* n 2) plist) result)
        (push (nth (1+ (* n 2)) plist) result)))
    (reverse result)))

(defun listify (string)
  "Turn the string (may be a property string) into a list of substrings and properties."
  (let* ((plain-string (substring-no-properties string))
         (object-intervals (object-intervals string))
         (result nil))
    (mapcar
     (lambda (interval)
       ;; The cons cell has (string . properties).
       (cons (substring plain-string (nth 0 interval) (nth 1 interval))
             (take-pairs (nth 2 interval) text-property-keys)))
     object-intervals)))

;; (cl-subseq (listify (peekable-string)) 0 20)

(defun serialize (str-lists)
  (json-encode-list (mapcar (lambda (x) (push 'text x) (ht<-plist x)) str-lists)))

