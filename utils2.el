;;; -*- lexical-binding: t; -*-

(cl-defun effective-point (&optional (window (car (window-list))))
  (let ((from (window-start window))
        result)
    (- (point) from -1)))

(cl-defun %%peekable-string-and-text-properties (&optional (window (car (window-list))))
  (let* ((buffer (window-buffer window))
         (from (window-start window))
         (to (window-end window))
         (string (buffer-string)))
    (setf string (substring string (1- from)))
    (setf string (substring string 0 (min (- to from) (length string))))
    (with-current-buffer buffer
      (let* ((plain-text (substring-no-properties string))
             (text-properties (object-intervals string)))
        (list :plain-text plain-text
              :text-properties text-properties
              :effective-point (effective-point window))))))

(cl-defun %%peekable-overlay-properties (&optional (window (car (window-list))))
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
                      (overlay-properties overlay)
                      ;; (filter-overlay-properties (overlay-properties overlay))
                      ))
              overlays))))

(cl-defun %%peekable-data (&optional (window (car (window-list))))
  (append
   (%%peekable-string-and-text-properties)
   (list :overlays (%%peekable-overlay-properties))))

(defun index-in-prop? (index prop)
  (<= (nth 0 prop) index (1- (nth 1 prop))))

(defun specified? (x)
  (not (eq 'unspecified x)))

(defun get-face-attr (face attr frame)
  (cond ((null face) 'unspecified)
        ((symbolp face)
         (face-attribute face attr frame t))
        ((listp face)
         (or (cl-getf face attr) 'unspecified))))

(defun wrap! (character properties)
  (let ((result (make-hash-table))
        (props (ht<-plist properties))
        (frame (selected-frame)))
    (setf (gethash :t result) character) ; :t stands for 'text'
    (maphash (lambda (key val)
               (when (eq key 'face)
                 (let ((face val))
                   (let ((foreground  (get-face-attr face :foreground  frame))
                         (background  (get-face-attr face :background  frame))
                         (weight      (get-face-attr face :weight      frame))
                         ;; (font-size   (get-face-attr face :font-size   frame))
                         ;; (font        (get-face-attr face :font        frame))
                         )
                     (when (specified? foreground)
                       (setf (gethash :fg result) foreground))
                     (when (specified? background)
                       (setf (gethash :bg result) background))
                     (when (specified? weight)
                       (setf (gethash :wt result) weight))
                     ;; (when (specified? font-size)
                     ;;   ;; fs translates to font-size in JS
                     ;;   (setf (gethash :fs result) font-size))
                     ;; (when (specified? font-family)
                     ;;   ;; ff translates to font-family in JS
                     ;;   (setf (gethash :ff result) font))
                     )))
               ;; TODO Implement for fontified and other properties.
               ;; (when (eq key 'fontified)
               ;;   "TODO")
               ;; TODO Implement for overlays properties.
               )
             props)
    result))

(defun process-data (data)
  (let* ((plain-text (cl-getf data :plain-text))
         (text-properties (cl-getf data :text-properties))
         (effective-point (cl-getf data :effective-point))
         (overlays (cl-getf data :overlays))
         (result (list :effective-point effective-point)))
    (let ((current-text-prop (pop text-properties))
          (current-character))
      (cl-loop
       for index from 0 to (1- (length plain-text))
       do (setf current-character (substring plain-text index (1+ index)))
       do (unless (index-in-prop? index current-text-prop)
            (setf current-text-prop (pop text-properties)))
       do (push (wrap! current-character (nth 2 current-text-prop))
                (cl-getf result :text))
       ;; TODO Implement for overlays too.
       ))
    ;; TODO Don't use reverse. it's too slow.
    (setf (cl-getf result :text) (reverse (cl-getf result :text)))
    ;; Merge chars alike together.
    ;; NOTE I'm not sure if this is going to make the performance better.
    ;; If not, you should be able to take this away directly.
    (setf (cl-getf result :text) (merge-alike (cl-getf result :text)))
    result))

;; An utility function.
(cl-defun seq-group (xs transformation &key (test #'equal))
  "Expect XS to be a list of objects. Traverse through each X in XS,
collect the X's that are identical after transformation into a
list sequentially, and return the list of such lists.

For example:
  (seq-group '(0 1 2 4 3 7 -1 8 10) #'cl-evenp) ; => ((0) (1) (2 4) (3 7 -1) (8 10))"
  (let* ((result nil)
         (current-value (car xs))
         (current-group (list current-value)))
    (dolist (x (cdr xs))
      (let ((transformed-value (funcall transformation x)))
        (if (funcall test transformed-value current-value)
            (push x current-group)
          (progn
            (when current-group
              (push (nreverse current-group) result))
            (setq current-group (list x))
            (setq current-value transformed-value)))))
    (when current-group
      (push (nreverse current-group) result))
    (nreverse result)))

(cl-assert
 (equal (seq-group '(0 1 1 3 1 1 2 4 6 8 3 7 -1 8 10) #'cl-evenp)
        '((0) (1 1 3 1 1) (2 4 6 8) (3 7 -1) (8 10))))

(cl-assert
 (let ((x (make-hash-table))
       (y (make-hash-table))
       (z (make-hash-table)))
   (setf (gethash :a x) 1)
   (setf (gethash :a y) 1)
   (setf (gethash :a z) 2)
   (equal
    (seq-group (list x y y z x y y z z)
               #'identity
               :test #'ht-equal?)
    (list (list x y y) (list z) (list x y y) (list z z)))))

(defun merge-alike (propertied-chars)
  (cl-labels
      ((%merge-characters (propertied-chars)
         "A propertied character is a hash table with the character stored
in the key :t, and all other properties stored in other keys. We
merge all characters, and use the properties of the 0th
propertied-char."
         (when propertied-chars
           (let ((result (car propertied-chars)))
             (cl-check-type result hash-table)
             (setf (gethash :t result)
                   (apply #'concat
                          (mapcar (lambda (ht)
                                    (cl-check-type ht hash-table)
                                    (gethash :t ht))
                                  propertied-chars)))
             result))))
    (mapcar #'%merge-characters
            (seq-group propertied-chars
                       (lambda (propertied-char)
                         ;; NOTE Copy may be bad for performance.
                         (let ((tmp-ht (copy-hash-table propertied-char)))
                           ;; (cl-check-type tmp-ht hash-table)
                           (remhash :t tmp-ht)
                           tmp-ht))
                       :test #'ht-equal?))))
