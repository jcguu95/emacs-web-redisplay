;;; -*- lexical-binding: t; -*-

(cl-defun %%peekable-string-and-text-properties (&optional (window (car (window-list))))
  (let* ((buffer (window-buffer window))
         (from (window-start window))
         (to (window-end window))
         (string (buffer-string)))
    (setf string (substring string (1- from)))
    (setf string (substring string 0 (min (- to from) (length string))))
    (with-current-buffer buffer
      (let* ((plain-text (substring-no-properties string))
             (text-properties (object-intervals string))
             (effective-point (- (point) from -1)))
        (list :plain-text plain-text
              :text-properties text-properties
              :effective-point effective-point)))))

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
                      (filter-overlay-properties (overlay-properties overlay))))
              overlays))))

(cl-defun %%peekable-data (&optional (window (car (window-list))))
  (append
   (%%peekable-string-and-text-properties)
   (list :overlays (%%peekable-overlay-properties))))

(defun index-in-prop? (index prop)
  (<= (nth 0 prop) index (1- (nth 1 prop))))

(defun wrap! (character properties)
  (let ((result (make-hash-table))
        (props (ht<-plist properties))
        (frame (selected-frame)))
    (setf (gethash :char result) character)
    (maphash (lambda (key val)
               (when (eq key 'face)
                 (let ((face val))
                   (setf (gethash :foreground result)
                         (face-attribute face :foreground frame t))
                   (setf (gethash :background result)
                         (face-attribute face :background frame t))
                   (setf (gethash :weight result)
                         (face-attribute face :weight     frame t))))
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
       do (setf current-character (aref plain-text index))
       do (unless (index-in-prop? index current-text-prop)
            (setf current-text-prop (pop text-properties)))
       do (push (wrap! current-character (nth 2 current-text-prop))
                result)
       ;; TODO Implement for overlays too.
       ))
    result))

;; FIXME Characters are now numbers. Fix this.
(json-encode-list (process-data (%%peekable-data)))
