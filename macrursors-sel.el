(require 'macrursors)
(require 'thingatpt)
(require 'cl)
(require 'isearch)

(defun macrursors-sel--select (beg end &optional type)
  (or mouse-secondary-start
      (setq mouse-secondary-start (make-marker)))
  (move-marker mouse-secondary-start beg)
  (move-overlay mouse-secondary-overlay beg end (current-buffer))
  (overlay-put mouse-secondary-overlay 'macrursors-sel-type type)
  (when type (message "%S" type))
  (gui-set-selection
   'SECONDARY
   (buffer-substring (overlay-start mouse-secondary-overlay)
                     (overlay-end mouse-secondary-overlay))))

(defun macrursors-sel--region ()
  (macrursors-sel--select (region-beginning) (region-end) 'region)
  (deactivate-mark))

(defmacro macrursors-sel--type (type)
  (declare (indent defun))
  `(defun ,(intern (format "macrursors-sel--%S" type)) ()
    (when-let* ((bounds (bounds-of-thing-at-point ',type))
                (beg (car bounds))
                (end (cdr bounds)))
     (macrursors-sel--select beg end ',type))))

(macrursors-sel--type defun)
(macrursors-sel--type line)
(macrursors-sel--type list)

(defcustom macrursors-sel-types '(defun list line)
  ";TODO: "
  :group 'macrursors
  :type 'list
  :local t)

(defun macrursors-sel--filter-cursors ()
  (when (and macrursors--overlays
             (macrursors--inside-secondary-selection))
    (let ((beg (overlay-start mouse-secondary-overlay))
          (end (overlay-end mouse-secondary-overlay)))
      (setq macrursors--overlays
            (cl-loop for ov in macrursors--overlays
                     if (and (<= beg (overlay-start ov))
                             (>= end (overlay-end ov)))
                     collect ov into live-overlays
                     else do (delete-overlay ov)
                     finally return live-overlays))
      (when defining-kbd-macro
	  (end-kbd-macro)
	  (macrursors-start)))))

;;;###autoload
(defun macrursors-sel-cycle ()
  (interactive)
  (let (search-start search-end)
  (cond
   ((use-region-p)
    (setq search-start (region-beginning)
          search-end   (region-end))
    (macrursors-sel--region))
   ((macrursors--inside-secondary-selection)
    (when-let ((sel-type
                (or
                 (get-char-property (max (1- (point)) (point-min)) 'macrursors-sel-type)
                 (get-char-property (min (1+ (point)) (point-max)) 'macrursors-sel-type))))
      (setq search-start (overlay-start mouse-secondary-overlay)
            search-end (overlay-end mouse-secondary-overlay))
      (funcall
       (intern
        (format "macrursors-sel--%s"
                (car (or (cl-loop for (head . tail) on macrursors-sel-types
                                  when (eq sel-type head)
                                  return tail)
                         macrursors-sel-types)))))))
   (t (funcall (intern (format "macrursors-sel--%s"
                               (car macrursors-sel-types))))))
  ;; Add new overlays inside
  (when (and search-start search-end
             (or (>= search-start (overlay-start mouse-secondary-overlay))
                 (<= search-end   (overlay-end mouse-secondary-overlay))))
    ;; TODO: Repeat search with the right string
    (when defining-kbd-macro
      (end-kbd-macro)
      (macrursors-start)))
  
  ;; Remove overlays outside
  (macrursors-sel--filter-cursors)
  (set-transient-map macrursors-sel-map)))

(defvar macrursors-sel-map
  (let ((map (make-sparse-keymap)))
    (define-key map " " 'macrursors-sel-cycle)
    (define-key map [remap keyboard-quit] 'macrursors-sel-clear)
    map))

(defun macrursors-sel-clear ()
  (interactive)
  (and mouse-secondary-overlay
       (eq (overlay-buffer mouse-secondary-overlay) (current-buffer))
       (delete-overlay mouse-secondary-overlay)))

(defun macrursors-sel-mark-from-isearch (&optional arg)
  "Make macrursors from the last search string."
  (interactive "P")
  (or isearch-success (user-error "Nothing to match."))
  (let* ((regexp
          (cond
           ((functionp isearch-regexp-function)
            (funcall isearch-regexp-function isearch-string))
           (isearch-regexp-function (word-search-regexp isearch-string))
           (isearch-regexp isearch-string)
           (t (regexp-quote isearch-string))))
         (selection-p (macrursors--inside-secondary-selection))
         (search-start (if selection-p
		           (overlay-start mouse-secondary-overlay)
                         0))
         (search-end (and selection-p
                          (overlay-end mouse-secondary-overlay)))
         orig-point)
    (goto-char (max (point) isearch-other-end))
    (isearch-exit)
    (setq orig-point (point))
    (save-excursion
      (goto-char search-start)
      (macrursors--mark-all-instances-of regexp orig-point search-end))
    (macrursors-start)))

(define-key macrursors-mark-map (kbd "SPC") #'macrursors-sel-cycle)
(define-key macrursors-mark-map [remap keyboard-quit] #'macrursors-sel-clear)
(define-key isearch-mode-map (kbd "C-;") #'macrursors-sel-mark-from-isearch)

(provide 'macrursors-sel)
