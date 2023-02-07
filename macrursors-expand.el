(require 'macrursors-sel)
(require 'expand-region)

(defun macrursors-sel-expand (&optional arg)
  (interactive "p")
  (unless (featurep 'expand-region)
    (user-error "`macrursors-sel-expand' requires `expand-region'."))
  (save-excursion
    (when (macrursors--inside-secondary-selection)
      (secondary-selection-to-region))
    (let ((expand-region-fast-keys-enabled))
      (er/expand-region arg))
    ;; (secondary-selection-from-region)
    (macrursors-sel--region))
  (macrursors-sel--filter-cursors)
  (set-transient-map macrursors-sel-map))

(define-key macrursors-mark-map (kbd ",") #'macrursors-sel-expand)
(define-key macrursors-sel-map (kbd ",") #'macrursors-sel-expand)

(provide 'macrursors-expand)
