(require 'macrursors-select)
(require 'expand-region)

;;;###autoload
(defun macrursors-select-expand (&optional arg)
  (interactive "p")
  (unless (featurep 'expand-region)
    (user-error "`macrursors-select-expand' requires `expand-region'."))
  (let ((search-start) (search-end)
        (transient-mark-mode `(only . ,transient-mark-mode)))
    (save-excursion
      (when (macrursors--inside-secondary-selection)
        (setq search-start (overlay-start mouse-secondary-overlay)
              search-end (overlay-end mouse-secondary-overlay))
        (secondary-selection-to-region))
      (let ((expand-region-fast-keys-enabled))
        (if (< arg 0)
            (er/contract-region (- arg))
          (er/expand-region arg)))
      (macrursors-select--region))

    (macrursors-select--filter-cursors)
    (macrursors-select--expand-cursors search-start search-end)
    (set-transient-map macrursors-select-map)))

;;;###autoload
(defun macrursors-select-contract (&optional arg)
  (interactive "p")
  (macrursors-select-expand (- arg)))

(provide 'macrursors-select-expand)
