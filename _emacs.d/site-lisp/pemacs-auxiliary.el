;;; Enabling indexing xcscope
;; cscope package is required
(unless (eq system-type 'windows-nt)
  (require 'xcscope))

;; flymake
(when (load-file
       (concat pemacs-install-dir "extensions/flymake/flymake.el"))
  (load-library "flymake-cursor"))
;; enable flymake if possible
;;(add-hook 'find-file-hook 'flymake-find-file-hook)
;; keybindings
(global-set-key [f10] 'flymake-goto-prev-error)
(global-set-key [f11] 'flymake-goto-next-error)

(provide 'pemacs-auxiliary)
