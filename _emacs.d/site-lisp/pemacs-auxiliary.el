;;; Enabling indexing xcscope
;; cscope package is required
(unless (eq system-type 'windows-nt)
  (require 'xcscope))

;; flymake
(when (load-file
       (concat pemacs-install-dir "extensions/flymake/flymake.el"))
  (load-library "flymake-cursor"))
;; enable flymake if possible
(add-hook 'find-file-hook 'flymake-find-file-hook)

;;; Generic helper function for flymake checker
(defun pemacs-flymake-generic-init (cmd &optional opts)
  (let* ((temp-file (flymake-init-create-temp-buffer-copy
		     'flymake-create-temp-inplace))
	 (local-file (file-relative-name
		      temp-file
		      (file-name-directory buffer-file-name))))
    (list cmd (append opts (list local-file)))))
;; if there was Makefile
(defun pemacs-flymake-make-or-generic-init (cmd &optional opts)
  (if (file-exists-p "Makefile")
      (flymake-simple-make-init)
    (pemacs-flymake-generic-init cmd opts)))

;; keybindings
(global-set-key [f10] 'flymake-goto-prev-error)
(global-set-key [f11] 'flymake-goto-next-error)

;; projects
(require 'eproject)

(provide 'pemacs-auxiliary)
