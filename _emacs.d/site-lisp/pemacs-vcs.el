;;; Backups
;; enable local backup files
(setq make-backup-files t)
;; enable versioning with default values (keep five last version)
(setq version-control t)
;; save all backup file in the following directory
(setq backup-directory-alist
      (quote ((".*" . "~/.emacs_backups/"))))

;;; Git
(require 'egg)

(provide 'pemacs-vcs)
