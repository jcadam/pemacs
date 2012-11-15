;;; Enabling indexing xcscope
;; cscope package is required
(unless (eq system-type 'windows-nt)
  (require 'xcscope))

(provide 'pemacs-auxiliary)
