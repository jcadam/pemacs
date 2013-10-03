;; Opening other kinds of files
; http://www.emacswiki.org/cgi-bin/wiki/TrivialMode
; by Cyprian Laskowski
; Modified to make buffer read only and to suppress output of command
(defun define-trivial-mode(mode-prefix file-regexp &optional command)
  (or command (setq command mode-prefix))
  (let ((mode-command (intern (concat mode-prefix "-mode"))))
    (fset mode-command
          `(lambda ()
             (interactive)
             (toggle-read-only t)
             (start-process ,mode-prefix nil
                            ,command (buffer-file-name))
             ;; Select correct buffer
             (let ((obuf (other-buffer (current-buffer) t))
                   (kbuf (current-buffer)))
               ;; set it as current
               (set-buffer obuf)
               ;; kill temporary buffer
               (kill-buffer kbuf))))
    (add-to-list 'auto-mode-alist (cons file-regexp mode-command))))

(if (eq system-type 'windows-nt)
    (progn
      (define-trivial-mode "AcroRd32.exe" "\\.pdf$")
      (define-trivial-mode "oocalc" "\\.xls.*$")
      (define-trivial-mode "oowriter" "\\.doc.*$"))
  (progn
    (define-trivial-mode "gv" "\\.ps$")
    (define-trivial-mode "evince" "\\.pdf$")
    (define-trivial-mode "ooffice" "\\.xls.*$")
    (define-trivial-mode "ooffice" "\\.doc.*$")
    (define-trivial-mode "ooffice" "\\.ppt.*$")
    (define-trivial-mode "smplayer" "\\.[Mm][Pp].*$")
    (define-trivial-mode "smplayer" "\\.[Ww][Aa][Vv]$")
    )
  )

;; Search between files
;; Ack
(require 'ack-and-a-half)
;; Create shorter aliases
(defalias 'ack 'ack-and-a-half)
(defalias 'ack-same 'ack-and-a-half-same)
(defalias 'ack-find-file 'ack-and-a-half-find-file)
(defalias 'ack-find-file-same 'ack-and-a-half-find-file-same)

;;; Auto return at 72 charactors
(setq-default fill-column 72)
;; Turn on Auto Fill mode automatically in all modes
;;
;; Auto-fill-mode the the automatic wrapping of lines and insertion of
;; newlines when the cursor goes over the column limit.
;;
;; This should actually turn on auto-fill-mode by default in all major
;; modes. The other way to do this is to turn on the fill for specific
;; modes via hooks.
(setq auto-fill-mode 1)


(provide 'pemacs-browsing)
