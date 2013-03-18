;;; ======================================================================= ;;;
;;; MISC SETTINGS
;;; ======================================================================= ;;;
(defun mis-setting ()
  (interactive)
  ;;; No startup message
  (setq inhibit-startup-message t)
  ;;; Let scrollbar on right
  (set-scroll-bar-mode 'right)
  ;;; Syntax highlight
  (global-font-lock-mode t)
  ;;; Cursor blink disable
  (blink-cursor-mode -1)
  ;;; Tab block
  (setq x-stretch-cursor t)
  ;;; Disable scratch message
  (setq initial-scratch-message nil)
  ;;; No menu on console
  (or window-system(menu-bar-mode 0))
  ;;(menu-bar-mode 0)
  ;;; No toolbar always
  (tool-bar-mode 0)
  ;;; show paren
  (show-paren-mode 1)
  ;;; Yes to y, No to n
  (fset 'yes-or-no-p 'y-or-n-p)
  ;;; don't blink on parents
  (setq blink-matching-parent nil))

(mis-setting)

(provide 'pemacs-misc)
