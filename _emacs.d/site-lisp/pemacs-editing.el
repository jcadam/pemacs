;;; VI-like stuff
;; moving between matching braces with %
;; just like vim could do
(defun match-paren (arg)
  "Go to the matching paren if on a paren; otherwise insert %."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
        ((looking-at "\\s\)") (forward-char 1) (backward-list 1))
        (t (self-insert-command (or arg 1)))))
(global-set-key "%" 'match-paren)

;; Open next line
;; works like 'o' command in VI
;;    [Ctrl-o] : open next line
;;     [Alt-o] : open a new line previous current line
(require 'open-next-line)

;;; Show line numbers
(add-hook 'c-mode-common-hook (lambda () (linum-mode t)))
(add-hook 'java-mode-hook (lambda () (linum-mode t)))
(add-hook 'python-mode-hook (lambda () (linum-mode t)))
(add-hook 'makefile-mode-hook (lambda () (linum-mode t)))

;;; Show whitespaces on GUI
(require 'whitespace)
(global-set-key (kbd "\C-c w") 'whitespace-mode)
(global-set-key (kbd "\C-c t") 'whitespace-toggle-options)
;; remap the mark
(setq whitespace-display-mappings
      '(
        (space-mark 32 [183] [46])     ; normal space
        (space-mark 160 [164] [95])
        (space-mark 2208 [2212] [95])
        (space-mark 2336 [2340] [95])
        (space-mark 3616 [3620] [95])
        (space-mark 3872 [3876] [95])
        (newline-mark 10 [8629 10])    ; newlne
        ;;(newline-mark 10 [182 10])   ; newlne
        (tab-mark 9 [187 9] [92 9])    ; tab
        ;;(tab-mark 9 [9655 9] [92 9]) ; tab
        ))
;;; let tab and trailing whitespace be visiable
;; (setq whitespace-style '(trailing tabs tab-mark newline-mark))
;;; show tab as tab-mark
(setq whitespace-style '(trailing tabs tab-mark))

(add-hook 'c-mode-common-hook (lambda () (whitespace-mode)))
(add-hook 'java-mode-hook (lambda () (whitespace-mode)))
(add-hook 'python-mode-hook (lambda () (whitespace-mode)))
(add-hook 'makefile-mode-hook (lambda () (whitespace-mode)))

;;; Auto-complete
(require 'auto-complete)
(setq ac-auto-start 3)
;; select candidates with c-n/c-p only when completion menu is displayed
(setq ac-use-menu-map t)
(define-key ac-menu-map "\C-n" 'ac-next)
(define-key ac-menu-map "\C-p" 'ac-previous)
(define-key ac-menu-map " "    'ac-complete)
(define-key ac-menu-map "\r"   'ac-complete)
(define-key ac-complete-mode-map "\t"   'ac-complete)
(define-key ac-complete-mode-map "\r"   nil)
(define-key ac-complete-mode-map "\M-/" 'ac-stop)
;; set up dictionary for auto-complete
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories
	     (concat pemacs-install-dir "data/ac-dict"))
(ac-config-default)

;;; Intend when RETURN
(global-set-key (kbd "RET") 'newline-and-indent)

;;; Region/Line opertions
;;; This part worths a summary on all keybindings
;;;
;;;    [Ctrl-c y]   : duplicated the current line
;;;    [Ctrl-c c]   : duplicated a new line and comment the current line
;;;    [Ctrl-c l]   : mark the current line
;;;    [Alt-<up>]   : move the selected region up
;;;    [Alt-<down>] : move the selected region down
;;;
;; code borrowed from
;; http://emacs-fu.blogspot.com/2010/01/duplicating-lines-and-commenting-them.html
(defun djcb-duplicate-line (&optional commentfirst)
  "comment line at point; if COMMENTFIRST is non-nil, comment the
  original" (interactive)
  (beginning-of-line)
  (push-mark)
  (end-of-line)
  (let ((str (buffer-substring (region-beginning) (region-end))))
    (when commentfirst
      (comment-region (region-beginning) (region-end)))
    (insert-string
     (concat (if (= 0 (forward-line 1)) "" "\n") str "\n"))
    (forward-line -1)))

;; duplicate a line
(global-set-key (kbd "C-c y") 'djcb-duplicate-line)

;; duplicate a line and comment the first
(global-set-key (kbd "C-c c") (lambda() (interactive) (djcb-duplicate-line t)))

;; Mark whole line
(defun mark-line (&optional arg)
  "Marks a line"
  (interactive "p")
  (beginning-of-line)
  (push-mark (point) nil t)
  (end-of-line))

(global-set-key (kbd "C-c l") 'mark-line)

;;; Move region/line up and down
;; code copied from
;; http://stackoverflow.com/questions/2423834/move-line-region-up-and-down-in-emacs
(defun move-text-internal (arg)
  (cond
   ((and mark-active transient-mark-mode)
    (if (> (point) (mark))
        (exchange-point-and-mark))
    (let ((column (current-column))
          (text (delete-and-extract-region (point) (mark))))
      (forward-line arg)
      (move-to-column column t)
      (set-mark (point))
      (insert text)
      (exchange-point-and-mark)
      (setq deactivate-mark nil)))
   (t
    (let ((column (current-column)))
      (beginning-of-line)
      (when (or (> arg 0) (not (bobp)))
        (forward-line)
        (when (or (< arg 0) (not (eobp)))
          (transpose-lines arg))
        (forward-line -1))
      (move-to-column column t)))))

(defun move-text-down (arg)
  "Move region (transient-mark-mode active) or current line
  arg lines down."
  (interactive "*p")
  (move-text-internal arg))

(defun move-text-up (arg)
  "Move region (transient-mark-mode active) or current line
  arg lines up."
  (interactive "*p")
  (move-text-internal (- arg)))

;; Mimic Alt-<up> Alt-<down> in Eclipse
(global-set-key (kbd "M-<up>") 'move-text-up)
(global-set-key (kbd "M-<down>") 'move-text-down)

;;; Delete selected text when typing
(delete-selection-mode 1)

(provide 'pemacs-editing)
