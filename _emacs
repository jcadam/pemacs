;;; ======================================================================= ;;;
;;; Author: Adam Jiang
;;; Reversion: 2.0
;;; Update: 2012/10/02
;;; ======================================================================= ;;;

;;; split the previous version to several parts; do NOT use byte
;;; compile anymore; all extra libraries except those with distro
;;; binaries will be put into ${HOME}/.emacs.d/site-lisp/
;;; Use this file on Ubuntu 11.04 with following package installed
;;; emacs cscope w3m

;;; ======================================================================= ;;;
;;; EXECUTABLE PATH
;;; ======================================================================= ;;;
;;; have a private script directory for emacs only; all utilities will
;;; be put into ~/.emacs.d/scripts
(setenv "PATH" (concat (getenv "PATH") ":~/.emacs.d/scripts"))
(setq exec-path (append exec-path '("~/.emacs.d/scripts")))

;;; ======================================================================= ;;;
;;; LOADPATH
;;; ======================================================================= ;;;
(let ((default-directory "~/.emacs.d/site-lisp/"))
  (normal-top-level-add-to-load-path '("."))
  (normal-top-level-add-subdirs-to-load-path))

;;; ======================================================================= ;;;
;;; USER INFO
;;; ======================================================================= ;;;
(setq user-full-name "Adam Jiang")
(setq user-mail-address "jiang.adam@gmail.com")

;;; ======================================================================= ;;;
;;; PACKAGE REPOSITORY
;;; ======================================================================= ;;;
(require 'package)
(add-to-list 'package-archives
	     '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)

;;; ======================================================================= ;;;
;;; PACKAGES
;;; ======================================================================= ;;;
;;; shameless stolen from prelude
;;; https://github.com/bbatsov/prelude/

;; required because of a package.el bug
(setq url-http-attempt-keepalives nil)

(defvar prelude-packages
  '(ack-and-a-half go-mode pony-mode)
  "A list of packages to ensure are installed at launch.")

(require 'cl)
(defun prelude-packages-installed-p ()
  (loop for p in prelude-packages
        when (not (package-installed-p p)) do (return nil)
        finally (return t)))

(defun prelude-install-packages ()
  (unless (prelude-packages-installed-p)
    ;; check for new packages (package versions)
    (message "%s" "Emacs Prelude is now refreshing its package database...")
    (package-refresh-contents)
    (message "%s" " done.")
    ;; install the missing packages
    (dolist (p prelude-packages)
      (unless (package-installed-p p)
        (package-install p)))))

(prelude-install-packages)

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
  ;;(or window-system(menu-bar-mode 0))
  (menu-bar-mode 0)
  ;;; No toolbar always
  (tool-bar-mode 0)
  ;;; show paren
  (show-paren-mode 1)
  ;;; Yes to y, No to n
  (fset 'yes-or-no-p 'y-or-n-p))
(mis-setting)

;;; ======================================================================= ;;;
;;; GUI FONTS
;;; ======================================================================= ;;;
(defun set-gui-fonts ()
  (interactive)
  (if window-system
    (progn
      ;; Set default fonts to ProFont 8pt
      (set-default-font "profont-8")
      ;;; JP fonts
      (set-fontset-font (frame-parameter nil 'font)
                        'japanese-jisx0208
                        '("Monospace" . "unicode-bmp"))
      ;;; zh_CN fonts
      (set-fontset-font (frame-parameter nil 'font)
                        'han
                        '("WenQuanYi Micro Hei Mono" . "unicode-bmp"))
      ;;; cjk-misc fonts
      (set-fontset-font (frame-parameter nil 'font)
                        'cjk-misc
                        '("WenQuanYi Micro Hei Mono" . "unicode-bmp"))
      )))
(set-gui-fonts)

;;; ======================================================================= ;;;
;;; COLOR
;;; ======================================================================= ;;;
(defun set-color-theme ()
  (interactive)
  (if window-system
      (progn
	(require 'color-theme)
	(eval-after-load "color-theme"
	  '(progn
	     (color-theme-initialize)
	     ;; dark theme
	     (color-theme-fischmeister)
	     ;; light theme
	     ;; (color-theme-jsc-light)
	     )))))

(set-color-theme)

;;; ======================================================================= ;;;
;;; FRAMESIZE
;;; ======================================================================= ;;;
(defun set-frame-size-according-to-resolution ()
  (interactive)
  (if window-system
      (progn
	;; use 120 char wide window for largeish displays
	;; and smaller 80 column windows for smaller displays
	;; pick whatever numbers make sense for you
	(if (> (x-display-pixel-width) 1280)
	    (add-to-list 'default-frame-alist (cons 'width 120))
	  (add-to-list 'default-frame-alist (cons 'width 80)))
	;; for the height, subtract a couple hundred pixels
	;; from the screen height (for panels, menubars and
	;; whatnot), then divide by the height of a char to
	;; get the height we want
	(add-to-list 'default-frame-alist
		     (cons 'height (/ (- (x-display-pixel-height) 150)
				      (frame-char-height)))))))

(set-frame-size-according-to-resolution)

;;; ======================================================================= ;;;
;;; BACKUPS
;;; ======================================================================= ;;;
;;; Enanble backup files.
(setq make-backup-files t)
;;; Enable versioning with default values (keep five last versions)
(setq version-control t)
;;; Save all backup file in this directory
(setq backup-directory-alist
      (quote ((".*" . "~/.emacs_backups/"))))

;;; ======================================================================= ;;;
;;; MODE LINE
;;; ======================================================================= ;;;
;;; Enable Line and Column Numbering
;;; Show line-number in the mode line
(line-number-mode 1)
;;; Show column-number in the mode line
(column-number-mode 1)

;;; ======================================================================= ;;;
;;; AUTOFILL AND TEXT
;;; ======================================================================= ;;;
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
;; Text mode the default mode for new buffers
(setq default-major-mode 'text-mode)

;;; ======================================================================= ;;;
;;; BUFFERS
;;; ======================================================================= ;;;
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward
      uniquify-separator ":")

;;; ======================================================================= ;;;
;;; W3M BROWSER
;;; ======================================================================= ;;;
(require 'w3m)
(require 'w3m-dka-keymap)

;;; ======================================================================= ;;;
;;; EXTERNAL TOOLS
;;; ======================================================================= ;;;
; Opening other kinds of files
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

(define-trivial-mode "gv" "\\.ps$")
(define-trivial-mode "evince" "\\.pdf$")
(define-trivial-mode "oocalc" "\\.xls.*$")
(define-trivial-mode "oowriter" "\\.doc.*$")

;;; ======================================================================= ;;;
;;; PROGRAMMING
;;; ======================================================================= ;;;
;; don't blink on parents
(setq blink-matching-parent nil)
;; jump between parents with %
(defun match-paren (arg)
      "Go to the matching paren if on a paren; otherwise insert %."
      (interactive "p")
      (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
            ((looking-at "\\s\)") (forward-char 1) (backward-list 1))
            (t (self-insert-command (or arg 1)))))
(global-set-key "%" 'match-paren)
;; bind M-n to jump to next error
(add-hook 'c-mode-hook
	  '(lambda() (local-set-key "\M-n" 'next-error)))
;; add a function to save-and-compile
(defun program-save-and-compile ()
  (interactive "")
  (save-buffer 0)
  (compile "make -k"))
(add-hook 'c-mode-hook
	  '(lambda() (local-set-key
		      "\C-c\C-c" 'program-save-and-compile)))

;;; WHITESPACES
;; show whitespace/tabs
(require 'whitespace)
(global-set-key "\C-c_w" 'whitespace-mode)
(global-set-key "\C-c_t" 'whitespace-toggle-options)
(add-hook 'c-mode-common-hook (lambda () (whitespace-mode)))
;; remap the mark
(setq whitespace-display-mappings
 '(
   (space-mark 32 [183] [46]) ; normal space
   (space-mark 160 [164] [95])
   (space-mark 2208 [2212] [95])
   (space-mark 2336 [2340] [95])
   (space-mark 3616 [3620] [95])
   (space-mark 3872 [3876] [95])
   (newline-mark 10 [8629 10]) ; newlne
   ;;(newline-mark 10 [182 10]) ; newlne
   (tab-mark 9 [187 9] [92 9]) ;tab
   ;;(tab-mark 9 [9655 9] [92 9]) ; tab
))
;; let tab and trailing whitespace be visiable
;; show tab as tab-mark
;;(setq whitespace-style '(trailing tabs tab-mark newline-mark))
(setq whitespace-style '(trailing tabs tab-mark))

;;; LINE NUMBER
;;; Show linumb always
(add-hook 'c-mode-common-hook (lambda () (linum-mode t)))
(add-hook 'java-mode-common-hook (lambda () (linum-mode t)))
(add-hook 'python-mode-common-hook (lambda () (linum-mode t)))

;;; STYLES
;;; c
(setq c-mode-hook
      (function (lambda()
		  '(c-set-style linux))))

;;; indent with space only in C++ and Java mode
;;; java
(setq java-mode-hook
      (function (lambda()
		  (setq indent-tabs-mode nil)
		  (setq c-indent-level 4))))
;;; c++
(setq c++-mode-hook
      (function (lambda()
		  (setq indent-tabs-mode nil)
		  (setq c-indent-level 4))))

;;; java specials
;;; treat @-style annotation as comments
(add-hook 'java-mode-hook
	  '(lambda () "Treat Java @-style annotiation as comments."
	     (setq c-comment-start-regexp "\\(@\\|/\\(/\\|[*][*]?\\)\\)")
	     (modify-syntax-entry ?@ "< b" java-mode-syntax-table)))

;;; AUTO-COMPLETE
(require 'auto-complete)
(global-auto-complete-mode  t)
(define-key ac-complete-mode-map "\C-n" 'ac-next)
(define-key ac-complete-mode-map "\C-p" 'ac-previous)
(define-key ac-complete-mode-map "\t"   'ac-expand)
(define-key ac-complete-mode-map "\r"   'ac-complete)
(define-key ac-complete-mode-map " "    'ac-complete)
(define-key ac-complete-mode-map "\M-/" 'ac-stop)

;;; ======================================================================= ;;;
;;; VI-LIKE
;;; ======================================================================= ;;;
;; moving between matching braces with %
;; just like vim could do
(global-set-key "%" 'match-paren)

(defun match-paren (arg)
  "Go to the matching paren if on a paren; otherwise insert %."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
	((looking-at "\\s\)") (forward-char 1) (backward-list 1))
	(t (self-insert-command (or arg 1)))))

(custom-set-variables
 ;; replace the default indexer with smart-indexer; this script will be
 ;; placed into .emacs.d/scripts folder
 '(cscope-indexing-script "smart-cscope-indexer"))
(custom-set-faces)

;; cscope has to be here. Otherwise, key binding for c++ file is not
;; working.
(require 'xcscope)

;;; ======================================================================= ;;;
;;; VERSION CONTROL TOOLS
;;; ======================================================================= ;;;
(require 'egg)

;;; ======================================================================= ;;;
;;; PYTHON PLUGINS COLLECTION
;;; ======================================================================= ;;;
(require 'epy-setup)
;; python facilities
(require 'epy-python)
;; autocompletion
(require 'epy-completion)
;; editing
(add-hook 'python-mode
	  '(lambda () "Enabbling python-editing tools only for python mode."
	     (require 'epy-editing)
	     ;; nose enabling
	     ;; nose is an easy python test running in Emacs
	     (require 'epy-nose)))

;;; ======================================================================= ;;;
;;; DJANGO
;;; ======================================================================= ;;;
(require 'pony-mode)
