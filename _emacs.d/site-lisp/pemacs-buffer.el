;; Enable iBuffer by default
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; Quick matching in buffer switching
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward
      uniquify-separator ":")

;; Ido mode with fuzzy matching
;;; wild used key-bindings
;;; C-s(next)
;;; C-r(previous) : move through the list
;;;         [Tab] : display possible completion in a buffer
;;;         [RET] : type to go down inside the directory in from of the list
;;;   [Backspace] : got up to parent directory
;;;            // : go to the root directory
;;;            ~/ : go to the home directory
;;;           C-d : enter Dired for this directory
;;;           C-j : create a new file named with the text you entered
(require 'ido)

(ido-mode t)
;; enable fuzzy matching
(setq ido-enable-flex-matching t)

;; Show current position of cursor in all buffers
(line-number-mode 1)
(column-number-mode 1)

;; Set default mode for new buffers
(setq default-major-mode 'text-mode)

(provide 'pemacs-buffer)
