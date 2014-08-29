;;; Small detailed stuff
(require 'autopair)
;; enable autopair globally except following modes
(autopair-global-mode)
(add-hook 'text-mode-hook #'(lambda () (setq autopair-dont-activate t)))
;; prevent the '{' (opening brace) character from being autopaired in
;; C++ comments
(add-hook 'c++-mode-hook
          #'(lambda ()
	      (push ?{
		    (getf autopair-dont-pair :comment))))
;; autopair <> for C++ templates
(add-hook 'c++-mode-hook
          #'(lambda ()
              (push '(?< . ?>)
                    (getf autopair-extra-pairs :code))))

;; work with triple quoting in python mode
(add-hook 'python-mode-hook
          #'(lambda ()
              (setq autopair-handle-action-fns
                    (list #'autopair-default-handle-action
                          #'autopair-python-triple-quote-action))))
;; work with elisp '\''
(add-hook 'lisp-mode-hook
	  #'(lambda ()
	      (push ?'
		    (getf autopair-dont-pair :code))))

;;; YaSnippet for snips
(require 'yasnippet)
(yas/initialize)
;; develop in data/yasnippet/snippets
(setq yas/root-directory (concat pemacs-install-dir "data/yasnippet/snippets"))
(yas/load-directory yas/root-directory)
;; other options
(setq yas/prompt-functions '(yas/dropdown-prompt yas/ido-prompt yas/x-prompt))
(setq yas/wrap-around-region 'cua)

;;; Auto-inserting for templates
(require 'autoinsert)
(require 'cl)

(setq auto-insert 'other)
(setq auto-insert-query nil)

(setq auto-insert-directory (concat pemacs-install-dir "data/templates/"))
(setq auto-insert-alist
      (append '(("\\.c$"   . ["template.c"   pemacs-template])
		("\\.cpp$" . ["template.cpp" pemacs-template])
		("\\.h$"   . ["template.h"   pemacs-template])
		("\\.hpp$" . ["template.hpp" pemacs-template]))
	      auto-insert-alist))

(defvar pemacs-template-replacement-alists
  '(("%file%"   . (lambda() (file-name-nondirectory (buffer-file-name))))
    ("%file-without-ext%" . (lambda() (file-name-sans-extension
				       (file-name-nondirectory (buffer-file-name)))))
    ("%include-guard%" . (lambda() (format "__%s_H__"
					   (upcase (file-name-sans-extension
						    (file-name-nondirectory
						     (buffer-file-name)))))))
    ("%name%"   . user-full-name)
    ("%mail%"   . (lambda() (identity user-mail-address)))
    ("%cyear%"  . (lambda() (substring (current-time-string) -4)))))

(defun pemacs-template ()
  (time-stamp)
  (mapc #'(lambda(c)
	    (progn
	      (goto-char (point-min))
	      (replace-string (car c) (funcall (cdr c)) nil)))
	pemacs-template-replacement-alists)
  (goto-char (point-max))
  (message "autoinsert done."))

;; insert header for new files only
(add-hook 'find-file-not-found-hooks 'auto-insert)

(provide 'pemacs-completing)
