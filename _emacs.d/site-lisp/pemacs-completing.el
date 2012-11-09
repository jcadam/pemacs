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

(provide 'pemacs-completing)
