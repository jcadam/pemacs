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
;;; INITIALIZE PEMACS
;;; ======================================================================= ;;;
(require 'pemacs-init)
