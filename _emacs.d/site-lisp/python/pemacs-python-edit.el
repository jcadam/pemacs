;;; pemacs-python-edit.el --- 

;; Copyright (C) 2013  Adam Jiang

;; Author: Adam Jiang <jiang.adam@gmail.com>
;; Keywords: convenience

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Functions to support editing python source code

;;; Code:

(add-hook 'python-mode-hook
	  '(lambda()
	     (setq-default indent-tabs-mode nil)
	     (setq-default tab-width 4)))

;;; flymake for python
(require 'flymake)
(defun pemacs-flymake-python-init ()
  (pemacs-flymake-generic-init
   "~/.emacs.d/scripts/python-style-checker.sh"))
(push '("\\.py$" pemacs-flymake-python-init)
      flymake-allowed-file-name-masks)

(provide 'pemacs-python-edit)
;;; pemacs-python-edit.el ends here
