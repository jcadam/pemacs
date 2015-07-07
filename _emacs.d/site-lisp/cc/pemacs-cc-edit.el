;;; edit.el --- Auxillary tools for editing in CC-mode

;; Copyright (C) 2013  Adam Jiang

;; Author: Adam Jiang <jiang.adam@gmail.com>
;; Keywords:

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

;;

;;; Code:

(require 'google-c-style)
;; set google-c-style as default when cc-mode is open
(add-hook 'c-mode-common-hook 'google-set-c-style)
(add-hook 'c-mode-common-hook 'google-make-newline-indent)

;; set width of the line to 79 charactors
(add-hook 'c-mode-common-hook
	  '(lambda () (setq-default fill-column 79)))

;; define caplab-c-style
(defconst caplab-c-style
  '("Google"
    (c-basic-offset . 4)
    (c-offsets-alist . (((innamespace . 0))))))
(defun caplab-set-style ()
  (c-add-style "caplab" caplab-c-style))
(add-hook 'c++-mode-hook 'caplab-set-style)

;; treat .h file as c++ source code
;;(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))


(provide 'pemacs-cc-edit)
;;; edit.el ends here
