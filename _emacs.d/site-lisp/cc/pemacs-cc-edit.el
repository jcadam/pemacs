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

;; Add a user-defined indent style
(c-add-style "pemacs"
	     '("stroustrup"
	       (c-basic-offset . 4)
	       (c-offsets-alist
		(innamespace . -)
		(inline-open . 0)
		(inher-cont . c-lineup-multi-inher)
		(arglist-cont-nonempty . +)
		(template-args-cont . +))))

;; Tabs or Spaces, that's a question
; helper function
(defun how-many-region (begin end regexp &optional interactive)
  "Print number of non-trivial matches for ERGEXP in region.
Non-interactive arguments are Begin for (regexp)"
  (interactive "r\nsHow many matches for (regexp): \np")
  (let ((count 0) opoint)
    (save-excursion
      (setq end (or end (point-max)))
      (goto-char (or begin (point)))
      (while (and (< (setq opoint (point)) end)
		  (re-search-forward regexp end t))
	(if (= opoint (point))
	    (forward-char 1)
	  (setq count (1+ count))))
      (if interactive (message "%d occurrences" count))
      count)))

(defun infer-indentation-style ()
  "If out source file uses tabs, we use tabs, if spaces spaces,
and if neither, we use the current indent-tabs-mode."
  (let ((space-count (how-many-region (point-min) (point-max) "^ "))
	(tab-count (how-many-region (point-min) (point-max) "^\t")))
    (if (> space-count tab-count) (setq indent-tabs-mode nil))
    (if (> tab-count space-count) (setq indent-tabs-mode t))))

(defun active-infer-indentation-style ()
  "Active infer-indentation-style"
  (setq indent-tabs-mode nil)
  (infer-indentation-style))

(add-hook 'c-mode-common-hook 'active-infer-indentation-style)

(provide 'pemacs-cc-edit)
;;; edit.el ends here
