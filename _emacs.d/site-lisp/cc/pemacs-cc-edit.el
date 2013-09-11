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

(provide 'pemacs-cc-edit)
;;; edit.el ends here
