;;; pemacs-org-init.el --- 

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

;;; global keybindings
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

;;; gtd
(require 'dto-org-gtd)

;;; wikimedia backend
(require 'org-mw)

;;; ditaa and dot
(setq org-ditaa-jar-path
      (expand-file-name
       "~/.emacs.d/scripts/ditaa.jar"))
(require 'org-exp-blocks)

(provide 'pemacs-org-init)
;;; pemacs-org-init.el ends here
