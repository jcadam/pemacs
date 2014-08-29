;;; 

;;; ======================================================================= ;;;
;;; GUI FONTS
;;; ======================================================================= ;;;
(defun set-gui-fonts ()
  (interactive)
  (if window-system
      (progn
	;; Set default fonts to monospace-10
	(set-default-font "monospace-10")
	
	;; Japanese characters
	(set-fontset-font (frame-parameter nil 'font)
			  'japanese-jisx0208
			  '("WenQuanYi Micro Hei Mono" . "unicode-bmp"))
	;; Han(Kanji) characters
	(set-fontset-font (frame-parameter nil 'font)
			  'han
			  '("WenQuanYi Micro Hei Mono" . "unicode-bmp"))
	;; CJK miscs
	(set-fontset-font (frame-parameter nil 'font)
			  'cjk-misc
			  '("WenQuanYi Micro Hei Mono" . "unicode-bmp"))
	)))

;;; ======================================================================= ;;;
;;; COLORS
;;; ======================================================================= ;;;
(defun set-color-theme ()
  (interactive)
  (require 'color-theme)
        (eval-after-load "color-theme"
          '(progn
             (color-theme-initialize)
	     (color-theme-jsc-light)
             )))

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

(set-gui-fonts)
(set-color-theme)
(set-frame-size-according-to-resolution)

;; Fonts speedbar, ediff etc
(add-to-list 'default-frame-alist '(font . "monospace-10"))

(provide 'pemacs-apperance)
