;; Author: Lesley Lai <laisililai@live.cn>
;;
;; Note: Mimics the Qt Creators' default color theme.

(deftheme qtcreator
  "Mimics the Qt Creator's default color theme.")

(defgroup qtcreator-theme nil
  "Qtcreator theme options. Reload the theme after changing to see effect."
  :group 'faces)

(defcustom qtcreator-theme-bold-current-linum t
  " Bold the current line number if you have opened the `linum-mode'."
  :type 'boolean
  :group 'qtcreator-theme)

(unless (>= emacs-major-version 24)
  (error "qtcreator-theme requires Emacs 24 or later."))

;;  LocalWords:  pre

(let ((class '((class color) (min-colors 89)))
      ;; Tango palette colors.

      (butter-1 "#fce94f") (butter-2 "#edd400") (butter-3 "#c4a000")
      (orange-1 "#fcaf3e") (orange-2 "#f57900") (orange-3 "#ce5c00")
      (choc-1 "#e9b96e") (choc-2 "#c17d11") (choc-3 "#8f5902")
      (cham-1 "#8ae234") (cham-2 "#73d216") (cham-3 "#4e9a06")
      (blue-1 "#729fcf") (blue-2 "#3465a4") (blue-3 "#204a87")
      (blue-4 "#092e64")
      (plum-1 "#ad7fa8") (plum-2 "#75507b") (plum-3 "#5c3566")
      (red-1 "#ef2929")  (red-2 "#cc0000")  (red-3 "#a40000")
      (alum-1 "#eeeeec") (alum-2 "#d3d7cf") (alum-3 "#babdb6")
      (alum-4 "#888a85") (alum-5 "#5f615c") (alum-6 "#2e3436")
      ;; Not in Tango palette; used for better contrast.
      (cham-4 "#346604") (blue-0 "#8cc4ff") (orange-4 "#b35000"))

  (custom-theme-set-faces
   'qtcreator
   `(default ((t (:foreground ,"#000000" :background ,"#FFFFFF"))))
   ;; ;; Highlighting faces
   ;; `(fringe ((,class (:background ,alum-2))))
   ;; `(highlight ((,class (:background ,alum-3))))
    `(region ((t (:foreground "#FFFFFF" :background "#2D83DE"))))
   ;; `(secondary-selection ((,class (:background ,blue-0))))
   `(isearch ((,class (:foreground "#ffffff" :background ,orange-3))))
   `(lazy-highlight ((,class (:background ,"#FEEE0B"))))
   `(linum ((,class (:inherit default :background ,"#CDCDCD" :foreground ,"#ABABAB"))))
   ;; `(trailing-whitespace ((,class (:background ,red-1))))
   ;; ;; Mode line faces
   ;; `(mode-line ((,class (:box (:line-width -1 :style released-button)
   ;;  		 :background ,alum-2 :foreground ,alum-6))))
   ;; `(mode-line-inactive ((,class (:box (:line-width -1 :style released-button)
   ;;  			  :background ,alum-4 :foreground ,alum-6))))
   ;; ;; Escape and prompt faces
   ;; `(minibuffer-prompt ((,class (:weight bold :foreground ,blue-3))))
   ;; `(escape-glyph ((,class (:foreground ,red-3))))
   ;; `(error ((,class (:foreground ,red-3))))
   ;; `(warning ((,class (:foreground ,orange-3))))
   ;; `(success ((,class (:foreground ,cham-3))))
   `(show-paren-match ((,t (:background "#B4EDB3" :foreground "#FE0000"))))
   ;; ;; Font lock faces
   `(font-lock-builtin-face ((,t (:foreground ,"#808000"))))
   `(font-lock-comment-face ((,t (:foreground ,"#008000"))))
   `(font-lock-comment-delimiter-face((t (:inherit font-lock-comment-face))))
   `(font-lock-constant-face ((,t (:foreground ,"#000080"))))
   ;; `(font-lock-function-name-face ((,class (:foreground ,red-3))))
   `(font-lock-keyword-face ((,class (:foreground ,"#808000"))))
   `(font-lock-string-face ((,class (:foreground ,"#008000"))))
   `(font-lock-type-face ((,class (:foreground ,"#800080"))))
   `(font-lock-preprocessor-face ((,class (:foreground ,"#000080"))))
   `(font-lock-variable-name-face ((,class (:foreground ,orange-4))))
   ;; ;; Button and link faces
   `(link ((,class (:underline t :foreground ,"#0000FE"))))
   ;; `(link-visited ((,class (:underline t :foreground ,blue-2))))
   
   ;; Dired+
   '(diredp-dir-name ((t (:foreground "blue" :weight bold))))
   '(diredp-dir-priv ((t (:background "LightGray" :foreground "blue" :weight bold))))
   '(diredp-rare-priv ((t (:background "SpringGreen" :foreground "Magenta" :weight bold))))
   
   ;; ;; Message faces
   ;; `(message-header-name ((,class (:foreground ,blue-3))))
   ;; `(message-header-cc ((,class (:foreground ,butter-3))))
   ;; `(message-header-other ((,class (:foreground ,choc-2))))
   ;; `(message-header-subject ((,class (:foreground ,red-3))))
   ;; `(message-header-to ((,class (:weight bold :foreground ,butter-3))))
   ;; `(message-cited-text ((,class (:slant italic :foreground ,alum-5))))
   ;; `(message-separator ((,class (:weight bold :foreground ,cham-3))))
   ;; ;; SMerge
   ;; `(smerge-refined-change ((,class (:background ,plum-1))))
   ;; ;; Ediff
   ;; `(ediff-current-diff-A ((,class (:background ,blue-1))))
   ;; `(ediff-fine-diff-A ((,class (:background ,plum-1))))
   ;; `(ediff-current-diff-B ((,class (:background ,butter-1))))
   ;; `(ediff-fine-diff-B ((,class (:background ,orange-1))))
   )

  (custom-theme-set-variables
   'qtcreator
   `(ansi-color-names-vector [,alum-6 ,red-3 ,cham-3 ,butter-3
				      ,blue-3 ,plum-3 ,blue-1 ,alum-1])
   `(git-gutter:modified-sign "✱"))
  )



;;----------------------------------------------------------------------------
;;  Bold line number currently pointed by cursor
;;----------------------------------------------------------------------------
;; See http://stackoverflow.com/questions/10591334/colorize-current-line-number

(defface current-linum
  `((t :inherit linum :weight bold :foreground ,"#888888"))
  "Face for the current line number."
  :group 'linum)

(defvar current-linum-format-string "%3d")

(add-hook 'linum-before-numbering-hook 'current-linum-get-format-string)

(defun current-linum-get-format-string ()
  (let* ((width (1+  (length (number-to-string
                             (count-lines (point-min) (point-max))))))
         (format (concat "%" (number-to-string width) "d")))
    (setq current-linum-format-string format)))

(defvar current-linum 0)

(setq linum-format 'current-linum-format)

(defun current-linum-format (line-number)
  (propertize (format current-linum-format-string line-number) 'face
              (if (eq line-number current-linum)
                  'current-linum
                'linum)))

(defadvice linum-update (around current-linum-update activate compile)
  (if qtcreator-theme-bold-current-linum
    (let ((current-linum (line-number-at-pos)))
      ad-do-it)))

;;(ad-activate 'linum-update)

;;----------------------------------------------------------------------------

;;;###autoload
;; (when load-file-name
;;   (add-to-list 'custom-theme-load-path
;;                (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'qtcreator)

;; Local Variables:
;; no-byte-compile: t
;; End:

;;; qtcreator-theme.el ends here
