(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp"))

;;--------------------------------------------------------------------
;; Which functionality to enable (use t or nil for true and false)
;;--------------------------------------------------------------------
(setq *is-a-mac* (eq system-type 'darwin))
(setq *win64* (eq system-type 'windows-nt) )
(setq *cygwin* (eq system-type 'cygwin) )
(setq *linux* (or (eq system-type 'gnu/linux) (eq system-type 'linux)) )
(setq *unix* (or *linux* (eq system-type 'usg-unix-v) (eq system-type 'berkeley-unix)) )

;; @see https://www.reddit.com/r/emacs/comments/3kqt6e/2_easy_little_known_steps_to_speed_up_emacs_start/
;;(set gc-cons-threshold 100000000)
;; Normally file-name-handler-alist is set to
;; (("\\`/[^/]*\\'" . tramp-completion-file-name-handler)
;; ("\\`/[^/|:][^/|]*:" . tramp-file-name-handler)
;; ("\\`/:" . file-name-non-special))
;; Which means on every .el and .elc file loaded during start up, it has to runs those regexps against the filename.
(let ((file-name-handler-alist nil))
  ;; Universal packages
  (require 'cl)
  
   ;; Do this before other things
  (require 'init-elpa)

  (require 'init-company)
  (require 'init-css)
  (require 'init-cc-mode)
  (require 'init-c++)
  (require 'init-dired)
  (require 'init-flycheck)
  (require 'init-ui)
  (require 'init-linum-mode)
  (require 'init-lisp)
  (require 'init-misc)
  (require 'init-yasnippet)
  (require 'init-web)
  (require 'init-ruby)

  (require 'init-keybinding)

  ;; Customize {{
  ;; Theme
  (setq custom-theme-directory "~/.emacs.d/themes")
  
  ;; Private Setting
  (setq custom-file "~/.emacs.d/custom/custom.el")
  (if (file-exists-p "~/.emacs.d/custom/custom.el") (load-file "~/.emacs.d/custom/custom.el"))
  ;; }}
  )
