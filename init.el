(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp"))

;;----------------------------------------------------------------------------
;; Which functionality to enable (use t or nil for true and false)
;;----------------------------------------------------------------------------
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

  (require 'init-keybinding)

  ;; Customize {{
  ;; Theme
  (setq custom-theme-directory "~/.emacs.d/themes")
  
  ;; Private Setting
  (if (file-exists-p "~/.emacs.d/custom/custom.el") (load-file "~/.emacs.d/custom/custom.el"))
  ;; }}
  )



(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#2e3436" "#a40000" "#4e9a06" "#c4a000" "#204a87" "#5c3566" "#729fcf" "#eeeeec"])
 '(custom-enabled-themes (quote (qtcreator)))
 '(custom-safe-themes
   (quote
    ("694dab4e0ec836cb4611b7e719c1acdc6e768f29026254074c80c5d9604b8907" "39eb5be8708007478e2bc2e3403200c4f7477aeecf37b2ed8d98cda6cc7794f8" "02451dc7833900ade905ca51f10029e7ccea36b8af7536a97f44b29d28db96e2" "47586379a76a98f2a35378cace6cd4669dd5d4dfbe8470c10f24aa1c9b933102" "b2d859f0083fdba5183584d3e848c83733ae60a13c2e7df5a8efda3dec6ce789" "89c7a46ef943bb98dd463b150723b1485e384cfc8a8b602869221b24bc5d07e9" default)))
 '(git-gutter:modified-sign "✱"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(diredp-dir-name ((t (:foreground "blue" :weight bold))))
 '(diredp-dir-priv ((t (:background "LightGray" :foreground "blue" :weight bold))))
 '(diredp-rare-priv ((t (:background "SpringGreen" :foreground "Magenta" :weight bold)))))
