;; ==============================================
;; Initializes the UI of emacs that fit my habits
;; ==============================================

;; Fonts
(set-face-attribute 'line-number nil :inherit 'fixed-pitch)
(set-face-attribute 'cursor nil :inherit 'fixed-pitch)

;; Window/frame titles
(customize-set-variable 'frame-title-format (list "%b " "%[ - GNU %F " emacs-version))
(customize-set-variable 'icon-title-format (list "%b " " - GNU %F " emacs-version))

(show-paren-mode 1)

(customize-set-variable 'indent-tabs-mode nil)

(setq select-enable-clipboard t
      select-enable-primary t
      save-interprogram-paste-before-kill t
      apropos-do-all t
      mouse-yank-at-point t
      require-final-newline t
      visible-bell t
      load-prefer-newer t
      ediff-window-setup-function 'ediff-setup-windows-plain
      save-place-file (concat user-emacs-directory "places")
      backup-directory-alist `(("." . ,(concat user-emacs-directory
                                               "backups"))))

(use-package all-the-icons
  :ensure t
  :defer 0.5)

(use-package doom-modeline
  :after all-the-icons
  :ensure t
  :init (doom-modeline-mode 1)
  :custom
  (doom-modeline-minor-modes t)
  (doom-modeline-buffer-encoding nil)
  (doom-modeline-lsp nil))

;; Emacs which-key showes displays the key bindings following your
;; currently entered incomplete command (a prefix) in a popup.
(use-package which-key
  :ensure t
  :diminish
  :defer 5
  :custom
  (which-key-enable-extended-define-key t)
  :config
  (which-key-mode)
  (which-key-setup-side-window-right-bottom)
  (push '((nil . "ryo:.*:") . (nil . "")) which-key-replacement-alist))

;; Cursor Type
(setq-default cursor-type 'bar)

;; No useless GUI features
(customize-set-variable 'use-file-dialog nil)
(customize-set-variable 'use-dialog-box nil)
(push '(tool-bar-lines . 0) default-frame-alist)
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(when (fboundp 'horizontal-scroll-bar-mode)
  (horizontal-scroll-bar-mode -1))

;; No startup screen
(customize-set-variable 'inhibit-startup-screen t)
(customize-set-variable 'inhibit-startup-echo-area-message t)

;; Non-zero values for `line-spacing' can mess up ansi-term and co,
;; so we zero it explicitly in those cases.
(add-hook 'term-mode-hook
          (lambda ()
            (customize-set-variable 'line-spacing 0)))

(defun prettify-symbols-hook ()
  "Set pretty symbols for programming modes."
  (setq-local
   prettify-symbols-alist
   (append
        '(;; Symbols
          ("lambda" . ?λ)

          ;; Operators
          ("<=" . ?≤)
          (">=" . ?≥)
          ("<-" . ?←)
          ("->" . ?→)
          ("=>" . ?⇒)
          (".." . ?‥)
          ("...". ?…)
          ("infinity" . ?∞)
          ;; ("in" . ?∈)
          ;; ("not in" . ?∉)

          ;; Math
          ("pi" . ?π)
          ("theta" . ?θ)
          ("sqrt" . ?√)
          ("sum"  . ?Σ)
          )
        prettify-symbols-alist
        )))

(use-package prettify-symbols-mode
  :init
  (global-prettify-symbols-mode 1)
  :bind ("C-c <C-return>" . prettify-symbols-mode)
  :config
  (setq prettify-symbols-unprettify-at-point 'right-edge)
  :hook((prog-mode . prettify-symbols-hook))
  )

(use-package paradox
  :ensure t
  :defer 1
  :custom
  (paradox-column-width-package 27)
  (paradox-column-width-version 13)
  (paradox-execute-asynchronously t)
  (paradox-hide-wiki-packages t)
  :config
  (paradox-enable)
  (remove-hook 'paradox-after-execute-functions #'paradox--report-buffer-print))

(use-package helpful
  :ensure t
  :bind
  ([remap describe-function] . helpful-callable)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-key] . helpful-key)
  :ryo
  ("SPC h"
     (("a" apropos-command)
      ("b" describe-bindings)
      ("c" describe-char)
      ("f" describe-function)
      ("k" describe-key)
      ("q" which-key-show-full-major-mode :name "show major mode keys")
      ("w" which-key-show-minor-mode-keymap :name "show minor mode keys")
      ("t" which-key-show-top-level :name "show top-level keys")
      ("m"  describe-mode)
      ("v" describe-variable)
      ("s" describe-syntax))
     :name "help"))

(use-package posframe :defer t :ensure t)

(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook)
  (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*"))))

(use-package face-remap
  :diminish buffer-face-mode)

(provide 'init-ui)
