(require 'package)
(add-to-list 'package-archives '("marmalade" . "https://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))

;; Packages to install
(defvar my-packages '(counsel ;; auto completion tool
                      clang-format ;; clang format
                      paredit ;; keeps parentheses under control.
                      idle-highlight-mode
                      ;;ido-ubiquitous
                      ;;find-file-in-project
                      magit ;; ui for git
                      modern-cpp-font-lock ;; Modern c++ syntax highlighter
                      smex ;; M-x enhancement
                      undo-tree
                      rainbow-delimiters ;;Highlight brackets according to their depth
                      rainbow-mode ;;Colorize color names in buffers
                      ;;scpaste
                      ))

(package-initialize)
(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(provide 'init-elpa)
