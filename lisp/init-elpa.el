(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("marmalade" . "https://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))

;; Packages to install
(defvar my-packages '(counsel ;; auto completion tool
                      company ;; auto completion framework
                      company-c-headers
                      company-tern
                      clang-format ;; clang format

                      ;; Directory management {{
                      dired+
                      ;; }}
                      
                      paredit ;; keeps parentheses under control.
                      ggtags ;; GNU Global source code tagging system

                      ;; flycheck for syntax check {{
                      flycheck
                      flycheck-cstyle
                      ;; }}
                      
                      idle-highlight-mode
                      ido-ubiquitous ;; Use ido everywhere
                      magit ;; ui for git
                      modern-cpp-font-lock ;; Modern c++ syntax highlighter
                      smex ;; M-x enhancement
                      undo-tree
                      rainbow-delimiters ;;Highlight brackets according to their depth
                      rainbow-mode ;;Colorize color names in buffers
                      yasnippet

                      ;; web {{
                      web-mode
                      sml-mode
                      ;; }}
                      ))

(package-initialize)
(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(provide 'init-elpa)
