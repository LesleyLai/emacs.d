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
                      sudo-edit

                      ;; Directory management {{
                      dired+
                      ;; }}
                      
                      paredit ;; keeps parentheses under control.
                      ggtags ;; GNU Global source code tagging system

                      ;; flycheck for syntax check {{
                      flycheck
                      flycheck-cstyle
                      ;; }}

                      ;; ui
                      ;; {{
                      smart-mode-line
                      smart-mode-line-powerline-theme
                      fill-column-indicator ;; 80 line check
                      idle-highlight-mode
                      ;; }}
                      
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


;;; On-demand installation of packages
(defun require-package (package &optional min-version no-refresh)
  "Install given PACKAGE, optionally requiring MIN-VERSION.
If NO-REFRESH is non-nil, the available package lists will not be
re-downloaded in order to locate PACKAGE."
  (if (package-installed-p package min-version)
      t
    (if (or (assoc package package-archive-contents) no-refresh)
        (if (boundp 'package-selected-packages)
            ;; Record this as a package the user installed explicitly
            (package-install package nil)
          (package-install package))
      (progn
        (package-refresh-contents)
        (require-package package min-version t)))))

(provide 'init-elpa)
