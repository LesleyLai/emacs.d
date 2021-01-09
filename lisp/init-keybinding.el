;; -*- lexical-binding: t -*-

;; Customized functions
(defun xah-delete-blank-lines ()
  "Delete all newline around cursor.

URL `http://ergoemacs.org/emacs/emacs_shrink_whitespace.html'
Version 2018-04-02"
  (interactive)
  (let ($p3 $p4)
          (skip-chars-backward "\n")
          (setq $p3 (point))
          (skip-chars-forward "\n")
          (setq $p4 (point))
          (delete-region $p3 $p4)))

(defun xah-shrink-whitespaces ()
  "Remove whitespaces around cursor to just one, or none.

Shrink any neighboring space tab newline characters to 1 or none.
If cursor neighbor has space/tab, toggle between 1 or 0 space.
If cursor neighbor are newline, shrink them to just 1.
If already has just 1 whitespace, delete it.

URL `http://ergoemacs.org/emacs/emacs_shrink_whitespace.html'
Version 2018-04-02T14:38:04-07:00"
  (interactive)
  (let* (
         ($eol-count 0)
         ($p0 (point))
         $p1 ; whitespace begin
         $p2 ; whitespace end
         ($charBefore (char-before))
         ($charAfter (char-after ))
         ($space-neighbor-p (or (eq $charBefore 32) (eq $charBefore 9) (eq $charAfter 32) (eq $charAfter 9)))
         $just-1-space-p
         )
    (skip-chars-backward " \n\t")
    (setq $p1 (point))
    (goto-char $p0)
    (skip-chars-forward " \n\t")
    (setq $p2 (point))
    (goto-char $p1)
    (while (search-forward "\n" $p2 t )
      (setq $eol-count (1+ $eol-count)))
    (setq $just-1-space-p (eq (- $p2 $p1) 1))
    (goto-char $p0)
    (cond
     ((eq $eol-count 0)
      (if $just-1-space-p
          (delete-horizontal-space)
        (progn (delete-horizontal-space)
               (insert " "))))
     ((eq $eol-count 1)
      (if $space-neighbor-p
          (delete-horizontal-space)
        (progn (xah-delete-blank-lines) (insert " "))))
     ((eq $eol-count 2)
      (if $space-neighbor-p
          (delete-horizontal-space)
        (progn
          (xah-delete-blank-lines)
          (insert "\n"))))
     ((> $eol-count 2)
      (if $space-neighbor-p
          (delete-horizontal-space)
        (progn
          (goto-char $p2)
          (search-backward "\n" )
          (delete-region $p1 (point))
          (insert "\n"))))
     (t (progn
          (message "nothing done. logic error 40873. shouldn't reach here" ))))))


(defun xah-beginning-of-line-or-block ()
  "Move cursor to beginning of line or previous paragraph.

• When called first time, move cursor to beginning of char in current line. (if already, move to beginning of line.)
• When called again, move cursor backward by jumping over any sequence of whitespaces containing 2 blank lines.

URL `http://ergoemacs.org/emacs/emacs_keybinding_design_beginning-of-line-or-block.html'
Version 2017-05-13"
  (interactive)
  (let (($p (point)))
    (if (or (equal (point) (line-beginning-position))
            (equal last-command this-command ))
        (if (re-search-backward "\n[\t\n ]*\n+" nil "NOERROR")
            (progn
              (skip-chars-backward "\n\t ")
              (forward-char ))
          (goto-char (point-min)))
      (progn
        (back-to-indentation)
        (when (eq $p (point))
          (beginning-of-line))))))

(defun xah-end-of-line-or-block ()
  "Move cursor to end of line or next paragraph.

• When called first time, move cursor to end of line.
• When called again, move cursor forward by jumping over any sequence of whitespaces containing 2 blank lines.

URL `http://ergoemacs.org/emacs/emacs_keybinding_design_beginning-of-line-or-block.html'
Version 2017-05-30"
  (interactive)
  (if (or (equal (point) (line-end-position))
          (equal last-command this-command ))
      (progn
        (re-search-forward "\n[\t\n ]*\n+" nil "NOERROR" ))
    (end-of-line)))

;; A lot of things come from ergoemacs
(global-set-key (kbd "M-i") 'previous-line)
(global-set-key (kbd "M-j") 'backward-char)
(global-set-key (kbd "M-k") 'next-line)
(global-set-key (kbd "M-l") 'forward-char)

 ;; Move by word
(global-set-key (kbd "M-u") 'backward-word)
(global-set-key (kbd "M-o") 'forward-word)

 ;; Move by sexp
(global-set-key (kbd "C-M-j") 'backward-sexp)
(global-set-key (kbd "C-M-l") 'forward-sexp)

;; Edit
(global-set-key (kbd "M-e") 'backward-kill-word)
(global-set-key (kbd "M-r") 'kill-word)
(global-set-key (kbd "M-d") 'backward-delete-char)
(global-set-key (kbd "M-f") 'delete-forward-char)

;; CUA mode enables copy, paste and cut
(use-package cua-base
  :config (cua-mode)
  :bind (:map cua--cua-keys-keymap
              ;; The keys for yank are implemented by a hydra later
              ("C-v" . nil)
              ("M-v" . nil)))

(use-package undo-tree
  :ensure t
  :diminish
  :config
  (global-undo-tree-mode 1)
  (defalias 'redo 'undo-tree-redo)
  :bind (
         ("C-z" . undo)
         ("C-S-z" . redo) ; Apple style redo
         ("C-y" . undo-tree-redo) ; MS style redo
         :map undo-tree-map
         ("C-?" . nil)
         ("C-/" . nil)
         ("C-_" . nil)
         ("M-_" . nil)))

(global-set-key (kbd "C-/") 'comment-line)
(global-unset-key (kbd "C-_"))
(global-unset-key (kbd "C-x u"))


;; ;; Buffer movement
;; ;; Use <Shift> + arrowkeys to move between buffers
;; ;; {{
(windmove-default-keybindings)
;; ;; }}

;; Search & Replace {{
(global-set-key (kbd "C-f") 'isearch-forward)
(global-set-key (kbd "C-S-f") 'query-replace)


;; ;; set arrow keys in isearch. left/right is backward/forward,
;; ;; up/down is history. press Return to exit
(define-key isearch-mode-map (kbd "<up>") 'isearch-ring-retreat)
(define-key isearch-mode-map (kbd "<down>") 'isearch-ring-advance)
(define-key isearch-mode-map (kbd "<left>") 'isearch-repeat-backward)
(define-key isearch-mode-map (kbd "<right>") 'isearch-repeat-forward)
(define-key isearch-mode-map (kbd "M-i") 'isearch-ring-retreat)
(define-key isearch-mode-map (kbd "M-k") 'isearch-ring-advance)
(define-key isearch-mode-map (kbd "M-j") 'isearch-repeat-backward)
(define-key isearch-mode-map (kbd "M-l") 'isearch-repeat-forward)
(define-key isearch-mode-map (kbd "C-f") 'isearch-repeat-forward)

(define-key minibuffer-local-isearch-map (kbd "<left>")
  'isearch-reverse-exit-minibuffer)
(define-key minibuffer-local-isearch-map (kbd "M-j")
  'isearch-reverse-exit-minibuffer)
(define-key minibuffer-local-isearch-map (kbd "<right>")
  'isearch-forward-exit-minibuffer)
(define-key minibuffer-local-isearch-map (kbd "M-l")
  'isearch-forward-exit-minibuffer)
;; }}



;; ;; Menu bar
(global-set-key [f1] 'menu-bar-mode)

;; reclaim some binding used by ibuffer.el
(add-hook 'ibuffer-mode-hook
 (lambda ()
   (define-key ibuffer-mode-map (kbd "M-j") 'backward-char) ; was ibuffer-jump-to-filter-group.
   (define-key ibuffer-mode-map (kbd "M-o") 'forward-word) ; was ibuffer-visit-buffer-1-window
))

;; prevent cua-mode from going into selection mode when commands with Shift key is used.
(add-hook 'cua-mode-hook
 (lambda ()
    (put 'cua-scroll-down 'CUA nil)
    (put 'cua-scroll-up 'CUA nil)
    (put 'backward-paragraph 'CUA nil)
    (put 'forward-paragraph 'CUA nil)
    (put 'beginning-of-buffer 'CUA nil)
    (put 'end-of-buffer 'CUA nil)
    (put 'move-end-of-line 'CUA nil)))

;; Standard Shortcuts
(global-set-key (kbd "C-w") 'kill-current-buffer) ; Close
(global-set-key (kbd "C-o") 'find-file) ; Openi
(global-set-key (kbd "C-s") 'save-buffer) ; Save
(global-set-key (kbd "C-S-s") 'write-file) ; Save As.
(global-set-key (kbd "C-a") 'mark-whole-buffer) ; Select all

;; Command history of interpretor
(use-package comint
  :defer t
  :config
  (progn
    (define-key comint-mode-map (kbd "<down>") #'comint-next-input)
    (define-key comint-mode-map (kbd "<up>") #'comint-previous-input)
    (setf comint-prompt-read-only t
          comint-history-isearch t)))

;; Dired
(add-hook 'dired-mode-hook
 (lambda ()
  (define-key dired-mode-map (kbd "C-n") 'new-empty-buffer) ; was dired-next-line
  (define-key dired-mode-map (kbd "M-o") 'forward-word) ; was dired-omit-mode
  (define-key dired-mode-map (kbd "M-i") 'previous-line)
  (define-key dired-mode-map (kbd "M-l") 'forward-char)
  ))

(use-package hydra
  :ensure t
  :config

  (use-package major-mode-hydra
    :ensure t
    :after all-the-icons
    :bind
    ("<f9>" . major-mode-hydra)
    :config
    (defun with-faicon (icon str &optional height v-adjust)
      (s-concat (all-the-icons-faicon icon :v-adjust (or v-adjust 0) :height (or height 1)) " " str)))

  ;; Support for nested hydra
  (defvar hydra-stack nil)

  (defun hydra-push (expr)
    (push `(lambda () ,expr) hydra-stack))

  (defun hydra-pop ()
    (interactive)
    (let ((x (pop hydra-stack)))
      (when x
        (funcall x))))

  ;; Hydra for moving window splitter
  (defun hydra-move-splitter-left (arg)
    "Move window splitter left."
    (interactive "p")
    (if (let ((windmove-wrap-around))
          (windmove-find-other-window 'right))
        (shrink-window-horizontally arg)
      (enlarge-window-horizontally arg)))

  (defun hydra-move-splitter-right (arg)
    "Move window splitter right."
    (interactive "p")
    (if (let ((windmove-wrap-around))
          (windmove-find-other-window 'right))
        (enlarge-window-horizontally arg)
      (shrink-window-horizontally arg)))

  (defun hydra-move-splitter-up (arg)
    "Move window splitter up."
    (interactive "p")
    (if (let ((windmove-wrap-around))
          (windmove-find-other-window 'up))
        (enlarge-window arg)
      (shrink-window arg)))

  (defun hydra-move-splitter-down (arg)
    "Move window splitter down."
    (interactive "p")
    (if (let ((windmove-wrap-around))
          (windmove-find-other-window 'up))
        (shrink-window arg)
      (enlarge-window arg)))

  (defhydra hydra-splitter(:color amaranth)
    "Window Resizing"
    ("j" hydra-move-splitter-left "Move splitter left")
    ("k" hydra-move-splitter-down "Move splitter down")
    ("i" hydra-move-splitter-up "Move splitter up")
    ("l" hydra-move-splitter-right "Move splitter right")
    ("q" hydra-pop "Exit" :exit t))

  ;; Hydra for window management
  (defhydra hydra-window (:pre (setq which-key-inhibit t)
                               :post (setq which-key-inhibit nil)
                               :color amaranth)
    "Window"
    ("k" windmove-down "down" :column "Navigation")
    ("i" windmove-up "up")
    ("j" windmove-left "left")
    ("l" windmove-right "right")
    ("<down>" windmove-down nil)
    ("<up>" windmove-up nil)
    ("<left>" windmove-left nil)
    ("<right>" windmove-right nil)
    ("1" (progn
           (delete-other-windows)
           (hydra-pop)) "delete other windows" :column "Management")
    ("2" split-window-below "split below")
    ("3" split-window-right "split right")
    ("0" delete-window "delete current window")
    ("s" (progn
         (hydra-splitter/body)
         (hydra-push '(hydra-window/body)))
     "Resize window" :exit t)
    ("t" (progn
         (hydra-tab-bar/body)
         (hydra-push '(hydra-window/body)))
     "Manage Tabs" :exit t)
    ("b" switch-to-buffer "Switch buffer" :column "Buffer")
    ("o" find-file "Find file")
    ("w" kill-current-buffer "Kill current buffer")
    ("a" execute-extended-command "M-x" :column "Other")
    ("z" winner-undo "undo")
    ("y" winner-redo "redo")
    ("q" hydra-pop "Exit" :exit t))
  (global-set-key (kbd "C-c w") #'hydra-window/body)
  (global-set-key (kbd "C-x 1") #'hydra-window/lambda-1)
  (global-set-key (kbd "C-x 2") #'hydra-window/split-window-below)
  (global-set-key (kbd "C-x 3") #'hydra-window/split-window-right)
  (global-set-key (kbd "C-x 0") #'hydra-window/delete-window)

  ;; Hydra for yank
  (defhydra hydra-yank-pop ()
    "yank"
    ("C-v" yank nil)
    ("M-v" yank-pop nil)
    ("m" (yank-pop 1) "next")
    ("n" (yank-pop -1) "prev")
    ("l" helm-show-kill-ring "list" :color blue)
    ("q" nil "Exit" :exit t))
  (global-set-key (kbd "M-v") #'hydra-yank-pop/yank-pop)
  (global-set-key (kbd "C-v") #'hydra-yank-pop/yank)

  ;; Hydra for keyboard macro
  (defhydra hydra-keyboard-macros ()
    "Keyboard Macro"
    ("e" kmacro-end-or-call-macro-repeat "execute")
    ("u" (lambda()
           (interactive)
           (let ((current-prefix-arg 0))
             (call-interactively #'kmacro-end-or-call-macro)))
     "loop")
    ("s" (lambda ()
           (interactive)
           (isearch-forward-regexp)
           (hydra-keyboard-macros/body))  "search" :color blue)
    ("r" (lambda ()
           (interactive)
           (isearch-backward-regexp)
           (hydra-keyboard-macros/body))  "reverse search" :color blue)
    ("'" edit-last-kbd-macro "edit")
    ("q" nil "Exit"))
  (global-set-key
   (kbd "C-x e")
   #'hydra-keyboard-macros/kmacro-end-or-call-macro-repeat))

;;;; hydra for rectangle-mark-mode
(defhydra hydra-rectangle (:pre (rectangle-mark-mode 1)
                                :color pink
                                :hint nil
                                :post (deactivate-mark))
  "
  Rectangle editing
  ^_i_^       _c_opy        _o_pen       _N_umber-lines
_j_   _l_     _v_ paste     _t_ype       _e_xchange-point
  ^_k_^       _x_ cut       _T_ insert   _r_eset-region-mark
^^^^          _z_ undo      _SPC_ clear  _q_uit
  "
  ("i" rectangle-previous-line)
  ("k" rectangle-next-line)
  ("j" rectangle-backward-char)
  ("l" rectangle-forward-char)
  ("x" kill-rectangle)                    ;; C-x r k
  ("v" yank-rectangle)                    ;; C-x r y
  ("c" copy-rectangle-as-kill)            ;; C-x r M-w
  ("o" open-rectangle)                    ;; C-x r o
  ("t" string-rectangle)                  ;; C-x r t
  ("T" string-insert-rectangle)           ;; C-x r t
  ("SPC" clear-rectangle)                 ;; C-x r c
  ("e" rectangle-exchange-point-and-mark) ;; C-x C-x
  ("N" rectangle-number-lines)            ;; C-x r N
  ("r" (if (region-active-p)
           (deactivate-mark)
         (rectangle-mark-mode 1)))
  ("z" undo nil)
  ("<down-mouse-1>" mouse-start-rectangle nil)
  ("q" nil))

(global-set-key (kbd "S-<down-mouse-1>") #'hydra-rectangle/mouse-start-rectangle)
(global-set-key (kbd "C-x SPC") #'hydra-rectangle/body)
;;;; Qt Creator style keybindings
(global-set-key (kbd "<M-up>") #'hydra-rectangle/rectangle-previous-line)
(global-set-key (kbd "<M-down>") #'hydra-rectangle/rectangle-next-line)
(global-set-key (kbd "<M-left>") #'hydra-rectangle/rectangle-backward-char)
(global-set-key (kbd "<M-right>") #'hydra-rectangle/rectangle-forward-char)

;;;; Rectangle-mark-mode for mouse
(defun mouse-start-rectangle (start-event)
  (interactive "e")
  (deactivate-mark)
  (mouse-set-point start-event)
  (rectangle-mark-mode +1)
  (let ((drag-event))
    (track-mouse
      (while (progn
               (setq drag-event (read-event))
               (mouse-movement-p drag-event))
        (mouse-set-point drag-event)))))

;; mode toggling
(pretty-hydra-define hydra-toggles
  (:color amaranth :quit-key "q" :title (with-faicon "toggle-on" "Toggles" 1 -0.05))
  ("Basic"
   (("n" display-line-numbers-mode "line number" :toggle t)
    ("w" whitespace-mode "whitespace" :toggle t)
    ("r" rainbow-mode "rainbow" :toggle t))
   "Coding"
   (("S" superword-mode "superword mode" :toggle t)
    ("s" subword-mode "subword mode" :toggle t)
    ("f" flycheck-mode "flycheck" :toggle t)
    ("c" company-mode "company mode" :toggle t))
   "Debug"
   (("e" toggle-debug-on-error "debug on error" :toggle (default-value 'debug-on-error))
    ("g" toggle-debug-on-quit "debug on quit" :toggle (default-value 'debug-on-quit)))))
(global-set-key (kbd "C-c t") 'hydra-toggles/body)

;; Ryo modal mode
(use-package ryo-modal
  :after hydra
  :ensure t
  :commands ryo-modal-mode
  :diminish ryo-modal-mode
  :bind (("C-c SPC" . ryo-modal-mode)
         ("<escape>" . ryo-modal-mode))
  :config
  (add-hook 'text-mode-hook #'ryo-modal-mode)
  (add-hook 'prog-mode-hook #'ryo-modal-mode)
  (setq ryo-modal-cursor-color nil)
  (defun suppress ()
    (interactive) (message "This key is not binded"))

  ;; Convenience
  (ryo-modal-keys
   ("." ryo-modal-repeat)
   ("f" ryo-modal-mode))

  ;; Movements
  (ryo-modal-keys
   (:mc-all)
   ("a" "M-x")
   ("i" previous-line)
   ("j" backward-char)
   ("k" next-line)
   ("l" forward-char)
   ("u" backward-word)
   ("o" forward-word)
   ("q" switch-window)
   ("h" xah-beginning-of-line-or-block)
   (";" xah-end-of-line-or-block))

  ;; Editing
  (ryo-modal-keys
   ("w" xah-shrink-whitespaces)
   ("z" "C-z")
   ("x" kill-region)
   ("c" copy-region-as-kill)
   ("v" hydra-yank-pop/yank)

   ("e" "M-e")
   ("r" "M-r")
   ("y" "M-y")
   ("d" "M-d")
   ("g" "M-f")

   ("t" cua-set-mark))

  ;; Space leader key
  (ryo-modal-key
   "SPC" '(("a" mark-whole-buffer)
           ("b" switch-to-buffer)
           ("g" magit-status)
           ("w" hydra-window/body :name "Window navigation and management")
           ("r" hydra-rectangle/body :name "Rectangle Editing")
           ("<tab>" hydra-tab-bar/body :name "Tab Bar")
           ("t" hydra-toggles/body :name "Toggle")
           ("SPC" major-mode-hydra)))

  (ryo-modal-keys
   ("SPC o"
    (("SPC" whitespace-mode)
     ("a" org-agenda)
     ("d" dired)
     ("p" list-packages)
     ("t" term)
     ("r" read-only-mode))
    :name "options and open"))

  (ryo-modal-keys
   ("SPC f"
    (("i" ibuffer :name "list buffers")
     ("b" bookmark-bmenu-list :name "list bookmarks")
     ("m" bookmark-set :name "set bookmark")
     ("f" find-file))
    :name "find and files"))

  ;; Unbind unassigned keys
  (ryo-modal-keys
   ("-" suppress)
   ("p" suppress)
   ("s" suppress)
   ("b" suppress)
   ("'" suppress)
   ("m" suppress)
   ("n" suppress)
   ("," suppress)
   ("/" suppress)
   ("-" suppress)
   ("=" suppress)))

;; Moving lines or selected region up and down
(use-package move-lines
  :load-path "site-lisp/move-lines"
  :config
    (defhydra hydra-move-lines ()
      "Move Lines"
      ("i" move-lines-up "Move lines up")
      ("k" move-lines-down "Move lines down")
      ("<up>" move-lines-up nil)
      ("<down>" move-lines-down nil)
      ("q" nil "Exit" :exit t))
  :bind (("C-S-i" . hydra-move-lines/move-lines-up)
         ("C-S-k" . hydra-move-lines/move-lines-down)
         ("C-S-<up>" . hydra-move-lines/move-lines-up)
         ("C-S-<down>" . hydra-move-lines/move-lines-down)))

(use-package tab-bar
  :config
  (defhydra hydra-tab-bar (:pre (setq which-key-inhibit t)
                                :post (setq which-key-inhibit nil)
                                :color amaranth)
    "Tab Bar Operations"
    ("t" tab-new "Create a new tab" :column "Creation")
    ("d" dired-other-tab "Open Dired in another tab")
    ("f" find-file-other-tab "Find file in another tab")
    ("0" tab-close "Close current tab")
    ("m" tab-move "Move current tab" :column "Management")
    ("r" tab-rename "Rename Tab")
    ("<return>" tab-bar-select-tab-by-name "Select tab by name" :column "Navigation")
    ("C-<tab>" tab-next nil)
    ("C-S-<tab>" tab-previous nil)
    ("<tab>" tab-next nil)
    ("S-<tab>" tab-previous nil)
    ("l" tab-next "Next Tab")
    ("j" tab-previous "Previous Tab")
    ("w" (progn
           (hydra-window/body)
           (hydra-push '(hydra-tab-bar/body)))
     "Manage Windows" :exit t :column "Others")
    ("q" hydra-pop "Exit" :exit t))
  :bind (("C-x t" . hydra-tab-bar/body)
         ("C-t" . hydra-tab-bar/tab-new)
         ("C-<tab>" . hydra-tab-bar/tab-next)
         ("C-S-<tab>" . hydra-tab-bar/tab-previous)))

;;
;; Statistics
;; -----------------------------------------------------------------------------

;; https://github.com/dacap/keyfreq
(use-package keyfreq
  :ensure
  :hook
  (after-init . keyfreq-mode)
  (after-init . keyfreq-autosave-mode)
  :config
  (setq keyfreq-excluded-commands
        '(self-insert-command
          mouse-drag-region
          mouse-set-point
          mouse-set-region
          mwheel-scroll
          forward-char
          backward-char
          right-char
          left-char
          previous-line
          next-line
          newline
          forward-word
          backward-word
          cua-cut-region
          cua-paste
          dap-tooltip-mouse-motion
          lsp-ui-doc--handle-mouse-movement
          org-cycle
          org-return
          org-self-insert-command
          org-delete-backward-char
          ignore
          save-buffer
          delete-backward-char
          paredit-backward-delete
          c-electric-backspace
          eshell-previous-matching-input-from-input
          isearch-printing-char
          eshell-send-input
          backward-kill-word
          cua-copy-region
          company-complete-selection
          pdf-util-image-map-mouse-event-proxy
          vterm--self-insert
          )))

(provide 'init-keybinding)
