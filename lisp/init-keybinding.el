;; -*- lexical-binding: t -*-

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

;; Copy, paste and cut
(cua-mode)

(use-package undo-tree
  :ensure t
  :config
  (global-undo-tree-mode 1)
  (defalias 'redo 'undo-tree-redo)
  :bind (
         ("C-z" . undo-tree-undo)
         ("C-S-z" . undo-tree-redo) ; Apple style redo
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

; Which key mode
(use-package which-key
  :ensure
  :config
  (which-key-setup-side-window-right-bottom)
  (push '((nil . "ryo:.*:") . (nil . "")) which-key-replacement-alist))

;; Dired
(add-hook 'dired-mode-hook
 (lambda ()
  (define-key dired-mode-map (kbd "C-n") 'new-empty-buffer) ; was dired-next-line
  (define-key dired-mode-map (kbd "M-o") 'forward-word) ; was dired-omit-mode
  (define-key dired-mode-map (kbd "M-i") 'previous-line)
  (define-key dired-mode-map (kbd "M-l") 'forward-char)
  ))

(use-package ryo-modal
  :ensure t
  :commands ryo-modal-mode
  :bind (("C-c SPC" . ryo-modal-mode)
         ("<escape>" . ryo-modal-mode))
  :config
  (add-hook 'text-mode-hook #'ryo-modal-mode)
  (add-hook 'prog-mode-hook #'ryo-modal-mode)
  (setq ryo-modal-cursor-color nil)
  (defun suppress ()
      (interactive) (message "This key is not binded"))

  ;; Movements
  (ryo-modal-keys
   ("," ryo-modal-repeat)
   ("f" ryo-modal-mode)
   ("a" "M-x")
   ("i" previous-line)
   ("j" backward-char)
   ("k" next-line)
   ("l" forward-char)
   ("u" backward-word)
   ("o" forward-word)
   ("h" "M-m")
   (";" "C-e")
   ("." "M-g M-g"))

  ;; Number arguments
  (ryo-modal-keys
   ;; First argument to ryo-modal-keys may be a list of keywords.
   ;; These keywords will be applied to all keybindings.
   (:norepeat t)
   ("0" "M-0")
   ("1" "M-1")
   ("2" "M-2")
   ("3" "M-3")
   ("4" "M-4")
   ("5" "M-5")
   ("6" "M-6")
   ("7" "M-7")
   ("8" "M-8")
   ("9" "M-9"))

  ;; Editing
  (ryo-modal-keys
   ("z" "C-z")
   ("x" kill-region)
   ("c" copy-region-as-kill)
   ("v" yank)

   ("e" "M-e")
   ("r" "M-r")
   ("y" "M-y")
   ("d" "M-d")
   ("g" "M-f")

   ("t" cua-set-mark))

  ;; Space leader key
  (ryo-modal-key
   "SPC" '(("g" magit-status)
           ("a" org-agenda)))

  ;; Unbind unassigned keys
  (ryo-modal-keys
   ("-" suppress)
   ("q" suppress)
   ("w" suppress)
   ("p" suppress)
   ("s" suppress)
   ("'" suppress)
   ("b" suppress)
   ("n" suppress)
   ("m" suppress)
   ("/" suppress)
   ("-" suppress)
   ("=" suppress)))

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
          )))

(provide 'init-keybinding)
