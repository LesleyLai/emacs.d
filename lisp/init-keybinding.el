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

(global-unset-key (kbd "C-/"))
(global-unset-key (kbd "C-_"))
(global-unset-key (kbd "C-x u"))


;; ;; Buffer movement
;; ;; Use <Shift> + arrowkeys to move between buffers
;; ;; {{
(windmove-default-keybindings)
;; ;; }}

;; Search & Replace {{
(global-set-key (kbd "C-f") 'isearch-forward-regexp)
(global-set-key (kbd "C-S-f") 'query-replace-regexp)


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
    (put 'move-end-of-line 'CUA nil)
   )
 )

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
  )

;; Dired
(add-hook 'dired-mode-hook
 (lambda ()
  (define-key dired-mode-map (kbd "C-n") 'new-empty-buffer) ; was dired-next-line
  (define-key dired-mode-map (kbd "M-o") 'forward-word) ; was dired-omit-mode
  (define-key dired-mode-map (kbd "M-i") 'previous-line)
  (define-key dired-mode-map (kbd "M-l") 'forward-char)
  ))

(use-package modalka
  :ensure t
  :config
  (global-set-key (kbd "<escape>") #'modalka-mode)
  (add-hook 'text-mode-hook #'modalka-mode)
  (add-hook 'prog-mode-hook #'modalka-mode)

  (defun modalka-unbind-kbd (key)
      (define-key modalka-mode-map (kbd key)
        '(lambda () (interactive) (format "Key %s in is modalka unbinded" key))))

  ;; CUA
  (modalka-define-kbd "z" "C-z")
  (define-key modalka-mode-map (kbd "x") #'cua-cut-region)
  (define-key modalka-mode-map (kbd "c") #'cua-copy-region)
  (define-key modalka-mode-map (kbd "v") #'cua-paste)

  ;; Movements
  (modalka-define-kbd "u" "M-u")
  (modalka-define-kbd "i" "M-i")
  (modalka-define-kbd "o" "M-o")
  (modalka-define-kbd "j" "M-j")
  (modalka-define-kbd "k" "M-k")
  (modalka-define-kbd "l" "M-l")
  (modalka-define-kbd "h" "M-m") ;; Back to indentation
  (modalka-define-kbd ";" "C-e") ;; Move to endline

  ;; Shift
  (modalka-define-kbd "J" "M-J")
  (modalka-define-kbd "K" "M-K")
  (modalka-define-kbd "L" "M-L")
  (modalka-define-kbd "I" "M-I")

  (modalka-define-kbd "M-j" "C-M-j")
  (modalka-define-kbd "M-l" "C-M-l")
  ;; Jump
  (modalka-define-kbd "/" "M-g M-g")

  (modalka-unbind-kbd "1")
  (modalka-unbind-kbd "2")
  (modalka-unbind-kbd "3")
  (modalka-unbind-kbd "4")
  (modalka-unbind-kbd "5")
  (modalka-unbind-kbd "6")
  (modalka-unbind-kbd "7")
  (modalka-unbind-kbd "8")
  (modalka-unbind-kbd "9")
  (modalka-unbind-kbd "0")
  (modalka-unbind-kbd "-")
  (modalka-unbind-kbd "=")

  (modalka-unbind-kbd "q")
  (modalka-unbind-kbd "w")
  (modalka-define-kbd "e" "M-e")
  (modalka-define-kbd "r" "M-r")
  (modalka-unbind-kbd "t")
  (modalka-unbind-kbd "y")
  (modalka-unbind-kbd "p")
  (modalka-unbind-kbd "a")
  (modalka-unbind-kbd "s")
  (modalka-define-kbd "d" "M-d")
  (define-key modalka-mode-map (kbd "f") #'modalka-mode)
  (modalka-define-kbd "g" "M-f")
  (modalka-unbind-kbd "'")
  (modalka-unbind-kbd "b")
  (modalka-unbind-kbd "n")
  (modalka-unbind-kbd "m")
  (modalka-unbind-kbd ",")
  (modalka-unbind-kbd ".")

  ;; Mark
  (modalka-define-kbd "SPC" "C-SPC")
)

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
          )))

(provide 'init-keybinding)
