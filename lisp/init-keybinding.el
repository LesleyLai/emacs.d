;; A lot of thing come from ergoemacs

(bind-keys*
 ;; Single char movement
 ("M-i" . previous-line)
 ("M-j" . backward-char)
 ("M-k" . next-line)
 ("M-l" . forward-char)
 ;; Move by word
 ("M-u" . backward-word)
 ("M-o" . forward-word)
 )

;; Copy, paste and cut
(cua-mode)

(use-package undo-tree
  :ensure t
  :config
  (global-undo-tree-mode 1)
  (defalias 'redo 'undo-tree-redo)
  (global-set-key (kbd "C-z") 'undo) 
  (global-set-key (kbd "C-S-z") 'redo) ;Apple style redo
  (global-set-key (kbd "C-y") 'redo) ;MS style redo
  )

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
(define-key isearch-mode-map (kbd "C-f") 'isearch-repeat-forward)

(define-key minibuffer-local-isearch-map (kbd "<left>")
  'isearch-reverse-exit-minibuffer)
(define-key minibuffer-local-isearch-map (kbd "<right>")
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
(global-set-key (kbd "C-o") 'find-file) ; Open
(global-set-key (kbd "C-s") 'save-buffer) ; Save
(global-set-key (kbd "C-S-s") 'write-file) ; Save As.
(global-set-key (kbd "C-a") 'mark-whole-buffer) ; Select all

;; Roll my own modal keys
(use-package modalka
  :ensure
  :demand
  :bind (("<escape>" . modalka-mode)
         ("<apps>" . modalka-mode))
  :init
  (add-to-list 'modalka-excluded-modes 'magit-status-mode)
  (add-to-list 'modalka-excluded-modes 'magit-popup-mode)
  (add-to-list 'modalka-excluded-modes 'dired-mode)
  (add-to-list 'modalka-excluded-modes 'help-mode)
  (add-to-list 'modalka-excluded-modes 'eshell-mode)

  (setq-default cursor-type '(bar . 1))
  (setq modalka-cursor-type 'box)
  :config
  ; Command inputs
  (modalka-define-kbd "a" "M-x")
  (modalka-define-kbd ":" "M-x")

  ; Navigation
  (modalka-define-kbd "i" "M-i")
  (modalka-define-kbd "j" "M-j")
  (modalka-define-kbd "k" "M-k")
  (modalka-define-kbd "l" "M-l")
  (modalka-define-kbd "u" "M-u")
  (modalka-define-kbd "o" "M-o")

  (define-key modalka-mode-map (kbd "f") #'modalka-mode)
  (define-key modalka-mode-map (kbd "h") #'move-beginning-of-line)
  (define-key modalka-mode-map (kbd ";") #'move-end-of-line)

  ;; CUA
  (define-key modalka-mode-map (kbd "z") #'undo)
  (define-key modalka-mode-map (kbd "x") #'cua-cut-region)
  (define-key modalka-mode-map (kbd "c") #'cua-copy-region)
  (define-key modalka-mode-map (kbd "v") #'cua-paste) 

  ;; Deleter
  (modalka-define-kbd "d" "DEL")

  ;; space leading group {{
  (define-key modalka-mode-map (kbd "SPC 0") #'delete-window)
  (define-key modalka-mode-map (kbd "SPC 1") #'delete-other-window)
  (define-key modalka-mode-map (kbd "SPC 2") #'split-window-below)
  (define-key modalka-mode-map (kbd "SPC 3") #'split-window-right)

  ;; "SPC g" used by magit
  ;; }}

  (modalka-global-mode 1)
  )

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

(provide 'init-keybinding)
