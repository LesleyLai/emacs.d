;; A lot of thing come from ergoemacs

(use-package xah-fly-keys
  :ensure t
  :config
  (xah-fly-keys-set-layout "qwerty")
  (xah-fly-keys 1)

  (global-set-key (kbd "<apps>") 'xah-fly-command-mode-activate)
  (global-set-key (kbd "<escape>") 'xah-fly-command-mode-activate))


;; Cursur movement
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

;; Replace
(global-set-key (kbd "C-f") 'query-replace-regexp)
;; }}

;; ;; Buffer movement
;; ;; Use <Shift> + arrowkeys to move between buffers
;; ;; {{
(windmove-default-keybindings)
;; ;; }}


;; ;; Copy, paste and cut
(cua-mode)

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
  :defer t
  :config
  (which-key-setup-side-window-right-bottom)
  )


(provide 'init-keybinding)
