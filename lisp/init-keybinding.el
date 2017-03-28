(cua-mode)

(global-set-key [f1] 'menu-bar-mode)

(global-set-key (kbd "C-x C-m") 'execute-extended-command)

(if window-system
  (progn
    (define-key global-map (kbd "C-2") 'er/expand-region)
    (define-key global-map (kbd "C-M-2") 'er/contract-region)
    )
  (define-key global-map (kbd "C-@") 'er/expand-region)
  (define-key global-map (kbd "C-M-@") 'er/contract-region))

(global-set-key (kbd "C-h a") 'apropos)

(when (display-graphic-p)
  (global-set-key (kbd "C-=") 'text-scale-increase)
  (global-set-key (kbd "C--") 'text-scale-decrease)
  (global-set-key (kbd "C-0") 'text-scale-adjust)

  (if *win64* (
    (global-set-key (kbd "<C-wheel-up>") 'text-scale-increase)
    (global-set-key (kbd "<C-wheel-down>") 'text-scale-decrease)))
  (global-set-key (kbd "<C-mouse-4>") 'text-scale-increase)
  (global-set-key (kbd "<C-mouse-5>") 'text-scale-decrease)

)

(global-set-key (kbd "C-x o") 'switch-window)

;; Upset arrow keys
(global-unset-key (kbd "<up>"))    ; ↑
(global-unset-key (kbd "<down>"))  ; ↓
(global-unset-key (kbd "<left>"))  ; ←
(global-unset-key (kbd "<right>")) ; →

;; {{
(global-set-key (kbd "M-i") 'previous-line)
(global-set-key (kbd "M-j") 'backward-char)
(global-set-key (kbd "M-k") 'next-line)
(global-set-key (kbd "M-l") 'forward-char)

(global-set-key (kbd "M-u") 'backward-word)
(global-set-key (kbd "M-o") 'forward-word)

(global-set-key (kbd "M-d") 'delete-backward-char) ;Similar to Backspace
(global-set-key (kbd "M-f") 'delete-char) ;Simalar to Delete
;; }}

(global-set-key (kbd "M-e") 'backward-kill-word)
(global-set-key (kbd "M-r") 'kill-word)


(when isearch-mode-hook
  :modify-map t
  :full-shortcut-map t
(define-key isearch-mode-map (kbd "C-f") 'isearch-repeat-forward)
(define-key isearch-mode-map (kbd "M-k") 'isearch-repeat-forward)
(define-key isearch-mode-map (kbd "C-S-f") 'isearch-repeat-backward)
(define-key isearch-mode-map (kbd "M-i") 'isearch-repeat-backward)
)

;;Use regex searches by default.
;; {{
(global-set-key (kbd "C-f")   'isearch-forward-regexp)
(global-set-key (kbd "C-S-f")   'isearch-backward-regexp)
(global-set-key (kbd "C-M-f")   'isearch-forward)
(global-set-key (kbd "C-M-S-f")   'isearch-backward)
;; }}

;; Save
;; {{
(global-set-key (kbd "C-s") 'save-buffer) ; Save

(global-set-key (kbd "C-x C-s") 'write-file)
(global-set-key (kbd "C-S-s") 'write-file)
;; }}

;; Undo/redo
;; {{
(require 'undo-tree)
(global-undo-tree-mode 1)

(defalias 'redo 'undo-tree-redo)
(global-set-key (kbd "C-z") 'undo) ; 
(global-set-key (kbd "C-S-z") 'redo) ;Apple style redo
(global-set-key (kbd "C-y") 'redo) ;MS style redo
;; }}

(global-set-key (kbd "C-a") 'mark-whole-buffer)

;; goto line
(global-set-key (kbd "C-l") 'goto-line)

(global-set-key (kbd "<mouse-8>") 'cua-scroll-up)
(global-set-key (kbd "<mouse-9>") 'cua-scroll-down)

(provide 'init-keybinding)
