;; A lot of thing come from ergoemacs

;; Cursur movement
;; {{

;; Single char movement
(global-set-key (kbd "M-i") 'previous-line)
(global-set-key (kbd "M-j") 'backward-char)
(global-set-key (kbd "M-k") 'next-line)
(global-set-key (kbd "M-l") 'forward-char)

;; Move by word
(global-set-key (kbd "M-u") 'backward-word)
(global-set-key (kbd "M-o") 'forward-word)

;; Move by paragraph
(global-set-key (kbd "M-U") 'backward-paragraph)
(global-set-key (kbd "M-O") 'forward-paragraph)

;; Move to beginning/ending of line
(global-set-key (kbd "M-h") 'move-beginning-of-line)
(global-set-key (kbd "M-H") 'move-end-of-line)
;; }}

;; Search
;; {{
;;Use regex searches by default.

(global-set-key (kbd "C-f")   'isearch-forward-regexp)
(global-set-key (kbd "C-S-f")   'isearch-backward-regexp)
(global-set-key (kbd "C-M-f")   'isearch-forward)
(global-set-key (kbd "C-M-S-f")   'isearch-backward)

;; Replace
(global-set-key (kbd "M-%") 'query-replace-regexp)
;; }}

;; Buffer movement
;; Use <Shift> + arrowkeys to move between buffers
;; {{
(windmove-default-keybindings)
;; }}

;; Editing Commands
;; {{
(global-set-key (kbd "M-d") 'delete-backward-char) ;Similar to Backspace
(global-set-key (kbd "M-f") 'delete-char) ;Simalar to Delete

(global-set-key (kbd "M-e") 'backward-kill-word)
(global-set-key (kbd "M-r") 'kill-word)

(global-set-key (kbd "<delete>") 'delete-char) ; the Del key for forward delete.


;; Copy, paste and cut
(cua-mode)

;;   Undo/redo
(use-package undo-tree
             :ensure t
             :config
             (global-undo-tree-mode 1)

             (defalias 'redo 'undo-tree-redo)
             (global-set-key (kbd "C-z") 'undo) ; 
             (global-set-key (kbd "C-S-z") 'redo) ;Apple style redo
             (global-set-key (kbd "C-y") 'redo) ;MS style redo
             )
;; }}

;; Standard Shortcuts
(global-set-key (kbd "C-n") 'new-empty-buffer) ; Open New File
(global-set-key (kbd "C-S-n") 'make-frame-command) ; open a new window
(global-set-key (kbd "C-w") 'close-current-buffer) ; Close
(global-set-key (kbd "C-o") 'find-file) ; Open
(global-set-key (kbd "C-s") 'save-buffer) ; Save
(global-set-key (kbd "C-S-s") 'write-file) ; Save As.
(global-set-key (kbd "C-a") 'mark-whole-buffer) ; Select all
(global-set-key (kbd "C-l") 'goto-line) ; goto line

;; Menu bar
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

  (global-set-key (kbd "<C-mouse-4>") 'text-scale-increase)
  (global-set-key (kbd "<C-mouse-5>") 'text-scale-decrease)
  )


(global-set-key (kbd "<mouse-8>") 'cua-scroll-up)
(global-set-key (kbd "<mouse-9>") 'cua-scroll-down)

;; Modes fix
;;; isearch
(when isearch-mode-hook
  :modify-map t
  :full-shortcut-map t
(define-key isearch-mode-map (kbd "C-f") 'isearch-repeat-forward)
(define-key isearch-mode-map (kbd "M-k") 'isearch-repeat-forward)
(define-key isearch-mode-map (kbd "C-S-f") 'isearch-repeat-backward)
(define-key isearch-mode-map (kbd "M-i") 'isearch-repeat-backward)
)

;; Dired
(add-hook 'dired-mode-hook
 (lambda ()
  (define-key dired-mode-map (kbd "C-n") 'new-empty-buffer) ; was dired-next-line
  (define-key dired-mode-map (kbd "M-o") 'forward-word) ; was dired-omit-mode
  (define-key dired-mode-map (kbd "M-i") 'previous-line)
  (define-key dired-mode-map (kbd "M-l") 'forward-char)
  ))

;; Paredit
(add-hook
 'paredit-mode-hook
 (lambda ()
   (define-key paredit-mode-map (kbd "C-J") 'paredit-backward)
   (define-key paredit-mode-map (kbd "C-L") 'paredit-forward)
   ))


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

;; Functions

;;; Buffer related
(defun next-user-buffer ()
  "Switch to the next user buffer.
User buffers are those whose name does not start with *."
  (interactive)
  (next-buffer)
  (let ((i 0))
    (while (and (string-match "^*" (buffer-name)) (< i 50))
      (setq i (1+ i)) (next-buffer) )))

(defun previous-user-buffer ()
  "Switch to the previous user buffer.
User buffers are those whose name does not start with *."
  (interactive)
  (previous-buffer)
  (let ((i 0))
    (while (and (string-match "^*" (buffer-name)) (< i 50))
      (setq i (1+ i)) (previous-buffer) )))

(defun next-emacs-buffer ()
  "Switch to the next emacs buffer.
Emacs buffers are those whose name starts with *."
  (interactive)
  (next-buffer)
  (let ((i 0))
    (while (and (not (string-match "^*" (buffer-name))) (< i 50))
      (setq i (1+ i)) (next-buffer) )))

(defun previous-emacs-buffer ()
  "Switch to the previous emacs buffer.
Emacs buffers are those whose name starts with *."
  (interactive)
  (previous-buffer)
  (let ((i 0))
    (while (and (not (string-match "^*" (buffer-name))) (< i 50))
      (setq i (1+ i)) (previous-buffer) )))

(defun new-empty-buffer ()
  "Opens a new empty buffer."
  (interactive)
  (let ((buf (generate-new-buffer "untitled")))
    (switch-to-buffer buf)
    (funcall (and initial-major-mode))
    (setq buffer-offer-save t)))
;; note: emacs won't offer to save a buffer that's
;; not associated with a file,
;; even if buffer-modified-p is true.
;; One work around is to define your own my-kill-buffer function
;; that wraps around kill-buffer, and check on the buffer modification
;; status to offer save
;; This custome kill buffer is close-current-buffer.

(defvar recently-closed-buffers (cons nil nil) "A list of recently closed buffers. The max number to track is controlled by the variable recently-closed-buffers-max.")
(defvar recently-closed-buffers-max 10 "The maximum length for recently-closed-buffers.")

;; Close the current buffer with check weather it related to a file or not

(define-key menu-bar-file-menu [kill-buffer] '(menu-item "Close" close-current-buffer
                                                         :help "Close the current buffer."))

(defun close-current-buffer ()
"Close the current buffer.

Similar to (kill-buffer (current-buffer)) with the following addition:

• prompt user to save if the buffer has been modified even if the buffer is not associated with a file.
• make sure the buffer shown after closing is a user buffer.
• if the buffer is a file, add the path to the list recently-closed-buffers.

A emacs buffer is one who's name starts with *.
Else it is a user buffer."
 (interactive)
 (let (emacsBuff-p isEmacsBufferAfter)
   (if (string-match "^*" (buffer-name))
       (setq emacsBuff-p t)
     (setq emacsBuff-p nil))

   ;; offer to save buffers that are non-empty and modified, even for non-file visiting buffer. (because kill-buffer does not offer to save buffers that are not associated with files)
   (when (and (buffer-modified-p)
              (not emacsBuff-p)
              (not (string-equal major-mode "dired-mode"))
              (if (equal (buffer-file-name) nil) 
                  (if (string-equal "" (save-restriction (widen) (buffer-string))) nil t)
                t
                )
              )
     (if (y-or-n-p
            (concat "Buffer " (buffer-name) " modified; Do you want to save?"))
       (save-buffer)
       (set-buffer-modified-p nil)))

   ;; save to a list of closed buffer
   (when (not (equal buffer-file-name nil))
     (setq recently-closed-buffers
           (cons (cons (buffer-name) (buffer-file-name)) recently-closed-buffers))
     (when (> (length recently-closed-buffers) recently-closed-buffers-max)
           (setq recently-closed-buffers (butlast recently-closed-buffers 1))
           )
     )

   ;; close
   (kill-buffer (current-buffer))

   ;; if emacs buffer, switch to a user buffer
   (if (string-match "^*" (buffer-name))
       (setq isEmacsBufferAfter t)
     (setq isEmacsBufferAfter nil))
   (when isEmacsBufferAfter
     (previous-user-buffer)
     )
   )
 )


(provide 'init-keybinding)
