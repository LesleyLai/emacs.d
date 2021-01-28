(use-package multiple-cursors
  :ensure t
  :bind
  (
   ;; multiple-cursors
   ("C-<" . mc/mark-previous-like-this)
   ("C->" . mc/mark-next-like-this)
   ("C-+" . mc/mark-next-like-this)
   ("C-c C-<" . mc/mark-all-like-this)
   ("C-c m c" . mc/edit-lines)
   ("C-c m e" . mc/edit-ends-of-lines)
   ("C-c m a" . mc/edit-beginnings-of-lines)

   (:map mc/keymap
         ("C-v" . nil))))

(provide 'init-multiple-cursors)
