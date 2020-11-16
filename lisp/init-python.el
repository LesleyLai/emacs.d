(defun python-mode-hook-setup ()
  (subword-mode +1)
  (eldoc-mode 1)
  )

(add-hook 'python-mode-hook 'python-mode-hook-setup)

;; Major mode to edit pip requirements files
;; enable autopep8 formatting on save
;; need autopep8 installed
(use-package pip-requirements
  :ensure t
  :defer t)

(defun python-shell-completion-native-try ()
  "Return non-nil if can trigger native completion."
  (with-eval-after-load 'python
    '(let ((python-shell-completion-native-enable t)
           (python-shell-completion-native-output-timeout
            python-shell-completion-native-try-output-timeout))
       (python-shell-completion-native-get-completions
        (get-buffer-process (current-buffer))
        nil "_"))))

(provide 'init-python)
