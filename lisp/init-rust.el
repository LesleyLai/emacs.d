(use-package rust-mode
  :ensure t
  :defer t
  :after major-mode-hydra
  :custom
  (rust-format-on-save t)
  :config
  (major-mode-hydra-define rust-mode (:title "Rust Mode" :color teal :quit-key "q")
    ("Project"
     (("b" cargo-process-build "build")
      ("c" cargo-process-clean "clean")
      ("db" cargo-process-doc "Build docs")
      ("do" cargo-process-doc-open "Open docs"))

     "Test"
     (("tt" cargo-process-test "all tests")
      ("to" cargo-process-current-file-tests "file tests")
      ("tf" cargo-process-current-test "current test")
      ("te" cargo-process-bench "benchmarks"))

     "Crates"
     (("pa" cargo-process-add "Add")
      ("pu" cargo-process-update "update")
      ("pr" cargo-process-rm "Remove")
      ("pa" cargo-process-audit "Audit"))

     "Misc"
     (("y" cargo-process-repeat "repeat last cargo command")
      ("s" cargo-process-search "search")
      ("e" cargo-process-run "run"))

     "navigation"
     (("i" previous-line nil :color amaranth)
      ("j" backward-char nil :color amaranth)
      ("k" next-line nil :color amaranth)
      ("l" forward-char nil :color amaranth)
      ("u" backward-word nil :color amaranth)
      ("o" forward-word nil :color amaranth)
      ("." lsp-find-definition "Find Definition" :color amaranth)
      ("fr" lsp-find-references "Find References" :color blue))
      "Actions"
      (("r" lsp-rename "Rename Symbol" :color amaranth)))))

(use-package cargo
  :ensure t
  :after (rust-mode)
  :defer 3
  :hook ((rust-mode . cargo-minor-mode)))

(use-package flycheck-rust
  :ensure t
  :after (rust-mode)
  :defer t
  :config
  (with-eval-after-load 'rust-mode
    (add-hook 'flycheck-mode-hook #'flycheck-rust-setup)))



(provide 'init-rust)
