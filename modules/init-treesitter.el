;;; init-treesitter.el --- Treesitter configuration -*- lexical-binding: t -*-

;; Auto-remap major modes to treesitter variants
(setq major-mode-remap-alist
      '((python-mode . python-ts-mode)
        (javascript-mode . js-ts-mode)
        (typescript-mode . typescript-ts-mode)
        (js-json-mode . json-ts-mode)
        (css-mode . css-ts-mode)
        (c-mode . c-ts-mode)
        (c++-mode . c++-ts-mode)
        (rust-mode . rust-ts-mode)
        (go-mode . go-ts-mode)
        (ruby-mode . ruby-ts-mode)
        (html-mode . html-ts-mode)
        (yaml-mode . yaml-ts-mode)
        (bash-mode . bash-ts-mode)))

;; Where to install grammars
(setq treesit-language-source-alist
      '((python "https://github.com/tree-sitter/tree-sitter-python")
        (javascript "https://github.com/tree-sitter/tree-sitter-javascript")
        (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
        (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
        (json "https://github.com/tree-sitter/tree-sitter-json")
        (css "https://github.com/tree-sitter/tree-sitter-css")
        (html "https://github.com/tree-sitter/tree-sitter-html")
        (c "https://github.com/tree-sitter/tree-sitter-c")
        (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
        (rust "https://github.com/tree-sitter/tree-sitter-rust")
        (go "https://github.com/tree-sitter/tree-sitter-go")
        (ruby "https://github.com/tree-sitter/tree-sitter-ruby")
        (yaml "https://github.com/tree-sitter-grammars/tree-sitter-yaml")
        (bash "https://github.com/tree-sitter/tree-sitter-bash")
        (toml "https://github.com/tree-sitter/tree-sitter-toml")))

;; Prompt to install missing grammars (only when treesit is available)
(when (fboundp 'treesit-language-at)
  (defun my/treesit-install-grammar-if-missing ()
    "Prompt to install treesitter grammar if missing for current buffer."
    (when-let* ((lang (treesit-language-at (point-min)))
                ((not (treesit-language-available-p lang)))
                ((assoc lang treesit-language-source-alist))
                ((y-or-n-p (format "Install treesitter grammar for %s? " lang))))
      (treesit-install-language-grammar lang)))

  (add-hook 'prog-mode-hook #'my/treesit-install-grammar-if-missing))

(provide 'init-treesitter)
;;; init-treesitter.el ends here
