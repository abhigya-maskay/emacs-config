;;; init-keybindings.el --- Keybinding configuration -*- lexical-binding: t -*-

(defun sudo-edit-current-file ()
  "Reopen the current file as root using TRAMP sudo."
  (interactive)
  (let ((file (buffer-file-name)))
    (if file
        (find-file (concat "/sudo::" file))
      (message "Buffer is not visiting a file"))))

(use-package general
  :config
  (general-evil-setup t)

  (general-create-definer leader-keys
    :keymaps '(normal visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC")

  (leader-keys
    ;; Top-level
    "SPC" '(execute-extended-command :wk "M-x")

    ;; Buffers
    "b" '(:ignore t :wk "buffer")
    "bb" '(consult-buffer :wk "switch buffer")
    "bd" '(kill-current-buffer :wk "kill buffer")
    "bn" '(next-buffer :wk "next buffer")
    "bp" '(previous-buffer :wk "previous buffer")

    ;; Files
    "f" '(:ignore t :wk "file")
    "ff" '(find-file :wk "find file")
    "fr" '(consult-recent-file :wk "recent files")
    "fs" '(save-buffer :wk "save file")
    "fS" '(sudo-edit-current-file :wk "sudo edit")
    "ft" '(treemacs :wk "file tree")

    ;; Search
    "s" '(:ignore t :wk "search")
    "ss" '(consult-line :wk "search buffer")
    "sp" '(consult-ripgrep :wk "search project")
    "si" '(consult-imenu :wk "imenu")

    ;; Jump
    "j" '(:ignore t :wk "jump")
    "jj" '(avy-goto-char :wk "jump to char")
    "jw" '(avy-goto-word-1 :wk "jump to word")
    "jl" '(avy-goto-line :wk "jump to line")

    ;; Windows
    "w" '(:ignore t :wk "window")
    "wh" '(evil-window-left :wk "window left")
    "wj" '(evil-window-down :wk "window down")
    "wk" '(evil-window-up :wk "window up")
    "wl" '(evil-window-right :wk "window right")
    "wv" '(split-window-right :wk "split vertical")
    "ws" '(split-window-below :wk "split horizontal")
    "wd" '(delete-window :wk "delete window")
    "ww" '(other-window :wk "other window")

    ;; Help
    "h" '(:ignore t :wk "help")
    "hf" '(helpful-callable :wk "describe function")
    "hv" '(helpful-variable :wk "describe variable")
    "hk" '(helpful-key :wk "describe key")

    ;; Project
    "p" '(:ignore t :wk "project")
    "pp" '(project-switch-project :wk "switch project")
    "pf" '(project-find-file :wk "find file")
    "ps" '(consult-ripgrep :wk "search project")
    "pb" '(project-switch-to-buffer :wk "project buffer")
    "pa" '(init-utils-project-remember-current :wk "add project")
    "pk" '(project-kill-buffers :wk "kill project buffers")

    ;; Code/LSP
    "c" '(:ignore t :wk "code")
    "ca" '(eglot-code-actions :wk "code actions")
    "cr" '(eglot-rename :wk "rename")
    "cf" '(eglot-format :wk "format")
    "cd" '(xref-find-definitions :wk "go to definition")
    "cD" '(xref-find-references :wk "find references")
    "ce" '(eglot :wk "start eglot")
    "cx" '(flymake-show-buffer-diagnostics :wk "list errors")
    "cn" '(flymake-goto-next-error :wk "next error")
    "cp" '(flymake-goto-prev-error :wk "prev error")
    "ch" '(eldoc-box-quit-frame :wk "dismiss docs")

    ;; Git
    "g" '(:ignore t :wk "git")
    "gg" '(magit-status :wk "status")
    "gb" '(magit-blame :wk "blame")
    "gl" '(magit-log-current :wk "log")

    ;; Open
    "o" '(:ignore t :wk "open")
    "op" '(treemacs :wk "project sidebar")
    "ot" '(eat-bottom-panel :wk "terminal")
    "ou" '(undo-tree-visualize :wk "undo tree")

    ;; Quit
    "q" '(:ignore t :wk "quit")
    "qq" '(save-buffers-kill-terminal :wk "quit emacs")

    ;; Emacs
    "e" '(:ignore t :wk "emacs")
    "eq" '(save-buffers-kill-terminal :wk "quit")
    "er" '(restart-emacs :wk "restart")
    "eR" '(init-utils-reload-config :wk "reload config")))

(provide 'init-keybindings)
;;; init-keybindings.el ends here
