;;; init-markdown.el --- Markdown configuration -*- lexical-binding: t -*-

;;; Commentary:
;; Rich markdown editing with GFM support, hidden markup, and variable-pitch fonts.

;;; Code:

(use-package markdown-mode
  :mode (("\\.md\\'" . gfm-mode)
         ("\\.markdown\\'" . gfm-mode))
  :hook ((gfm-mode . visual-line-mode)
         (gfm-mode . olivetti-mode)
         (gfm-mode . variable-pitch-mode))
  :custom
  (markdown-hide-markup t)
  (markdown-hide-urls t)
  (markdown-header-scaling t)
  (markdown-fontify-code-blocks-natively t)
  (markdown-gfm-use-electric-backquote nil)
  :config
  (set-face-attribute 'markdown-code-face nil
                      :family "Monaspace Argon Frozen"
                      :inherit 'fixed-pitch))

(use-package olivetti
  :custom
  (olivetti-body-width 88))

(defun my/markdown-line-number-setup ()
  "Style line numbers to stand out from centered prose."
  (face-remap-add-relative 'line-number
                           :background (face-background 'fringe nil t))
  (face-remap-add-relative 'line-number-current-line
                           :background (face-background 'fringe nil t)))

(add-hook 'gfm-mode-hook #'my/markdown-line-number-setup)

(use-package mixed-pitch
  :hook (gfm-mode . mixed-pitch-mode)
  :custom
  (mixed-pitch-fixed-pitch-faces
   '(markdown-code-face
     markdown-inline-code-face
     markdown-pre-face
     markdown-table-face)))

(use-package mermaid-mode
  :mode "\\.mmd\\'")

(use-package markdown-preview-mode
  :commands markdown-preview-mode
  :config
  (setq markdown-preview-stylesheets
        '("https://cdnjs.cloudflare.com/ajax/libs/github-markdown-css/5.5.1/github-markdown.min.css"))
  (setq markdown-preview-javascript
        '("https://cdn.jsdelivr.net/npm/mermaid@10/dist/mermaid.min.js"
          "<script>mermaid.initialize({startOnLoad: false, theme: 'default'});</script>"))
  (setq markdown-preview-script-onupdate "
(function() {
  var selectors = [
    'pre[lang=\"mermaid\"]',
    'pre code[lang=\"mermaid\"]',
    'pre code.language-mermaid',
    'pre code.mermaid',
    'pre.mermaid'
  ];
  var blocks = [];
  selectors.forEach(function(sel) {
    var found = document.querySelectorAll('#markdown-body ' + sel);
    if (found.length > 0) blocks = found;
  });
  blocks.forEach(function(el) {
    var container = el.tagName === 'PRE' ? el : el.closest('pre');
    var div = document.createElement('div');
    div.className = 'mermaid';
    div.textContent = el.textContent;
    container.parentNode.replaceChild(div, container);
  });
  if (blocks.length > 0) {
    mermaid.run();
  }
})();
"))

(with-eval-after-load 'general
  (general-define-key
   :states '(normal visual)
   :keymaps 'gfm-mode-map
   :prefix "SPC"
   "m" '(:ignore t :wk "markdown")
   "mp" '(markdown-preview-mode :wk "preview")))

(provide 'init-markdown)
;;; init-markdown.el ends here
