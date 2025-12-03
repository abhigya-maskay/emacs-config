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
  ;; Hide markup for clean reading
  (markdown-hide-markup t)
  (markdown-hide-urls t)
  ;; Scale headings
  (markdown-header-scaling t)
  ;; Syntax highlight code blocks
  (markdown-fontify-code-blocks-natively t)
  ;; GFM features
  (markdown-gfm-use-electric-backquote nil)
  :config
  ;; Ensure code blocks use monospace font
  (set-face-attribute 'markdown-code-face nil
                      :family "Monaspace Argon Frozen"
                      :inherit 'fixed-pitch))

(use-package olivetti
  :custom
  (olivetti-body-width 88))

;; Visual separation between line numbers and prose
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

;; Mermaid syntax highlighting in code blocks
(use-package mermaid-mode
  :mode "\\.mmd\\'")

;; Live browser preview with Mermaid support
(use-package markdown-preview-mode
  :commands markdown-preview-mode
  :config
  ;; These are defvar, not defcustom, so must use setq
  (setq markdown-preview-stylesheets
        '("https://cdnjs.cloudflare.com/ajax/libs/github-markdown-css/5.5.1/github-markdown.min.css"))
  (setq markdown-preview-javascript
        '("https://cdn.jsdelivr.net/npm/mermaid@10/dist/mermaid.min.js"
          "<script>mermaid.initialize({startOnLoad: false, theme: 'default'});</script>"))
  ;; This script runs after EVERY websocket content update
  (setq markdown-preview-script-onupdate "
(function() {
  // Debug: log all pre and code elements to find the right selector
  console.log('=== DEBUG: All PRE elements ===');
  document.querySelectorAll('#markdown-body pre').forEach(function(el, i) {
    console.log(i, 'PRE:', el.outerHTML.substring(0, 200));
  });
  console.log('=== DEBUG: All CODE elements ===');
  document.querySelectorAll('#markdown-body code').forEach(function(el, i) {
    console.log(i, 'CODE:', el.outerHTML.substring(0, 200));
  });

  // Try multiple selectors for mermaid blocks
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
    console.log('Selector', sel, 'found:', found.length);
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

;; Mode-local leader keybindings for markdown
(with-eval-after-load 'general
  (general-define-key
   :states '(normal visual)
   :keymaps 'gfm-mode-map
   :prefix "SPC"
   "m" '(:ignore t :wk "markdown")
   "mp" '(markdown-preview-mode :wk "preview")))

(provide 'init-markdown)
;;; init-markdown.el ends here
