;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;; RANDOM SHIT

;; enable bidirectional synchronization of lsp workspace folders and treemacs projects.
;; FIX THIS: assumes lsp-mode was loaded enabled in the language-specific file 
(use-package lsp-treemacs
  :after (treemacs lsp-mode)
  :defer t
  :ensure t)
(lsp-treemacs-sync-mode 1)
