;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;; jasobrown's emacs config.

;; if starting emacs with a custom user-emacs-directory
;; https://emacs.stackexchange.com/questions/4253/how-to-start-emacs-with-a-custom-user-emacs-directory
(setq user-init-file (or load-file-name (buffer-file-name)))
(setq user-emacs-directory (file-name-directory user-init-file))

;; save customized variables into a separate file, not init.el
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Saving-Customizations.html
;; (setq custom-file (expand-file-name "custom.el" user-emacs-directory))
;; (load-file custom-file)

(setq confirm-kill-emacs 'yes-or-no-p)
(fset 'yes-or-no-p 'y-or-n-p)
(tool-bar-mode 0)
(menu-bar-mode 0)
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode 0))

;; backup/auto-save config - mostly to not pollute my emacs home config dir
;; Set up backup files in XDG-compliant directories
(defvar user-cache-directory
  (or (getenv "XDG_CACHE_HOME")
      (concat (getenv "HOME") "/.cache")))

(defvar user-backup-directory
  (concat user-cache-directory "/emacs/backup"))

(defvar user-auto-save-directory
  (concat user-cache-directory "/emacs/auto-save"))

;; Create the backup directory if it doesn't exist
(make-directory user-backup-directory t)
(make-directory user-auto-save-directory t)

;; Configure backup settings
(setq backup-directory-alist `((".*" . ,user-backup-directory)))
(setq auto-save-file-name-transforms `((".*" ,user-auto-save-directory t)))
(setq auto-save-list-file-prefix (concat user-auto-save-directory "/.saves-"))

;; Additional backup settings (optional but recommended)
(setq backup-by-copying t)
(setq delete-old-versions t)
(setq kept-new-versions 6)
(setq kept-old-versions 2)
(setq version-control t)


(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

(show-paren-mode 1)
(column-number-mode 1)
(global-display-line-numbers-mode 1)

;; parens matching - automatically pairs braces when you create them
(electric-pair-mode 1)

;; this function finds the line number for a matching paren, and prints to minibuffer.
;; chatgpt created this for me
(defun show-paren-line-number ()
  "Display the line number of the matching paren."
  (when (and (bound-and-true-p show-paren-mode)
             (or (eq (char-after) ?\() (eq (char-after) ?\[) (eq (char-after) ?\{)
                 (eq (char-before) ?\)) (eq (char-before) ?\]) (eq (char-before) ?\})))
    (let* ((pos (save-excursion
                  (condition-case nil
                      (progn
                        (forward-list (if (looking-at-p "[[({]") 1 -1))
                        (point))
                    (error nil))))
           (line-number (when pos (line-number-at-pos pos))))
      (when line-number
        (message "Matching paren is on line: %d" line-number)))))

;; Hook the function to run when show-paren-mode is active
(add-hook 'post-command-hook 'show-paren-line-number)

;; don't use global line highlight
(global-hl-line-mode 0)

;; save mini-buffer history
(setq history-length 25)
(savehist-mode 1)

;; Remember and restore the last cursor location of opened files
(save-place-mode 1)

;; Revert buffers when the underlying file has changed
(global-auto-revert-mode 1)
;; Revert Dired and other buffers
(setq global-auto-revert-non-file-buffers t)

;; mode-line - The ModeLine is defined by the variable ‘mode-line-format’
(setq-default mode-line-format
              '("%e"
               mode-line-buffer-identification
               "   "
               mode-line-position
               "  "
               mode-line-misc-info))

;; raise the gc collection threshold, due to lsp-mode
;; https://emacs-lsp.github.io/lsp-mode/page/performance/#adjust-gc-cons-threshold
(setq gc-cons-threshold 100000000)
;; Increase the amount of data which Emacs reads from the process
(setq read-process-output-max (* 1024 1024))


;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;; package support via straight.el
;; https://systemcrafters.net/advanced-package-management/using-straight-el/
;; https://github.com/radian-software/straight.el 

;; copy-and-paste base install block for getting straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
      (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
        "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
        'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; point straight-use-package.el to straight
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

;; make sure to use builtins
;; 2025-Sept-26 - ran into problems when getting eglot/treesitter to work
(use-package project  :straight nil)
(use-package xref     :straight nil)
(use-package eglot    :straight nil)
(use-package eldoc    :straight nil)
(use-package flymake  :straight nil)

(use-package hydra)

;; themes
;; currently liking prot's https://github.com/protesilaos/ef-themes
(use-package ef-themes)
(load-theme 'ef-maris-dark t)

;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;; dired hacking
(use-package dired
  :straight nil
  :commands (dired dired-jump)
  :bind (("C-x C-j" . dired-jump))
  :custom ((dired-listing-switches "-agho")))


;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;; emacs <-> system copy/paste integration

;; Use system clipboard for all copy/paste
(setq select-enable-clipboard t
      select-enable-primary t)

;; Wayland clipboard integration
(setq x-select-enable-clipboard-manager t)

;; If running in terminal Emacs under Wayland
(unless (display-graphic-p)
  (setq interprogram-cut-function
        (lambda (text &optional _push)
          (let ((process-connection-type nil))
            (let ((proc (start-process "wl-copy" "*Messages*" "wl-copy" "-f" "-n")))
              (process-send-string proc text)
              (process-send-eof proc)))))
  (setq interprogram-paste-function
        (lambda ()
          (shell-command-to-string "wl-paste -n | tr -d '\r'"))))


;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;; Minibuffer completion

;; which-key: displays in the mini buffer the key bindings following
;; your currently entered incomplete command
;; https://github.com/justbur/emacs-which-key
(use-package which-key
  :init
  (which-key-mode))

;; vertico - vertical completion UI (for example, when selecting files
;; in a directory, in the mini buffer). replacement of older selectrum.el
;; https://github.com/minad/vertico
(use-package vertico
  :init
  (vertico-mode))

;; Orderless: flexible matching (fuzzy-like, space-separated terms)
(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides
   '((file (styles basic partial-completion)) ;; sane file paths
     (eglot (styles orderless)))))            ;; fuzzy LSP completions

;; Marginalia: annotations (docs, file metadata, etc.)
(use-package marginalia
  :init
  (marginalia-mode))

;; Consult: better versions of common commands
(use-package consult
  :bind
  (("C-s" . consult-line)              ;; search buffer
   ("C-x b" . consult-buffer)          ;; switch buffer
   ("M-y" . consult-yank-pop)          ;; browse kill ring
   ("C-c r" . consult-ripgrep)         ;; project grep
   ("C-x 4 b" . consult-buffer-other-window)
   ("C-x 5 b" . consult-buffer-other-frame)))


;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;; In-buffer completions

;; Corfu: popup completion at point
(use-package corfu
  :init
  (global-corfu-mode)    ;; enable everywhere
  :custom
  (corfu-auto t)         ;; auto popup
  (corfu-auto-prefix 2)  ;; show after 2 chars
  (corfu-cycle t)        ;; cycle around candidates
  (corfu-preselect-first nil)
  :bind
  (:map corfu-map
        ("C-n" . corfu-next)
        ("C-p" . corfu-previous)
        ("M-<" . corfu-first)
        ("M->" . corfu-last)))

;; Yasnippet: snippet expansion
(use-package yasnippet
  :config
  (yas-global-mode))

;; Cape: add extra completion sources on top of eglot
(use-package cape
  :init
  (add-to-list 'completion-at-point-functions #'cape-dabbrev) ;; words in buffer
  (add-to-list 'completion-at-point-functions #'cape-file)    ;; filenames
  (add-to-list 'completion-at-point-functions #'cape-symbol)) ;; elisp symbols

;; Completion function: snippets > completion > indent
(defun tab-indent-or-complete ()
  "Try yasnippet, then completion, else indent."
  (interactive)
  (cond
   ((minibufferp) (minibuffer-complete))
   ((yas-expand))
   ((completion-at-point))
   (t (indent-for-tab-command))))

;; Bind CAPS_LOCK to completion (requires OS-level remapping first)
(global-set-key (kbd "C-t") #'tab-indent-or-complete)


;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;; vterm
(use-package vterm
  :custom
  (vterm-always-compile-module t)
  :bind (:map vterm-mode-map
              ("C-g" . vterm-send-escape)))

;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;; ace-window - modern window switching with visual feedback
;; https://github.com/abo-abo/ace-window
(use-package ace-window
  :straight t
  :bind ("M-o" . ace-window)
  :custom
  (aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  (aw-scope 'frame)
  (aw-background t)
  (aw-char-position 'top-left)
  (aw-minibuffer-char ?z))

;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;; IBuffer: buffer list manangement: IBuffer (https://www.emacswiki.org/emacs/IbufferMode)
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; auto-mode-alist adjustments (https://www.emacswiki.org/emacs/AutoModeAlist)
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.cc\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.cpp\\'" . c++-mode))
;; CUDA-related files should just use c++ mode
(add-to-list 'auto-mode-alist '("\\.cu\\'" . c++-mode))
;; CMake
(add-to-list 'auto-mode-alist '("CMakeLists\\.txt\\'" . cmake-mode))
(add-to-list 'auto-mode-alist '("\\.cmake\\'" . cmake-mode))
;; Docker
(add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))
;; Protobuf
(add-to-list 'auto-mode-alist '("\\.proto\\'" . protobuf-mode))
;; Protobuf
(add-to-list 'auto-mode-alist '("\\.gnuplot\\'" . gnuplot-mode))

;; define and then _load_ filter
(setq ibuffer-saved-filter-groups
      (quote (("home"
               ("rust" (or
                        (mode . rust-mode)
                        (mode . rustic-mode)))
               ("java" (mode . java-mode))
               ("python" (mode . python-mode))
               ("ruby" (mode . ruby-mode))
               ("golang" (mode . go-mode))
               ("c/c++" (or
                         (mode . c-mode)
                         (mode . c++-mode)))
               ("lua" (mode . lua-mode))
               ("toml" (mode . toml-mode))
               ("yaml" (mode . yaml-mode))
               ("shell script" (or
                                (mode . shell-script-mode)
                                (mode . sh-mode)))
               ("databass" (or
                            (mode . sql-mode)
                            (mode . "\\.test")
                            (name . "\\.spec")))
               ("docs" (mode . markdown-mode))
               ("gnuplot" (mode . markdown-mode))
               ("emacs" (or
                         (name . "\\.el")
                         (name . "^\\*scratch\\*$")
                         (name . "^\\*Messages\\*$")))
               ))))

(add-hook 'ibuffer-mode-hook
	  (lambda ()
	     (ibuffer-switch-to-saved-filter-groups "home")))


;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;; magit magick!
(use-package magit)

;; convenience functions for pushing branches up to gerrit,
;; via magit. prints result of the operation to the mini-buffer.
(defun gerrit-push-origin-head ()
  "Push the current branch to origin as refs/for/main using magit."
  (interactive)
  (let ((project-root (or (rustic-buffer-crate)
                          (magit-toplevel))))
    (when project-root
      (let ((default-directory project-root)
            (output-buffer "*Git Push Output*"))
        (with-output-to-temp-buffer output-buffer
          (let ((exit-code (call-process "git" nil output-buffer t
                                         "push" "origin" "HEAD:refs/for/main")))
            (if (eq exit-code 0)
                (message "Pushed to origin HEAD:refs/for/main successfully.")
              (message "Git push failed. See *Git Push Output* buffer for details."))))))))

(transient-define-prefix my-magit-push-menu ()
  "My Magit Push Menu"
  ["Actions"
   ["Push"
    ("p" "Push to origin HEAD:refs/for/main" gerrit-push-origin-head)]])

(define-key magit-mode-map (kbd "C-c C-p") 'my-magit-push-menu)


;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;; projectile

(use-package projectile
  :straight t
  :init
  (projectile-mode +1)
  :bind (:map projectile-mode-map
              ("s-p" . projectile-command-map)
              ("C-c p" . projectile-command-map))
  :custom
  ;; courtesy of claude.ai:
  ;; Configure Projectile for better TRAMP performance
  (projectile-enable-caching t)  ; Important for TRAMP performance
  (projectile-file-exists-remote-cache-expire nil)  ; Cache remote file existence

  ;; If you want to exclude some remote operations for better performance
  (projectile-file-exists-remote-cache-expire nil)
  (projectile-git-command "git ls-files -zc --exclude-standard")  ; Faster than default
  (projectile-indexing-method 'alien)  ; Usually faster for git projects
)

;; apparently, `deadgrep` is thw newer hotness, but not sure
;; if it integrates with projectile
(use-package ripgrep
  :straight t)


;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;; inline errors
(use-package flycheck)

;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;; org-mode and friends
(use-package org
  :straight t)

(use-package markdown-mode
  :straight t)


;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;; LLM magick
(use-package claude-code-ide
  :straight (:type git :host github :repo "manzaltu/claude-code-ide.el")
  :bind ("C-c '" . claude-code-ide-menu)
  :config
  (claude-code-ide-emacs-tools-setup))


;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;; rust fun!
;; rustic = basic rust-mode + additions
;; define the hook functions before they are referenced in use-package

;; 2024-Aug-23 unclear if this is even necessary
;; (defun rk/rustic-mode ()
;;   ;; so that run C-c C-c C-r works without having to confirm, but don't try to
;;   ;; save rust buffers that are not file visiting. Once
;;   ;; https://github.com/brotzeit/rustic/issues/253 has been resolved this should
;;   ;; no longer be necessary.
;;   (when buffer-file-name
;;     (setq-local buffer-save-without-query t))
;;   (add-hook 'before-save-hook 'lsp-format-buffer nil t))

;; set the cargo build directory to a local dir in `./target'.
;; this allows rust-analyzer (called from emacs) to have it's own compilation dir.
;; 2024-Aug-23 not sure this is working as hoped. var gets set in local buffer, but
;; not being respected.
(defun jeb/set-cargo-target-dir ()
  "Set Cargo's target directory to 'target/emacs' relative to the workspace root using Projectile."
    (let ((project-root (projectile-project-root)))
      (if project-root
            (setq-local lsp-rust-analyzer-cargo-override-command
                        `("cargo" "build" "--target-dir" ,(concat project-root "target/emacs")))
        (message "Could not determine project root using Projectile"))))

;; Set up Emacs-specific cargo build directory - in lieu oif the above busted projectile-thing ...
;; (let ((emacs-cargo-dir (expand-file-name "emacs/cargo-builds" 
;;                                         (or (getenv "XDG_CACHE_HOME") 
;;                                             "~/.cache"))))
;;   (setenv "CARGO_TARGET_DIR" emacs-cargo-dir)
;;   ;; Ensure the directory exists
;;   (make-directory emacs-cargo-dir t))


;; Make sure the Rust Tree-sitter grammar exists:
;; M-x treesit-install-language-grammar RET rust RET

;; Enable Tree-sitter in rust-mode
(use-package rust-mode
  :ensure t
  :init
  (setq rust-mode-treesitter-derive t))

;; Load Rustic after rust-mode
(use-package rustic
  :straight (rustic :type git :host github :repo "emacs-rustic/rustic")
  :after rust-mode
;; ;;  :hook (rustic-mode . (lambda() (rk/rustic-mode) ))
;; ;;  :hook (rustic-mode . jeb/set-cargo-target-dir)
  :custom
  (rustic-lsp-client 'eglot))

(use-package toml-mode
  :straight t)

(use-package yaml-mode
  :straight t)


;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;; java-related settings, originally borrowed from
;; https://github.com/neppramod/java_emacs/tree/master
;;
;; lsp-java (https://github.com/emacs-lsp/lsp-java) is a thin, emacs
;; client/wrapper around the eclipse jdtls (https://github.com/eclipse-jdtls/eclipse.jdt.ls).
(use-package java-mode
  :straight nil
  :mode ("\\.java\\'" . java-ts-mode)
  :hook (java-ts-mode . eglot-ensure)
  :config
  ;; Optional: customize tree-sitter features
  (setq java-ts-mode-indent-offset 4))


;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;; c/c++-related settings, mostly borrowed from a post on the emacs lsp page:
;; https://emacs-lsp.github.io/lsp-mode/tutorials/CPP-guide/
(add-hook 'c-mode-hook 'lsp)
(add-hook 'c++-mode-hook 'lsp)


;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;; python-related settings, mostly borrowed from a post on the emacs lsp page:
;; https://emacs-lsp.github.io/lsp-mode/page/lsp-pylsp/
;;
;; using pylsp: https://github.com/python-lsp/python-lsp-server
(use-package python-mode
  :straight t
  :hook (python-mode . lsp))


;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;; golang-related settings. Need a Go SDK installed, and the go-lsp server (gopls)
;; Make sure the the go tools bin is on the path ($HOME/go/bin): 
;; https://github.com/golang/tools/blob/master/gopls/doc/emacs.md
;; autoload only when needed
(use-package go-mode
  :straight t
  :mode ("\\.go\\'" . go-mode))

; autoload only when needed
(use-package lua-mode
  :straight t
  :mode ("\\.lua\\'" . lua-mode))


;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;; SQL related

;; sqlformat: https://github.com/purcell/sqlformat?tab=readme-ov-file
;; currentlyusing pgformatter to do the dirty work: https://github.com/darold/pgFormatter
(use-package sqlformat
  :straight t
  :custom
  (sqlformat-command 'pgformatter))

;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;; random LSP-related

;; for shell lsp fun, use this bash-language-server ()https://github.com/bash-lsp/bash-language-server).
;; you'll need to hand install it via `npm i -g bash-language-server'


;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;; now set up eglot or lsp-mode, *after* all the language modes
(use-package eglot
  :straight nil 
  :commands (eglot eglot-ensure)
  :hook ((rustic-mode . eglot-ensure)
         (python-mode . eglot-ensure)
         (go-mode . eglot-ensure)
         (sh-mode . eglot-ensure))
  :config
  (add-to-list 'eglot-server-programs
               '(sh-mode . ("bash-language-server" "start")))
  :custom
  (eglot-events-buffer-size 0)
  (eglot-extend-to-xref t)
  (eglot-ignored-server-capabilities '(:inlayHintProvider))
  (eglot-connect-timeout 120)
  (eglot-sync-connect nil)
  (eglot-workspace-configuration
   '((:rust-analyzer
      :cargo (:buildScripts (:enable t)
              :features "all")
      :procMacro (:enable t
                  :ignored {}
                  :attributes (:enable t))
      :diagnostics (:disabled ["unresolved-proc-macro"]
                    :experimental (:enable t))
      :checkOnSave (:command "clippy"
                    :extraArgs ["--target-dir" "target/rust-analyzer"])
      :inlayHints (:lifetimeElisionHints (:enable "skip_trivial")
                   :chainingHints (:enable t)
                   :closureReturnTypeHints (:enable "always")))
     (:bashIde
      :globPattern "**/*@(.sh|.inc|.bash|.command|.zsh)"))))

;; eglot doesn't sort the xref output, whereas lsp-mode did :homer-cry:
;; i asked chatgpt for this.
(defun my/xref-sort (xrefs)
  "Sort XREFS by file name, then line number."
  (cl-sort (copy-sequence xrefs)
           (lambda (a b)
             (let* ((loc-a (xref-item-location a))
                    (loc-b (xref-item-location b))
                    (file-a (xref-location-group loc-a))
                    (file-b (xref-location-group loc-b)))
               (if (string= file-a file-b)
                   (< (xref-location-line loc-a)
                      (xref-location-line loc-b))
                 (string< file-a file-b))))))

(advice-add 'xref--show-xrefs :around
            (lambda (orig fetcher display-action &rest args)
              (funcall orig
                       (lambda ()
                         (my/xref-sort (funcall fetcher)))
                       display-action
                       args)))

;; eglot doesn't have the nice breadcrumb header, and i miss it
(use-package breadcrumb
  :straight t
  :commands (breadcrumb-local-mode breadcrumb-imenu-crumbs breadcrumb-project-crumbs)
  :init
  (require 'subr-x)
  :config
  ;;(add-hook 'eglot-managed-mode-hook #'jason/eglot-breadcrumb-toggle)
  )
