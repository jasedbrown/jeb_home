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


;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;; dired hacking
(use-package dired
  :straight nil
  :commands (dired dired-jump)
  :bind (("C-x C-j" . dired-jump))
  :custom ((dired-listing-switches "-agho")))


;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
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

(use-package vterm
  :custom
  (vterm-always-compile-module t)
  :bind (:map vterm-mode-map
              ("C-g" . vterm-send-escape)))

;; themes
;; currently liking prot's https://github.com/protesilaos/ef-themes
(use-package ef-themes)
(load-theme 'ef-maris-dark t)

(use-package hydra)

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
  (let ((project-root (or (lsp-workspace-root)
                          (rustic-buffer-crate)
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
;; yas - snippets. i really don't use this (now), but I have
;; other stuffs that does (which I haven't cleaned up yet)
(use-package yasnippet
  :config
  (yas-reload-all)
  :hook ((prog-mode . yas-minor-mode)
         (text-mode . yas-minor-mode)
         (lsp-mode  . yas-minor-mode)))

;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;; company - auto-completion
(use-package company
  ;;  :after lsp-mode
  ;;  :hook (lsp-mode . company-mode)
  :bind
  (:map company-active-map
              ("C-n". company-select-next)
              ("C-p". company-select-previous)
              ("M-<". company-select-first)
              ("M->". company-select-last))
  (:map company-mode-map
        ("<tab>". tab-indent-or-complete)
        ("TAB". tab-indent-or-complete))
  :custom
  (company-minimum-index-length 2))


;; JEB- ???
(defun company-yasnippet-or-completion ()
  (interactive)
  (or (do-yas-expand)
      (company-complete-common)))

(defun check-expansion ()
  (save-excursion
    (if (looking-at "\\_>") t
      (backward-char 1)
      (if (looking-at "\\.") t
        (backward-char 1)
        (if (looking-at "::") t nil)))))

;; JEB???
(defun do-yas-expand ()
  (let ((yas-fallback-behavior 'return-nil))
    (yas-expand)))

(defun tab-indent-or-complete ()
  (interactive)
  (if (minibufferp)
      (minibuffer-complete)
    (if (or (not yas-minor-mode)
            (null (do-yas-expand)))
        (if (check-expansion)
            (company-complete-common)
          (indent-for-tab-command)))))


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
  (rustic-lsp-client 'eglot)

;; pre-2025-Sept-26 settings
;;   (rustic-format-trigger "on-save")
  (lsp-rust-analyzer-display-lifetime-elision-hints-enable "skip_trivial")
  (lsp-rust-analyzer-display-chaining-hints t)
  (lsp-rust-analyzer-display-closure-return-type-hints t)
  ;; (lsp-rust-analyzer-cargo-extra-args "--locked")
  )

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


;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;; now set up eglot or lsp-mode, *after* all the language modes
(use-package eglot
  :straight nil 
  :commands (eglot eglot-ensure)
  :hook ((rustic-mode . eglot-ensure))
  :custom
  (eglot-events-buffer-size 0)
  (eglot-extend-to-xref t)
  (eglot-inlay-hints-mode f))
