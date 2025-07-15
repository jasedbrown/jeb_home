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

(use-package vterm)


;; themes
;; currently liking prot's https://github.com/protesilaos/ef-themes
(use-package ef-themes)
(load-theme 'ef-maris-dark t)


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
;; LLM magick

;; *** take 1: gptel + elysium ***
;; mostly taken from https://github.com/lanceberge/elysium
;; (use-package elysium
;;   :straight t
;;   :custom
;;   ;; Below are the default values
;;   (elysium-window-size 0.33) ; The elysium buffer will be 1/3 your screen
;;   (elysium-window-style 'vertical)) ; Can be customized to horizontal

;; (use-package gptel
;;   :straight t
;;   :custom
;;   (gptel-model 'claude-3-5-sonnet-20240620)
;;   :config
;;   (defun read-file-contents (file-path)
;;     "Read the contents of FILE-PATH and return it as a string."
;;     (with-temp-buffer
;;       (insert-file-contents file-path)
;;       (buffer-string)))
;;   (defun gptel-api-key ()
;;     (read-file-contents "~/secrets/claude_key"))
;;   (setq
;;    gptel-backend (gptel-make-anthropic "Claude"
;;                    :stream t
;;                    :key #'gptel-api-key)))

;; (use-package smerge-mode
;;   :straight t
;;   :hook
;;   (prog-mode . smerge-mode))

;; *** take 2: aider.el ***
;; https://github.com/tninja/aider.el
;; (use-package aider
;;   :straight (:host github :repo "tninja/aider.el" :files ("aider.el"))
;;   :config
;;   (setq aider-args '("--model" "claude-3-5-sonnet-20241022"))
;;   ;; (setq aider-args '("--model" "ollama/deepseek-coder-v2"))
;;   (setenv "ANTHROPIC_API_KEY" "<insert key>")
;;   (setenv "OLLAMA_API_BASE" "http://127.0.0.1:11434")
;;   ;; Optional: Set a key binding for the transient menu
;;   (global-set-key (kbd "C-c a") 'aider-transient-menu))

;; *** take 3: aidermacs ***
;; https://github.com/MatthewZMD/aidermacs
(use-package aidermacs
  :straight (:host github :repo "MatthewZMD/aidermacs" :files ("*.el"))
  :config
  (setq aidermacs-default-model "ollama_chat/deepseek-r1")
  (global-set-key (kbd "C-c C-a") 'aidermacs-transient-menu)
  ; Ensure emacs can access *_API_KEY through .bashrc or setenv
  ; (setenv "ANTHROPIC_API_KEY" anthropic-api-key)
  (setq aidermacs-use-architect-mode t)
  (setq aidermacs-backend 'vterm))


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
  ;; courtesy of cluade.ai:
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
;; lsp-mode and friends

(use-package lsp-mode
  :straight t
  :commands lsp
  :init
  ; this is for which-key integration documentation, need to use lsp-mode-map
  (setq lsp-keymap-prefix "C-c l" )
  :custom
  (lsp-idle-delay 0.5)
  ;; This controls the overlays that display type and other hints inline. Enable
  ;; / disable as you prefer. Well require a `lsp-workspace-restart' to have an
  ;; effect on open projects.
  (lsp-inlay-hint-enable nil)
  ;; the next two disable crap down in the modeline
  (lsp-modeline-code-actions-enable nil)
  (lsp-modeline-diagnostics-enable nil)
  :config
  (define-key lsp-mode-map (kbd "C-c l") lsp-command-map)
  :hook ((lsp-mode . lsp-ui-mode)
         (lsp-mode . lsp-enable-which-key-integration))
  :bind (:map lsp-mode-map
              ("M-j" . lsp-ui-imenu)
              ("M-?" . lsp-find-references)
              ("C-c C-c l" . flycheck-list-errors)
              ("C-c C-c a" . lsp-execute-code-action)
              ("C-c C-c r" . lsp-rename)
              ("C-c C-c d" . dap-hydra))
 )

;; wtf - semgrep just made a mess of things, especially with rustic-mode
(setq lsp-disabled-clients '(semgrep-ls))

(use-package lsp-ui
  :commands lsp-ui-mode
  :custom
  (lsp-ui-peek-always-show t)
  (lsp-ui-sideline-show-hover t)
  (lsp-ui-doc-enable nil)
  (lsp-ui-sideline-show-hover nil)
  :bind (:map lsp-ui-mode-map
              ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
              ([remap xref-find-references] . lsp-ui-peek-find-references))
  :init (setq lsp-ui-doc-delay 0.5
              lsp-ui-doc-position 'bottom
	          lsp-ui-doc-max-width 100
              )
)


(setq lsp-file-watch-threshold nil)

;; lsp helper - Increase the amount of data which Emacs reads from the process
(setq read-process-output-max (* 1024 1024))


;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;; setting up debugging support with dap-mode

(use-package exec-path-from-shell
  :init (exec-path-from-shell-initialize))

(use-package dap-mode
  ;;:after (lsp-mode dap-gdb-lldb)
  ;; (require  dap-lldb  dap-cpptools   dap-dlv-go dap-java) ;; go-lang and java

  :config
  ;; dap-auto-configure-features!
  (dap-auto-configure-mode)
  (dap-ui-mode)
  (dap-ui-controls-mode 1)
  (require 'dap-lldb)
  (require 'dap-gdb-lldb)
  ;; installs .extension/vscode
  (dap-gdb-lldb-setup)
  (dap-register-debug-template
   "JEB::Rust::LLDB Run Configuration"
   (list :type "lldb"
         :request "launch"
         :name "LLDB::Run"
	     :gdbpath "rust-lldb"
         ))
  (dap-register-debug-template
   "JEB::Rust::GDB Run Configuration"
   (list :type "gdb"
         :request "launch"
         :name "GDB::Run"
	     :gdbpath "rust-gdb"
         )))


;;(use-package hydra)
;; dap-hydra??

;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;; inline errors

;;(use-package flycheck)


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


(use-package rustic
  :straight (rustic :type git :host github :repo "emacs-rustic/rustic")
  :after lsp-mode
;;  :hook (rustic-mode . (lambda() (rk/rustic-mode) ))
  :hook (rustic-mode . jeb/set-cargo-target-dir)
  :custom
  (rustic-format-trigger "on-save")
  (lsp-rust-analyzer-display-lifetime-elision-hints-enable "skip_trivial")
  (lsp-rust-analyzer-display-chaining-hints t)
  (lsp-rust-analyzer-display-closure-return-type-hints t)
  ;; (lsp-rust-analyzer-cargo-extra-args "--locked")
  )

;; arguably, cargo-mode might not be unnecessary ...
(use-package cargo-mode
  :straight t
  :hook (rust-mode . cargo-minor-mode)
  :custom
  (compilation-scroll-output t)
  (define-key cargo-minor-mode-map (kbd "C-c o") 'cargo-mode-command-map))

(use-package toml-mode
  :straight t)

(use-package yaml-mode
  :straight t)


;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;; java-related settings, mostly borrowed from
;; https://github.com/neppramod/java_emacs/tree/master
;;
;; also, you might hit problems if you have a jvm that doesn't support
;; gradle/kotlin correctly (:facepalm:). I had jdk-20, but needed to
;; drop down to jdk-17. Also, jdk version prefs might get recorded into
;; conf files after you change the jdk/JAVA_HOME. I hacked this file:
;; <emacs_dir>/workspace/.metadata/.plugins/org.eclipse.core.runtime/.settings/org.eclipse.jdt.launching.prefs
;; also, the FAQ under lsp-java helped, too: https://github.com/emacs-lsp/lsp-java
;; as of Oct 2024, lsp-java sets 'lsp-java-jdt-download-url' to version 1.23, which
;; is a old. current is v1.40. 
(setq lsp-java-jdt-download-url "https://www.eclipse.org/downloads/download.php?file=/jdtls/milestones/1.40.0/jdt-language-server-1.40.0-202409261450.tar.gz")
(use-package lsp-java
  :straight t
  :hook (java-mode . lsp)
  :custom
  ;; bump the jdtls JVM args. taken from https://github.com/emacs-lsp/lsp-java,
  ;; which is taken from VSCode
  (lsp-java-vmargs '("-XX:+UseParallelGC" "-XX:GCTimeRatio=4" "-XX:AdaptiveSizePolicyWeight=90" "-Dsun.zip.disableMemoryMapping=true" "-Xmx2G" "-Xms100m"))
)


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
(use-package go-mode
  :straight t
  :hook (go-mode . lsp-deferred))


(use-package lua-mode
  :straight t)

;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;; GUTTER configs
;; stuff I don't want to delete just yet, in case I end up
;; trying to ressurect it.


;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;; tree-sitter configs
;; currently using treesit-auto as a quality of life plaugin,
;; even the maintainer admits it might be obsolete by emacs 30 ...

;; NOTE: i tried this but it barfed as I excluded rust ...
;; something in my project config looks for rust even when opening
;; a java file?!?! wtf ...

;; basically, everything except for rust, for now ...
;; (delete 'rust treesit-auto-langs)
;; (use-package treesit-auto
;;   :custom
;;   (treesit-auto-install 'prompt)
;;   :config
;;   (treesit-auto-add-to-auto-mode-alist 'all)
;;   (global-treesit-auto-mode))



;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;; (use-package eglot-java
;;   :hook ('java-mode . 'eglot-java-mode))


;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;; buffer-move - helpful to move windows around
;; https://github.com/lukhas/buffer-move
;; (use-package buffer-move)
;; (require 'buffer-move)
;; if you want to have key bindings for moving the buffers, uncommment below
;; (global-set-key (kbd "<C-S-up>")     'buf-move-up)
;; (global-set-key (kbd "<C-S-down>")   'buf-move-down)
;; (global-set-key (kbd "<C-S-left>")   'buf-move-left)
;; (global-set-key (kbd "<C-S-right>")  'buf-move-right)


;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;; hideshow - it's a builtin minor mode.
;; the following enables it by default everywhere, rather than needing to
;; do it in every damn buffer i open
;; I tried to map this onto Hyper_l, even remapped CapsLock to Hyper_l
;; in PopOS via the gnome tweaks (and can confirm via `xev`). But emacs
;; is not seeing the Hyper_l event :(
;; (setq-default hs-minor-mode t)
;; (global-set-key (kbd "C-c C-h") (kbd "C-c @ C-h"))         ;;hiding block of code
;; (global-set-key (kbd "C-c C-r") (kbd "C-c @ C-s"))         ;;revealing block of code
;; (global-set-key (kbd "H-h") (kbd "C-c @ C-h"))         ;;hiding block of code
;; (global-set-key (kbd "H-s") (kbd "C-c @ C-s"))         ;;revealing block of code
;; (add-hook 'prog-mode-hook #'hs-minor-mode)


;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;; winum - easier window switching
;; https://github.com/deb0ch/emacs-winum

;; note: the keymapping be declared before the require command :shrug:
;; (use-package winum)
;; (setq winum-keymap
;;     (let ((map (make-sparse-keymap)))
;;       (define-key map (kbd "M-0") 'winum-select-window-0-or-10)
;;       (define-key map (kbd "M-1") 'winum-select-window-1)
;;       (define-key map (kbd "M-2") 'winum-select-window-2)
;;       (define-key map (kbd "M-3") 'winum-select-window-3)
;;       (define-key map (kbd "M-4") 'winum-select-window-4)
;;       (define-key map (kbd "M-5") 'winum-select-window-5)
;;       (define-key map (kbd "M-6") 'winum-select-window-6)
;;       (define-key map (kbd "M-7") 'winum-select-window-7)
;;       (define-key map (kbd "M-8") 'winum-select-window-8)
;;       map))
;; (require 'winum)
;; (winum-mode)

