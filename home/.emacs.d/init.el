;; if starting emacs with a custom user-emacs-directory
;; https://emacs.stackexchange.com/questions/4253/how-to-start-emacs-with-a-custom-user-emacs-directory
(setq user-init-file (or load-file-name (buffer-file-name)))
(setq user-emacs-directory (file-name-directory user-init-file))

;; save customized variables into a separate file, not init.el
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Saving-Customizations.html
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load-file custom-file)

(setq confirm-kill-emacs 'yes-or-no-p)
(fset 'yes-or-no-p 'y-or-n-p)
(tool-bar-mode 0)
(menu-bar-mode 0)
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode 0))

(setq backup-directory-alist
      `(("." . ,(concat user-emacs-directory "backups"))))

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

(show-paren-mode 1)
(column-number-mode 1)

; automatically pairs braces when you create them
(electric-pair-mode 1)

;; don't use global line highlight
(global-hl-line-mode 0)

; mode-line - The ModeLine is defined by the variable ‘mode-line-format’
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

;; package.el
(require 'package)
(add-to-list 'package-archives '("tromey" . "http://tromey.com/elpa/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(setq package-user-dir (expand-file-name "elpa/" user-emacs-directory))
(package-initialize)
;; force install of use-package 
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))


;; which-key: displays in the mini buffer the key bindings following
;; your currently entered incomplete command
;; https://github.com/justbur/emacs-which-key
(use-package which-key
  :ensure
  :init
  (which-key-mode))

;; vertico - vertical completion UI (for example, when selecting files
;; in a directory, in the mini buffer). replacement of older selectrum.el
;; https://github.com/minad/vertico
(use-package vertico
  :ensure
  :init
  (vertico-mode))

;; themes
;; currently liking prot's https://github.com/protesilaos/ef-themes
(use-package ef-themes
  :ensure t)
(load-theme 'ef-maris-dark t)



;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;; IBuffer: buffer list manangement: IBuffer (https://www.emacswiki.org/emacs/IbufferMode)
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; auto-mode-alist adjustments (https://www.emacswiki.org/emacs/AutoModeAlist)
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.cc\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.cpp\\'" . c++-mode))
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
;; treemacs support and config
;; this is mostly just taken from the docs (https://github.com/Alexander-Miller/treemacs)
(use-package treemacs
  :ensure t
  :defer t
  :bind
  (:map global-map
        ("<f9>"      . treemacs)
        ("<f8>"      . treemacs-select-window)
        ))

(use-package treemacs-magit
  :after (treemacs magit)
  :ensure t)



;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;; rustic = basic rust-mode + additions

(use-package rustic
  :ensure
  :bind (:map rustic-mode-map
              ("M-j" . lsp-ui-imenu)
              ("M-?" . lsp-find-references)
              ("C-c C-c l" . flycheck-list-errors)
              ("C-c C-c a" . lsp-execute-code-action)
              ("C-c C-c r" . lsp-rename)
              ("C-c C-c d" . dap-hydra))
   :config

  ;; comment to disable rustfmt on save
  (add-hook 'rustic-mode-hook 'rk/rustic-mode-hook)
  )

(defun rk/rustic-mode-hook ()
  ;; so that run C-c C-c C-r works without having to confirm, but don't try to
  ;; save rust buffers that are not file visiting. Once
  ;; https://github.com/brotzeit/rustic/issues/253 has been resolved this should
  ;; no longer be necessary.
  (when buffer-file-name
    (setq-local buffer-save-without-query t))
  (add-hook 'before-save-hook 'lsp-format-buffer nil t))

(use-package toml-mode :ensure)

;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;; lsp-mode and friends

(use-package lsp-mode
  :ensure
  :commands lsp
  :init
  ; this is for which-key integration documentation, need to use lsp-mode-map
  (setq lsp-keymap-prefix "C-c l" )
  :custom
  ;; what to use when checking on-save. "check" is default, I prefer clippy
  (lsp-rust-analyzer-cargo-watch-command "clippy")
  (lsp-idle-delay 0.5)
  ;; This controls the overlays that display type and other hints inline. Enable
  ;; / disable as you prefer. Well require a `lsp-workspace-restart' to have an
  ;; effect on open projects.
  (lsp-inlay-hint-enable nil)
  (lsp-rust-analyzer-display-lifetime-elision-hints-enable "skip_trivial")
  (lsp-rust-analyzer-display-chaining-hints t)
  (lsp-rust-analyzer-display-lifetime-elision-hints-use-parameter-names nil)
  (lsp-rust-analyzer-display-closure-return-type-hints t)
  (lsp-rust-analyzer-display-parameter-hints nil)
  (lsp-rust-analyzer-display-reborrow-hints nil)
  ;; ???
  (define-key lsp-mode-map (kbd "C-c l") lsp-command-map)
  :config
  (add-hook 'lsp-mode-hook 'lsp-ui-mode)
  (add-hook 'java-mode #'lsp-deferred)
  (add-hook 'lsp-mode-hook 'lsp-enable-which-key-integration)
 )

(use-package lsp-ui
  :ensure
  :commands lsp-ui-mode
  :custom
  (lsp-ui-peek-always-show t)
  (lsp-ui-sideline-show-hover t)
  (lsp-ui-doc-enable nil)
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

;; lsp-booster integration
;; https://github.com/blahgeek/emacs-lsp-booster
(defun lsp-booster--advice-json-parse (old-fn &rest args)
  "Try to parse bytecode instead of json."
  (or
   (when (equal (following-char) ?#)
     (let ((bytecode (read (current-buffer))))
       (when (byte-code-function-p bytecode)
         (funcall bytecode))))
   (apply old-fn args)))
(advice-add (if (progn (require 'json)
                       (fboundp 'json-parse-buffer))
                'json-parse-buffer
              'json-read)
            :around
            #'lsp-booster--advice-json-parse)

(defun lsp-booster--advice-final-command (old-fn cmd &optional test?)
  "Prepend emacs-lsp-booster command to lsp CMD."
  (let ((orig-result (funcall old-fn cmd test?)))
    (if (and (not test?)                             ;; for check lsp-server-present?
             (not (file-remote-p default-directory)) ;; see lsp-resolve-final-command, it would add extra shell wrapper
             lsp-use-plists
             (not (functionp 'json-rpc-connection))  ;; native json-rpc
             (executable-find "emacs-lsp-booster"))
        (progn
          (message "Using emacs-lsp-booster for %s!" orig-result)
          (cons "emacs-lsp-booster" orig-result))
      orig-result)))
(advice-add 'lsp-resolve-final-command :around #'lsp-booster--advice-final-command)


;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;; inline errors

(use-package flycheck :ensure)


;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;; yas - snippets. i really don't use this (now), but I have
;; other stuffs that does (which I haven't cleaned up yet)
(use-package yasnippet
  :ensure
  :config
  (yas-reload-all)
  (add-hook 'prog-mode-hook 'yas-minor-mode)
  (add-hook 'text-mode-hook 'yas-minor-mode))

;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;; company - auto-completion
(use-package company
  :ensure
  :bind
  (:map company-active-map
              ("C-n". company-select-next)
              ("C-p". company-select-previous)
              ("M-<". company-select-first)
              ("M->". company-select-last))
  (:map company-mode-map
        ("<tab>". tab-indent-or-complete)
        ("TAB". tab-indent-or-complete)))

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
;; java-related settings, mostly borrowed from
;; https://github.com/neppramod/java_emacs/tree/master
;;
;; also, you might hit problems if you have a jvm that doesn't support
;; gradle/kotlin correctly (:facepalm:). I had jdk-20, but needed to
;; drop down to jdk-17. Also, jdk version prefs might get recorded into
;; conf files after you change the jdk/JAVA_HOME. I hacked this file:
;; <emacs_dir>/workspace/.metadata/.plugins/org.eclipse.core.runtime/.settings/org.eclipse.jdt.launching.prefs
;; alos, the FAQ under lsp-java helped, too: https://github.com/emacs-lsp/lsp-java

(use-package lsp-java
  :ensure t
  :config (add-hook 'java-mode-hook 'lsp))

;; bump the jdtls JVM args. taken from https://github.com/emacs-lsp/lsp-java, which is taken from VSCode
(setq lsp-java-vmargs '("-XX:+UseParallelGC" "-XX:GCTimeRatio=4" "-XX:AdaptiveSizePolicyWeight=90" "-Dsun.zip.disableMemoryMapping=true" "-Xmx2G" "-Xms100m"))


;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;; c/c++-related settings, mostly borrowed from a post on the emacs lsp page:
;; https://emacs-lsp.github.io/lsp-mode/tutorials/CPP-guide/

;; (add-hook 'c-mode-hook 'lsp)
;; (add-hook 'c++-mode-hook 'lsp)


;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;; python-related settings, mostly borrowed from a post on the emacs lsp page:
;; https://emacs-lsp.github.io/lsp-mode/page/lsp-pylsp/
;;
;; using pylsp: https://github.com/python-lsp/python-lsp-server

;; (use-package lsp-mode
;;   :hook
;;   ((python-mode . lsp)))
;; (use-package python-mode
;;   :ensure t)
;; (add-hook 'python-mode-hook 'lsp)


;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;; golang-related settings. Need a Go SDK installed, and the go lsp server (gopls)
;; Make sure the the go tools bin is on the path ($HOME/go/bin): 
;; https://github.com/golang/tools/blob/master/gopls/doc/emacs.md
;; (use-package go-mode
;;   :ensure t)
;; (add-hook 'go-mode-hook #'lsp-deferred)


;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;; setting up debugging support with dap-mode

;; (use-package exec-path-from-shell
;;   :ensure
;;   :init (exec-path-from-shell-initialize))

;; (use-package dap-mode
;;   :ensure
;;   :config
;;   (dap-auto-configure-mode)
;;   (dap-ui-mode)
;;   (dap-ui-controls-mode 1)

;;   (require 'dap-lldb)
;;   (require 'dap-gdb-lldb)
;;   (require 'dap-java)
;;   ;; (require 'dap-dlv-go) ;; go-lang

;;   ;; installs .extension/vscode
;;   (dap-gdb-lldb-setup)
;;   (dap-register-debug-template
;;    "Rust::LLDB Run Configuration"
;;    (list :type "lldb"
;;          :request "launch"
;;          :name "LLDB::Run"
;; 	     :gdbpath "rust-lldb"
;;          ;; uncomment if lldb-mi is not in PATH
;;          ;; :lldbmipath "path/to/lldb-mi"
;;          )))

;;(use-package hydra)

;; enable bidirectional synchronization of lsp workspace folders and treemacs projects.
(use-package lsp-treemacs
  :after (treemacs lsp-mode)
  :defer t
  :ensure t)
(lsp-treemacs-sync-mode t)


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
;;   :ensure t
;;   )
;; (add-hook 'java-mode-hook 'eglot-java-mode)


;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;; buffer-move - helpful to move windows around
;; https://github.com/lukhas/buffer-move
;; (use-package buffer-move
;;   :ensure t)
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
;; (use-package winum
;;   :ensure t)
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


;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;; (recentf-mode 1)
;; (setq recentf-max-saved-items 100
;;       inhibit-startup-message t
;;       ring-bell-function 'ignore)

