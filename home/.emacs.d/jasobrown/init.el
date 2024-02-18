;; This is to support loading from a non-standard .emacs.d
;; via emacs -q --load "/path/to/standalone.el"
;; see https://emacs.stackexchange.com/a/4258/22184

(setq user-init-file (or load-file-name (buffer-file-name)))
(setq user-emacs-directory (file-name-directory user-init-file))
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(setq confirm-kill-emacs 'yes-or-no-p)

(require 'package)
(add-to-list 'package-archives '("tromey" . "http://tromey.com/elpa/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(setq package-user-dir (expand-file-name "elpa/" user-emacs-directory))
(package-initialize)

;; Install use-package that we require for managing all other dependencies
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
(load-theme 'ef-maris-dark t)

(fset 'yes-or-no-p 'y-or-n-p)
(recentf-mode 1)
(setq recentf-max-saved-items 100
      inhibit-startup-message t
      ring-bell-function 'ignore)

(tool-bar-mode 0)
(menu-bar-mode 0)
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode 0))

(load-file (expand-file-name "jeb.el" user-emacs-directory))

(defun server-shutdown ()
  "Save buffers, Quit, and Shutdown (kill) server"
  (interactive)
  (save-some-buffers)
  (kill-emacs)
  )
