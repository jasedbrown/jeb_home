;;----------------------------------------------
;; STARTUP STUFFS - http://blog.d46.us/advanced-emacs-startup

;; Use a hook so the message doesn't get clobbered by other messages.
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs ready in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

;; Make startup faster by reducing the frequency of garbage
;; collection.  The default is 800 kilobytes.  Measured in bytes.
(setq gc-cons-threshold (* 50 1000 1000))

;;--------------------------------------------
;; The rest of the init file.
(setq backup-directory-alist
      `(("." . ,(concat user-emacs-directory "backups"))))

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

;; save yank to system clipboard
(setq yank-pop-change-selection t)

(show-paren-mode 1)
(setq column-number-mode t)

;; minimalistic emacs at startup
(menu-bar-mode -1)
;;(tool-bar-mode -1)
;;(scroll-bar-mode -1)

;; don't use global line highlight
(global-hl-line-mode 0)

;; supress welcome screen
(setq inhibit-startup-message t)

;;---------------------------------------------------
;; PACKAGE MANAGEMENT
(require 'package)
(add-to-list 'package-archives
             ;'("melpa" . "https://melpa.org/packages/"))
             '("melpa-stable" . "https://stable.melpa.org/packages/"))
(package-initialize)

; install use-package as that installs everything else
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))


;;---------------------------------------------
;; ENDING STUFFS
;; Make gc pauses faster by decreasing the threshold.
(setq gc-cons-threshold (* 2 1000 1000))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(go-mode rust-mode yaml-mode use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
