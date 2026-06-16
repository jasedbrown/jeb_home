;; Disable package.el in favor of straight.el,
;; this allows a completely reproducable set up.
(setq package-enable-at-startup nil)

;; Keep GC out of the hot startup path, then restore the value this config
;; expects for LSP/editor work once init is done.
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold 100000000
                  gc-cons-percentage 0.1)))
