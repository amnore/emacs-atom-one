;;; modeline.el --- Colors for modeline -*- lexical-binding: t; -*-
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;; Code:

(define-namespace one-light-- :global t

(custom-theme-set-faces
 'one-light

 ;; doom-modeline-core.el
 `(doom-modeline-buffer-modified ((t (:foreground ,(hsl-to-hex text-color-modified)))))
 `(doom-modeline-info ((t (:foreground ,(hsl-to-hex background-color-success)))))
 `(doom-modeline-bar ((t (:background ,(hsl-to-hex ui-site-color-2)))))
 `(doom-modeline-bar-inactive ((t (:background ,(hsl-to-hex tab-background-color)))))
 `(doom-modeline-evil-emacs-state ((t (:foreground ,(face-foreground 'mode-line-inactive)))))
 `(doom-modeline-evil-insert-state ((t (:foreground ,(hsl-to-hex ui-site-color-1)))))
 `(doom-modeline-evil-motion-state ((t (:foreground ,(hsl-to-hex ui-site-color-3)))))
 `(doom-modeline-evil-normal-state ((t (:foreground ,(hsl-to-hex ui-site-color-2)))))
 `(doom-modeline-evil-operator-state ((t (:foreground ,(hsl-to-hex ui-site-color-3)))))
 `(doom-modeline-evil-visual-state ((t (:foreground ,(hsl-to-hex ui-site-color-4)))))
 `(doom-modeline-lsp-running ((t (:inherit info :weight normal))))
 )

)

;;; modeline.el ends here
