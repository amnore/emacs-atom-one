;;; latex.el --- Colors for latex -*- lexical-binding: t; -*-
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Colors for latex
;;
;;; Code:

(define-namespace one-light-- :global t

(custom-theme-set-faces
 'one-light

 `(font-latex-sectioning-5-face ((t :weight bold :inherit font-lock-type-face)))
 `(font-latex-bold-face ((t :inherit bold)))
 `(font-latex-italic-face ((t :inherit italic)))
 `(font-latex-underline-face ((t :inherit underline)))
 `(font-latex-math-face ((t :foreground ,(hsl-to-hex hue-6))))
 `(font-latex-sedate-face ((t :inherit bold)))
 `(font-latex-string-face ((t :inherit font-lock-string-face)))
 `(font-latex-warning-face ((t :inherit font-lock-keyword-face)))
 `(font-latex-verbatim-face ((t :inherit tex-verbatim)))
 `(font-latex-script-char-face ((t)))
 `(font-latex-slide-title-face ((t :height 1.2 :inherit font-latex-sectioning-0-face)))
 )

)

;;; latex.el ends here
