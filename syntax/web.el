;;; web.el --- Colors for web related modes -*- lexical-binding: t; -*-
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Source: https://github.com/pulsar-edit/pulsar/blob/master/packages/one-light-syntax/styles/syntax/base.less
;;  Source: https://github.com/pulsar-edit/pulsar/blob/master/packages/one-light-syntax/styles/syntax/css.less
;;
;;; Code:

(define-namespace one-light-- :global t

(custom-theme-set-faces
 'one-light

 ;; css-mode.el
 `(css-selector ((t (:foreground ,(hsl-to-hex hue-6)))))
 `(css-property ((t (:foreground ,(hsl-to-hex mono-1)))))

 ;; web-mode.el
 `(web-mode-error-face ((t (:inherit error))))
 `(web-mode-symbol-face ((t (:inherit font-lock-variable-name-face))))
 `(web-mode-doctype-face ((t (:foreground ,(hsl-to-hex mono-1)))))
 `(web-mode-html-tag-face ((t (:inherit font-lock-tag-face))))
 `(web-mode-html-tag-bracket-face ((t (:foreground ,(hsl-to-hex mono-1)))))
 `(web-mode-html-attr-name-face ((t (:inherit font-lock-attribute-face))))
 `(web-mode-html-attr-engine-face
   ((t (:inherit web-mode-html-attr-name-face))))
 `(web-mode-css-selector-face ((t (:inherit css-selector))))
 `(web-mode-css-pseudo-class-face
   ((t (:inherit web-mode-css-selector-face))))
 `(web-mode-css-property-name-face ((t (:inherit css-property))))
 `(web-mode-css-color-face ((t (:foreground ,(hsl-to-hex hue-6)))))
 `(web-mode-css-priority-face ((t (:inherit font-lock-keyword-face))))
 `(web-mode-css-function-face ((t (:inherit font-lock-function-name-face))))
 `(web-mode-css-variable-face ((t (:foreground ,(hsl-to-hex hue-5)))))
 )

)

;;; web.el ends here
