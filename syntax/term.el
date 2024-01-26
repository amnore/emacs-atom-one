;;; term.el --- Colors for terminals -*- lexical-binding: t; -*-
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Source: https://github.com/pulsar-edit/pulsar/blob/master/packages/one-light-syntax/styles/syntax/base.less
;;
;;; Code:

(define-namespace one-light-- :global t

(custom-theme-set-faces
 'one-light

 `(ansi-color-black ((t :foreground ,(hsl-to-hex mono-1) :background ,(hsl-to-hex mono-1))))
 `(ansi-color-red ((t :foreground ,(hsl-to-hex hue-5-2) :background ,(hsl-to-hex hue-5-2))))
 `(ansi-color-green ((t :foreground ,(hsl-to-hex hue-4) :background ,(hsl-to-hex hue-4))))
 `(ansi-color-yellow ((t :foreground ,(hsl-to-hex hue-6) :background ,(hsl-to-hex hue-6))))
 `(ansi-color-blue ((t :foreground ,(hsl-to-hex hue-2) :background ,(hsl-to-hex hue-2))))
 `(ansi-color-magenta ((t :foreground ,(hsl-to-hex hue-3) :background ,(hsl-to-hex hue-3))))
 `(ansi-color-cyan ((t :foreground ,(hsl-to-hex hue-1) :background ,(hsl-to-hex hue-1))))
 `(ansi-color-white ((t :foreground ,(hsl-to-hex mono-3) :background ,(hsl-to-hex mono-3))))
 `(ansi-color-bright-black ((t :foreground ,(hsl-to-hex mono-2) :background ,(hsl-to-hex mono-2))))
 `(ansi-color-bright-red ((t :foreground ,(hsl-to-hex hue-5) :background ,(hsl-to-hex hue-5))))
 `(ansi-color-bright-green ((t :foreground ,(hsl-to-hex hue-4) :background ,(hsl-to-hex hue-4))))
 `(ansi-color-bright-yellow ((t :foreground ,(hsl-to-hex hue-6-2) :background ,(hsl-to-hex hue-6-2))))
 `(ansi-color-bright-blue ((t :foreground ,(hsl-to-hex hue-2) :background ,(hsl-to-hex hue-2))))
 `(ansi-color-bright-magenta ((t :foreground ,(hsl-to-hex hue-3) :background ,(hsl-to-hex hue-3))))
 `(ansi-color-bright-cyan ((t :foreground ,(hsl-to-hex hue-1) :background ,(hsl-to-hex hue-1))))
 `(ansi-color-bright-white ((t :foreground ,(hsl-to-hex syntax-bg) :background ,(hsl-to-hex syntax-bg))))
 )

)

;;; term.el ends here
