;;; colors.el --- Base colors -*- lexical-binding: t; -*-
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Source: https://github.com/pulsar-edit/pulsar/blob/master/packages/one-light-syntax/styles/colors.less
;;  Source: https://github.com/pulsar-edit/pulsar/blob/master/packages/one-light-syntax/styles/syntax-variables.less
;;
;;; Code:

(define-namespace one-light-- :global t

(setq
 ;; Config
 syntax-hue (degree-to-ratio 230)
 syntax-saturation 0.01
 syntax-brightness 0.98
 ;; Monochrome
 mono-1 `(,syntax-hue 0.08 0.24)
 mono-2 `(,syntax-hue 0.06 0.44)
 mono-3 `(,syntax-hue 0.04 0.64)
 ;; Colors
 hue-1 `(,(degree-to-ratio 198) 0.99 0.37) ; cyan
 hue-2 `(,(degree-to-ratio 221) 0.87 0.6) ; blue
 hue-3 `(,(degree-to-ratio 301) 0.63 0.4) ; purple
 hue-4 `(,(degree-to-ratio 119) 0.34 0.47) ; green
 hue-5 `(,(degree-to-ratio 5) 0.74 0.59) ; red 1
 hue-5-2 `(,(degree-to-ratio 344) 0.84 0.43) ; red 2
 hue-6 `(,(degree-to-ratio 35) 0.99 0.36) ; orange 1
 hue-6-2 `(,(degree-to-ratio 35) 0.99 0.4) ; orange 2
 ;; Base colors
 syntax-fg mono-1
 syntax-bg `(,syntax-hue ,syntax-saturation ,syntax-brightness)
 syntax-gutter (darken syntax-bg 0.36)
 syntax-guide (mix syntax-fg syntax-bg 0.2)
 syntax-accent `(,syntax-hue 1 0.66)

 ;; General colors
 syntax-text-color syntax-fg
 syntax-cursor-color syntax-accent
 syntax-selection-color (darken syntax-bg 0.08)
 syntax-selection-flash-color syntax-accent
 syntax-background-color syntax-bg
 ;; Guide colors
 syntax-wrap-guide-color syntax-guide
 syntax-indent-guide-color syntax-guide
 syntax-invisible-character-color syntax-guide
 ;; For find and replace markers
 syntax-result-marker-color (mix syntax-accent syntax-bg 0.2)
 syntax-result-marker-color-selected syntax-accent
 ;; Gutter colors
 syntax-gutter-text-color syntax-gutter
 syntax-gutter-text-color-selected syntax-fg
 syntax-gutter-background-color syntax-bg
 syntax-gutter-background-color-selected (darken syntax-bg 0.08)
 ;; Git colors - For git diff info. i.e. in the gutter
 syntax-color-renamed `(,(degree-to-ratio 208) 1 0.66)
 syntax-color-added `(,(degree-to-ratio 132) 0.6 0.44)
 syntax-color-modified `(,(degree-to-ratio 40) 0.9 0.5)
 syntax-color-removed `(,(degree-to-ratio 0) 1 0.54)
 ;; For language entity colors
 syntax-color-variable hue-5
 syntax-color-constant hue-6
 syntax-color-property syntax-fg
 syntax-color-value syntax-fg
 syntax-color-function hue-2
 syntax-color-method hue-2
 syntax-color-class hue-6-2
 syntax-color-keyword hue-3
 syntax-color-tag hue-5
 syntax-color-attribute hue-6
 syntax-color-import hue-3
 syntax-color-snippet hue-4
 ;; Custom Syntax Variables
 syntax-cursor-line (mix syntax-fg syntax-bg 0.05)
 syntax-deprecated-fg (darken syntax-color-modified 0.5)
 syntax-deprecated-bg syntax-color-modified
 syntax-illegal-fg (name-to-hsl "white")
 syntax-illegal-bg syntax-color-removed
 )

)

;;; colors.el ends here
