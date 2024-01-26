;;; base.el --- Base colors -*- lexical-binding: t; -*-
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Source: https://github.com/pulsar-edit/pulsar/blob/master/packages/one-light-syntax/styles/editor.less
;;
;;; Code:

(define-namespace one-light-- :global t

(custom-theme-set-faces
 'one-light

 ;; faces.el
 `(default ((t (:foreground ,(hsl-to-hex syntax-text-color)
                  :background ,(hsl-to-hex syntax-background-color)))))
 ;; `(shadow ((t (:foreground ,(hsl-to-hex text-color-subtle)))))
 `(link ((t (:inherit underline
               :foreground ,(hsl-to-hex hue-1)))))
 `(link-visited ((t (:inherit link))))
 `(highlight ((t (:background ,(hsl-to-hex syntax-selection-color)))))
 `(region ((t (:background ,(hsl-to-hex syntax-selection-color)))))
 `(secondary-selection ((t (:inherit region))))
 `(line-number ((t (:foreground ,(hsl-to-hex syntax-gutter-text-color)))))
 `(line-number-current-line ((t (:foreground ,(hsl-to-hex syntax-gutter-text-color-selected)))))
 `(fill-column-indicator ((t (:foreground ,(hsl-to-hex syntax-wrap-guide-color)))))
 `(escape-glyph ((t (:foreground ,(hsl-to-hex hue-4)))))
 `(minibuffer-prompt ((t nil)))
 `(cursor ((t (:background ,(hsl-to-hex syntax-cursor-color)))))
 `(glyphless-char ((t (:foreground ,(hsl-to-hex syntax-invisible-character-color)))))
 `(show-paren-match ((t (:underline ,(hsl-to-hex syntax-cursor-color)))))
 `(show-paren-match-expression ((t (:inherit show-paren-match))))
 `(show-paren-mismatch ((t nil)))

 ;; whitespace.el
 `(whitespace-space ((t (:inherit glyphless-char))))
 `(whitespace-hspace ((t (:inherit glyphless-char))))
 `(whitespace-tab ((t (:inherit glyphless-char))))
 `(whitespace-newline ((t (:inherit glyphless-char))))
 `(whitespace-trailing ((t (:inherit glyphless-char))))
 `(whitespace-line ((t nil)))
 `(whitespace-space-before-tab ((t (:inherit glyphless-char))))
 `(whitespace-indentation ((t (:inherit glyphless-char))))
 `(whitespace-big-indent ((t (:inherit glyphless-char))))
 `(whitespace-missing-newline-at-eof ((t (:inherit glyphless-char))))
 `(whitespace-empty ((t (:inherit glyphless-char))))
 `(whitespace-space-after-tab ((t (:inherit glyphless-char))))

 ;; hl-line.el
 `(hl-line ((t (:background ,(hsl-to-hex syntax-cursor-line)))))

 ;; isearch.el
 `(isearch
   ((t (:inherit lazy-highlight
          :box (:line-width 2
                :color ,(hsl-to-hex syntax-result-marker-color-selected))))))
 `(lazy-highlight
   ((t (:background ,(hsl-to-hex syntax-result-marker-color)))))

 ;; button.el
 `(button ((t (:inherit underline))))

 ;; font-lock.el
 `(font-lock-warning-face ((t (:inherit warning))))
 `(font-lock-function-name-face ((t (:foreground ,(hsl-to-hex hue-2)))))
 `(font-lock-variable-name-face ((t (:foreground ,(hsl-to-hex mono-1)))))
 `(font-lock-keyword-face ((t (:foreground ,(hsl-to-hex hue-3)))))
 `(font-lock-comment-face ((t (:foreground ,(hsl-to-hex mono-3)
                                 :slant italic))))
 `(font-lock-comment-delimiter-face ((t (:inherit font-lock-comment-face))))
 `(font-lock-type-face ((t (:foreground ,(hsl-to-hex hue-1)))))
 `(font-lock-constant-face ((t (:foreground ,(hsl-to-hex hue-6)))))
 `(font-lock-builtin-face ((t (:foreground ,(hsl-to-hex hue-5)))))
 `(font-lock-preprocessor-face
   ((t (:inherit font-lock-keyword-face))))
 `(font-lock-string-face ((t (:foreground ,(hsl-to-hex hue-4)))))
 `(font-lock-doc-face ((t (:inherit font-lock-comment-face))))
 `(font-lock-doc-markup-face ((t (:foreground ,(hsl-to-hex (lighten mono-3 0.06))
                                    :weight bold))))
 `(font-lock-negation-char-face ((t (:foreground ,(hsl-to-hex hue-3)))))
 `(font-lock-regexp-grouping-backslash ((t (:foreground ,(hsl-to-hex hue-1)))))
 `(font-lock-regexp-grouping-construct ((t (:foreground ,(hsl-to-hex hue-1)))))
 `(font-lock-member-face ((t (:foreground ,(hsl-to-hex hue-5)))))
 `(font-lock-tag-face ((t (:foreground ,(hsl-to-hex hue-5)))))
 `(font-lock-attribute-face ((t (:foreground ,(hsl-to-hex hue-6)))))
 )

(custom-theme-set-variables
 'one-light
 `(hl-line-sticky-flag nil))
)

;;; base.el ends here
