;;; one-light-syntax.el --- One Light syntax colors -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2022 czg
;;
;; Author: czg <https://github.com/czg>
;; Maintainer: czg <me@markle.one>
;; Created: January 25, 2022
;; Modified: January 25, 2022
;; Version: 0.0.1
;; Keywords: faces one-light
;; Homepage: https://github.com/czg/one-light-syntax
;; Package-Requires: ((emacs "26.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; This file is ported from One Light Syntax theme in Atom.
;;
;;; Code:
(require 'one-light-lib)

(defface info nil
  "Info face, inherited by other faces.")

(defface lsp-face-semhl-declaration nil
  "LSP face for declaration modifier.")

(defface lsp-face-semhl-readonly nil
  "LSP face for readonly modifier.")

(defface lsp-face-semhl-abstract nil
  "LSP face for abstract modifier.")

(defface lsp-face-semhl-async nil
  "LSP face for async modifier.")

(defface lsp-face-semhl-modification nil
  "LSP face for modification modifier.")

(defface lsp-face-semhl-documentation nil
  "LSP face for documentation modifier.")

(with-eval-after-load 'lsp-semantic-tokens
  (setq lsp-semantic-token-modifier-faces
        '(("declaration" . lsp-face-semhl-declaration)
          ("definition" . lsp-face-semhl-definition)
          ("readonly" . lsp-face-semhl-readonly)
          ("static" . lsp-face-semhl-static)
          ("deprecated" . lsp-face-semhl-deprecated)
          ("abstract" . lsp-face-semhl-abstract)
          ("async" . lsp-face-semhl-async)
          ("modification" . lsp-face-semhl-modification)
          ("documentation" . lsp-face-semhl-documentation)
          ("defaultLibrary" . lsp-face-semhl-default-library))))

(cl-flet ((to-ratio (degree) (one-light-lib--degree-to-ratio degree))
          (mix (c1 c2 &optional w) (one-light-lib--mix c1 c2 w))
          (darken (color ratio) (apply 'color-darken-hsl
                                       (append color (list (* ratio 100)))))
          (lighten (color ratio) (apply 'color-lighten-hsl
                                        (append color (list (* ratio 100)))))
          (luma (color) (one-light-lib--luma color))
          (name-to-hsl (name) (apply 'color-rgb-to-hsl
                                     (color-name-to-rgb name)))
          (hsv-to-hsl (hsv) (one-light-lib--color-hsv-to-hsl hsv))
          (hex-to-hsl (hex) (one-light-lib--color-hex-to-hsl hex))
          (to-hex (hsl) (apply 'color-rgb-to-hex (apply 'color-hsl-to-rgb hsl)))
          (contrast (color dark light &optional threshold)
                    (one-light-lib--color-contrast color dark light threshold))
          (hue (hsl) (nth 0 hsl))
          (saturation (hsl) (nth 1 hsl))
          (lightness (hsl) (nth 2 hsl)))
  (let* (;;; colors.less
         ;; Config
         (syntax-hue (to-ratio 230))
         (syntax-saturation 0.01)
         (syntax-brightness 0.98)
         ;; Monochrome
         (mono-1 `(,syntax-hue 0.08 0.24))
         (mono-2 `(,syntax-hue 0.06 0.44))
         (mono-3 `(,syntax-hue 0.04 0.64))
         ;; Colors
         (hue-1 `(,(to-ratio 198) 0.99 0.37)) ; cyan
         (hue-2 `(,(to-ratio 221) 0.87 0.6)) ; blue
         (hue-3 `(,(to-ratio 301) 0.63 0.4)) ; purple
         (hue-4 `(,(to-ratio 119) 0.34 0.47)) ; green
         (hue-5 `(,(to-ratio 5) 0.74 0.59)) ; red 1
         (hue-5-2 `(,(to-ratio 344) 0.84 0.43)) ; red 2
         (hue-6 `(,(to-ratio 35) 0.99 0.36)) ; orange 1
         (hue-6-2 `(,(to-ratio 35) 0.99 0.4)) ; orange 2
         ;; Base colors
         (syntax-fg mono-1)
         (syntax-bg `(,syntax-hue ,syntax-saturation ,syntax-brightness))
         (syntax-gutter (darken syntax-bg 0.36))
         (syntax-guide (mix syntax-fg syntax-bg 0.2))
         (syntax-accent `(,syntax-hue 1 0.66))

         ;;; syntax-variables.less
         ;; General colors
         (syntax-text-color syntax-fg)
         (syntax-cursor-color syntax-accent)
         (syntax-selection-color (darken syntax-bg 0.08))
         (syntax-selection-flash-color syntax-accent)
         (syntax-background-color syntax-bg)
         ;; Guide colors
         (syntax-wrap-guide-color syntax-guide)
         (syntax-indent-guide-color syntax-guide)
         (syntax-invisible-character-color syntax-guide)
         ;; For find and replace markers
         (syntax-result-marker-color (mix syntax-accent syntax-bg 0.2))
         (syntax-result-marker-color-selected syntax-accent)
         ;; Gutter colors
         (syntax-gutter-text-color syntax-gutter)
         (syntax-gutter-text-color-selected syntax-fg)
         (syntax-gutter-background-color syntax-bg)
         (syntax-gutter-background-color-selected (darken syntax-bg 0.08))
         ;; Git colors - For git diff info. i.e. in the gutter
         (syntax-color-renamed `(,(to-ratio 208) 1 0.66))
         (syntax-color-added `(,(to-ratio 132) 0.6 0.44))
         (syntax-color-modified `(,(to-ratio 40) 0.9 0.5))
         (syntax-color-removed `(,(to-ratio 0) 1 0.54))
         ;; For language entity colors
         (syntax-color-variable hue-5)
         (syntax-color-constant hue-6)
         (syntax-color-property syntax-fg)
         (syntax-color-value syntax-fg)
         (syntax-color-function hue-2)
         (syntax-color-method hue-2)
         (syntax-color-class hue-6-2)
         (syntax-color-keyword hue-3)
         (syntax-color-tag hue-5)
         (syntax-color-attribute hue-6)
         (syntax-color-import hue-3)
         (syntax-color-snippet hue-4)
         ;; Custom Syntax Variables
         (syntax-cursor-line (mix syntax-fg syntax-bg 0.05))
         (syntax-deprecated-fg (darken syntax-color-modified 0.5))
         (syntax-deprecated-bg syntax-color-modified)
         (syntax-illegal-fg (name-to-hsl "white"))
         (syntax-illegal-bg syntax-color-removed)

         (display '((class color) (min-colors 89))))
    (custom-theme-set-faces
     'one-light

     ;; faces.el
     `(default ((,display (:foreground ,(to-hex syntax-text-color)
                           :background ,(to-hex syntax-background-color)))))
                                        ; editor.less: atom-text-editor
     ;; `(shadow ((,display (:foreground ,(to-hex text-color-subtle)))))
     `(link ((,display (:inherit underline
                        :foreground ,(to-hex hue-1)))))
                                        ; base.less: .syntax--link,
                                        ; .syntax--markup.syntax--underline
     `(link-visited ((,display (:inherit link))))
     `(highlight ((,display (:background ,(to-hex syntax-selection-color)))))
     `(region ((,display (:background ,(to-hex syntax-selection-color)))))
                                        ; editor.less: .region
     `(secondary-selection ((,display (:inherit region))))
                                        ; editor.less: .region
     ;; `(trailing-whitespace ((,display (:underline ,(to-hex accent-color)))))
     `(line-number
       ((,display (:foreground ,(to-hex syntax-gutter-text-color)))))
                                        ; editor.less: .gutter.line-number
     `(line-number-current-line
       ((,display (:foreground ,(to-hex syntax-gutter-text-color-selected)))))
                                        ; editor.less: .gutter.cursor-line,
                                        ; .gutter.cursor-line-no-selection
     `(fill-column-indicator
       ((,display (:foreground ,(to-hex syntax-wrap-guide-color)))))
                                        ; editor.less: .wrap-guide
     `(escape-glyph ((,display (:foreground ,(to-hex hue-4)))))
                                        ; base.less: .syntax-escape

     `(minibuffer-prompt ((,display nil)))

     `(cursor ((,display (:background ,(to-hex syntax-cursor-color)))))
                                        ; editor.less: .cursor
     `(glyphless-char
       ((,display (:foreground ,(to-hex syntax-invisible-character-color)))))
                                        ; editor.less: .invisible-character
     `(show-paren-match ((,display (:underline ,(to-hex syntax-cursor-color)))))
                                        ; editor.less: .bracket-matcher.region
     `(show-paren-match-expression ((,display (:inherit show-paren-match))))
     `(show-paren-mismatch ((,display nil)))

     ;; hl-line.el
     `(hl-line ((,display (:background ,(to-hex syntax-cursor-line)))))

     ;; isearch.el
     `(isearch
       ((,display
         (:inherit lazy-highlight
          :box (:line-width 2
                :color ,(to-hex syntax-result-marker-color-selected))))))
                                        ; editor.less: .current-result
     `(lazy-highlight
       ((,display (:background ,(to-hex syntax-result-marker-color)))))
                                        ; editor.less: .find-result

     ;; font-lock.el
     `(font-lock-warning-face ((,display (:inherit warning))))
     `(font-lock-function-name-face ((,display (:foreground ,(to-hex hue-2)))))
                                        ; base.less: .syntax--function
     `(font-lock-variable-name-face ((,display (:foreground ,(to-hex mono-1)))))
                                        ; base.less: .syntax--entity
     `(font-lock-keyword-face ((,display (:foreground ,(to-hex hue-3)))))
                                        ; base.less: .syntax--keyword
     `(font-lock-comment-face ((,display (:foreground ,(to-hex mono-3)
                                          :slant italic))))
                                        ; base.less: .syntax--comment
     `(font-lock-comment-delimiter-face
       ((,display (:inherit font-lock-comment-face))))
     `(font-lock-type-face ((,display (:foreground ,(to-hex hue-1)))))
                                        ; base.less: .syntax--type
     `(font-lock-constant-face ((,display (:foreground ,(to-hex hue-6)))))
                                        ; base.less: .syntax--constant
     `(font-lock-builtin-face ((,display (:foreground ,(to-hex hue-5)))))
                                        ; base.less: .syntax--function
     `(font-lock-preprocessor-face
       ((,display (:inherit font-lock-keyword-face))))
     `(font-lock-string-face ((,display (:foreground ,(to-hex hue-4)))))
                                        ; base.less: .syntax--string
     `(font-lock-doc-face ((,display (:inherit font-lock-comment-face))))
     `(font-lock-doc-markup-face ((,display (:foreground ,(to-hex (lighten mono-3 0.06))
                                             :weight bold))))
                                        ; base.less: .syntax--comment.syntax--caption
     ; base.less: .
     `(font-lock-negation-char-face ((,display (:foreground ,(to-hex hue-3)))))
                                        ; base.less: .syntax--operator
     `(font-lock-regexp-grouping-backslash ((,display (:foreground ,(to-hex hue-3)))))
                                        ; base.less: syntax--regex.syntax--punctuation
     `(font-lock-regexp-grouping-construct ((,display (:foreground ,(to-hex hue-3)))))
                                        ; base.less: syntax--regex.syntax--punctuation

     ;; lsp-semantic-tokens.el
     `(lsp-face-semhl-namespace ((,display (:inherit lsp-face-semhl-constant))))
     `(lsp-face-semhl-regexp ((,display (:inherit lsp-face-semhl-string))))
     `(lsp-face-semhl-operator ((,display (:inherit lsp-face-semhl-keyword))))
     `(lsp-face-semhl-enum ((,display (:inherit lsp-face-semhl-type))))
     `(lsp-face-semhl-member ((,display (:foreground ,(to-hex hue-5)))))
                                        ; base.less: .syntax--variable
     `(lsp-face-semhl-property
       ((,display (:inherit lsp-face-semhl-member))))
     `(lsp-face-semhl-macro ((,display (:inherit lsp-face-semhl-constant))))
     `(lsp-face-semhl-label ((,display (:underline t))))

     ;; TODO: Add face to modifiers
     `(lsp-face-semhl-declaration ((,display nil)))
     `(lsp-face-semhl-definition ((,display nil)))
     `(lsp-face-semhl-readonly ((,display nil)))
     `(lsp-face-semhl-static ((,display nil)))
     `(lsp-face-semhl-deprecated ((,display nil)))
     `(lsp-face-semhl-abstract ((,display nil)))
     `(lsp-face-semhl-async ((,display nil)))
     `(lsp-face-semhl-modification ((,display nil)))
     `(lsp-face-semhl-documentation ((,display nil)))
     `(lsp-face-semhl-default-library ((,display nil)))

     ;; button.el
     `(button ((,display (:inherit underline))))

     ;; mmm-vars.el
     `(mmm-default-submode-face ((,display ())))

     ;; hl-todo.el
     `(hl-todo ((,display (:foreground ,(to-hex (lighten mono-3 0.06))
                           :weight bold))))
                                        ; base.less: .syntax--comment.syntax--caption

     ;; css-mode.el
     `(css-selector ((,display (:foreground ,(to-hex hue-6)))))
     `(css-property ((,display (:foreground ,(to-hex mono-1)))))

     ;; web-mode.el
     `(web-mode-error-face ((,display (:inherit error))))
     `(web-mode-symbol-face ((,display (:inherit font-lock-variable-name-face))))
     `(web-mode-doctype-face ((,display (:foreground ,(to-hex mono-1)))))
                                        ; syntax-legacy/_base.less: .syntax--meta.syntax--tag
     `(web-mode-html-tag-face ((,display (:foreground ,(to-hex hue-5)))))
                                        ; base.less: .syntax--entity.syntax--span
     `(web-mode-html-tag-bracket-face ((,display (:foreground ,(to-hex mono-1)))))
                                        ; base.less: .syntax--punctuation
     `(web-mode-html-attr-name-face ((,display (:foreground ,(to-hex hue-6)))))
                                        ; base.less: .syntax--entity.syntax--attribute
     `(web-mode-html-attr-engine-face ((,display (:inherit web-mode-html-attr-name-face))))
     `(web-mode-css-selector-face ((,display (:inherit css-selector))))
                                        ; css.less: .syntax--css.syntax--selector
     `(web-mode-css-pseudo-class-face ((,display (:inherit web-mode-css-selector-face))))
     `(web-mode-css-property-name-face ((,display (:inherit css-property))))
     `(web-mode-css-color-face ((,display (:foreground ,(to-hex hue-6)))))
                                        ; css.less: .syntax--css.syntax--constant.syntax--color
     `(web-mode-css-priority-face ((,display (:inherit font-lock-keyword-face))))
     `(web-mode-css-function-face ((,display (:inherit font-lock-function-name-face))))
     `(web-mode-css-variable-face ((,display (:foreground ,(to-hex hue-5))))))))

(provide 'one-light-syntax)
;;; one-light-syntax.el ends here
