;;; one-light-theme.el --- Port of Atom One Light -*- lexical-binding: t -*-
;;
;; Copyright (C) 2024 amnore
;;
;; Author: amnore <me@markle.one>
;; URL: https://github.com/amnore/emacs-one-light-port
;; Keywords: faces one-light
;; Version: 0.1.0
;; Package-Requires: ((emacs "30.1") (names "20180321.1155") (nerd-icons "0.1.0"))
;;
;;; Commentary:
;;
;; Yet another port of Atom's One Light theme
;;
;;; Code:
(require 'color)
(require 'cl-lib)
(require 'subr-x)
(require 'nerd-icons)
(eval-when-compile (require 'names))

;;;###theme-autoload
(deftheme one-light
  "Yet another port of Atom's One Light theme.")

(define-namespace one-light--

;;;; Color conversion functions
(defun name-to-hsl (name)
  "Convert color NAME to HSL format."
  (apply #'color-rgb-to-hsl (color-name-to-rgb name)))

(defun hsl-to-hex (color)
  "Convert COLOR to hex string."
  (apply #'color-rgb-to-hex (apply #'color-hsl-to-rgb color)))

(defun hsv-to-hsl (color)
  "Convert hsv COLOR to hsl."
  (let* ((h (nth 0 color))
         (s (nth 1 color))
         (v (nth 2 color))
         (l (* v (- 1 (/ s 2)))))
    (list h
          (if (or (= l 0) (= l 1))
              0
            (/ (- v l) (min l (- 1 l))))
          l)))

(defun hex-to-hsl (color)
  "Convert hex COLOR to hsl, each component can have 1, 2 or 4 digits."
  (let* ((digits (/ (length color) 3))
         (hex-max (- (expt 16.0 digits) 1))
         (subhex (lambda (pos) (string-to-number
                           (substring color
                                      (+ 1 (* digits pos))
                                      (+ 1 (* digits (+ 1 pos))))
                           16))))
    (apply #'color-rgb-to-hsl
           (mapcar (lambda (x) (/ x hex-max))
                   (mapcar subhex '(0 1 2))))))

(defun hsl-to-hsv (color)
  "Convert hsl COLOR to hsv."
  (let* ((h (nth 0 color))
         (s (nth 1 color))
         (l (nth 2 color))
         (v (+ l (* s (min l (- 1 l))))))
    (list h
          (if (= v 0)
              0
            (* 2 (- 1 (/ l v))))
          v)))

(defun degree-to-ratio (degree)
  "Convert DEGREE to the ratio of a circle."
  (/ degree 360.0))

;;; Color manipulating functions
(defun mix (color1 color2 &optional weight)
  "Mix COLOR1 and COLOR2.
COLOR1 has a weight of WEIGHT, COLOR2 has (1 - WEIGHT).
If WEIGHT is not specified, default to 0.5."
  (let ((color1-rgb (apply #'color-hsl-to-rgb color1))
        (color2-rgb (apply #'color-hsl-to-rgb color2))
        (w (or weight 0.5)))
    (apply #'color-rgb-to-hsl
           (cl-mapcar (lambda (c1 c2) (+ (* c1 w) (* c2 (- 1 w))))
                      color1-rgb
                      color2-rgb))))

(defun contrast (color &optional dark light threshold)
  "Choose between DARK and LIGHT based on the luma value of COLOR.
If the luma is less than THRESHOLD, choose LIGHT, otherwise choose DARK.
If THRESHOLD if omitted, use 0.43 by default."
  (let ((black (or dark '(0 0 0)))
        (white (or light '(0 0 1)))
        (thld (or threshold 0.43)))
    (if (< (luma color) thld)
        white
      black)))

(defun darken (color amount)
  "Darken COLOR by AMOUNT."
  (list (nth 0 color)
        (nth 1 color)
        (color-clamp (- (nth 2 color) amount))))

(defun lighten (color amount)
  "Lighten COLOR by AMOUNT, relatively if RELATIVE is non-nil."
  (darken color (- amount)))

;;; Color property functions
(defun luma (color)
  "Calculate the luma of COLOR."
  (let* ((rgb (apply #'color-hsl-to-rgb color))
         (gamma-expand (lambda (u) (if (< u 0.03928)
                                  (/ u 12.92)
                                (expt (/ (+ u 0.055) 1.055) 2.4))))
         (rgb-linear (mapcar gamma-expand rgb)))
    (cl-reduce #'+ (cl-mapcar (lambda (w u) (* w u))
                              '(0.2126 0.7152 0.0722)
                              rgb-linear))))

(defun hue (color)
  "Get the hue of COLOR."
  (nth 0 color))

(defun saturation (color)
  "Get the saturation of COLOR."
  (nth 1 color))

(defun lightness (color)
  "Get the lightness of COLOR."
  (nth 2 color))

(defun hsvvalue (color)
  "Get the v value in hsv of COLOR."
  (nth 2 (hsl-to-hsv color)))

(let* (;;; Syntax Colors
       ;; Config
       (syntax-hue (degree-to-ratio 230))
       (syntax-saturation 0.01)
       (syntax-brightness 0.98)
       ;; Monochrome
       (mono-1 `(,syntax-hue 0.08 0.24))
       (mono-2 `(,syntax-hue 0.06 0.44))
       (mono-3 `(,syntax-hue 0.04 0.64))
       ;; Colors
       (hue-1 `(,(degree-to-ratio 198) 0.99 0.37)) ; cyan
       (hue-2 `(,(degree-to-ratio 221) 0.87 0.6)) ; blue
       (hue-3 `(,(degree-to-ratio 301) 0.63 0.4)) ; purple
       (hue-4 `(,(degree-to-ratio 119) 0.34 0.47)) ; green
       (hue-5 `(,(degree-to-ratio 5) 0.74 0.59)) ; red 1
       (hue-5-2 `(,(degree-to-ratio 344) 0.84 0.43)) ; red 2
       (hue-6 `(,(degree-to-ratio 35) 0.99 0.36)) ; orange 1
       (hue-6-2 `(,(degree-to-ratio 35) 0.99 0.4)) ; orange 2
       ;; Base colors
       (syntax-fg mono-1)
       (syntax-bg `(,syntax-hue ,syntax-saturation ,syntax-brightness))
       (syntax-gutter (darken syntax-bg 0.36))
       (syntax-guide (mix syntax-fg syntax-bg 0.2))
       (syntax-accent `(,syntax-hue 1 0.66))
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
       (syntax-color-renamed `(,(degree-to-ratio 208) 1 0.66))
       (syntax-color-added `(,(degree-to-ratio 132) 0.6 0.44))
       (syntax-color-modified `(,(degree-to-ratio 40) 0.9 0.5))
       (syntax-color-removed `(,(degree-to-ratio 0) 1 0.54))
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

       ;;; UI colors
       (ui-syntax-color `(,(degree-to-ratio 230) 0.01 0.98))
       (ui-hue (if-let* ((ui-s-h (hue ui-syntax-color))
                         (no-hue (= 0 (hue ui-syntax-color))))
                   (degree-to-ratio 220)
                 (hue ui-syntax-color)))
       (ui-saturation (min (saturation ui-syntax-color) 0.24))
       (ui-lightness (max (lightness ui-syntax-color) 0.92))
       (--ui-bg `(,ui-hue ,ui-saturation ,ui-lightness))
       (--base-background-color --ui-bg)
       (--level-3-color (darken --base-background-color 0.06))
       ;; Main colors
       (ui-fg `(,ui-hue ,ui-saturation ,(- ui-lightness 0.72)))
       (ui-bg --ui-bg)
       (ui-border (darken --level-3-color 0.06))
       ;; Background (Custom)
       (level-1-color (lighten --base-background-color 0.04))
       (level-2-color --base-background-color)
       (level-3-color --level-3-color)
       (level-3-color-hover (darken level-3-color 0.06))
       (level-3-color-active (darken level-3-color 0.03))
       ;; Base
       (base-background-color --base-background-color)
       (base-border-color ui-border)
       ;; Text
       (text-color ui-fg)
       (text-color-subtle (lighten text-color 0.3))
       (text-color-highlight (darken text-color 0.12))
       (text-color-selected (darken text-color-highlight 0.12))
       (text-color-info `(,(degree-to-ratio 208) 1 0.54))
       (text-color-success `(,(degree-to-ratio 132) 0.6 0.44))
       (text-color-warning `(,(degree-to-ratio 37) 0.9 0.44))
       (text-color-error `(,(degree-to-ratio 9) 0.9 0.56))
       ;; Text (Custom)
       (text-color-faded (mix text-color ui-bg 0.3))
       (text-color-added text-color-success)
       (text-color-ignored text-color-subtle)
       (text-color-modified text-color-warning)
       (text-color-removed text-color-error)
       (text-color-renamed text-color-info)
       ;; Background
       (background-color-info `((degree-to-ratio 208) 1 0.56))
       (background-color-success `(,(degree-to-ratio 132) 0.52 0.48))
       (background-color-warning `(,(degree-to-ratio 40) 0.6 0.48))
       (background-color-error `(,(degree-to-ratio 5) 0.72 0.56))
       (background-color-highlight (darken level-3-color 0.02))
       (background-color-selected (darken level-3-color 0.06))
       (app-background-color level-3-color)
       ;; Accent (Custom)
       (accent-luma (luma `(,ui-hue 0.5 0.5)))
       ;; used for marker, inputs (smaller things)
       ;; mix hsv + hsl (favor hsl for dark, hsv for light colors)
       (accent-color (mix (hsv-to-hsl `(,ui-hue 0.6 0.6))
                          `(,ui-hue 1 0.68)
                          (* 2 accent-luma)))
       (accent-text-color (contrast accent-color
                                    `(,ui-hue 1 0.16)
                                    (hex-to-hsl "#fff")
                                    0.4))
       ;; used for button, tooltip (larger things)
       (accent-bg-color (mix (hsv-to-hsl `(,ui-hue 0.4 0.72))
                             `(,ui-hue 1 0.66)
                             (* 2 accent-luma)))
       (accent-bg-text-color (contrast accent-bg-color
                                       `(,ui-hue 1 0.1)
                                       (hex-to-hsl "#fff")
                                       0.4))
       ;; used for text only
       (accent-only-text-color (mix (hsv-to-hsl `(,ui-hue 0.7 0.5))
                                    `(,ui-hue 1 0.6)
                                    (* 2 accent-luma)))
       ;; Components
       (pane-item-background-color base-background-color)
       (pane-item-border-color base-border-color)
       (input-background-color level-1-color)
       (input-border-color base-border-color)
       (tool-panel-background-color level-3-color)
       (tool-panel-border-color base-border-color)
       (inset-panel-background-color (lighten level-2-color 0.04))
       (inset-panel-border-color (mix base-border-color ui-bg 0.15))
       (panel-heading-background-color level-2-color)
       (panel-heading-border-color base-border-color)
       (overlay-background-color (mix level-2-color level-3-color))
       (overlay-border-color base-border-color)
       (button-background-color level-1-color)
       (button-background-color-hover (darken button-background-color 0.04))
       (button-background-color-selected accent-bg-color)
       (button-border-color base-border-color)
       (tab-bar-background-color level-3-color)
       (tab-bar-border-color base-border-color)
       (tab-background-color level-3-color)
       (tab-background-color-active level-2-color)
       (tab-border-color base-border-color)
       (tree-view-background-color level-3-color)
       (tree-view-border-color base-border-color)
       (ui-site-color-1 `(,(degree-to-ratio 208) 1 0.56))
       (ui-site-color-2 `(,(degree-to-ratio 132) 0.48 0.48))
       (ui-site-color-3 `(,(degree-to-ratio 40) 0.6 0.52))
       (ui-site-color-4 (name-to-hsl "#D831B0"))
       (ui-site-color-5 (name-to-hsl "#EBDD5B"))
       ;; Components (Custom)
       (badge-background-color background-color-selected)
       (button-text-color-selected accent-bg-text-color)
       (button-border-color-selected accent-color)
       (checkbox-background-color (mix accent-bg-color ui-bg 0.33))
       (input-background-color-focus `(,ui-hue 1 0.96))
       (input-selection-color (mix (hsv-to-hsl `(,ui-hue 0.33 0.95))
                                   `(,ui-hue 1 0.98)
                                   (* 2 accent-luma)))
       (input-selection-color-focus (mix (hsv-to-hsl `(,ui-hue 0.44 0.9))
                                         `(,ui-hue 1 0.94)
                                         (* 2 accent-luma)))
       (overlay-backdrop-color `(,ui-hue
                                 ,(* 0.4 ui-saturation)
                                 ,(* 0.8 ui-lightness)))
       (overlay-backdrop-opacity 0.66)
       (progress-background-color accent-color)
       (scrollbar-color (darken level-3-color 0.14))
       (scrollbar-background-color level-3-color)
       (scrollbar-color-editor (contrast ui-syntax-color
                                         (darken ui-syntax-color 0.14)
                                         (lighten ui-syntax-color 0.09)))
       (scrollbar-background-color-editor ui-syntax-color)
       (tab-text-color text-color-subtle)
       (tab-text-color-active text-color-highlight)
       (tab-text-color-editor (contrast ui-syntax-color
                                        (lighten ui-syntax-color 0.7)
                                        text-color-highlight))
       (tab-inactive-status-added (mix text-color-success ui-bg 0.77))
       (tab-inactive-status-modified (mix text-color-warning ui-bg 0.77))
       (tooltip-background-color accent-bg-color)
       (tooltip-text-color accent-bg-text-color)
       (tooltip-text-key-color tooltip-background-color)
       (tooltip-background-key-color tooltip-text-color)
       ;; Sizes
       (font-size 9)
       (input-font-size 12)
       (disclosure-arrow-size 12)
       (component-padding 10)
       (component-icono-padding 5)
       (component-icon-size 16)
       (component-line-height 25)
       (component-border-radius 3)
       (tab-height 30)
       ;; Sizes (Custom)
       (ui-size 1)
       (ui-input-size (* 1.15 ui-size))
       (ui-padding (* 1.5 ui-size))
       (ui-padding-pane (* 0.5 ui-size))
       (ui-padding-icon (/ ui-padding 3.3))
       (ui-line-height (* 2 ui-size))
       (ui-tab-height (* 2.5 ui-size)))
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
   `(header-line ((t (:foreground ,(hsl-to-hex text-color-subtle)
                      :background ,(hsl-to-hex base-background-color)))))
   `(header-line-highlight ((t (:inherit header-line :underline t))))
   `(vertical-border ((t (:inherit border))))
   `(window-divider ((t (:inherit border))))
   `(window-divider-first-pixel ((t (:inherit window-divider))))
   `(window-divider-last-pixel ((t (:inherit window-divider))))
   `(internal-border ((t (:inherit border))))
   `(child-frame-border ((t (:inherit border))))
   `(fringe ((t ())))
   `(scroll-bar ((t (:foreground ,(hsl-to-hex scrollbar-color-editor)
                     :background ,(hsl-to-hex scrollbar-background-color-editor)))))
   `(border ((t (:foreground ,(hsl-to-hex base-border-color)
                 :background ,(hsl-to-hex base-border-color)))))
   `(tool-bar ((t (:background-color ,(hsl-to-hex level-3-color)))))
   `(tab-bar ((t (:foreground ,(hsl-to-hex tab-text-color)
                  :background ,(hsl-to-hex tab-background-color)))))
   `(tab-line ((t (:inherit tab-bar))))
   `(error ((t (:foreground ,(hsl-to-hex text-color-error)))))
   `(warning ((t (:foreground ,(hsl-to-hex text-color-warning)))))
   `(info ((t (:foreground ,(hsl-to-hex text-color-info)))))
   `(success ((t (:foreground ,(hsl-to-hex text-color-success)))))
   `(tab-line ((t (:background ,(hsl-to-hex tab-bar-background-color)))))

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
   `(font-lock-function-call-face ((t (:foreground ,(hsl-to-hex hue-2)))))
   `(font-lock-variable-name-face ((t (:foreground ,(hsl-to-hex hue-5)))))
   `(font-lock-variable-use-face ((t (:foreground ,(hsl-to-hex mono-1)))))
   `(font-lock-keyword-face ((t (:foreground ,(hsl-to-hex hue-3)))))
   `(font-lock-comment-face ((t (:foreground ,(hsl-to-hex mono-3)
                                 :slant italic))))
   `(font-lock-comment-delimiter-face ((t (:inherit font-lock-comment-face))))
   `(font-lock-type-face ((t (:foreground ,(hsl-to-hex hue-1)))))
   `(font-lock-constant-face ((t (:foreground ,(hsl-to-hex hue-6)))))
   `(font-lock-builtin-face ((t (:foreground ,(hsl-to-hex hue-3)))))
   `(font-lock-preprocessor-face ((t (:inherit font-lock-keyword-face))))
   `(font-lock-string-face ((t (:foreground ,(hsl-to-hex hue-4)))))
   `(font-lock-doc-face ((t (:inherit font-lock-comment-face))))
   `(font-lock-doc-markup-face ((t (:foreground ,(hsl-to-hex (lighten mono-3 0.06))
                                    :weight bold))))
   `(font-lock-negation-char-face ((t (:foreground ,(hsl-to-hex hue-1)))))
   `(font-lock-regexp-face ((t (:foreground ,(hsl-to-hex hue-1)))))
   `(font-lock-regexp-grouping-backslash ((t (:foreground ,(hsl-to-hex hue-1)))))
   `(font-lock-regexp-grouping-construct ((t (:foreground ,(hsl-to-hex hue-1)))))
   `(font-lock-escape-face ((t (:foreground ,(hsl-to-hex hue-1)))))
   `(font-lock-number-face ((t (:inherit font-lock-constant-face))))
   `(font-lock-operator-face ((t (:foreground ,(hsl-to-hex hue-3)))))
   `(font-lock-property-name-face ((t (:foreground ,(hsl-to-hex hue-5)))))
   `(font-lock-property-use-face ((t (:foreground ,(hsl-to-hex hue-5)))))
   `(font-lock-bracket-face ((t (:foreground ,(hsl-to-hex mono-1)))))
   `(font-lock-delimiter-face ((t (:foreground ,(hsl-to-hex mono-1)))))
   `(font-lock-misc-punctuation-face ((t (:foreground ,(hsl-to-hex mono-1)))))
   `(font-lock-punctuation-face ((t (:foreground ,(hsl-to-hex mono-1)))))
   `(font-lock-member-face ((t (:foreground ,(hsl-to-hex hue-5)))))
   `(font-lock-tag-face ((t (:foreground ,(hsl-to-hex hue-5)))))
   `(font-lock-attribute-face ((t (:foreground ,(hsl-to-hex hue-6)))))

   ;; hl-todo.el
   `(hl-todo ((t (:foreground ,(hsl-to-hex (lighten mono-3 0.06))
                  :weight bold))))

   ;; mmm-vars.el
   `(mmm-default-submode-face ((t ())))

   ;; org-faces.el
   `(org-ellipsis ((t (:foreground ,(hsl-to-hex syntax-gutter-text-color-selected)))))

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

   ;; ansi-color.el
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

   ;; popup.el
   `(popup-face ((t (:foreground ,(hsl-to-hex text-color)
                     :background ,(hsl-to-hex overlay-background-color)))))
   `(popup-summary-face ((t (:inherit popup-face
                             :foreground ,(hsl-to-hex text-color-subtle)))))
   `(popup-scroll-bar-foreground-face
     ((t (:background ,(hsl-to-hex scrollbar-color)))))
   `(popup-scroll-bar-background-face
     ((t (:background ,(hsl-to-hex scrollbar-background-color)))))
   `(popup-isearch-match ((t (:inherit isearch))))
   `(popup-tip-face ((t (:inherit popup-face))))
   `(popup-menu-face ((t (:inherit popup-face))))
   `(popup-menu-mouse-face ((t nil)))
   `(popup-menu-selection-face
     ((t (:foreground ,(hsl-to-hex text-color-selected)
          :background ,(hsl-to-hex (mix text-color-highlight
                                        overlay-background-color))))))
   `(popup-menu-summary-face ((t (:inherit popup-summary-face))))

   ;; tab-line.el
   `(tab-line-tab ((t (:foreground ,(hsl-to-hex tab-text-color-active)
                       :background ,(hsl-to-hex tab-background-color-active)))))
   `(tab-line-tab-inactive ((t (:foreground ,(hsl-to-hex tab-text-color)
                                :background ,(hsl-to-hex tab-background-color)))))
   `(tab-line-tab-inactive-alternate ((t (:inherit tab-line-tab-inactive))))
   `(tab-line-tab-special ((t (:slant italic))))
   `(tab-line-tab-modified ((t)))
   `(tab-line-tab-group ((t)))
   `(tab-line-tab-current ((t (:foreground ,(hsl-to-hex tab-text-color-active)
                               :background ,(hsl-to-hex tab-background-color-active)))))
   `(tab-line-highlight ((t)))
   `(tab-line-close-highlight ((t (:foreground ,(hsl-to-hex accent-text-color)
                                   :background ,(hsl-to-hex accent-color)))))

   ;; compilation.el
   `(compilation-info ((t (:inherit info))))

   ;; flymake.el
   `(flymake-error
     ((t (:underline (:style wave
                      :color ,(hsl-to-hex text-color-error))))))
   `(flymake-warning
     ((t (:underline (:style wave
                      :color ,(hsl-to-hex text-color-warning))))))
   `(flymake-note
     ((t (:underline (:style wave
                      :color ,(hsl-to-hex text-color-info))))))

   ;; flymake-popon.el
   `(flymake-popon
     ((t (:foreground ,(hsl-to-hex text-color)
          :background ,(hsl-to-hex overlay-background-color)))))
   `(flymake-popon-posframe-border ((t (:foreground ,(hsl-to-hex overlay-border-color)))))

   ;; faces.el
   `(mode-line ((t (:background ,(hsl-to-hex level-3-color)))))
   `(mode-line-inactive ((t (:foreground ,(hsl-to-hex tab-text-color)))))
   `(mode-line-highlight ((t (:background ,(hsl-to-hex level-3-color-hover)))))
   `(mode-line-buffer-id ((t (:inherit mode-line))))

   ;; doom-modeline-core.el
   `(doom-modeline-buffer-modified ((t (:foreground ,(hsl-to-hex text-color-modified)))))
   `(doom-modeline-info ((t (:foreground ,(hsl-to-hex background-color-success)))))
   `(doom-modeline-bar ((t (:background ,(hsl-to-hex level-3-color)))))
   `(doom-modeline-bar-inactive ((t (:background ,(face-background 'default)))))
   `(doom-modeline-evil-emacs-state ((t (:foreground ,(face-foreground 'mode-line-inactive)))))
   `(doom-modeline-evil-insert-state ((t (:foreground ,(hsl-to-hex ui-site-color-1)))))
   `(doom-modeline-evil-motion-state ((t (:foreground ,(hsl-to-hex ui-site-color-3)))))
   `(doom-modeline-evil-normal-state ((t (:foreground ,(hsl-to-hex ui-site-color-2)))))
   `(doom-modeline-evil-operator-state ((t (:foreground ,(hsl-to-hex ui-site-color-3)))))
   `(doom-modeline-evil-visual-state ((t (:foreground ,(hsl-to-hex ui-site-color-4)))))
   `(doom-modeline-lsp-running ((t (:inherit info :weight normal))))

   ;; treesit-fold.el
   `(treesit-fold-replacement-face ((t (:inherit default))))
   `(treesit-fold-replacement-mouse-face ((t (:inherit default))))

   ;; flyspell.el
   `(flyspell-incorrect ((t (:underline (:style dashes :color ,(hsl-to-hex text-color-error))))))
   `(flyspell-duplicate ((t (:underline (:style dashes :color ,(hsl-to-hex text-color-warning))))))
   )

  (custom-theme-set-variables
   'one-light
   `(menu-bar-mode nil)
   `(tool-bar-mode nil)
   `(frame-resize-pixelwise t)
   `(org-ellipsis (nerd-icons-octicon "nf-oct-ellipsis"))
   `(treesit-fold-replacement (nerd-icons-octicon "nf-oct-ellipsis"))
   `(scroll-bar-adjust-thumb-portion nil)
   `(treesit-font-lock-level 4)))
)

;;;###autoload
(when (and (boundp 'custom-theme-load-path) load-file-name)
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'one-light)
;;; one-light-theme.el ends here
