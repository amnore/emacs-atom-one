;; one-light-port.el --- Port of One Light -*- lexical-binding: t -*-

;; Author: Chen Zhenge <me@markle.one>
;; URL: https://github.com/amnore/emacs-one-light-port
;; Version: 1
;; Package-Requires ((emacs "24"))
;;
;;; Commentary:
;;
;; Yet another port of Atom's One Light theme

;;; Code:
(require 'color)
(require 'cl-seq)
(require 'cl-macs)
(require 'subr-x)

(deftheme one-light-port
  "Yet another port of Atom's One Light theme.")

(defun one-light-port--mix (color1 color2 &optional weight)
  "Mix COLOR1 and COLOR2.
COLOR1 has a weight of WEIGHT, COLOR2 has (1 - WEIGHT).
If WEIGHT is not specified, default to 0.5."
  (let ((color1-rgb (apply 'color-hsl-to-rgb color1))
        (color2-rgb (apply 'color-hsl-to-rgb color2))
        (w (or weight 0.5)))
    (apply 'color-rgb-to-hsl
           (cl-mapcar (lambda (c1 c2) (+ (* c1 w) (* c2 (- 1 w))))
                      color1-rgb
                      color2-rgb))))

(defun one-light-port--luma (color)
  "Calculate the luma of COLOR."
  (let* ((rgb (apply 'color-hsl-to-rgb color))
         (gamma-expand (lambda (u) (if (< u 0.3928)
                                           (/ u 12.92)
                                         (expt (/ (+ u 0.055) 1.055) 2.4))))
         (rgb-linear (mapcar gamma-expand rgb)))
    (cl-reduce '+ (cl-mapcar (lambda (w u) (* w u))
                             '(0.2126 0.1752 0.0722)
                             rgb-linear))))

(defun one-light-port--color-hsv-to-hsl (color)
  "Convert hsv COLOR to hsl."
  (let* ((h (nth 0 color))
         (s (nth 1 color))
         (v (nth 2 color))
         (l (* v (- 1 (/ s 2)))))
    (list h
          l
          (if (or (= l 0) (= l 1))
              0
            (/ (- v l) (min l (- 1 l)))))))

(defun one-light-port--degree-to-ratio (degree)
  "Convert DEGREE to its ratio of a circle."
  (/ degree 360.0))

(defun one-light-port--color-hex-to-hsl (color)
  "Convert hex COLOR to hsl, each component can have 1, 2 or 4 digits."
  (let* ((digits (/ (length color) 3))
         (hex-max (- (expt 16.0 digits) 1))
         (subhex (lambda (pos) (string-to-number
                           (substring color
                                      (+ 1 (* digits pos))
                                      (+ 1 (* digits (+ 1 pos))))
                           16))))
    (apply 'color-rgb-to-hsl
           (mapcar (lambda (x) (/ x hex-max))
                   (mapcar subhex '(0 1 2))))))

(defun one-light-port--color-contrast (color dark light &optional threshold)
  "Choose between DARK and LIGHT based on the luma value of COLOR.
If the luma is less than THRESHOLD, choose LIGHT, otherwise choose DARK.
If THRESHOLD if omitted, use 0.43 by default."
  (let ((thld (or threshold 0.43)))
    (if (< (one-light-port--luma color) thld)
        light
      dark)))

(cl-flet ((to-ratio (degree) (one-light-port--degree-to-ratio degree))
          (mix (c1 c2 &optional w) (one-light-port--mix c1 c2 w))
          (darken (color ratio) (apply 'color-darken-hsl
                                       (append color (list (* ratio 100)))))
          (lighten (color ratio) (apply 'color-lighten-hsl
                                        (append color (list (* ratio 100)))))
          (luma (color) (one-light-port--luma color))
          (name-to-hsl (name) (apply 'color-rgb-to-hsl
                                     (color-name-to-rgb name)))
          (hsv-to-hsl (hsv) (one-light-port--color-hsv-to-hsl hsv))
          (hex-to-hsl (hex) (one-light-port--color-hex-to-hsl hex))
          (to-hex (hsl) (apply 'color-rgb-to-hex (apply 'color-hsl-to-rgb hsl)))
          (contrast (color dark light &optional threshold)
                    (one-light-port--color-contrast color dark light threshold))
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

         ;;; ui-variables-custom.less, ui-variables.less
         (ui-syntax-color `(,(to-ratio 220) 0.01 0.98)) ; fallback color
         (ui-hue (if-let* ((ui-s-h (hue ui-syntax-color))
                           (no-hue (= 0 (hue ui-syntax-color))))
                     (to-ratio 220)
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
         (text-color-info `(,(to-ratio 208) 1 0.54))
         (text-color-success `(,(to-ratio 132) 1 0.44))
         (text-color-warning `(,(to-ratio 37) 0.9 0.44))
         (text-color-error `(,(to-ratio 9) 0.9 0.56))
         ;; Text (Custom)
         (text-color-faded (mix text-color ui-bg 0.3))
         (text-color-added text-color-success) ; green
         (text-color-ignored text-color-subtle) ; faded
         (text-color-modified text-color-warning) ; orange
         (text-color-removed text-color-error) ; red
         (text-color-renamed text-color-info) ; blue
         ;; Background
         (background-color-info `((to-ratio 208) 1 0.56))
         (background-color-success `(,(to-ratio 132) 0.52 0.48))
         (background-color-warning `(,(to-ratio 40) 0.6 0.48))
         (background-color-error `(,(to-ratio 5) 0.72 0.56))
         (background-color-highlight (darken level-3-color 0.02))
         (background-color-selected (darken level-3-color 0.06))
         (app-background-color level-3-color)
         ;; Accent (Custom)
         (accent-luma (luma `(,ui-hue 0.5 0.5))) ; get lightness of current hue
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
         (ui-site-color-1 `(,(to-ratio 208) 1 0.56)) ; blue
         (ui-site-color-2 `(,(to-ratio 132) 0.48 0.48)) ; green
         (ui-site-color-3 `(,(to-ratio 40) 0.6 0.52)) ; orange
         (ui-site-color-4 (name-to-hsl "#D831B0")) ; pink
         (ui-site-color-5 (name-to-hsl "#EBDD5B")) ; yellow
         ;; Components (Custom)
         (badge-background-color background-color-selected)
         (button-text-color-selected accent-bg-text-color)
         (button-border-color-selected accent-color)
         (checkbox-background-color (mix accent-bg-color ui-bg 0.33))
         (input-background-color-focus `(,ui-hue 1 0.96))
         (input-selection-color (mix (hsv-to-hsl `(,ui-hue 0.33 0.95))
                                     `(,ui-hue 1 0.98)
                                     (* 2 accent-luma))) ; mix hsv + hsl
         (input-selection-color-focus (mix (hsv-to-hsl `(,ui-hue 0.44 0.9))
                                           `(,ui-hue 1 0.94)
                                           (* 2 accent-luma))) ; mix hsv + hsl
         (overlay-backdrop-color `(,ui-hue
                                   ,(* 0.4 ui-saturation)
                                   ,(* 0.8 ui-lightness)))
         (overlay-backdrop-opacity 0.66)
         (progress-background-color accent-color)
         (scrollbar-color (darken level-3-color 0.14))
         (scrollbar-background-color level-3-color)
         (scroll-bar-color-editor ui-syntax-color)
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
         (font-size 12) ; TODO: use the correct size
         (input-font-size 14)
         (disclosure-arrow-size 12)
         (component-padding 10)
         (component-icono-padding 5)
         (component-icon-size 16) ; needs to stay 16px to look sharpest
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
         (ui-tab-height (* 2.5 ui-size))

         (display '((class color) (min-colors 89))))
    (custom-theme-set-faces
     'one-light-port

     ;; faces.el
     `(default ((,display (:foreground ,(to-hex syntax-text-color)
                           :background ,(to-hex syntax-background-color)))))
                                        ; editor.less: atom-text-editor
     `(shadow ((,display (,(to-hex text-color-subtle)))))
     `(link ((,display (:inherit underline
                        :foreground ,(to-hex hue-1)))))
                                        ; base.less: .syntax--link,
                                        ; .syntax--markup.syntax--underline
     `(link-visited ((,display (:inherit link))))
     `(highlight ((,display
                   (:background ,(to-hex background-color-highlight)))))
                                        ; text.less: .highlight
     `(region ((,display (:background ,(to-hex syntax-selection-color)))))
                                        ; editor.less: .region
     `(secondary-selection ((,display (:inherit region))))
                                        ; editor.less: .region
     `(trailing-whitespace ((,display (:underline ,(to-hex accent-color)))))
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
     `(mode-line ((,display (:background ,(to-hex level-3-color)))))
                                        ; status-bar.less: .status-bar
     `(mode-line-inactive ((,display (:foreground ,(to-hex tab-text-color)))))
                                        ; tabs.less: .tab
     `(mode-line-highlight
       ((,display (:background ,(to-hex level-3-color-hover)))))
                                        ; status-bar.less:
                                        ; .status-bar.inline-block:hover
     `(mode-line-buffer-id ((,display (:inherit mode-line))))
     `(header-line ((,display (:foreground ,(to-hex text-color-subtle)
                               :background ,(to-hex base-background-color)))))
                                        ; settings-view.less: .breadcrumb
     `(header-line-highlight ((,display (:inherit header-line :underline t))))
     `(vertical-border ((,display (:inherit border))))
     `(window-divider ((,display (:inherit border))))
     `(window-divider-first-pixel ((,display (:inherit window-divider))))
     `(window-divider-last-pixel ((,display (:inherit window-divider))))
     `(internal-border ((,display (:inherit border))))
     `(child-frame-border ((,display (:inherit border))))
     `(minibuffer-prompt
       ((,display (:foreground ,(to-hex text-color-highlight)
                   :background ,(to-hex input-background-color)))))
                                        ; editor.less: atom-text-editor[mini]
     `(fringe ((,display ())))
     `(scroll-bar ((,display (:foreground ,(to-hex scrollbar-color)))))
                                        ; atom.less: ::-webkit-scrollbar-thumb
     `(border ((,display (:foreground ,(to-hex base-border-color)))))
                                        ; panes.less: atom-pane
     `(cursor ((,display (:background ,(to-hex syntax-cursor-color)))))
                                        ; editor.less: .cursor
     `(tool-bar ((,display (:background-color ,(to-hex level-3-color)))))
                                        ; packages.less: .tool-bar
     `(tab-bar ((,display (:foreground ,(to-hex tab-text-color)
                           :background ,(to-hex tab-background-color)))))
                                        ; tabs.less: .tab-bar, .tab
     `(tab-line ((,display (:inherit tab-bar)))) ; tabs.less: .tab-bar
     `(glyphless-char
       ((,display (:foreground ,(to-hex syntax-invisible-character-color)))))
                                        ; editor.less: .invisible-character
     `(error ((,display (:foreground ,(to-hex text-color-error)))))
                                        ; text.less: .text-error
     `(warning ((,display (:foreground ,(to-hex text-color-warning)))))
                                        ; text.less: .text-warning
     `(success ((,display (:foreground ,(to-hex text-color-success)))))
                                        ; text.less: .text-success
     `(show-paren-match ((,display (:underline ,(to-hex syntax-cursor-color)))))
                                        ; editor.less: .bracket-matcher.region
     `(show-paren-match-expression ((,display (:inherit show-paren-match))))
     `(show-paren-mismatch
       ((,display (:foreground ,(to-hex text-color-error)))))

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
     `(font-lock-negation-char-face ((,display (:foreground ,(to-hex hue-3)))))
                                        ; base.less: .syntax--operator

     ;; company.el
     `(company-tooltip
       ((,display (:foreground ,(to-hex text-color)
                                        ; autocomplete.less: li
                   :background ,(to-hex overlay-background-color)))))
                                        ; lists.less: .select-list.popover-list
     `(company-tooltip-selection
       ((,display (:background ,(to-hex background-color-selected)))))
                                        ; lists.less: li.selected
     `(company-tooltip-search ((,display (:inherit company-tooltip-common))))
     `(company-tooltip-search-selection
       ((,display (:inherit company-tooltip-common-selection))))
     `(company-tooltip-mouse ((,display (:inherit company-tooltip))))
     `(company-tooltip-common
       ((,display (:foreground ,(to-hex text-color-highlight)
                   :weight bold)))) ; autocomplete.less: .character-match
     `(company-tooltip-common-selection
       ((,display (:foreground ,(to-hex text-color-selected)
                                        ; autocomplete.less:
                                        ; li.selected.character-match
                   :weight bold)))) ; autocomplete.less: .character-match
     `(company-tooltip-annotation
       ((,display (:inherit company-tooltip
                   :foreground ,(to-hex text-color-subtle)))))
                                        ; autocomplete.less: .right-label
     `(company-tooltip-annotation-selection
       ((,display (:inherit company-tooltip-selection
                   :foreground ,(to-hex (mix text-color-selected
                                             overlay-backdrop-color))))))
                                        ; autocomplete.less:
                                        ; li.selected.right-label
     `(company-scrollbar-fg
       ((,display (:background ,(to-hex scrollbar-color)))))
     `(company-scrollbar-bg
       ((,display (:background ,(to-hex scrollbar-background-color)))))
     `(company-preview ((,display (:background "#ff0000")))) ; TODO: add face
     `(company-preview-common ((,display (:background "#ff0000")))) ; TODO: add face
     `(company-preview-search ((,display (:background "#ff0000")))) ; TODO: add face
     `(company-echo nil ((,display (:background "#ff0000")))) ; TODO: add face
     `(company-echo-common ((,display (:background "#ff0000")))) ; TODO: add face
     )))

;;;###autoload
(when (and (boundp 'custom-theme-load-path) load-file-name)
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'one-light-port)
(provide 'one-light-port-theme)
;;; one-light-port-theme.el ends here
