;;; colors.el --- Color definitions -*- lexical-binding: t; -*-
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Source: https://github.com/pulsar-edit/pulsar/blob/master/packages/one-light-ui/styles/ui-variables-custom.less
;;  Source: https://github.com/pulsar-edit/pulsar/blob/master/packages/one-light-ui/styles/ui-variables.less
;;
;;; Code:

(define-namespace one-light-- :global t

(setq
 ui-syntax-color `(,(degree-to-ratio 230) 0.01 0.98)
 ui-hue (if-let* ((ui-s-h (hue ui-syntax-color))
                  (no-hue (= 0 (hue ui-syntax-color))))
            (degree-to-ratio 220)
          (hue ui-syntax-color))
 ui-saturation (min (saturation ui-syntax-color) 0.24)
 ui-lightness (max (lightness ui-syntax-color) 0.92)
 --ui-bg `(,ui-hue ,ui-saturation ,ui-lightness)
 --base-background-color --ui-bg
 --level-3-color (darken --base-background-color 0.06)
 ;; Main colors
 ui-fg `(,ui-hue ,ui-saturation ,(- ui-lightness 0.72))
 ui-bg --ui-bg
 ui-border (darken --level-3-color 0.06)
 ;; Background (Custom)
 level-1-color (lighten --base-background-color 0.04)
 level-2-color --base-background-color
 level-3-color --level-3-color
 level-3-color-hover (darken level-3-color 0.06)
 level-3-color-active (darken level-3-color 0.03)
 ;; Base
 base-background-color --base-background-color
 base-border-color ui-border
 ;; Text
 text-color ui-fg
 text-color-subtle (lighten text-color 0.3)
 text-color-highlight (darken text-color 0.12)
 text-color-selected (darken text-color-highlight 0.12)
 text-color-info `(,(degree-to-ratio 208) 1 0.54)
 text-color-success `(,(degree-to-ratio 132) 0.6 0.44)
 text-color-warning `(,(degree-to-ratio 37) 0.9 0.44)
 text-color-error `(,(degree-to-ratio 9) 0.9 0.56)
 ;; Text (Custom)
 text-color-faded (mix text-color ui-bg 0.3)
 text-color-added text-color-success
 text-color-ignored text-color-subtle
 text-color-modified text-color-warning
 text-color-removed text-color-error
 text-color-renamed text-color-info
 ;; Background
 background-color-info `((degree-to-ratio 208) 1 0.56)
 background-color-success `(,(degree-to-ratio 132) 0.52 0.48)
 background-color-warning `(,(degree-to-ratio 40) 0.6 0.48)
 background-color-error `(,(degree-to-ratio 5) 0.72 0.56)
 background-color-highlight (darken level-3-color 0.02)
 background-color-selected (darken level-3-color 0.06)
 app-background-color level-3-color
 ;; Accent (Custom)
 accent-luma (luma `(,ui-hue 0.5 0.5))
 ;; used for marker, inputs (smaller things)
 ;; mix hsv + hsl (favor hsl for dark, hsv for light colors)
 accent-color (mix (hsv-to-hsl `(,ui-hue 0.6 0.6))
                   `(,ui-hue 1 0.68)
                   (* 2 accent-luma))
 accent-text-color (contrast accent-color
                             `(,ui-hue 1 0.16)
                             (hex-to-hsl "#fff")
                             0.4)
 ;; used for button, tooltip (larger things)
 accent-bg-color (mix (hsv-to-hsl `(,ui-hue 0.4 0.72))
                      `(,ui-hue 1 0.66)
                      (* 2 accent-luma))
 accent-bg-text-color (contrast accent-bg-color
                                `(,ui-hue 1 0.1)
                                (hex-to-hsl "#fff")
                                0.4)
 ;; used for text only
 accent-only-text-color (mix (hsv-to-hsl `(,ui-hue 0.7 0.5))
                             `(,ui-hue 1 0.6)
                             (* 2 accent-luma))
 ;; Components
 pane-item-background-color base-background-color
 pane-item-border-color base-border-color
 input-background-color level-1-color
 input-border-color base-border-color
 tool-panel-background-color level-3-color
 tool-panel-border-color base-border-color
 inset-panel-background-color (lighten level-2-color 0.04)
 inset-panel-border-color (mix base-border-color ui-bg 0.15)
 panel-heading-background-color level-2-color
 panel-heading-border-color base-border-color
 overlay-background-color (mix level-2-color level-3-color)
 overlay-border-color base-border-color
 button-background-color level-1-color
 button-background-color-hover (darken button-background-color 0.04)
 button-background-color-selected accent-bg-color
 button-border-color base-border-color
 tab-bar-background-color level-3-color
 tab-bar-border-color base-border-color
 tab-background-color level-3-color
 tab-background-color-active level-2-color
 tab-border-color base-border-color
 tree-view-background-color level-3-color
 tree-view-border-color base-border-color
 ui-site-color-1 `(,(degree-to-ratio 208) 1 0.56)
 ui-site-color-2 `(,(degree-to-ratio 132) 0.48 0.48)
 ui-site-color-3 `(,(degree-to-ratio 40) 0.6 0.52)
 ui-site-color-4 (name-to-hsl "#D831B0")
 ui-site-color-5 (name-to-hsl "#EBDD5B")
 ;; Components (Custom)
 badge-background-color background-color-selected
 button-text-color-selected accent-bg-text-color
 button-border-color-selected accent-color
 checkbox-background-color (mix accent-bg-color ui-bg 0.33)
 input-background-color-focus `(,ui-hue 1 0.96)
 input-selection-color (mix (hsv-to-hsl `(,ui-hue 0.33 0.95))
                            `(,ui-hue 1 0.98)
                            (* 2 accent-luma))
 input-selection-color-focus (mix (hsv-to-hsl `(,ui-hue 0.44 0.9))
                                  `(,ui-hue 1 0.94)
                                  (* 2 accent-luma))
 overlay-backdrop-color `(,ui-hue
                          ,(* 0.4 ui-saturation)
                          ,(* 0.8 ui-lightness))
 overlay-backdrop-opacity 0.66
 progress-background-color accent-color
 scrollbar-color (darken level-3-color 0.14)
 scrollbar-background-color level-3-color
 scrollbar-color-editor (contrast ui-syntax-color
                                  (darken ui-syntax-color 0.14)
                                  (lighten ui-syntax-color 0.09))
 scrollbar-background-color-editor ui-syntax-color
 tab-text-color text-color-subtle
 tab-text-color-active text-color-highlight
 tab-text-color-editor (contrast ui-syntax-color
                                 (lighten ui-syntax-color 0.7)
                                 text-color-highlight)
 tab-inactive-status-added (mix text-color-success ui-bg 0.77)
 tab-inactive-status-modified (mix text-color-warning ui-bg 0.77)
 tooltip-background-color accent-bg-color
 tooltip-text-color accent-bg-text-color
 tooltip-text-key-color tooltip-background-color
 tooltip-background-key-color tooltip-text-color
 ;; Sizes
 font-size 9
 input-font-size 12
 disclosure-arrow-size 12
 component-padding 10
 component-icono-padding 5
 component-icon-size 16
 component-line-height 25
 component-border-radius 3
 tab-height 30
 ;; Sizes (Custom)
 ui-size 1
 ui-input-size (* 1.15 ui-size)
 ui-padding (* 1.5 ui-size)
 ui-padding-pane (* 0.5 ui-size)
 ui-padding-icon (/ ui-padding 3.3)
 ui-line-height (* 2 ui-size)
 ui-tab-height (* 2.5 ui-size)
 )

)

;;; colors.el ends here
