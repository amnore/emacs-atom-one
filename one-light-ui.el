;;; one-light-ui.el --- One Light UI theme -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2022 czg
;;
;; Author: czg <https://github.com/czg>
;; Maintainer: czg <me@markle.one>
;; Created: January 25, 2022
;; Modified: January 25, 2022
;; Version: 0.0.1
;; Keywords: faces one-light
;; Homepage: https://github.com/czg/one-light-ui
;; Package-Requires: ((emacs "26.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; This file is ported from One Light UI theme in Atom.
;;
;;; Code:

(require 'one-light-lib)

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
          (contrast (color &optional dark light threshold)
                    (one-light-lib--color-contrast color dark light threshold))
          (hue (hsl) (nth 0 hsl))
          (saturation (hsl) (nth 1 hsl))
          (lightness (hsl) (nth 2 hsl)))
  (let* (;;; ui-variables-custom.less, ui-variables.less
         (ui-syntax-color `(,(to-ratio 230) 0.01 0.98)) ; fallback color
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
         (text-color-success `(,(to-ratio 132) 0.6 0.44))
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
         (font-size 9) ; TODO: use the correct size
         (input-font-size 12)
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
     'one-light

     ;; faces.el
     ;; status-bar.less, tabs.less, settings-view.less, atom.less,
     ;; panes.less, packages.less
     `(mode-line ((,display (:background ,(to-hex level-3-color)))))
                                        ; .status-bar
     `(mode-line-inactive ((,display (:foreground ,(to-hex tab-text-color)))))
                                        ; .tab
     `(mode-line-highlight
       ((,display (:background ,(to-hex level-3-color-hover)))))
                                        ; .status-bar.inline-block:hover
     `(mode-line-buffer-id ((,display (:inherit mode-line))))
     `(header-line ((,display (:foreground ,(to-hex text-color-subtle)
                               :background ,(to-hex base-background-color)))))
                                        ; .breadcrumb
     `(header-line-highlight ((,display (:inherit header-line :underline t))))
     `(vertical-border ((,display (:inherit border))))
     `(window-divider ((,display (:inherit border))))
     `(window-divider-first-pixel ((,display (:inherit window-divider))))
     `(window-divider-last-pixel ((,display (:inherit window-divider))))
     `(internal-border ((,display (:inherit border))))
     `(child-frame-border ((,display (:inherit border))))
     `(fringe ((,display ())))
     `(scroll-bar ((,display (:foreground ,(to-hex scrollbar-color)))))
                                        ; ::-webkit-scrollbar-thumb
     `(border ((,display (:foreground ,(to-hex base-border-color)
                          :background ,(to-hex base-border-color)))))
                                        ; atom-pane
     `(tool-bar ((,display (:background-color ,(to-hex level-3-color)))))
                                        ; .tool-bar
     `(tab-bar ((,display (:foreground ,(to-hex tab-text-color)
                           :background ,(to-hex tab-background-color)))))
                                        ; .tab-bar, .tab
     `(tab-line ((,display (:inherit tab-bar)))) ; .tab-bar
     `(error ((,display (:foreground ,(to-hex text-color-error)))))
                                        ; text.less: .text-error
     `(warning ((,display (:foreground ,(to-hex text-color-warning)))))
                                        ; text.less: .text-warning
     `(info ((,display (:foreground ,(to-hex text-color-info)))))
                                        ; inherited by other faces
     `(success ((,display (:foreground ,(to-hex text-color-success)))))
                                        ; text.less: .text-success

     ;; flycheck.el
     `(flycheck-error
       ((,display (:underline (:style wave
                               :color ,(to-hex text-color-error))))))
     `(flycheck-warning
       ((,display (:underline (:style wave
                               :color ,(to-hex text-color-warning))))))
     `(flycheck-info
       ((,display (:underline (:style wave
                               :color ,(to-hex text-color-info))))))
     `(flycheck-fringe-error ((,display (:inherit error))))
     `(flycheck-fringe-warning ((,display (:inherit warning))))
     `(flycheck-fringe-info ((,display (:inherit info))))
     `(flycheck-error-list-error ((,display (:inherit error))))
     `(flycheck-error-list-warning ((,display (:inherit warning))))
     `(flycheck-error-list-info ((,display (:inherit info))))
     `(flycheck-error-list-filename ((,display ())))
     ;; `(flycheck-error-list-id
     ;;   ((,display (:foreground ,(to-hex text-color-subtle)))))
     `(flycheck-error-list-id-with-explainer
       ((,display (:inherit flycheck-error-list-id))))
     ;; `(flycheck-error-list-checker-name
     ;;   ((,display (:foreground ,(to-hex text-color-subtle)))))
     `(flycheck-error-list-highlight ((,display (:inherit highlight))))

     ;; flycheck-posframe.el
     ;; lists.less, modal.less
     `(flycheck-posframe-face ((,display (:foreground ,(to-hex text-color)
                                          :background ,(to-hex overlay-background-color)))))
     `(flycheck-posframe-info-face ((,display (:inherit flycheck-posframe-face
                                               :foreground ,(to-hex text-color-info)))))
     `(flycheck-posframe-warning-face ((,display (:inherit flycheck-posframe-face
                                                  :foreground ,(to-hex text-color-warning)))))
     `(flycheck-posframe-error-face ((,display (:inherit flycheck-posframe-face
                                                :foreground ,(to-hex text-color-error)))))
     `(flycheck-posframe-background-face ((,display (:background ,(to-hex overlay-background-color)))))
     `(flycheck-posframe-border-face ((,display (:foreground ,(to-hex overlay-border-color)))))

     ;; popup.el
     ;; lists.less
     `(popup-face ((,display (:foreground ,(to-hex text-color)
                              :background ,(to-hex overlay-background-color)))))
     `(popup-summary-face ((,display (:inherit popup-face
                                      :foreground ,(to-hex text-color-subtle)))))
     `(popup-scroll-bar-foreground-face
       ((,display (:background ,(to-hex scrollbar-color)))))
     `(popup-scroll-bar-background-face
       ((,display (:background ,(to-hex scrollbar-background-color)))))
     `(popup-isearch-match ((,display (:inherit isearch))))
     `(popup-tip-face ((,display (:inherit popup-face))))
     `(popup-menu-face ((,display (:inherit popup-face))))
     `(popup-menu-mouse-face ((,display nil)))
     `(popup-menu-selection-face
       ((,display (:foreground ,(to-hex text-color-selected)
                   :background ,(to-hex (mix text-color-highlight overlay-background-color))))))
     `(popup-menu-summary-face ((,display (:inherit popup-summary-face))))

     ;; company.el
     ;; autocomplete.less, lists.less
     `(company-tooltip
       ((,display (:foreground ,(to-hex text-color)
                                        ; li
                   :background ,(to-hex overlay-background-color)))))
                                        ; .select-list.popover-list
     `(company-tooltip-selection
       ((,display (:background ,(to-hex background-color-selected)))))
                                        ; li.selected
     `(company-tooltip-search ((,display (:inherit company-tooltip-common))))
     `(company-tooltip-search-selection
       ((,display (:inherit company-tooltip-common-selection))))
     `(company-tooltip-mouse ((,display (:inherit company-tooltip))))
     `(company-tooltip-common
       ((,display (:foreground ,(to-hex text-color-highlight)
                   :weight bold)))) ; .character-match
     `(company-tooltip-common-selection
       ((,display (:foreground ,(to-hex text-color-selected)
                                        ; li.selected.character-match
                   :weight bold)))) ; .character-match
     `(company-tooltip-annotation
       ((,display (:inherit company-tooltip
                   :foreground ,(to-hex text-color-subtle)))))
                                        ; .right-label
     `(company-tooltip-annotation-selection
       ((,display (:inherit company-tooltip-selection
                   :foreground ,(to-hex (mix text-color-selected
                                             overlay-backdrop-color))))))
                                        ; li.selected.right-label
     `(company-tooltip-scrollbar-thumb
       ((,display (:background ,(to-hex scrollbar-color)))))
     `(company-tooltip-scrollbar-track
       ((,display (:background ,(to-hex scrollbar-background-color)))))
     `(company-preview ((,display (:inherit shadow))))
     `(company-preview-common ((,display (:inherit (shadow bold)))))
     `(company-preview-search ((,display (:background "#ff0000")))) ; TODO: add face
     `(company-echo nil ((,display (:background "#ff0000")))) ; TODO: add face
     `(company-echo-common ((,display (:background "#ff0000")))) ; TODO: add face

     ;; company-box.el
     `(company-box-candidate ((,display :inherit company-tooltip)))
     `(company-box-scrollbar ((,display :background ,(to-hex scrollbar-color))))

     ;; company-posframe.el
     `(company-posframe-quickhelp ((,display :inherit company-tooltip)))
     `(company-posframe-quickhelp-header
       ((,display :inherit header-line
                  :background ,(to-hex overlay-background-color))))

     ;; doom-modeline-core.el
     `(doom-modeline-buffer-modified
       ((,display (:foreground ,(to-hex text-color-modified)))))
     `(doom-modeline-info ((,display (:foreground ,(to-hex background-color-success)))))
     `(doom-modeline-bar
       ((,display (:background ,(to-hex ui-site-color-2)))))
     `(doom-modeline-bar-inactive
       ((,display (:background ,(to-hex tab-background-color)))))
     `(doom-modeline-evil-emacs-state
       ((,display (:foreground ,(face-foreground 'mode-line-inactive)))))
     `(doom-modeline-evil-insert-state
       ((,display (:foreground ,(to-hex ui-site-color-1)))))
     `(doom-modeline-evil-motion-state
       ((,display (:foreground ,(to-hex ui-site-color-3)))))
     `(doom-modeline-evil-normal-state
       ((,display (:foreground ,(to-hex ui-site-color-2)))))
     `(doom-modeline-evil-operator-state
       ((,display (:foreground ,(to-hex ui-site-color-3)))))
     `(doom-modeline-evil-visual-state
       ((,display (:foreground ,(to-hex ui-site-color-4)))))
     `(doom-modeline-lsp-running
       ((,display (:inherit info :weight normal))))

     ;; treemacs-faces.el
     ;; tree-view.less, lists.less
     `(treemacs-directory-face ((,display (:inherit (variable-pitch default)))))
     `(treemacs-file-face ((,display (:inherit treemacs-directory-face))))
     `(treemacs-root-face ((,display (:inherit treemacs-directory-face))))
     `(treemacs-git-modified-face
       ((,display (:inherit treemacs-file-face
                   :foreground ,(to-hex text-color-modified)))))
    `(treemacs-git-renamed-face
      ((,display (:inherit treemacs-file-face
                  :foreground ,(to-hex text-color-renamed)))))
    `(treemacs-git-ignored-face
      ((,display (:inherit treemacs-file-face
                  :foreground ,(to-hex text-color-ignored)))))
    `(treemacs-git-untracked-face
      ((,display (:inherit treemacs-git-added-face))))
    `(treemacs-git-added-face
      ((,display (:inherit treemacs-file-face
                  :foreground ,(to-hex text-color-added))))))

    (add-hook
     'treemacs-mode-hook
     (lambda ()
       (setq-local
        line-spacing 5
        mode-line-format nil

        face-remapping-alist
        `((default
            :foreground ,(to-hex text-color)
            :background ,(to-hex tree-view-background-color)
            :height ,(* 10 font-size))
          (hl-line
           :foreground ,(to-hex (contrast button-background-color-selected))
           :background ,(to-hex button-background-color-selected)
           :extend t)
          (fringe :background ,(to-hex tree-view-background-color))))

       (treemacs-fringe-indicator-mode -1)

       (let* ((bg-focused (to-hex button-background-color-selected))
              (bg-unfocused (to-hex background-color-selected))
              (fg-focused (to-hex (contrast button-background-color-selected)))
              (treemacs-hl-line (lambda (window)
                                  (let ((inhibit-redisplay t)
                                        (focused (treemacs-is-treemacs-window-selected?)))
                                    (with-selected-window window
                                      (setcdr (assq 'hl-line face-remapping-alist)
                                              (if focused
                                                  (list :foreground fg-focused
                                                        :background bg-focused
                                                        :extend t)
                                                (list :background bg-unfocused
                                                      :extend t))))))))
         (add-hook 'window-selection-change-functions treemacs-hl-line 0 t))))

    (custom-theme-set-variables
     'one-light
     `(flycheck-posframe-border-width 1))

    (with-eval-after-load 'company-posframe
      (setq company-posframe-show-params '(:border-width 1)
            company-posframe-quickhelp-show-params
            '(:border-width 1
              :poshandler company-posframe-quickhelp-right-poshandler
              :timeout 60
              :no-properties nil)))
    

    (with-eval-after-load 'company-box
      (add-to-list 'company-box-frame-parameters '(child-frame-border-width . 1)))))

(provide 'one-light-ui)
;;; one-light-ui.el ends here
