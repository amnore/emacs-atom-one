;;; one-light-ui.el --- One Light UI theme -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'one-light-lib)
(eval-when-compile (require 'names))

(defface info nil
  "Info face, inherited by other faces.")

(define-namespace one-light-- :global t

(let* (;;; ui-variables-custom.less, ui-variables.less
       (ui-syntax-color `(,(degree-to-ratio 230) 0.01 0.98)) ; fallback color
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
       (text-color-added text-color-success) ; green
       (text-color-ignored text-color-subtle) ; faded
       (text-color-modified text-color-warning) ; orange
       (text-color-removed text-color-error) ; red
       (text-color-renamed text-color-info) ; blue
       ;; Background
       (background-color-info `((degree-to-ratio 208) 1 0.56))
       (background-color-success `(,(degree-to-ratio 132) 0.52 0.48))
       (background-color-warning `(,(degree-to-ratio 40) 0.6 0.48))
       (background-color-error `(,(degree-to-ratio 5) 0.72 0.56))
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
       (ui-site-color-1 `(,(degree-to-ratio 208) 1 0.56)) ; blue
       (ui-site-color-2 `(,(degree-to-ratio 132) 0.48 0.48)) ; green
       (ui-site-color-3 `(,(degree-to-ratio 40) 0.6 0.52)) ; orange
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

       ;; Require true color
       (tc '((class color) (min-colors 16777216))))

  (custom-theme-set-faces
   'one-light

   ;; faces.el
   ;; status-bar.less, tabs.less, settings-view.less, atom.less,
   ;; panes.less, packages.less
   `(mode-line ((,tc (:background ,(hsl-to-hex level-3-color)))))
                                        ; .status-bar
   `(mode-line-inactive ((,tc (:foreground ,(hsl-to-hex tab-text-color)))))
                                        ; .tab
   `(mode-line-highlight
     ((,tc (:background ,(hsl-to-hex level-3-color-hover)))))
                                        ; .status-bar.inline-block:hover
   `(mode-line-buffer-id ((,tc (:inherit mode-line))))
   `(header-line ((,tc (:foreground ,(hsl-to-hex text-color-subtle)
                        :background ,(hsl-to-hex base-background-color)))))
                                        ; .breadcrumb
   `(header-line-highlight ((,tc (:inherit header-line :underline t))))
   `(vertical-border ((,tc (:inherit border))))
   `(window-divider ((,tc (:inherit border))))
   `(window-divider-first-pixel ((,tc (:inherit window-divider))))
   `(window-divider-last-pixel ((,tc (:inherit window-divider))))
   `(internal-border ((,tc (:inherit border))))
   `(child-frame-border ((,tc (:inherit border))))
   `(fringe ((,tc ())))
   `(scroll-bar ((,tc (:foreground ,(hsl-to-hex scrollbar-color)))))
                                        ; ::-webkit-scrollbar-thumb
   `(border ((,tc (:foreground ,(hsl-to-hex base-border-color)
                   :background ,(hsl-to-hex base-border-color)))))
                                        ; atom-pane
   `(tool-bar ((,tc (:background-color ,(hsl-to-hex level-3-color)))))
                                        ; .tool-bar
   `(tab-bar ((,tc (:foreground ,(hsl-to-hex tab-text-color)
                    :background ,(hsl-to-hex tab-background-color)))))
                                        ; .tab-bar, .tab
   `(tab-line ((,tc (:inherit tab-bar)))) ; .tab-bar
   `(error ((,tc (:foreground ,(hsl-to-hex text-color-error)))))
                                        ; text.less: .text-error
   `(warning ((,tc (:foreground ,(hsl-to-hex text-color-warning)))))
                                        ; text.less: .text-warning
   `(info ((,tc (:foreground ,(hsl-to-hex text-color-info)))))
                                        ; inherited by other faces
   `(success ((,tc (:foreground ,(hsl-to-hex text-color-success)))))
                                        ; text.less: .text-success

   ;; flycheck.el
   `(flycheck-error
     ((,tc (:underline (:style wave
                        :color ,(hsl-to-hex text-color-error))))))
   `(flycheck-warning
     ((,tc (:underline (:style wave
                        :color ,(hsl-to-hex text-color-warning))))))
   `(flycheck-info
     ((,tc (:underline (:style wave
                        :color ,(hsl-to-hex text-color-info))))))
   `(flycheck-fringe-error ((,tc (:inherit error))))
   `(flycheck-fringe-warning ((,tc (:inherit warning))))
   `(flycheck-fringe-info ((,tc (:inherit info))))
   `(flycheck-error-list-error ((,tc (:inherit error))))
   `(flycheck-error-list-warning ((,tc (:inherit warning))))
   `(flycheck-error-list-info ((,tc (:inherit info))))
   `(flycheck-error-list-filename ((,tc ())))
   ;; `(flycheck-error-list-id
   ;;   ((,tc (:foreground ,(hsl-to-hex text-color-subtle)))))
   `(flycheck-error-list-id-with-explainer
     ((,tc (:inherit flycheck-error-list-id))))
   ;; `(flycheck-error-list-checker-name
   ;;   ((,tc (:foreground ,(hsl-to-hex text-color-subtle)))))
   `(flycheck-error-list-highlight ((,tc (:inherit highlight))))

   ;; flycheck-posframe.el
   ;; lists.less, modal.less
   `(flycheck-posframe-face
     ((,tc (:foreground ,(hsl-to-hex text-color)
            :background ,(hsl-to-hex overlay-background-color)))))
   `(flycheck-posframe-info-face
     ((,tc (:inherit (info flycheck-posframe-face)))))
   `(flycheck-posframe-warning-face
     ((,tc (:inherit (warning flycheck-posframe-face)))))
   `(flycheck-posframe-error-face
     ((,tc (:inherit (error flycheck-posframe-face)))))
   `(flycheck-posframe-background-face
     ((,tc (:background ,(hsl-to-hex overlay-background-color)))))
   `(flycheck-posframe-border-face
     ((,tc (:foreground ,(hsl-to-hex overlay-border-color)))))

   ;; popup.el
   ;; lists.less
   `(popup-face ((,tc (:foreground ,(hsl-to-hex text-color)
                       :background ,(hsl-to-hex overlay-background-color)))))
   `(popup-summary-face ((,tc (:inherit popup-face
                               :foreground ,(hsl-to-hex text-color-subtle)))))
   `(popup-scroll-bar-foreground-face
     ((,tc (:background ,(hsl-to-hex scrollbar-color)))))
   `(popup-scroll-bar-background-face
     ((,tc (:background ,(hsl-to-hex scrollbar-background-color)))))
   `(popup-isearch-match ((,tc (:inherit isearch))))
   `(popup-tip-face ((,tc (:inherit popup-face))))
   `(popup-menu-face ((,tc (:inherit popup-face))))
   `(popup-menu-mouse-face ((,tc nil)))
   `(popup-menu-selection-face
     ((,tc (:foreground ,(hsl-to-hex text-color-selected)
            :background ,(hsl-to-hex (mix text-color-highlight
                                          overlay-background-color))))))
   `(popup-menu-summary-face ((,tc (:inherit popup-summary-face))))

   ;; company.el
   ;; autocomplete.less, lists.less
   `(company-tooltip
     ((,tc (:foreground ,(hsl-to-hex text-color)
            :background ,(hsl-to-hex overlay-background-color)))))
                                        ; li, .select-list.popover-list
   `(company-tooltip-selection
     ((,tc (:background ,(hsl-to-hex background-color-selected)))))
                                        ; li.selected
   `(company-tooltip-search ((,tc (:inherit company-tooltip-common))))
   `(company-tooltip-search-selection
     ((,tc (:inherit company-tooltip-common-selection))))
   `(company-tooltip-mouse ((,tc (:inherit company-tooltip))))
   `(company-tooltip-common
     ((,tc (:foreground ,(hsl-to-hex text-color-highlight)
            :weight bold)))) ; .character-match
   `(company-tooltip-common-selection
     ((,tc (:foreground ,(hsl-to-hex text-color-selected)
                                        ; li.selected.character-match
            :weight bold))))
                                        ; .character-match
   `(company-tooltip-annotation
     ((,tc (:inherit company-tooltip
            :foreground ,(hsl-to-hex text-color-subtle)))))
                                        ; .right-label
   `(company-tooltip-annotation-selection
     ((,tc (:inherit company-tooltip-selection
            :foreground ,(hsl-to-hex (mix text-color-selected
                                          overlay-backdrop-color))))))
                                        ; li.selected.right-label
   `(company-tooltip-scrollbar-thumb
     ((,tc (:background ,(hsl-to-hex scrollbar-color)))))
   `(company-tooltip-scrollbar-track
     ((,tc (:background ,(hsl-to-hex scrollbar-background-color)))))
   `(company-preview ((,tc (:inherit shadow))))
   `(company-preview-common ((,tc (:inherit (shadow bold)))))
   `(company-preview-search ((,tc (:background "#ff0000")))) ; TODO: add face
   `(company-echo nil ((,tc (:background "#ff0000")))) ; TODO: add face
   `(company-echo-common ((,tc (:background "#ff0000")))) ; TODO: add face

   ;; company-box.el
   `(company-box-candidate ((,tc :inherit company-tooltip)))
   `(company-box-scrollbar ((,tc :background ,(hsl-to-hex scrollbar-color))))

   ;; company-posframe.el
   `(company-posframe-quickhelp ((,tc :inherit company-tooltip)))
   `(company-posframe-quickhelp-header
     ((,tc :inherit header-line
           :background ,(hsl-to-hex overlay-background-color))))

   ;; doom-modeline-core.el
   `(doom-modeline-buffer-modified
     ((,tc (:foreground ,(hsl-to-hex text-color-modified)))))
   `(doom-modeline-info ((,tc (:foreground ,(hsl-to-hex background-color-success)))))
   `(doom-modeline-bar
     ((,tc (:background ,(hsl-to-hex ui-site-color-2)))))
   `(doom-modeline-bar-inactive
     ((,tc (:background ,(hsl-to-hex tab-background-color)))))
   `(doom-modeline-evil-emacs-state
     ((,tc (:foreground ,(face-foreground 'mode-line-inactive)))))
   `(doom-modeline-evil-insert-state
     ((,tc (:foreground ,(hsl-to-hex ui-site-color-1)))))
   `(doom-modeline-evil-motion-state
     ((,tc (:foreground ,(hsl-to-hex ui-site-color-3)))))
   `(doom-modeline-evil-normal-state
     ((,tc (:foreground ,(hsl-to-hex ui-site-color-2)))))
   `(doom-modeline-evil-operator-state
     ((,tc (:foreground ,(hsl-to-hex ui-site-color-3)))))
   `(doom-modeline-evil-visual-state
     ((,tc (:foreground ,(hsl-to-hex ui-site-color-4)))))
   `(doom-modeline-lsp-running
     ((,tc (:inherit info :weight normal))))

   ;; treemacs-faces.el
   ;; tree-view.less, lists.less
   `(treemacs-directory-face ((,tc (:inherit (variable-pitch default)))))
   `(treemacs-file-face ((,tc (:inherit treemacs-directory-face))))
   `(treemacs-root-face ((,tc (:inherit treemacs-directory-face))))
   `(treemacs-git-modified-face
     ((,tc (:inherit treemacs-file-face
            :foreground ,(hsl-to-hex text-color-modified)))))
   `(treemacs-git-renamed-face
     ((,tc (:inherit treemacs-file-face
            :foreground ,(hsl-to-hex text-color-renamed)))))
   `(treemacs-git-ignored-face
     ((,tc (:inherit treemacs-file-face
            :foreground ,(hsl-to-hex text-color-ignored)))))
   `(treemacs-git-untracked-face
     ((,tc (:inherit treemacs-git-added-face))))
   `(treemacs-git-added-face
     ((,tc (:inherit treemacs-file-face
            :foreground ,(hsl-to-hex text-color-added))))))

  (custom-theme-set-variables
   'one-light
   `(flycheck-posframe-border-width 1)
   `(hl-line-sticky-flag nil))

  (defun treemacs-change-hl-line (window)
    (let ((focused (treemacs-is-treemacs-window-selected?)))
      (with-selected-window window
        (setcdr (assq 'hl-line face-remapping-alist)
                (if focused
                    (list :foreground
                          (hsl-to-hex (contrast button-background-color-selected))
                          :background
                          (hsl-to-hex button-background-color-selected)
                          :extend t)
                  (list :background (hsl-to-hex background-color-selected)
                        :extend t))))))

  (defun treemacs-hide-fringe (&rest _)
    (set-window-fringes nil 0 0))

  (defun treemacs-setup ()
    (hl-line-mode)
    (add-hook 'window-selection-change-functions #'treemacs-change-hl-line 0 t)

    (treemacs-fringe-indicator-mode -1)
    (treemacs-hide-fringe)
    (advice-add #'treemacs-select-window :after #'treemacs-hide-fringe)

    (setq-local
     line-spacing 5
     mode-line-format nil

     face-remapping-alist
     `((default
         :foreground ,(hsl-to-hex text-color)
         :background ,(hsl-to-hex tree-view-background-color)
         :height ,(* 10 font-size))
       (hl-line
        :foreground ,(hsl-to-hex (contrast button-background-color-selected))
        :background ,(hsl-to-hex button-background-color-selected)
        :extend t))))

  (with-eval-after-load 'treemacs
    (add-hook 'treemacs-mode-hook #'treemacs-setup))

  (with-eval-after-load 'company-posframe
    (setq company-posframe-show-params
          (plist-put company-posframe-show-params
                     :border-width 1))
    (cl-remf company-posframe-quickhelp-show-params
             :border-color))

  (with-eval-after-load 'company-box
    (add-to-list 'company-box-frame-parameters '(child-frame-border-width . 1))))

)

(provide 'one-light-ui)
;;; one-light-ui.el ends here
