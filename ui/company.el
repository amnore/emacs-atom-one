;;; company.el --- Colors for company -*- lexical-binding: t; -*-
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Source: https://github.com/pulsar-edit/pulsar/blob/master/packages/autocomplete-plus/styles/autocomplete.less
;;  Source: https://github.com/pulsar-edit/pulsar/blob/master/packages/one-light-ui/styles/lists.less
;;
;;; Code:

(define-namespace one-light-- :global t

(custom-theme-set-faces
 'one-light

 ;; company.el
 `(company-tooltip
   ((t (:foreground ,(hsl-to-hex text-color)
        :background ,(hsl-to-hex overlay-background-color)))))
 `(company-tooltip-selection ((t (:background ,(hsl-to-hex background-color-selected)))))
 `(company-tooltip-search ((t (:inherit company-tooltip-common))))
 `(company-tooltip-search-selection ((t (:inherit company-tooltip-common-selection))))
 `(company-tooltip-mouse ((t (:inherit company-tooltip))))
 `(company-tooltip-common
   ((t (:foreground ,(hsl-to-hex text-color-highlight)
        :weight bold))))
 `(company-tooltip-common-selection
   ((t (:foreground ,(hsl-to-hex text-color-selected)
        :weight bold))))
 `(company-tooltip-annotation
   ((t (:inherit company-tooltip
        :height 0.9
        :foreground ,(hsl-to-hex text-color-subtle)))))
 `(company-tooltip-annotation-selection
   ((t (:inherit company-tooltip-selection
        :height 0.9
        :foreground ,(hsl-to-hex (mix text-color-selected
                                      overlay-backdrop-color))))))
 `(company-tooltip-scrollbar-thumb ((t (:background ,(hsl-to-hex scrollbar-color)))))
 `(company-tooltip-scrollbar-track ((t (:background ,(hsl-to-hex scrollbar-background-color)))))
 `(company-preview ((t (:inherit shadow))))
 `(company-preview-common ((t (:inherit (shadow bold)))))
 `(company-preview-search ((t (:background "#ff0000")))) ; TODO: add face
 `(company-echo ((t (:background "#ff0000")))) ; TODO: add face
 `(company-echo-common ((t (:background "#ff0000")))) ; TODO: add face

 ;; company-box.el
 `(company-box-candidate ((t :inherit company-tooltip)))
 `(company-box-scrollbar ((t :background ,(hsl-to-hex scrollbar-color))))

 ;; company-posframe.el
 `(company-posframe-quickhelp ((t :inherit company-tooltip)))
 `(company-posframe-quickhelp-header
   ((t :inherit header-line
       :background ,(hsl-to-hex overlay-background-color))))
 )

(custom-theme-set-variables
 'one-light
 `(company-tooltip-minimum-width 50)
 `(company-tooltip-maximum-width 50))

(defun company-box--make-candidate-pixel-align (candidate)
  (cl-destructuring-bind (a annotation b _ c) candidate
    (list a
          annotation
          b
          (if annotation
              (/ (string-pixel-width (propertize annotation 'face 'company-tooltip-annotation))
                 (float (frame-char-width)))
            0)
          c)))

(with-eval-after-load 'company-posframe
  (setq company-posframe-show-params
        (plist-put company-posframe-show-params
                   :border-width 1))
  (cl-remf company-posframe-quickhelp-show-params
           :border-color))

(with-eval-after-load 'company-box
  (advice-add #'company-box--make-candidate :filter-return #'one-light--company-box--make-candidate-pixel-align)
  (add-to-list 'company-box-frame-parameters '(child-frame-border-width . 1)))
)

;;; company.el ends here
