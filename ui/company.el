;;; company.el --- Colors for company -*- lexical-binding: t; -*-
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Source: https://github.com/pulsar-edit/pulsar/blob/master/packages/autocomplete-plus/styles/autocomplete.less
;;  Source: https://github.com/pulsar-edit/pulsar/blob/master/packages/one-light-ui/styles/lists.less
;;  Source: https://github.com/pulsar-edit/pulsar/blob/master/packages/autocomplete-plus/styles/autocomplete.less
;;  Source: https://github.com/pulsar-edit/pulsar/blob/master/packages/autocomplete-plus/lib/suggestion-list-element.js
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

(with-eval-after-load 'company-posframe
  (setq company-posframe-show-params
        (plist-put company-posframe-show-params
                   :border-width 1))
  (cl-remf company-posframe-quickhelp-show-params
           :border-color))

(with-eval-after-load 'company-box
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

  (defun company-box--make-line-change-display-properties (line)
    (let ((len (length line)))
      (remove-text-properties 0 len '(mouse-face nil) line)
      (add-text-properties 0 len '(pointer arrow) line)
      line))

  (defun company-box-define-icon (type &optional icon color)
    (if (not icon)
        "  "
      (let* ((char-width (float (string-pixel-width "0")))
             (icon-width (/ (string-pixel-width icon) char-width))
             (fill-width (/ (- 2 icon-width) 2))
             (string (if (<= fill-width 0)
                         icon
                       (concat " " icon " ")))
             (fg-color (if (< -0.2 (- (hsvvalue color) (hsvvalue overlay-background-color)) 0.2)
                           (contrast overlay-background-color (darken color 0.3) (lighten color 0.3))
                         color))
             (bg-color (mix fg-color overlay-background-color 0.2)))
        (cons type (propertize string
                               'display `(space-width ,fill-width)
                               'face `(:foreground ,(hsl-to-hex fg-color)
                                       :background ,(hsl-to-hex bg-color)
                                       :weight bold))))))

  (defvar company-box-atom-icons
    (let ((icon-package (nerd-icons-octicon "nf-oct-package"))
          (icon-move-to-right (nerd-icons-octicon "nf-oct-move_to_end")))
      (mapcar (lambda (args) (apply #'one-light--company-box-define-icon args))
              `((Unknown)
                (Text)
                (Method "m" ,syntax-color-method)
                (Function "f" ,syntax-color-function)
                (Constructor "f" ,syntax-color-function)
                (Field "p" ,syntax-color-property)
                (Variable "v" ,syntax-color-variable)
                (Class "c" ,syntax-color-class)
                (Interface "c" ,syntax-color-class)
                (Module ,icon-package ,syntax-color-import)
                (Property "p" ,syntax-color-property)
                (Unit "v" ,syntax-color-value)
                (Value "v" ,syntax-color-value)
                (Enum "c" ,syntax-color-class)
                (Keyword "k" ,syntax-color-keyword)
                (Snippet ,icon-move-to-right ,syntax-color-snippet)
                (Color "v" ,syntax-color-value)
                (File)
                (Reference "v" ,syntax-color-variable)
                (Folder)
                (EnumMember "c" ,syntax-color-constant)
                (Constant "c" ,syntax-color-constant)
                (Struct "c" ,syntax-color-class)
                (Event "v" ,syntax-color-variable)
                (Operator "k" ,syntax-color-keyword)
                (TypeParameter "c" ,syntax-color-class)
                (Template ,icon-move-to-right ,syntax-color-snippet)
                (ElispFunction "f" ,syntax-color-function)
                (ElispVariable "v" ,syntax-color-variable)
                (ElispFeature ,icon-package ,syntax-color-import)
                (ElispFace "v" ,syntax-color-variable)))))

  (setq company-box-icons-alist 'one-light--company-box-atom-icons)
  (advice-add #'company-box--make-candidate :filter-return #'one-light--company-box--make-candidate-pixel-align)
  (advice-add #'company-box--make-line :filter-return #'one-light--company-box--make-line-change-display-properties)
  (add-to-list 'company-box-frame-parameters '(child-frame-border-width . 1)))
)

;;; company.el ends here
