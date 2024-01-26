;;; flycheck.el --- Colors for flycheck -*- lexical-binding: t; -*-
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Source: https://github.com/pulsar-edit/pulsar/blob/master/packages/one-light-ui/styles/lists.less
;;  Source: https://github.com/pulsar-edit/pulsar/blob/master/packages/one-light-ui/styles/modal.less
;;
;;; Code:

(define-namespace one-light-- :global t

(custom-theme-set-faces
 'one-light

 ;; flycheck.el
 `(flycheck-error
   ((t (:underline (:style wave
                      :color ,(hsl-to-hex text-color-error))))))
 `(flycheck-warning
   ((t (:underline (:style wave
                      :color ,(hsl-to-hex text-color-warning))))))
 `(flycheck-info
   ((t (:underline (:style wave
                      :color ,(hsl-to-hex text-color-info))))))
 `(flycheck-fringe-error ((t (:inherit error))))
 `(flycheck-fringe-warning ((t (:inherit warning))))
 `(flycheck-fringe-info ((t (:inherit info))))
 `(flycheck-error-list-error ((t (:inherit error))))
 `(flycheck-error-list-warning ((t (:inherit warning))))
 `(flycheck-error-list-info ((t (:inherit info))))
 `(flycheck-error-list-filename ((t ())))
 ;; `(flycheck-error-list-id ((t (:foreground ,(hsl-to-hex text-color-subtle)))))
 `(flycheck-error-list-id-with-explainer ((t (:inherit flycheck-error-list-id))))
 ;; `(flycheck-error-list-checker-name ((t (:foreground ,(hsl-to-hex text-color-subtle)))))
 `(flycheck-error-list-highlight ((t (:inherit highlight))))

 ;; flycheck-posframe.el
 `(flycheck-posframe-face
   ((t (:foreground ,(hsl-to-hex text-color)
          :background ,(hsl-to-hex overlay-background-color)))))
 `(flycheck-posframe-info-face ((t (:inherit (info flycheck-posframe-face)))))
 `(flycheck-posframe-warning-face ((t (:inherit (warning flycheck-posframe-face)))))
 `(flycheck-posframe-error-face ((t (:inherit (error flycheck-posframe-face)))))
 `(flycheck-posframe-background-face ((t (:background ,(hsl-to-hex overlay-background-color)))))
 `(flycheck-posframe-border-face ((t (:foreground ,(hsl-to-hex overlay-border-color)))))
 )

(custom-theme-set-variables
 'one-light
 `(flycheck-posframe-border-width 1))
)

;;; flycheck.el ends here
