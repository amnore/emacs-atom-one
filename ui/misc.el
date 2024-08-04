;;; misc.el --- Colors for misc. packages -*- lexical-binding: t; -*-
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Colors for misc. packages
;;  Source: https://github.com/pulsar-edit/pulsar/blob/master/packages/one-light-ui/styles/tabs.less
;;
;;; Code:

(define-namespace one-light-- :global t

(custom-theme-set-faces
 'one-light

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

 ;; faces.el
 `(tab-line ((t (:background ,(hsl-to-hex tab-bar-background-color)))))
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
 )
)

;;; misc.el ends here
