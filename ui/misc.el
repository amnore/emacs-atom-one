;;; misc.el --- Colors for misc. packages -*- lexical-binding: t; -*-
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Colors for misc. packages
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
 )

)

;;; misc.el ends here
