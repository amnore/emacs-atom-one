;;; base.el --- Colors for basic things -*- lexical-binding: t; -*-
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Source: https://github.com/pulsar-edit/pulsar/blob/master/packages/one-light-ui/styles/atom.less
;;  Source: https://github.com/pulsar-edit/pulsar/blob/master/packages/one-light-ui/styles/status-bar.less
;;  Source: https://github.com/pulsar-edit/pulsar/blob/master/packages/one-light-ui/styles/tabs.less
;;  Source: https://github.com/pulsar-edit/pulsar/blob/master/packages/one-light-ui/styles/settings-view.less
;;  Source: https://github.com/pulsar-edit/pulsar/blob/master/packages/one-light-ui/styles/panes.less
;;  Source: https://github.com/pulsar-edit/pulsar/blob/master/packages/one-light-ui/styles/packages.less
;;
;;; Code:

(defface info nil
  "Info face, inherited by other faces.")

(define-namespace one-light-- :global t

(custom-theme-set-faces
 'one-light

   ;; faces.el
   `(mode-line ((t (:background ,(hsl-to-hex level-3-color)))))
   `(mode-line-inactive ((t (:foreground ,(hsl-to-hex tab-text-color)))))
   `(mode-line-highlight ((t (:background ,(hsl-to-hex level-3-color-hover)))))
   `(mode-line-buffer-id ((t (:inherit mode-line))))
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
   `(scroll-bar ((t (:foreground ,(hsl-to-hex scrollbar-color)))))
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

 )

)

;;; base.el ends here
