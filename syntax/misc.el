;;; misc.el --- Colors for misc. modes -*- lexical-binding: t; -*-
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Source: https://github.com/pulsar-edit/pulsar/blob/master/packages/one-light-syntax/styles/syntax/base.less
;;
;;; Code:

(define-namespace one-light-- :global t

(custom-theme-set-faces
 'one-light

 ;; hl-todo.el
 `(hl-todo ((t (:foreground ,(hsl-to-hex (lighten mono-3 0.06))
                  :weight bold))))

 ;; mmm-vars.el
 `(mmm-default-submode-face ((t ())))
 )

)


;;; misc.el ends here
