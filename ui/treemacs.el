;;; treemacs.el --- Colors for treemacs -*- lexical-binding: t; -*-
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Source: https://github.com/pulsar-edit/pulsar/blob/master/packages/one-light-ui/styles/tree-view.less
;;  Source: https://github.com/pulsar-edit/pulsar/blob/master/packages/one-light-ui/styles/lists.less
;;
;;; Code:

(define-namespace one-light-- :global t

(custom-theme-set-faces
 'one-light

 ;; treemacs-faces.el
 `(treemacs-directory-face ((t (:inherit (variable-pitch default)))))
 `(treemacs-file-face ((t (:inherit treemacs-directory-face))))
 `(treemacs-root-face ((t (:inherit treemacs-directory-face))))
 `(treemacs-git-modified-face
   ((t (:inherit treemacs-file-face
        :foreground ,(hsl-to-hex text-color-modified)))))
 `(treemacs-git-renamed-face
   ((t (:inherit treemacs-file-face
        :foreground ,(hsl-to-hex text-color-renamed)))))
 `(treemacs-git-ignored-face
   ((t (:inherit treemacs-file-face
        :foreground ,(hsl-to-hex text-color-ignored)))))
 `(treemacs-git-untracked-face ((t (:inherit treemacs-git-added-face))))
 `(treemacs-git-added-face
   ((t (:inherit treemacs-file-face
        :foreground ,(hsl-to-hex text-color-added)))))
 )

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

)

;;; treemacs.el ends here
