;;; lsp.el --- Colors for lsp -*- lexical-binding: t; -*-
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Source: https://github.com/pulsar-edit/pulsar/blob/master/packages/one-light-syntax/styles/syntax/base.less
;;
;;; Code:

(defface lsp-face-semhl-declaration nil
  "LSP face for declaration modifier.")

(defface lsp-face-semhl-readonly nil
  "LSP face for readonly modifier.")

(defface lsp-face-semhl-abstract nil
  "LSP face for abstract modifier.")

(defface lsp-face-semhl-async nil
  "LSP face for async modifier.")

(defface lsp-face-semhl-modification nil
  "LSP face for modification modifier.")

(defface lsp-face-semhl-documentation nil
  "LSP face for documentation modifier.")

(defface font-lock-member-face nil
  "Font lock face used to highlight member variables.")

(defface font-lock-tag-face nil
  "Font lock face used to highlight xml tags.")

(defface font-lock-attribute-face nil
  "Font lock face used to highlight xml attributes.")

(define-namespace one-light-- :global t

(custom-theme-set-faces
 'one-light

 ;; lsp-semantic-tokens.el
 `(lsp-face-semhl-namespace ((t (:inherit lsp-face-semhl-constant))))
 `(lsp-face-semhl-regexp ((t (:inherit lsp-face-semhl-string))))
 `(lsp-face-semhl-operator ((t (:inherit lsp-face-semhl-keyword))))
 `(lsp-face-semhl-enum ((t (:inherit lsp-face-semhl-type))))
 `(lsp-face-semhl-member ((t (:inherit font-lock-member-face))))
 `(lsp-face-semhl-property
   ((t (:inherit lsp-face-semhl-member))))
 `(lsp-face-semhl-macro ((t (:inherit lsp-face-semhl-constant))))
 `(lsp-face-semhl-label ((t (:underline t))))
 ;; TODO: Add face to modifiers
 `(lsp-face-semhl-declaration ((t nil)))
 `(lsp-face-semhl-definition ((t nil)))
 `(lsp-face-semhl-readonly ((t nil)))
 `(lsp-face-semhl-static ((t nil)))
 `(lsp-face-semhl-deprecated ((t nil)))
 `(lsp-face-semhl-abstract ((t nil)))
 `(lsp-face-semhl-async ((t nil)))
 `(lsp-face-semhl-modification ((t nil)))
 `(lsp-face-semhl-documentation ((t nil)))
 `(lsp-face-semhl-default-library ((t nil)))
 )

(with-eval-after-load 'lsp-semantic-tokens
  (setq lsp-semantic-token-modifier-faces
        '(("declaration" . lsp-face-semhl-declaration)
          ("definition" . lsp-face-semhl-definition)
          ("readonly" . lsp-face-semhl-readonly)
          ("static" . lsp-face-semhl-static)
          ("deprecated" . lsp-face-semhl-deprecated)
          ("abstract" . lsp-face-semhl-abstract)
          ("async" . lsp-face-semhl-async)
          ("modification" . lsp-face-semhl-modification)
          ("documentation" . lsp-face-semhl-documentation)
          ("defaultLibrary" . lsp-face-semhl-default-library))))
)

;;; lsp.el ends here
