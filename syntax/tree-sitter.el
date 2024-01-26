;;; tree-sitter.el --- Colors for tree-sitter -*- lexical-binding: t; -*-
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

 ;; tree-sitter-hl.el
 `(tree-sitter-hl-face:function ((t (:inherit font-lock-function-name-face))))
 `(tree-sitter-hl-face:function.call ((t (:inherit font-lock-function-name-face))))
 `(tree-sitter-hl-face:function.builtin ((t (:inherit font-lock-function-name-face))))
 `(tree-sitter-hl-face:function.special ((t (:inherit font-lock-function-name-face))))
 `(tree-sitter-hl-face:function.macro ((t (:inherit font-lock-keyword-face))))
 `(tree-sitter-hl-face:method ((t (:inherit font-lock-function-name-face))))
 `(tree-sitter-hl-face:method.call ((t (:inherit font-lock-function-name-face))))
 `(tree-sitter-hl-face:type ((t (:inherit font-lock-type-face))))
 `(tree-sitter-hl-face:type.parameter ((t (:inherit font-lock-type-face))))
 `(tree-sitter-hl-face:type.argument ((t (:inherit font-lock-type-face))))
 `(tree-sitter-hl-face:type.builtin ((t (:inherit font-lock-type-face))))
 `(tree-sitter-hl-face:type.super ((t (:inherit font-lock-type-face))))
 `(tree-sitter-hl-face:constructor ((t (:inherit font-lock-function-name-face))))
 `(tree-sitter-hl-face:variable ((t (:inherit font-lock-variable-name-face))))
 `(tree-sitter-hl-face:variable.parameter ((t (:inherit font-lock-variable-name-face))))
 `(tree-sitter-hl-face:variable.builtin ((t (:inherit font-lock-variable-name-face))))
 `(tree-sitter-hl-face:variable.special ((t (:inherit font-lock-variable-name-face))))
 `(tree-sitter-hl-face:property ((t (:inherit (:inherit font-lock-member-face)))))
 `(tree-sitter-hl-face:property.definition ((t (:inherit font-lock-member-face))))
 `(tree-sitter-hl-face:comment ((t (:inherit font-lock-comment-face))))
 `(tree-sitter-hl-face:doc ((t (:inherit font-lock-doc-face))))
 `(tree-sitter-hl-face:string ((t (:inherit font-lock-string-face))))
 `(tree-sitter-hl-face:string.special ((t (:inherit font-lock-string-face))))
 `(tree-sitter-hl-face:escape ((t (:inherit font-lock-regexp-grouping-backslash))))
 `(tree-sitter-hl-face:embedded ((t (:inherit default))))
 `(tree-sitter-hl-face:keyword ((t (:inherit font-lock-keyword-face))))
 `(tree-sitter-hl-face:operator ((t (:inherit font-lock-keyword-face))))
 `(tree-sitter-hl-face:label ((t (:inherit font-lock-member-face))))
 `(tree-sitter-hl-face:constant ((t (:inherit font-lock-constant-face))))
 `(tree-sitter-hl-face:constant.builtin ((t (:inherit font-lock-constant-face))))
 `(tree-sitter-hl-face:number ((t (:inherit font-lock-constant-face))))
 `(tree-sitter-hl-face:punctuation ((t nil)))
 `(tree-sitter-hl-face:punctuation.bracket ((t nil)))
 `(tree-sitter-hl-face:punctuation.delimiter ((t nil)))
 `(tree-sitter-hl-face:punctuation.special ((t nil)))
 `(tree-sitter-hl-face:tag ((t (:inherit font-lock-tag-face))))
 `(tree-sitter-hl-face:attribute ((t (:inherit font-lock-attribute-face))))
 `(tree-sitter-hl-face:noise ((t (:foreground "red"))))
 )

)

;;; tree-sitter.el ends here
