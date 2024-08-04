;;; one-light-theme.el --- Port of Atom One Light -*- lexical-binding: t -*-
;;
;; Copyright (C) 2024 amnore
;;
;; Author: amnore <me@markle.one>
;; URL: https://github.com/amnore/emacs-one-light-port
;; Keywords: faces one-light
;; Version: 0.0.2
;; Package-Requires: ((emacs "25.1") (names "20180321.1155") (load-relative "1.3.2") (nerd-icons "0.1.0"))
;;
;;; Commentary:
;;
;; Yet another port of Atom's One Light theme
;;
;;; Code:

(require 'load-relative)
(require-relative 'one-light-lib)

(deftheme one-light
  "Yet another port of Atom's One Light theme.")

(let ((syntax-theme-files '("base" "latex" "lsp" "misc" "term" "tree-sitter" "web"))
      (ui-theme-files '("base" "company" "flycheck" "misc" "modeline" "treemacs")))
  (load-relative '("syntax/colors" "ui/colors"))
  (load-relative (mapcar (lambda (file) (concat "syntax/" file))
                         syntax-theme-files))
  (load-relative (mapcar (lambda (file) (concat "ui/" file))
                         ui-theme-files)))


;;;###autoload
(when (and (boundp 'custom-theme-load-path) load-file-name)
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'one-light)
;;; one-light-theme.el ends here
