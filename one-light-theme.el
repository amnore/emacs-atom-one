;;; one-light-theme.el --- One Light Theme -*- lexical-binding: t; -*-
(require 'atom-one)
(eval-when-compile (require 'atom-one-lib))

;;;###theme-autoload
(deftheme one-light
  "Port of Atom's One Light theme.")

(atom-one--define-theme 'one-light)

(provide-theme 'one-light)
