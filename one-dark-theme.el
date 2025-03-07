;;; one-dark-theme.el --- One Dark Theme -*- lexical-binding: t; -*-
(require 'atom-one)
(eval-when-compile (require 'atom-one-lib))

;;;###theme-autoload
(deftheme one-dark
  "Port of Atom's One Dark theme.")

(atom-one--define-theme 'one-dark)

(provide-theme 'one-dark)
