;;; one-light-theme.el --- Port of One Light -*- lexical-binding: t -*-
;;
;; Copyright (C) 2022 Chen Zhenge
;;
;; Author: Chen Zhenge <me@markle.one>
;; URL: https://github.com/amnore/emacs-one-light-port
;; Keywords: faces one-light
;; Version: 1
;; Package-Requires: ((emacs "25.1"))
;;
;;; Commentary:
;;
;; Yet another port of Atom's One Light theme

;;; Code:
(deftheme one-light
  "Yet another port of Atom's One Light theme.")

(require 'one-light-syntax)
(require 'one-light-ui)

;;;###autoload
(when (and (boundp 'custom-theme-load-path) load-file-name)
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'one-light)
;;; one-light-theme.el ends here
