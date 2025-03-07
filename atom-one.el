;;; atom-one.el --- Port of Atom's One Light and One Dark Theme -*- lexical-binding: t; -*-
;;
;; Copyright  2025 amnore
;;
;; Author: amnore <me@markle.one>
;; URL: https://github.com/amnore/emacs-one-light-port
;; Keywords: faces one-light
;; Version: 0.2.0
;; Package-Requires: ((emacs "30.1") (nerd-icons "0.1.0"))
;;
;;; Commentary:
;;
;; Port of Atom One theme
;;
;;; Code:
(require 'nerd-icons)
(require 'dbus)
(eval-when-compile (require 'cl-macs))

(defun atom-one--hs-setup-overlay (ov)
  (overlay-put ov 'display (nerd-icons-octicon "nf-oct-ellipsis")))

(defun atom-one-load-theme ()
  (cl-flet* ((switch-theme (value)
	       (let ((theme (cl-case value
			      ((0 2) 'one-light)
			      (1 'one-dark))))
		 (unless (eq theme (car custom-enabled-themes))
		   (load-theme theme t))))
	     (callback (value)
	       (switch-theme (caar value)))
	     (signal-callback (namespace key value)
	       (when (and (equal namespace "org.freedesktop.appearance")
			  (equal key "color-scheme"))
		 (switch-theme (car value)))))
    (dbus-call-method-asynchronously
     :session
     "org.freedesktop.portal.Desktop"
     "/org/freedesktop/portal/desktop"
     "org.freedesktop.portal.Settings"
     "Read"
     #'callback
     "org.freedesktop.appearance"
     "color-scheme")
    (dbus-register-signal
     :session
     "org.freedesktop.portal.Desktop"
     "/org/freedesktop/portal/desktop"
     "org.freedesktop.portal.Settings"
     "SettingChanged"
     #'signal-callback)))

;;;###autoload
(when (and (boundp 'custom-theme-load-path) load-file-name)
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide 'atom-one)
;;; atom-one.el ends here
