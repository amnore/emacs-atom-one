;; one-light-port.el --- Port of One Light -*- lexical-binding: t -*-

;; Author: Chen Zhenge <me@markle.one>
;; URL: https://github.com/amnore/emacs-one-light-port
;; Version: 1
;; Package-Requires ((emacs "24"))
;;
;;; Commentary:
;;
;; Yet another port of Atom's One Light theme

;;; Code:
(require 'color)

(defun one-light-port--degree-to-percent (deg)
  (/ (degrees-to-radians deg) float-pi))

(defun one-light-port--src-over-dst (src dst alpha)
  (let ((src-rgb (apply 'color-hsl-to-rgb src))
        (dst-rgb (apply 'color-hsl-to-rgb dst)))
    (apply 'color-rgb-to-hsl
           (mapcar (lambda (src dst) (+ (* src alpha) (* dst (- 1 alpha))))
                   (list src-rgb dst-rgb)))))

(let* ((to-percent (lambda (x) (one-light-port--degree-to-percent x)))
       (mix (lambda (src dst alpha) (one-light-port--src-over-dst src dst alpha)))
       (syntax-hue (to-percent 230))
       (syntax-saturation 0.01)
       (syntax-brightness 0.98)
       (mono-1 `(,syntax-hue 0.08 0.24))
       (mono-2 `(,syntax-hue 0.06 0.44))
       (mono-3 `(,syntax-hue 0.04 0.64))
       (hue-1 `(,(to-percent 198) 0.99 0.37))
       (hue-2 `(,(to-percent 221) 0.87 0.6))
       (hue-3 `(,(to-percent 301) 0.63 0.4))
       (hue-4 `(,(to-percent 119) 0.34 0.47))
       (hue-5 `(,(to-percent 5) 0.74 0.59))
       (hue-5-2 `(,(to-percent 344) 0.84 0.43))
       (hue-6 `(,(to-percent 35) 0.99 0.36))
       (hue-6-2 `(,(to-percent 35) 0.99 0.4))
       (syntax-fg mono-1)
       (syntax-bg `(,syntax-hue ,syntax-saturation ,syntax-brightness))
       (syntax-gutter ((apply-partially 'color-darken-hsl syntax-bg) 36))
       (syntax-guide (mix syntax-fg syntax-bg 0.2))
       (syntax-accent `(,syntax-hue 1 0.66))))

(provide 'one-light-port)
;;; one-light-port.el ends here
