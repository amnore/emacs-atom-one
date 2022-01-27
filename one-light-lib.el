;;; one-light-lib.el --- Functions for manipulating colors -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2022 czg
;;
;; Author: czg <https://github.com/czg>
;; Maintainer: czg <me@markle.one>
;; Created: January 25, 2022
;; Modified: January 25, 2022
;; Version: 0.0.1
;; Keywords: faces one-light
;; Homepage: https://github.com/czg/one-light-lib
;; Package-Requires: ((emacs "24.4"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; Functions used in this file are only used internally
;;
;;; Code:

(require 'color)
(require 'cl-lib)
(require 'subr-x)

(defun one-light-lib--mix (color1 color2 &optional weight)
  "Mix COLOR1 and COLOR2.
COLOR1 has a weight of WEIGHT, COLOR2 has (1 - WEIGHT).
If WEIGHT is not specified, default to 0.5."
  (let ((color1-rgb (apply 'color-hsl-to-rgb color1))
        (color2-rgb (apply 'color-hsl-to-rgb color2))
        (w (or weight 0.5)))
    (apply 'color-rgb-to-hsl
           (cl-mapcar (lambda (c1 c2) (+ (* c1 w) (* c2 (- 1 w))))
                      color1-rgb
                      color2-rgb))))

(defun one-light-lib--luma (color)
  "Calculate the luma of COLOR."
  (let* ((rgb (apply 'color-hsl-to-rgb color))
         (gamma-expand (lambda (u) (if (< u 0.3928)
                                           (/ u 12.92)
                                         (expt (/ (+ u 0.055) 1.055) 2.4))))
         (rgb-linear (mapcar gamma-expand rgb)))
    (cl-reduce '+ (cl-mapcar (lambda (w u) (* w u))
                             '(0.2126 0.1752 0.0722)
                             rgb-linear))))

(defun one-light-lib--color-hsv-to-hsl (color)
  "Convert hsv COLOR to hsl."
  (let* ((h (nth 0 color))
         (s (nth 1 color))
         (v (nth 2 color))
         (l (* v (- 1 (/ s 2)))))
    (list h
          l
          (if (or (= l 0) (= l 1))
              0
            (/ (- v l) (min l (- 1 l)))))))

(defun one-light-lib--degree-to-ratio (degree)
  "Convert DEGREE to its ratio of a circle."
  (/ degree 360.0))

(defun one-light-lib--color-hex-to-hsl (color)
  "Convert hex COLOR to hsl, each component can have 1, 2 or 4 digits."
  (let* ((digits (/ (length color) 3))
         (hex-max (- (expt 16.0 digits) 1))
         (subhex (lambda (pos) (string-to-number
                           (substring color
                                      (+ 1 (* digits pos))
                                      (+ 1 (* digits (+ 1 pos))))
                           16))))
    (apply 'color-rgb-to-hsl
           (mapcar (lambda (x) (/ x hex-max))
                   (mapcar subhex '(0 1 2))))))

(defun one-light-lib--color-contrast (color &optional dark light threshold)
  "Choose between DARK and LIGHT based on the luma value of COLOR.
If the luma is less than THRESHOLD, choose LIGHT, otherwise choose DARK.
If THRESHOLD if omitted, use 0.43 by default."
  (let ((black (or dark '(0 0 0)))
        (white (or light '(0 0 1)))
        (thld (or threshold 0.43)))
    (if (< (one-light-lib--luma color) thld)
        white
      black)))

(provide 'one-light-lib)
;;; one-light-lib.el ends here
