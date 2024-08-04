;;; one-light-lib.el --- Functions for manipulating colors -*- lexical-binding: t; -*-
;;; Commentary:
;;
;; Colors are internally represented in HSL format, as a triple (H S L),
;; each being a number between 0 and 1.
;;
;;; Code:

(require 'color)
(require 'cl-lib)
(require 'subr-x)
(eval-when-compile (require 'names))

(define-namespace one-light--

(defmacro setcolor (&rest pairs)
  "Set VARIABLE/VALUE pairs in the `one-light--' namespace.

\(fn [VARIABLE VALUE]...)"
  (declare (debug setq))
  (let (expr)
    (while pairs
      (push `(defvar
               ,(intern (concat "one-light--" (symbol-name (car pairs))))
               (define-namespace one-light-- :global t ,(cadr pairs)))
            expr)
      (setq pairs (cddr pairs)))
    `(progn ,@(nreverse expr))))

;;; Color conversion functions
(defun name-to-hsl (name)
  "Convert color NAME to HSL format."
  (apply #'color-rgb-to-hsl (color-name-to-rgb name)))

(defun hsl-to-hex (color)
  "Convert COLOR to hex string."
  (apply #'color-rgb-to-hex (apply #'color-hsl-to-rgb color)))

(defun hsv-to-hsl (color)
  "Convert hsv COLOR to hsl."
  (let* ((h (nth 0 color))
         (s (nth 1 color))
         (v (nth 2 color))
         (l (* v (- 1 (/ s 2)))))
    (list h
          (if (or (= l 0) (= l 1))
              0
            (/ (- v l) (min l (- 1 l))))
          l)))

(defun hex-to-hsl (color)
  "Convert hex COLOR to hsl, each component can have 1, 2 or 4 digits."
  (let* ((digits (/ (length color) 3))
         (hex-max (- (expt 16.0 digits) 1))
         (subhex (lambda (pos) (string-to-number
                           (substring color
                                      (+ 1 (* digits pos))
                                      (+ 1 (* digits (+ 1 pos))))
                           16))))
    (apply #'color-rgb-to-hsl
           (mapcar (lambda (x) (/ x hex-max))
                   (mapcar subhex '(0 1 2))))))

(defun hsl-to-hsv (color)
  "Convert hsl COLOR to hsv."
  (let* ((h (nth 0 color))
         (s (nth 1 color))
         (l (nth 2 color))
         (v (+ l (* s (min l (- 1 l))))))
    (list h
          (if (= v 0)
              0
            (* 2 (- 1 (/ l v))))
          v)))

(defun degree-to-ratio (degree)
  "Convert DEGREE to the ratio of a circle."
  (/ degree 360.0))

;;; Color manipulating functions
(defun mix (color1 color2 &optional weight)
  "Mix COLOR1 and COLOR2.
COLOR1 has a weight of WEIGHT, COLOR2 has (1 - WEIGHT).
If WEIGHT is not specified, default to 0.5."
  (let ((color1-rgb (apply #'color-hsl-to-rgb color1))
        (color2-rgb (apply #'color-hsl-to-rgb color2))
        (w (or weight 0.5)))
    (apply #'color-rgb-to-hsl
           (cl-mapcar (lambda (c1 c2) (+ (* c1 w) (* c2 (- 1 w))))
                      color1-rgb
                      color2-rgb))))

(defun contrast (color &optional dark light threshold)
  "Choose between DARK and LIGHT based on the luma value of COLOR.
If the luma is less than THRESHOLD, choose LIGHT, otherwise choose DARK.
If THRESHOLD if omitted, use 0.43 by default."
  (let ((black (or dark '(0 0 0)))
        (white (or light '(0 0 1)))
        (thld (or threshold 0.43)))
    (if (< (luma color) thld)
        white
      black)))

(defun darken (color amount)
  "Darken COLOR by AMOUNT."
  (list (nth 0 color)
        (nth 1 color)
        (color-clamp (- (nth 2 color) amount))))

(defun lighten (color amount)
  "Lighten COLOR by AMOUNT, relatively if RELATIVE is non-nil."
  (darken color (- amount)))

;;; Color property functions
(defun luma (color)
  "Calculate the luma of COLOR."
  (let* ((rgb (apply #'color-hsl-to-rgb color))
         (gamma-expand (lambda (u) (if (< u 0.03928)
                                  (/ u 12.92)
                                (expt (/ (+ u 0.055) 1.055) 2.4))))
         (rgb-linear (mapcar gamma-expand rgb)))
    (cl-reduce #'+ (cl-mapcar (lambda (w u) (* w u))
                              '(0.2126 0.7152 0.0722)
                              rgb-linear))))

(defun hue (color)
  "Get the hue of COLOR."
  (nth 0 color))

(defun saturation (color)
  "Get the saturation of COLOR."
  (nth 1 color))

(defun lightness (color)
  "Get the lightness of COLOR."
  (nth 2 color))

(defun hsvvalue (color)
  "Get the v value in hsv of COLOR."
  (nth 2 (hsl-to-hsv color)))

)

(provide 'one-light-lib)
;;; one-light-lib.el ends here
