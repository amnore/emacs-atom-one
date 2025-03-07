;;; atom-one-lib.el --- Port of Atom's One Light and One Dark Theme -*- lexical-binding: t; -*-
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
(require 'color)
(require 'cl-lib)

;;;; Color conversion functions
(defun atom-one--name-to-hsl (name)
  "Convert color NAME to HSL format."
  (apply #'color-rgb-to-hsl (color-name-to-rgb name)))

(defun atom-one--hsl-to-hex (color)
  "Convert COLOR to hex string."
  (apply #'color-rgb-to-hex (apply #'color-hsl-to-rgb color)))

(defun atom-one--hsv-to-hsl (color)
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

(defun atom-one--hex-to-hsl (color)
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

(defun atom-one--hsl-to-hsv (color)
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

(defun atom-one--degree-to-ratio (degree)
  "Convert DEGREE to the ratio of a circle."
  (/ degree 360.0))

;;; Color manipulating functions
(defun atom-one--mix (color1 color2 &optional weight)
  "Mix COLOR1 and COLOR2.
COLOR1 has a weight of WEIGHT, COLOR2 has (1 - WEIGHT).
If WEIGHT is not specified, default to 0.5."
  (let ((color1-rgb (apply #'color-hsl-to-rgb color1))
        (color2-rgb (apply #'color-hsl-to-rgb color2))
        (w (/ (or weight 0.5) 100.0)))
    (apply #'color-rgb-to-hsl
           (cl-mapcar (lambda (c1 c2) (+ (* c1 w) (* c2 (- 1 w))))
                      color1-rgb
                      color2-rgb))))

(defun atom-one--contrast (color &optional dark light threshold)
  "Choose between DARK and LIGHT based on the luma value of COLOR.
If the luma is less than THRESHOLD, choose LIGHT, otherwise choose DARK.
If THRESHOLD if omitted, use 0.43 by default."
  (let ((black (or dark '(0 0 0)))
        (white (or light '(0 0 1)))
        (thld (/ (or threshold 43) 100.0)))
    (if (< (atom-one--luma color) thld)
        white
      black)))

(defun atom-one--darken (color amount)
  "Darken COLOR by AMOUNT."
  (list (nth 0 color)
        (nth 1 color)
        (color-clamp (- (nth 2 color) (/ amount 100.0)))))

(defun atom-one--lighten (color amount)
  "Lighten COLOR by AMOUNT, relatively if RELATIVE is non-nil."
  (atom-one--darken color (- amount)))

;;; Color property functions
(defun atom-one--luma (color)
  "Calculate the luma of COLOR."
  (let* ((rgb (apply #'color-hsl-to-rgb color))
	 (gamma-expand (lambda (u) (if (< u 0.03928)
				       (/ u 12.92)
				     (expt (/ (+ u 0.055) 1.055) 2.4))))
	 (rgb-linear (mapcar gamma-expand rgb)))
    (* 100 (cl-reduce #'+ (cl-mapcar (lambda (w u) (* w u))
				     '(0.2126 0.7152 0.0722)
				     rgb-linear)))))

(defun atom-one--hue (color)
  "Get the hue of COLOR."
  (* 360 (nth 0 color)))

(defun atom-one--saturation (color)
  "Get the saturation of COLOR."
  (* 100 (nth 1 color)))

(defun atom-one--lightness (color)
  "Get the lightness of COLOR."
  (* 100 (nth 2 color)))

(defun atom-one--hsvvalue (color)
  "Get the v value in hsv of COLOR."
  (nth 2 (atom-one--hsl-to-hsv color)))

(defun atom-one--hsl (h s l)
  (list (atom-one--degree-to-ratio h) (/ s 100.0) (/ l 100.0)))

(defun atom-one--hsv (h s v)
  (atom-one--hsv-to-hsl (list (atom-one--degree-to-ratio h) (/ s 100.0) (/ v 100.0))))

(defun atom-one--hex (hex)
  (atom-one--hex-to-hsl hex))

(defmacro atom-one--define-theme (theme-name)
  (let* ((light (equal theme-name ''one-light))
	 (syntax-hue (if light 230 220))
	 (syntax-saturation (if light 1 13))
	 (syntax-brightness (if light 98 18))
	 (mono-1 (if light (atom-one--hsl syntax-hue 8 24)
		   (atom-one--hsl syntax-hue 14 71)))
	 (mono-2 (if light (atom-one--hsl syntax-hue 6 44)
		   (atom-one--hsl syntax-hue 9 55)))
	 (mono-3 (if light (atom-one--hsl syntax-hue 4 64)
		   (atom-one--hsl syntax-hue 10 40)))
	 (hue-1 (if light (atom-one--hsl 198 99 37)
		  (atom-one--hsl 187 47 55))) ; cyan
	 (hue-2 (if light (atom-one--hsl 221 87 60)
		  (atom-one--hsl 207 82 66))) ; blue
	 (hue-3 (if light (atom-one--hsl 301 63 40)
		  (atom-one--hsl 286 60 67))) ; purple
	 (hue-4 (if light (atom-one--hsl 119 34 47)
		  (atom-one--hsl 95 38 62))) ; green
	 (hue-5 (if light (atom-one--hsl 5 74 59)
		  (atom-one--hsl 355 65 65))) ; red 1
	 (hue-5-2 (if light (atom-one--hsl 344 84 43)
		    (atom-one--hsl 5 48 51))) ; red 2
	 (hue-6 (if light (atom-one--hsl 35 99 36)
		  (atom-one--hsl 29 54 61))) ; orange 1
	 (hue-6-2 (if light (atom-one--hsl 35 99 40)
		    (atom-one--hsl 39 67 69))) ; orange 2
	 (syntax-fg mono-1)
	 (syntax-bg (atom-one--hsl syntax-hue syntax-saturation syntax-brightness))
	 (syntax-gutter (if light (atom-one--darken syntax-bg 36)
			  (atom-one--darken syntax-fg 26)))
	 (syntax-guide (if light (atom-one--mix syntax-fg syntax-bg 20)
			 (atom-one--mix syntax-fg syntax-bg 15))) ; emulate fade with blend with background
	 (syntax-accent (atom-one--hsl syntax-hue 100 66))

	 (syntax-text-color syntax-fg)
	 (syntax-cursor-color syntax-accent)
	 (syntax-selection-color (if light (atom-one--darken syntax-bg 8)
				   (atom-one--lighten syntax-bg 10)))
	 (syntax-selection-flash-color syntax-accent)
	 (syntax-background-color syntax-bg)
	 (syntax-wrap-guide-color syntax-guide)
	 (syntax-indent-guide-color syntax-guide)
	 (syntax-invisible-character-color syntax-guide)
	 (syntax-result-marker-color (if light (atom-one--mix syntax-accent syntax-bg 20)
				       (atom-one--mix syntax-accent syntax-bg 24)))
	 (syntax-result-marker-color-selected syntax-accent)
	 (syntax-gutter-text-color syntax-gutter)
	 (syntax-gutter-text-color-selected syntax-fg)
	 (syntax-gutter-background-color syntax-bg)
	 (syntax-gutter-background-color-selected (if light (atom-one--darken syntax-bg 8)
						    (atom-one--lighten syntax-bg 8)))
	 (syntax-color-renamed (if light (atom-one--hsl 208 100 66)
				 (atom-one--hsl 208 100 60)))
	 (syntax-color-added (if light (atom-one--hsl 132 60 44)
			       (atom-one--hsl 150 60 54)))
	 (syntax-color-modified (if light (atom-one--hsl 40 90 50)
				  (atom-one--hsl 40 60 70)))
	 (syntax-color-removed (if light (atom-one--hsl 0 100 54)
				 (atom-one--hsl 0 70 60)))
	 (syntax-color-variable hue-5)
	 (syntax-color-constant hue-6)
	 (syntax-color-property syntax-fg)
	 (syntax-color-value syntax-fg)
	 (syntax-color-function hue-2)
	 (syntax-color-method hue-2)
	 (syntax-color-class hue-6-2)
	 (syntax-color-keyword hue-3)
	 (syntax-color-tag hue-5)
	 (syntax-color-attribute hue-6)
	 (syntax-color-import hue-3)
	 (syntax-color-snippet hue-4)
	 (syntax-cursor-line (if light (atom-one--mix syntax-fg syntax-bg 5)
			       (atom-one--mix (atom-one--hsl syntax-hue 100 80) syntax-bg 4)))
	 (syntax-deprecated-fg (atom-one--darken syntax-color-modified 50))
	 (syntax-deprecated-bg syntax-color-modified)
	 (syntax-illegal-fg (atom-one--name-to-hsl "white"))
	 (syntax-illegal-bg syntax-color-removed)

	 (ui-syntax-color syntax-background-color)
	 (ui-hue syntax-hue)
	 (ui-saturation syntax-saturation)
	 (ui-lightness syntax-brightness)
	 (--ui-bg (atom-one--hsl ui-hue ui-saturation ui-lightness))
	 (--base-background-color --ui-bg)
	 (--level-3-color (atom-one--darken --base-background-color 6))
	 (ui-fg (if light (atom-one--hsl ui-hue ui-saturation (- ui-lightness 72))
		  (atom-one--hsl ui-hue ui-saturation (* 3 ui-lightness))))
	 (ui-bg --ui-bg)
	 (ui-border (if light (atom-one--darken --level-3-color 6)
		      (atom-one--hsl ui-hue ui-saturation (* ui-lightness 0.6))))
	 (level-1-color (if light (atom-one--lighten --base-background-color 4)
			  (atom-one--lighten --base-background-color 6)))
	 (level-2-color --base-background-color)
	 (level-3-color (if light --level-3-color
			  (atom-one--darken --base-background-color 3)))
	 (level-3-color-hover (if light (atom-one--darken level-3-color 6)
				(atom-one--lighten level-3-color 6)))
	 (level-3-color-active (if light (atom-one--darken level-3-color 3)
				 (atom-one--lighten level-3-color 3)))
	 (base-background-color --base-background-color)
	 (base-border-color ui-border)
	 (text-color ui-fg)
	 (text-color-subtle (atom-one--lighten text-color 30))
	 (text-color-highlight (atom-one--darken text-color 12))
	 (text-color-selected (atom-one--darken text-color-highlight 12))
	 (text-color-info `(,(atom-one--degree-to-ratio 208) 1 0.54))
	 (text-color-success `(,(atom-one--degree-to-ratio 132) 0.6 0.44))
	 (text-color-warning `(,(atom-one--degree-to-ratio 37) 0.9 0.44))
	 (text-color-error `(,(atom-one--degree-to-ratio 9) 0.9 0.56))
	 (text-color-faded (if light (atom-one--mix text-color ui-bg 30)
			     (atom-one--mix text-color ui-bg 20)))
	 (text-color-added text-color-success)
	 (text-color-ignored text-color-subtle)
	 (text-color-modified text-color-warning)
	 (text-color-removed text-color-error)
	 (text-color-renamed text-color-info)
	 (background-color-info `((atom-one--degree-to-ratio 208) 1 0.56))
	 (background-color-success `(,(atom-one--degree-to-ratio 132) 0.52 0.48))
	 (background-color-warning `(,(atom-one--degree-to-ratio 40) 0.6 0.48))
	 (background-color-error `(,(atom-one--degree-to-ratio 5) 0.72 0.56))
	 (background-color-highlight (atom-one--darken level-3-color 2))
	 (background-color-selected (atom-one--darken level-3-color 6))
	 (app-background-color level-3-color)
	 (accent-luma (atom-one--luma (atom-one--hsl ui-hue 50 50)))
	 (accent-color (if light (atom-one--mix (atom-one--hsv ui-hue 60 60)
					  (atom-one--hsl ui-hue 100 68)
					  (* 2 accent-luma))
			 (atom-one--mix (atom-one--hsv ui-hue 100 66)
				  (atom-one--hsl ui-hue 100 70)
				  accent-luma)))
	 (accent-text-color (if light (atom-one--contrast accent-color
						    (atom-one--hsl ui-hue 100 16)
						    (atom-one--hex "#fff")
						    40)
			      (atom-one--contrast accent-color
					    (atom-one--hsl ui-hue 100 10)
					    (atom-one--hex "#fff")
					    25)))
	 (accent-bg-color (if light (atom-one--mix (atom-one--hsv ui-hue 40 72)
					     (atom-one--hsl ui-hue 100 66)
					     (* 2 accent-luma))
			    (atom-one--mix (atom-one--hsv ui-hue 66 66)
				     (atom-one--hsl ui-hue 66 60)
				     (* 2 accent-luma))))
	 (accent-bg-text-color (if light (atom-one--contrast accent-bg-color
						       (atom-one--hsl ui-hue 100 10)
						       (atom-one--hex-to-hsl "#fff")
						       40)
				 (atom-one--contrast accent-bg-color
					       (atom-one--hsl ui-hue 100 10)
					       (atom-one--hex "#fff")
					       30)))
	 (accent-only-text-color (if light (atom-one--mix (atom-one--hsv ui-hue 70 50)
						    (atom-one--hsl ui-hue 100 60)
						    (* 2 accent-luma))
				   (atom-one--mix (atom-one--hsv ui-hue 100 66)
					    (atom-one--hsl ui-hue 100 77)
					    accent-luma)))
	 (pane-item-background-color base-background-color)
	 (pane-item-border-color base-border-color)
	 (input-background-color level-1-color)
	 (input-border-color base-border-color)
	 (tool-panel-background-color level-3-color)
	 (tool-panel-border-color base-border-color)
	 (inset-panel-background-color (atom-one--lighten level-2-color 4))
	 (inset-panel-border-color (atom-one--mix base-border-color ui-bg 15))
	 (panel-heading-background-color level-2-color)
	 (panel-heading-border-color base-border-color)
	 (overlay-background-color (atom-one--mix level-2-color level-3-color))
	 (overlay-border-color base-border-color)
	 (button-background-color level-1-color)
	 (button-background-color-hover (atom-one--darken button-background-color 4))
	 (button-background-color-selected accent-bg-color)
	 (button-border-color base-border-color)
	 (tab-bar-background-color level-3-color)
	 (tab-bar-border-color base-border-color)
	 (tab-background-color level-3-color)
	 (tab-background-color-active level-2-color)
	 (tab-border-color base-border-color)
	 (tree-view-background-color level-3-color)
	 (tree-view-border-color base-border-color)
	 (ui-site-color-1 `(,(atom-one--degree-to-ratio 208) 1 0.56))
	 (ui-site-color-2 `(,(atom-one--degree-to-ratio 132) 0.48 0.48))
	 (ui-site-color-3 `(,(atom-one--degree-to-ratio 40) 0.6 0.52))
	 (ui-site-color-4 (atom-one--name-to-hsl "#D831B0"))
	 (ui-site-color-5 (atom-one--name-to-hsl "#EBDD5B"))
	 (badge-background-color (if light background-color-selected
				   (atom-one--lighten background-color-highlight 6)))
	 (button-text-color-selected accent-bg-text-color)
	 (button-border-color-selected (if light accent-color
					 base-border-color))
	 (checkbox-background-color (atom-one--mix accent-bg-color ui-bg 33))
	 (input-background-color-focus (if light (atom-one--hsl ui-hue 100 96)
					 (atom-one--mix accent-bg-color input-background-color 10)))
	 (input-selection-color (if light (atom-one--mix (atom-one--hsv ui-hue 33 95)
						   (atom-one--hsl ui-hue 100 98)
						   (* 2 accent-luma))
				  (atom-one--mix accent-color
					   input-background-color
					   25)))
	 (input-selection-color-focus (if light (atom-one--mix (atom-one--hsv ui-hue 44 90)
							 (atom-one--hsl ui-hue 100 94)
							 (* 2 accent-luma))
					(atom-one--mix accent-color
						 input-background-color
						 50)))
	 (overlay-backdrop-color (if light (atom-one--hsl ui-hue
						    (* 0.4 ui-saturation)
						    (* 0.8 ui-lightness))
				   (atom-one--hsl ui-hue
					    ui-saturation
					    (* 0.2 ui-lightness))))
	 (overlay-backdrop-opacity (if light 66 75))
	 (progress-background-color accent-color)
	 (scrollbar-color (if light (atom-one--darken level-3-color 14)
			    (atom-one--lighten ui-syntax-color 16)))
	 (scrollbar-background-color level-3-color)
	 (scrollbar-color-editor (if light (atom-one--contrast ui-syntax-color
							 (atom-one--darken ui-syntax-color 14)
							 (atom-one--lighten ui-syntax-color 9))
				   (atom-one--lighten ui-syntax-color 16)))
	 (scrollbar-background-color-editor ui-syntax-color)
	 (tab-text-color text-color-subtle)
	 (tab-text-color-active text-color-highlight)
	 (tab-text-color-editor (if light (atom-one--contrast ui-syntax-color
							(atom-one--lighten ui-syntax-color 70)
							text-color-highlight)
				  (atom-one--contrast ui-syntax-color
						(atom-one--darken ui-syntax-color 50)
						text-color-highlight)))
	 (tab-inactive-status-added (if light (atom-one--mix text-color-success ui-bg 77)
				      (atom-one--mix text-color-success ui-bg 55)))
	 (tab-inactive-status-modified (if light (atom-one--mix text-color-warning ui-bg 77)
					 (atom-one--mix text-color-warning ui-bg 55)))
	 (tooltip-background-color accent-bg-color)
	 (tooltip-text-color accent-bg-text-color)
	 (tooltip-text-key-color tooltip-background-color)
	 (tooltip-background-key-color tooltip-text-color))
    `(progn

       (custom-theme-set-faces
	,theme-name

	;; faces.el
	',`(default ((t (:foreground ,(atom-one--hsl-to-hex syntax-text-color)
				   :background ,(atom-one--hsl-to-hex syntax-background-color)))))
	',`(link ((t (:inherit underline
			     :foreground ,(atom-one--hsl-to-hex hue-1)))))
	',`(link-visited ((t (:inherit link))))
	',`(highlight ((t (:background ,(atom-one--hsl-to-hex syntax-selection-color)))))
	',`(region ((t (:background ,(atom-one--hsl-to-hex syntax-selection-color)))))
	',`(secondary-selection ((t (:inherit region))))
	',`(line-number ((t (:foreground ,(atom-one--hsl-to-hex syntax-gutter-text-color)))))
	',`(line-number-current-line ((t (:foreground ,(atom-one--hsl-to-hex syntax-gutter-text-color-selected)))))
	',`(fill-column-indicator ((t (:foreground ,(atom-one--hsl-to-hex syntax-wrap-guide-color)))))
	',`(escape-glyph ((t (:foreground ,(atom-one--hsl-to-hex hue-4)))))
	',`(minibuffer-prompt ((t nil)))
	',`(cursor ((t (:background ,(atom-one--hsl-to-hex syntax-cursor-color)))))
	',`(glyphless-char ((t (:foreground ,(atom-one--hsl-to-hex syntax-invisible-character-color)))))
	',`(show-paren-match ((t (:underline ,(atom-one--hsl-to-hex syntax-cursor-color)))))
	',`(show-paren-match-expression ((t (:inherit show-paren-match))))
	',`(show-paren-mismatch ((t nil)))
	',`(header-line ((t (:foreground ,(atom-one--hsl-to-hex text-color-subtle)
				       :background ,(atom-one--hsl-to-hex base-background-color)))))
	',`(header-line-highlight ((t (:inherit header-line :underline t))))
	',`(vertical-border ((t (:inherit border))))
	',`(window-divider ((t (:inherit border))))
	',`(window-divider-first-pixel ((t (:inherit window-divider))))
	',`(window-divider-last-pixel ((t (:inherit window-divider))))
	',`(internal-border ((t (:inherit border))))
	',`(child-frame-border ((t (:inherit border))))
	',`(fringe ((t ())))
	',`(scroll-bar ((t (:foreground ,(atom-one--hsl-to-hex scrollbar-color-editor)
				      :background ,(atom-one--hsl-to-hex scrollbar-background-color-editor)))))
	',`(border ((t (:foreground ,(atom-one--hsl-to-hex base-border-color)
				  :background ,(atom-one--hsl-to-hex base-border-color)))))
	',`(tool-bar ((t (:background-color ,(atom-one--hsl-to-hex level-3-color)))))
	',`(tab-bar ((t (:foreground ,(atom-one--hsl-to-hex tab-text-color)
				   :background ,(atom-one--hsl-to-hex tab-background-color)))))
	',`(tab-line ((t (:inherit tab-bar))))
	',`(error ((t (:foreground ,(atom-one--hsl-to-hex text-color-error)))))
	',`(warning ((t (:foreground ,(atom-one--hsl-to-hex text-color-warning)))))
	',`(info ((t (:foreground ,(atom-one--hsl-to-hex text-color-info)))))
	',`(success ((t (:foreground ,(atom-one--hsl-to-hex text-color-success)))))
	',`(tab-line ((t (:background ,(atom-one--hsl-to-hex tab-bar-background-color)))))

	;; whitespace.el
	',`(whitespace-space ((t (:inherit glyphless-char))))
	',`(whitespace-hspace ((t (:inherit glyphless-char))))
	',`(whitespace-tab ((t (:inherit glyphless-char))))
	',`(whitespace-newline ((t (:inherit glyphless-char))))
	',`(whitespace-trailing ((t (:inherit glyphless-char))))
	',`(whitespace-line ((t nil)))
	',`(whitespace-space-before-tab ((t (:inherit glyphless-char))))
	',`(whitespace-indentation ((t (:inherit glyphless-char))))
	',`(whitespace-big-indent ((t (:inherit glyphless-char))))
	',`(whitespace-missing-newline-at-eof ((t (:inherit glyphless-char))))
	',`(whitespace-empty ((t (:inherit glyphless-char))))
	',`(whitespace-space-after-tab ((t (:inherit glyphless-char))))

	;; hl-line.el
	',`(hl-line ((t (:background ,(atom-one--hsl-to-hex syntax-cursor-line)))))

	;; isearch.el
	',`(isearch
	  ((t (:inherit lazy-highlight
			:box (:line-width 2
					  :color ,(atom-one--hsl-to-hex syntax-result-marker-color-selected))))))
	',`(lazy-highlight
	  ((t (:background ,(atom-one--hsl-to-hex syntax-result-marker-color)))))

	;; button.el
	',`(button ((t (:inherit underline))))

	;; company.el
	',`(company-tooltip
	  ((t (:foreground ,(atom-one--hsl-to-hex text-color)
			   :background ,(atom-one--hsl-to-hex overlay-background-color)))))
	',`(company-tooltip-selection
	  ((t (:background ,(atom-one--hsl-to-hex background-color-selected)))))
	',`(company-tooltip-search ((t (:inherit company-tooltip-common))))
	',`(company-tooltip-search-selection
	  ((t (:inherit company-tooltip-common-selection))))
	',`(company-tooltip-mouse ((t (:inherit company-tooltip))))
	',`(company-tooltip-common
	  ((t (:foreground ,(atom-one--hsl-to-hex text-color-highlight)
			   :weight bold))))
	',`(company-tooltip-common-selection
	  ((t (:foreground ,(atom-one--hsl-to-hex text-color-selected)
			   :weight bold))))
	',`(company-tooltip-annotation
	  ((t (:inherit company-tooltip
			:foreground ,(atom-one--hsl-to-hex text-color-subtle)))))
	',`(company-tooltip-annotation-selection
	  ((t (:inherit company-tooltip-selection
			:foreground ,(atom-one--hsl-to-hex (atom-one--mix text-color-selected
							      overlay-backdrop-color))))))
	',`(company-tooltip-scrollbar-thumb
	  ((t (:background ,(atom-one--hsl-to-hex scrollbar-color)))))
	',`(company-tooltip-scrollbar-track
	  ((t (:background ,(atom-one--hsl-to-hex scrollbar-background-color)))))
	',`(company-preview ((t (:inherit shadow))))
	',`(company-preview-common ((t (:inherit (shadow bold)))))

	;; company-posframe.el
	',`(company-posframe-quickhelp ((t :inherit company-tooltip)))
	',`(company-posframe-quickhelp-header
	  ((t :inherit header-line
              :background ,(atom-one--hsl-to-hex overlay-background-color))))

	;; font-lock.el
	',`(font-lock-warning-face ((t (:inherit warning))))
	',`(font-lock-function-name-face ((t (:foreground ,(atom-one--hsl-to-hex hue-2)))))
	',`(font-lock-function-call-face ((t (:foreground ,(atom-one--hsl-to-hex hue-2)))))
	',`(font-lock-variable-name-face ((t (:foreground ,(atom-one--hsl-to-hex hue-5)))))
	',`(font-lock-variable-use-face ((t (:foreground ,(atom-one--hsl-to-hex mono-1)))))
	',`(font-lock-keyword-face ((t (:foreground ,(atom-one--hsl-to-hex hue-3)))))
	',`(font-lock-comment-face ((t (:foreground ,(atom-one--hsl-to-hex mono-3)
						  :slant italic))))
	',`(font-lock-comment-delimiter-face ((t (:inherit font-lock-comment-face))))
	',`(font-lock-type-face ((t (:foreground ,(atom-one--hsl-to-hex hue-1)))))
	',`(font-lock-constant-face ((t (:foreground ,(atom-one--hsl-to-hex hue-6)))))
	',`(font-lock-builtin-face ((t (:foreground ,(atom-one--hsl-to-hex hue-3)))))
	',`(font-lock-preprocessor-face ((t (:inherit font-lock-keyword-face))))
	',`(font-lock-string-face ((t (:foreground ,(atom-one--hsl-to-hex hue-4)))))
	',`(font-lock-doc-face ((t (:inherit font-lock-comment-face))))
	',`(font-lock-doc-markup-face ((t (:foreground ,(atom-one--hsl-to-hex (atom-one--lighten mono-3 6))
						     :weight bold))))
	',`(font-lock-negation-char-face ((t (:foreground ,(atom-one--hsl-to-hex hue-1)))))
	',`(font-lock-regexp-face ((t (:foreground ,(atom-one--hsl-to-hex hue-1)))))
	',`(font-lock-regexp-grouping-backslash ((t (:foreground ,(atom-one--hsl-to-hex hue-1)))))
	',`(font-lock-regexp-grouping-construct ((t (:foreground ,(atom-one--hsl-to-hex hue-1)))))
	',`(font-lock-escape-face ((t (:foreground ,(atom-one--hsl-to-hex hue-1)))))
	',`(font-lock-number-face ((t (:inherit font-lock-constant-face))))
	',`(font-lock-operator-face ((t (:foreground ,(atom-one--hsl-to-hex hue-3)))))
	',`(font-lock-property-name-face ((t (:foreground ,(atom-one--hsl-to-hex hue-5)))))
	',`(font-lock-property-use-face ((t (:foreground ,(atom-one--hsl-to-hex mono-1)))))
	',`(font-lock-bracket-face ((t (:foreground ,(atom-one--hsl-to-hex mono-1)))))
	',`(font-lock-delimiter-face ((t (:foreground ,(atom-one--hsl-to-hex mono-1)))))
	',`(font-lock-misc-punctuation-face ((t (:foreground ,(atom-one--hsl-to-hex mono-1)))))
	',`(font-lock-punctuation-face ((t (:foreground ,(atom-one--hsl-to-hex mono-1)))))
	',`(font-lock-member-face ((t (:foreground ,(atom-one--hsl-to-hex hue-5)))))
	',`(font-lock-tag-face ((t (:foreground ,(atom-one--hsl-to-hex hue-5)))))
	',`(font-lock-attribute-face ((t (:foreground ,(atom-one--hsl-to-hex hue-6)))))

	;; hl-todo.el
	',`(hl-todo ((t (:foreground ,(atom-one--hsl-to-hex (atom-one--lighten mono-3 6))
				   :weight bold))))

	;; mmm-vars.el
	',`(mmm-default-submode-face ((t ())))

	;; org-faces.el
	',`(org-ellipsis ((t (:foreground ,(atom-one--hsl-to-hex syntax-gutter-text-color-selected)))))

	;; css-mode.el
	',`(css-selector ((t (:foreground ,(atom-one--hsl-to-hex syntax-color-function)))))
	',`(css-property ((t (:foreground ,(atom-one--hsl-to-hex mono-1)))))

	;; web-mode.el
	',`(web-mode-error-face ((t (:inherit error))))
	',`(web-mode-symbol-face ((t (:inherit font-lock-variable-name-face))))
	',`(web-mode-doctype-face ((t (:foreground ,(atom-one--hsl-to-hex mono-1)))))
	',`(web-mode-html-tag-face ((t (:inherit font-lock-tag-face))))
	',`(web-mode-html-tag-bracket-face ((t (:foreground ,(atom-one--hsl-to-hex mono-1)))))
	',`(web-mode-html-attr-name-face ((t (:inherit font-lock-attribute-face))))
	',`(web-mode-html-attr-engine-face
	  ((t (:inherit web-mode-html-attr-name-face))))
	',`(web-mode-css-selector-face ((t (:inherit css-selector))))
	',`(web-mode-css-pseudo-class-face
	  ((t (:inherit web-mode-css-selector-face))))
	',`(web-mode-css-property-name-face ((t (:inherit css-property))))
	',`(web-mode-css-color-face ((t (:foreground ,(atom-one--hsl-to-hex hue-6)))))
	',`(web-mode-css-priority-face ((t (:inherit font-lock-keyword-face))))
	',`(web-mode-css-function-face ((t (:inherit font-lock-function-name-face))))
	',`(web-mode-css-variable-face ((t (:foreground ,(atom-one--hsl-to-hex hue-5)))))

	;; ansi-color.el
	',`(ansi-color-black ((t :foreground ,(atom-one--hsl-to-hex mono-1) :background ,(atom-one--hsl-to-hex mono-1))))
	',`(ansi-color-red ((t :foreground ,(atom-one--hsl-to-hex hue-5-2) :background ,(atom-one--hsl-to-hex hue-5-2))))
	',`(ansi-color-green ((t :foreground ,(atom-one--hsl-to-hex hue-4) :background ,(atom-one--hsl-to-hex hue-4))))
	',`(ansi-color-yellow ((t :foreground ,(atom-one--hsl-to-hex hue-6) :background ,(atom-one--hsl-to-hex hue-6))))
	',`(ansi-color-blue ((t :foreground ,(atom-one--hsl-to-hex hue-2) :background ,(atom-one--hsl-to-hex hue-2))))
	',`(ansi-color-magenta ((t :foreground ,(atom-one--hsl-to-hex hue-3) :background ,(atom-one--hsl-to-hex hue-3))))
	',`(ansi-color-cyan ((t :foreground ,(atom-one--hsl-to-hex hue-1) :background ,(atom-one--hsl-to-hex hue-1))))
	',`(ansi-color-white ((t :foreground ,(atom-one--hsl-to-hex mono-3) :background ,(atom-one--hsl-to-hex mono-3))))
	',`(ansi-color-bright-black ((t :foreground ,(atom-one--hsl-to-hex mono-2) :background ,(atom-one--hsl-to-hex mono-2))))
	',`(ansi-color-bright-red ((t :foreground ,(atom-one--hsl-to-hex hue-5) :background ,(atom-one--hsl-to-hex hue-5))))
	',`(ansi-color-bright-green ((t :foreground ,(atom-one--hsl-to-hex hue-4) :background ,(atom-one--hsl-to-hex hue-4))))
	',`(ansi-color-bright-yellow ((t :foreground ,(atom-one--hsl-to-hex hue-6-2) :background ,(atom-one--hsl-to-hex hue-6-2))))
	',`(ansi-color-bright-blue ((t :foreground ,(atom-one--hsl-to-hex hue-2) :background ,(atom-one--hsl-to-hex hue-2))))
	',`(ansi-color-bright-magenta ((t :foreground ,(atom-one--hsl-to-hex hue-3) :background ,(atom-one--hsl-to-hex hue-3))))
	',`(ansi-color-bright-cyan ((t :foreground ,(atom-one--hsl-to-hex hue-1) :background ,(atom-one--hsl-to-hex hue-1))))
	',`(ansi-color-bright-white ((t :foreground ,(atom-one--hsl-to-hex syntax-bg) :background ,(atom-one--hsl-to-hex syntax-bg))))

	',`(font-latex-sectioning-5-face ((t :weight bold :inherit font-lock-type-face)))
	',`(font-latex-bold-face ((t :inherit bold)))
	',`(font-latex-italic-face ((t :inherit italic)))
	',`(font-latex-underline-face ((t :inherit underline)))
	',`(font-latex-math-face ((t :foreground ,(atom-one--hsl-to-hex hue-6))))
	',`(font-latex-sedate-face ((t :inherit bold)))
	',`(font-latex-string-face ((t :inherit font-lock-string-face)))
	',`(font-latex-warning-face ((t :inherit font-lock-keyword-face)))
	',`(font-latex-verbatim-face ((t :inherit tex-verbatim)))
	',`(font-latex-script-char-face ((t)))
	',`(font-latex-slide-title-face ((t :height 1.2 :inherit font-latex-sectioning-0-face)))

	;; popup.el
	',`(popup-face ((t (:foreground ,(atom-one--hsl-to-hex text-color)
				      :background ,(atom-one--hsl-to-hex overlay-background-color)))))
	',`(popup-summary-face ((t (:inherit popup-face
					   :foreground ,(atom-one--hsl-to-hex text-color-subtle)))))
	',`(popup-scroll-bar-foreground-face
	  ((t (:background ,(atom-one--hsl-to-hex scrollbar-color)))))
	',`(popup-scroll-bar-background-face
	  ((t (:background ,(atom-one--hsl-to-hex scrollbar-background-color)))))
	',`(popup-isearch-match ((t (:inherit isearch))))
	',`(popup-tip-face ((t (:inherit popup-face))))
	',`(popup-menu-face ((t (:inherit popup-face))))
	',`(popup-menu-mouse-face ((t nil)))
	',`(popup-menu-selection-face
	  ((t (:foreground ,(atom-one--hsl-to-hex text-color-selected)
			   :background ,(atom-one--hsl-to-hex (atom-one--mix text-color-highlight
								 overlay-background-color))))))
	',`(popup-menu-summary-face ((t (:inherit popup-summary-face))))

	;; tab-line.el
	',`(tab-line-tab ((t (:foreground ,(atom-one--hsl-to-hex tab-text-color-active)
					:background ,(atom-one--hsl-to-hex tab-background-color-active)))))
	',`(tab-line-tab-inactive ((t (:foreground ,(atom-one--hsl-to-hex tab-text-color)
						 :background ,(atom-one--hsl-to-hex tab-background-color)))))
	',`(tab-line-tab-inactive-alternate ((t (:inherit tab-line-tab-inactive))))
	',`(tab-line-tab-special ((t (:slant italic))))
	',`(tab-line-tab-modified ((t)))
	',`(tab-line-tab-group ((t)))
	',`(tab-line-tab-current ((t (:foreground ,(atom-one--hsl-to-hex tab-text-color-active)
						:background ,(atom-one--hsl-to-hex tab-background-color-active)))))
	',`(tab-line-highlight ((t)))
	',`(tab-line-close-highlight ((t (:foreground ,(atom-one--hsl-to-hex accent-text-color)
						    :background ,(atom-one--hsl-to-hex accent-color)))))

	;; compilation.el
	',`(compilation-info ((t (:inherit info))))

	;; flymake.el
	',`(flymake-error
	  ((t (:underline (:style wave
				  :color ,(atom-one--hsl-to-hex text-color-error))))))
	',`(flymake-warning
	  ((t (:underline (:style wave
				  :color ,(atom-one--hsl-to-hex text-color-warning))))))
	',`(flymake-note
	  ((t (:underline (:style wave
				  :color ,(atom-one--hsl-to-hex text-color-info))))))

	;; flymake-popon.el
	',`(flymake-popon
	  ((t (:foreground ,(atom-one--hsl-to-hex text-color)
			   :background ,(atom-one--hsl-to-hex overlay-background-color)))))
	',`(flymake-popon-posframe-border ((t (:foreground ,(atom-one--hsl-to-hex overlay-border-color)))))

	;; faces.el
	',`(mode-line ((t (:background ,(atom-one--hsl-to-hex level-3-color)))))
	',`(mode-line-inactive ((t (:foreground ,(atom-one--hsl-to-hex tab-text-color)))))
	',`(mode-line-highlight ((t (:background ,(atom-one--hsl-to-hex level-3-color-hover)))))
	',`(mode-line-buffer-id ((t (:inherit mode-line))))

	;; doom-modeline-core.el
	',`(doom-modeline-buffer-modified ((t (:foreground ,(atom-one--hsl-to-hex text-color-modified)))))
	',`(doom-modeline-info ((t (:foreground ,(atom-one--hsl-to-hex background-color-success)))))
	',`(doom-modeline-bar ((t (:background ,(atom-one--hsl-to-hex level-3-color)))))
	',`(doom-modeline-bar-inactive ((t (:background ,(face-background 'default)))))
	',`(doom-modeline-evil-emacs-state ((t (:foreground ,(face-foreground 'mode-line-inactive)))))
	',`(doom-modeline-evil-insert-state ((t (:foreground ,(atom-one--hsl-to-hex ui-site-color-1)))))
	',`(doom-modeline-evil-motion-state ((t (:foreground ,(atom-one--hsl-to-hex ui-site-color-3)))))
	',`(doom-modeline-evil-normal-state ((t (:foreground ,(atom-one--hsl-to-hex ui-site-color-2)))))
	',`(doom-modeline-evil-operator-state ((t (:foreground ,(atom-one--hsl-to-hex ui-site-color-3)))))
	',`(doom-modeline-evil-visual-state ((t (:foreground ,(atom-one--hsl-to-hex ui-site-color-4)))))
	',`(doom-modeline-lsp-running ((t (:inherit info :weight normal))))

	;; treesit-fold.el
	',`(treesit-fold-replacement-face ((t (:inherit default))))
	',`(treesit-fold-replacement-mouse-face ((t (:inherit default))))

	;; flyspell.el
	',`(flyspell-incorrect ((t (:underline (:style dashes :color ,(atom-one--hsl-to-hex text-color-error))))))
	',`(flyspell-duplicate ((t (:underline (:style dashes :color ,(atom-one--hsl-to-hex text-color-warning)))))))
       (custom-theme-set-variables
	,theme-name
	',`(menu-bar-mode nil)
	',`(tool-bar-mode nil)
	',`(frame-resize-pixelwise t)
	',`(org-ellipsis (nerd-icons-octicon "nf-oct-ellipsis"))
	',`(treesit-fold-replacement (nerd-icons-octicon "nf-oct-ellipsis"))
	',`(scroll-bar-adjust-thumb-portion nil)
	',`(treesit-font-lock-level 4)
	',`(hs-set-up-overlay 'atom-one--hs-setup-overlay)))))

(provide 'atom-one-lib)
;;; atom-one-lib.el ends here
