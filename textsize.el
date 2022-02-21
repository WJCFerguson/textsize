;;; textsize.el --- Configure frame text size automatically  -*- lexical-binding: t; -*-
;;
;; Copyright (C) James Ferguson
;;
;; Author: James Ferguson <james@faff.org>
;; Version: 2.0
;; Package-Requires: ((emacs "26.1"))
;; Keywords: convenience
;; URL: https://github.com/WJCFerguson/textsize
;;
;; This file is not part of GNU Emacs.
;;
;;; License:
;;
;; Licensed under the same terms as Emacs.
;;
;;; Commentary:
;;
;; This package automatically calculates and adjusts the default text size for
;; the size and pixel pitch of the display.
;;
;; Hooks to perform the adjustment automatically are set up by calling
;; `textsize-setup' on initialization.  e.g.:
;;
;;     (use-package textsize
;;      :commands (textsize-setup)
;;      :init (textsize-setup))
;;
;; The adjustment may also be manually triggered by calling
;; `textsize-fix-frame'.
;;
;; You will first want to adjust `textsize-default-points' if the default does
;; not produce your preferred size.
;;
;; The calculation is very simplistic but should be adaptable to many scenarios.
;; The parameters for generation of the text size are in the -thresholds
;; customizations.
;;
;; You may wish to bind keys for manual adjustment with `textsize-increment',
;; `textsize-decrement', and `textsize-reset'
;;
;;; Code:


;; =============================================================================
(defgroup textsize nil
  "Automatically adjusting frame text sizes to suit the current display."
  :group 'convenience)

(defcustom textsize-default-points 15
  "The baseline font point size, to then be adjusted with -thresholds values.

Some fonts at least appear to render better at point sizes that
are multiples of 3."
  :type 'integer)

(defcustom textsize-monitor-size-thresholds '((0 . -3) (350 . 0) (500 . 3))
  "Point size offsets from the maximum monitor dimension in mm.

List of pairs of (monitor-size-in-mm . font-point-offset).

The 2nd value (cdr) of the final cell encountered where the 1st
value (car) is <= the monitor size in px, will be used as a
font point offset.  Thresholds should therefore be sorted in
rising order.

The default of ((500 . 3)) is to enlarge the font for most
non-laptop screens, and then pixel pitch adjustment should handle
issues with very large external (typically TV) screens."
  :type '(list (cons integer integer)))

(defcustom textsize-pixel-pitch-thresholds '((0 . 3) (0.12 . 0) (0.18 . -3))
  "List of (px-pitch-threshold . font-point-offset).

As with `textsize-monitor-size-thresholds', an offset will be
selected from the monitor's pixel pitch in mm."
  :type '(list (cons integer integer)))

;; =============================================================================
(defun textsize--monitor-size-mm (frame)
  "Return the max dimension of FRAME's monitor in mm."
  (apply 'max (frame-monitor-attribute 'mm-size frame)))


(defun textsize--monitor-size-px (frame)
  "Return the max dimension of FRAME's monitor in pixels."
  (apply 'max (cddr (frame-monitor-attribute 'geometry frame))))


(defun textsize--pixel-pitch (frame)
  "Calculate the pixel pitch for FRAME in mm."
  ;; On a rotated monitor, px geom is rotated but size is not, so use
  ;; max dimension of each.
  (/ (float (textsize--monitor-size-mm frame))
     (textsize--monitor-size-px frame)))


(defun textsize--threshold-offset (threshold-list val)
  "Find the offset for VAL in THRESHOLD-LIST."
  (let ((result 0))
    (dolist (threshold threshold-list)
      (when (>= val (car threshold))
        (setq result (cdr threshold))))
    result))

(defun textsize--point-size (frame)
  "Return the point size to use for this FRAME."
  (+ textsize-default-points
     ;; manual adjustment:
     (or (frame-parameter frame 'textsize-manual-adjustment) 0)
     ;; pixel pitch adjustment:
     (textsize--threshold-offset textsize-pixel-pitch-thresholds
                                 (textsize--pixel-pitch frame))
     ;; monitor size in mm adjustment:
     (textsize--threshold-offset textsize-monitor-size-thresholds
                                 (textsize--monitor-size-mm frame))))

(defun textsize--window-size-change (window-or-frame)
  "Defun for `window-size-change-functions' to fix WINDOW-OR-FRAME text size."
  (when (and (framep window-or-frame) (frame-size-changed-p window-or-frame))
    (textsize-fix-frame window-or-frame)))

;;;###autoload
(defun textsize-modify-manual-adjust (frame offset)
  "Adjust FRAME's font-point adjustment by OFFSET persistently.

Add a custom fixed offset to the textsize point size calculation.

If OFFSET is nil, reset adjustment to zero."
  (set-frame-parameter
   frame
   'textsize-manual-adjustment
   (if offset
       (+ offset (or (frame-parameter frame 'textsize-manual-adjustment) 0))
     0))
  (message "Setting default font to %s points" (textsize--point-size frame))
  (textsize-fix-frame frame))

;;;###autoload
(defun textsize-increment ()
  "Increment the current frame's automatic text size."
  (interactive)
  (textsize-modify-manual-adjust (selected-frame) 1))

;;;###autoload
(defun textsize-decrement ()
  "Decrement the current frame's automatic text size."
  (interactive)
  (textsize-modify-manual-adjust (selected-frame) -1))

;;;###autoload
(defun textsize-reset ()
  "Reset the adjustment on the current frame's automatic text size."
  (interactive)
  (textsize-modify-manual-adjust (selected-frame) nil))

;;;###autoload
(defun textsize-fix-frame (&optional frame)
  "Set the default text size appropriately for FRAME display."
  (interactive)
  (when (display-graphic-p)
    (let* ((frame (or frame (selected-frame))))
      (set-frame-parameter frame
                           'font
                           (format "%s-%d"
                                   (face-attribute 'default :family)
                                   (textsize--point-size frame))))))

;;;###autoload
(defun textsize-setup ()
  "Make textsize adjustment happen automatically on frame size change."
  (add-to-list 'window-size-change-functions #'textsize--window-size-change))

(provide 'textsize)
;;; textsize.el ends here
