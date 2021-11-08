;;; textsize.el --- Configure frame text size automatically
;;
;; Copyright (C) James Ferguson
;;
;; Author: James Ferguson <james@faff.org>
;; Version: 2.0
;; Package-Requires: ((emacs "24.4"))
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
;; This package attempts to set the default face's point size based on monitor
;; resolution and/or pixel pitch.  See customization items.
;;
;; Set up automatic adjustment by calling `textsize-setup' on initialization.
;; e.g.:
;;
;;     (use-package textsize
;;      :init (textsize-setup))
;;
;; The adjustment may be manually initiated with `textsize-fix-frame'.
;;
;; You may wish to configure manual adjustment to the text size by binding
;; `textsize-increment', `textsize-decrement', and `textsize-reset'
;;
;; If you find it lacking, let me know how you think it should be improved
;; (github issues).  Need more criteria for adjustment?  Frame width?
;;
;;; Code:


;; =============================================================================
(defgroup textsize nil
  "Setting up a frame font sizes to suit the current display."
  :group 'convenience)

(defcustom textsize-default-points 15
  "The baseline font point size, to then be adjusted with -thresholds values.

Some fonts at least appear to render better at point sizes that
are multiples of 3."
  :type 'string)

(defcustom textsize-monitor-size-thresholds '((500 . 3))
  "Point size offsets from the maximum monitor dimension in mm.

List of pairs of (monitor-size-in-pixels . font-point-offset).

The 2nd value (cdr) of the final cell encountered where the 1st
value (car) is <= the monitor size in px, will be used as a
font point offset.  Thresholds should therefore be sorted in
rising order.

The default of ((500 . 3)) is to enlarge the font for most
non-laptop screens, and then pixel pitch adjustment should handle
issues with very large external (typically TV) screens.
")

(defcustom textsize-pixel-pitch-thresholds '((0 . 3) (0.12 . 0) (0.18 . -3))
  "List of (px-pitch-threshold . font-point-offset).

As with `textsize-monitor-size-thresholds', an offset will be
selected from the monitor's pixel pitch.
")

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
  "Return the point size to use for this frame."
  (+ textsize-default-points
     ;; manual adjustment:
     (or (frame-parameter frame 'textsize-fixed-adjustment) 0)
     ;; pixel pitch adjustment:
     (textsize--threshold-offset textsize-pixel-pitch-thresholds
                                 (textsize--pixel-pitch frame))
     ;; monitor size in px adjustment:
     (textsize--threshold-offset textsize-monitor-size-thresholds
                                 (textsize--monitor-size-mm frame))))

;;;###autoload
(defun textsize-modify-manual-adjust (frame offset)
  "Adjust FRAME's font-point adjustment by OFFSET persistently.

Add a custom fixed offset to the textsize point size calculation.

If OFFSET is nil, reset adjustment to zero."
  (set-frame-parameter
   frame
   'textsize-fixed-adjustment
   (if offset
       (+ offset (or (frame-parameter frame 'textsize-fixed-adjustment) 0))
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
  "Set the default text size appropriately for the window display."
  (interactive)
  (when (display-graphic-p)
    (let* ((frame (or frame (selected-frame)))
           (monitor-size-px (textsize--monitor-size-px frame)))
      (set-frame-parameter frame
                           'font
                           (format "%s-%d"
                                   (face-attribute 'default :family)
                                   (textsize--point-size frame))))))

;;;###autoload
(defun textsize-window-size-change (window-or-frame)
  "Function for `window-size-change-functions' to fix the frame text size."
  (when (and (framep window-or-frame) (frame-size-changed-p window-or-frame))
    (textsize-fix-frame window-or-frame)))

;;;###autoload
(defun textsize-setup ()
  "Make textsize adjustment happen automatically on frame size change."
  (add-to-list #'window-size-change-functions #'textsize-window-size-change))

(provide 'textsize)
;;; textsize.el ends here
