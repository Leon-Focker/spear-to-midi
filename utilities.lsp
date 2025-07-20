;; * helpers.lsp

(in-package :spear-to-midi)

(require :cl-ppcre)
(require :cm)

;; ** converting partial timepoints into events

(defun partials-to-events (partials average &optional (power 1) (vel-range '(0 1)))
  (let ((min-amp (third (caar partials)))
	(max-amp (third (caar partials)))
	(events '()))
    (flet ((scale-velocity (val)
	     (max (expt (rescale val min-amp max-amp 0 1) power) (float 1/127))))
      (format t "~&Processing Partial Data...")
      ;; Convert partials to list of midi-like events (with absolute timings).
      ;; Calculates mean values of pitch and velocity for each event.
      (if average
	  (progn
	    (loop for partial in partials
		  for avg = (get-average-freq-amp partial)
		  ;; Midi Number
		  for avg-freq = (first avg)
		  for note = (frequency-to-midi avg-freq)
		  for vel = (second avg)
		  for start = (caar partial)
		  for dur = (- (caar (last partial)) start)
		  do (when (< vel min-amp) (setf min-amp vel))
		     (when (> vel max-amp) (setf max-amp vel))
		     (push (list note start vel dur) events))
	    ;; scale velocities
	    (loop for (note start vel dur) in events
		  for new-vel = (scale-velocity vel)
		  when (<= (first vel-range) new-vel (second vel-range))
		    collect (list note start new-vel dur)))
	  ;; when we want all segments of the parials, not just their average:
	  (progn
	    ;; convert time-points to events
	    (loop for partial in partials 
		  do (loop for (point1 point2) on partial while point2
			   for start = (first point1)
			   for dur = (- (first point2) start)
			   for note = (frequency-to-midi (/ (+ (second point1) (second point2)) 2))
			   for vel = (/ (+ (third point1) (third point2)) 2)
			   do (when (< vel min-amp) (setf min-amp vel))
			      (when (> vel max-amp) (setf max-amp vel))
			      (push (list note start vel dur) events)))
	    ;; scale velocities
	    ;; TODO would be nice to string together events with same note and amp that touch,
	    ;; but for now that's somethings FL Studio can do...
	    (loop for (note start vel dur) in events
		  for new-vel = (scale-velocity vel)
		  when (<= (first vel-range) new-vel (second vel-range))
		    collect (list note start new-vel dur)))))))

;; ** from the Python Mido Library

;; (defun second2tick (seconds ticks-per-beat tempo)
;;   "Convert absolute time in seconds to ticks.

;;    Returns absolute time in ticks for a chosen MIDI file time resolution
;;    (ticks/pulses per quarter note, also called PPQN) and tempo (microseconds
;;    per quarter note). Normal rounding applies."
;;   (let ((scale (/ (/ tempo 1000000) ticks-per-beat)))
;;     (round (/ seconds scale))))

;; (defun bpm2tempo (bpm &optional (time-sig '(4 4)))
;;     "Convert BPM (beats per minute) to MIDI file tempo (microseconds per
;;     quarter note).

;;     Depending on the chosen time signature a bar contains a different number of
;;     beats. These beats are multiples/fractions of a quarter note, thus the
;;     returned BPM depend on the time signature. Normal rounding applies.
;;     "
;;   (round (/ (* (/ (* 60 1000000) bpm) (second time-sig) 4))))

;; ** inspired by stephenjbradshaw's SPEAR-to-MIDI 

(defun frequency-to-midi (frequency)
  (round (+ (* 12 (log (/ frequency 440) 2)) 69)))

;; (defun convert-to-delta (midi-events ppqn tempo)
;;   "replacing absolute time values (in seconds) with delta time values between events
;;    (in ticks) midi_events is pre-sorted by absolute time (index [1] of each inner list)."
;;   (loop for event in midi-events
;; 	with prev-time = 0
;; 	for curr-time = (second event)
;; 	for delta-time = (- curr-time prev-time)
;;         for delta-ticks = (second2tick delta-time ppqn tempo)
;; 	collect event))

(defun process-line (line)
  "Parses line, a string of time, frequency, and amplitude values,
   and returns a list of lists of floats:
   ((time freq amp) ...)
   Each sublist represents a timepoint in a partial."
  (let ((elements (ppcre:split "\\s" (ppcre:regex-replace-all "," line "."))))
    (loop for (time freq amp) on elements by #'cdddr while amp
	  collect (list (read-from-string time)
			(read-from-string freq)
			(read-from-string amp)))))

(defun sublists-index-avg (ls index)
  "Returns the mean of all values at position INDEX in sublists of LST.
   Example:
   (sublists-index-avg '((1 2 3) (2 3 4)) 2) => 3.5"
  (let ((sum 0.0))
    (dolist (item ls)
      (incf sum (nth index item)))
    (/ sum (length ls))))

(defun get-average-freq-amp (partial)
  "Returns the average frequency and amplitude of a partial (list of sublists,
   where the second and third elements are the frequency and amplitude)."
  (loop for (a b c) in partial
	sum b into freq
	sum c into amp
	finally
	   (return (list (/ freq (length partial))
			 (/ amp (length partial))))))

;; ** copied from Michael Edwards' Slippery Chicken:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* utilities/rescale
;;; DATE
;;; June 8th 2016, Edinburgh
;;; 
;;; DESCRIPTION
;;; Given a value within an original range, return its value withing a new range
;;; 
;;; ARGUMENTS
;;; - the value we want to rescale
;;; - the original minimum
;;; - the original maximum
;;; - the new minimum
;;; - the new maximum
;;;
;;; OPTIONAL ARGUMENTS
;;; keyword arguments:
;;; :out-of-range. The function to call when the first argument is not within
;;; the range of arguments two and three. This would normally be #'error,
;;; #'warn or NIL. If #'warn or NIL, argument 1 will be hard-limited to the
;;; original range. Default = #'error
;;; :type-of-result. Usually this function uses float precision, but by setting
;;; type-of-result to #'double-float or #'rationalize, it is more precise.
;;; 
;;; RETURN VALUE
;;; The value within the new range (a number)
;;; 
;;; EXAMPLE
#|
(rescale .5 0 1 0 100)
==> 50.0
|#
;;; SYNOPSIS
(defun rescale (val min max new-min new-max &optional (out-of-range #'error)
                                              (type-of-result #'float))
;;; ****
  (flet ((oor () ; in case we need to call it on more than one occasion...
           (when (functionp out-of-range)
             (funcall out-of-range
                      "utilities::rescale: first argument (~a) should be ~
                       within the ~%original range (~a to ~a)" val min max))))
    (when (or (>= min max)
              (>= new-min new-max))
      (error "utilities::rescale: argument 2 (~a) must be < argument 3 (~a) ~
              ~%and sim. for argument 4 (~a) and 5 (~a)"
             min max new-min new-max))
    (unless (and (>= val min)
                 (<= val max))
      (oor)
      (setf val (if (> val max) max min)))
    (let* ((range1 (funcall type-of-result (- max min)))
           (range2 (funcall type-of-result (- new-max new-min)))
           (prop (funcall type-of-result (/ (- val min) range1))))
      (+ new-min (* prop range2)))))

(defun force-list (thing)
  ;; MDE Thu Oct 1 16:47:36 2015 -- added (when thing ...) so that we don't
  ;; force NIL into a list
  (when thing
    (if (listp thing)
        thing
        (list thing))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Returns whether all the elements in the list are atoms (i.e. no
;;; sublists). NB passing NIL will result in T 
(defun simple-listp (list)
  (when (listp list)
    (loop for i in list unless (atom i) do (return nil) finally (return t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* utilities/almost-flatten
;;; DATE
;;; September 4th 2013
;;; 
;;; DESCRIPTION
;;; Similar to flatten but allows one level of nesting
;;; 
;;; ARGUMENTS
;;; A list with an arbitrary level of nesting.
;;; 
;;; 
;;; RETURN VALUE
;;; A list with a maximum of one level of nesting
;;; 
;;; EXAMPLE
#|

(almost-flatten '((1 (2 3 4) (5 (6 7) (8 9 10 (11) 12)) 13) 14 15 (16 17)))
=> (1 (2 3 4) 5 (6 7) 8 9 10 (11) (12) (13) 14 15 (16 17))

|#
;;; SYNOPSIS
(defun almost-flatten (nested-list)
;;; ****
;;; ****
  (cond ((null nested-list) nil)
        ((or (simple-listp nested-list) (atom nested-list)) (list nested-list))
        (t (append (almost-flatten (first nested-list))
                   (almost-flatten (rest nested-list))))))

;; ** midi

;;; events being a list of lists with 4 values: (note start dur velocity)
(defun events-to-midi (events
		       &key
			 (tempo 60)
			 (channel 0)
			 (file (spear-to-midi-path "midi-output.mid")))
  (let ((midi-events '()))
    (setf midi-events
	  ;; this sort is important
	  (loop for event in (sort events #'(lambda (x y) (< (second x) (second y))))
		appending
		(cm::output-midi-note
		 (pop event)		; note
		 0
		 (pop event)		; start
		 (pop event)		; velo
		 (pop event)		; duration
		 channel)))             ; channel
    (cm::events
     (cm::new cm::seq :name (gensym) :time 0.0 :subobjects midi-events)
     file
     :tempo tempo)))

;; EOF helpers.lsp
