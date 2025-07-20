;; * helpers.lsp

(in-package :spear-to-midi)

(require :cl-ppcre)
(require :cm)

;; ** converting partial timepoints into events

(defun partials-to-events (partials average &optional (power 1) (vel-range '(0 1)))
  (let ((min-amp (third (caar partials)))
	(max-amp (third (caar partials))))
    (flet ((scale-velocity (val)
	     (max (expt (rescale val min-amp max-amp 0 1) power) (float 1/127))))
      (format t "~&Processing Partial Data...")
      ;; Convert partials to list of midi-like events (with absolute timings).
      ;; Calculates mean values of pitch and velocity for each event.
      (if average
	  (progn
	    ;; Get min and max Amplitude
	    (loop for partial in partials
		  for avg-amp = (sublists-index-avg partial 2)
		  minimize avg-amp into min
		  maximize avg-amp into max
		  finally (setf min-amp min max-amp max))
	
	    (loop for partial in partials
		  ;; Midi Number
		  for avg-freq = (sublists-index-avg partial 1)
		  for note = (frequency-to-midi avg-freq)
		  for vel = (scale-velocity (sublists-index-avg partial 2))
		  for start = (caar partial)
		  for dur = (- (caar (last partial)) start)
		  when (<= (first vel-range) vel (second vel-range))
		    collect (list note dur start vel)))
	  ;; when we want all segments of the parials, not just their average:
	  (progn
	    (let ((events '()))
	      ;; convert time-points to events
	      (loop for partial in partials 
		    do (loop for (point1 point2) on partial while point2
			     for start = (first point1)
			     for dur = (- (first point2) start)
			     for note = (frequency-to-midi (/ (+ (second point1) (second point2)) 2))
			     for amp = (/ (+ (third point1) (third point2)) 2)
			     do (when (< amp min-amp) (setf min-amp amp))
				(when (> amp max-amp) (setf max-amp amp))
				(push (list note dur start amp) events)))
	      ;; TODO would be nice to string together events with same note and amp that touch,
	      ;; but for now that's somethings FL Studio can do...
	      (loop for (note dur start vel) in events
		    for new-vel = (scale-velocity vel)
		    when (<= (first vel-range) new-vel (second vel-range))
		      collect (list note dur start new-vel))))))))

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

;; ** from my :layers Library

;; *** get-start-times
(defun get-start-times (list-of-durations)
  (loop for i in list-of-durations
     and sum = 0 then (+ sum i)
     collect sum))

;; *** lists-to-midi
;;; generate a midi file from lists of starting points, length, pitch...
;;; if one list is shorter than others it will be wrapped (mod).
;;; Has a lot less features than slippery chickens event-list-to-midi-file, but
;;; in return it's a lot easier and skipps the generation of an event, which
;;; might be favorable in some cases.
;;; pitch-list - A list of either sc-pitches or midi-key-numbers. Can also be a
;;;  list of chords, ie. lists of pitches.

(defmacro lists-to-midi-aux (note)
  ``(,(if (numberp ,note) ,note (error "lists-to-midi: Not a midi note: ~a" ,note))
     ,(nth (mod i start-len) start-time-list)
     ,(nth (mod i velo-len) velo)
     ,(nth (mod i duration-len) duration-list)
     ,(nth (mod i chan-len) channel)))

(defun lists-to-midi (note-list duration-list start-time-list
		      &key
			velocity-list
			(tempo 60)
			(channel '(0))
			(file (spear-to-midi-path "midi-output.mid")))
  (when (or (null note-list) (null duration-list))
    (error "please provide at least one value in the note and the duration ~
            lists in lists-to-midi"))
  (unless start-time-list (setf start-time-list
				(get-start-times duration-list)))
  (setf channel (force-list channel))
  (let* ((note-len (length note-list))
	 (duration-len (length duration-list))
	 (start-len (length start-time-list))
	 (velo (or velocity-list '(0.7)))
	 (velo-len (length velo))
	 (chan-len (length channel))
	 (total (apply #'max `(,note-len ,duration-len ,start-len ,velo-len)))
	 (events '()))
    (setf events
	  (sort (loop for i below total
		      for note = (nth (mod i note-len) note-list)
		      collect (lists-to-midi-aux
			       (if (listp note) (car note) note))
		      when (listp note)
			append (loop for p in (cdr note)
				     collect (lists-to-midi-aux p)))
		#'(lambda (x y) (< (second x) (second y))))
	  events (loop for event in events
		       appending
		       (cm::output-midi-note
			(pop event)
			0
			(pop event)
			(pop event)
			(pop event)
			(pop event))))
    (cm::events
     (cm::new cm::seq :name (gensym) :time 0.0 :subobjects events)
     file
     :tempo tempo)))

;; EOF helpers.lsp
