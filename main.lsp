;; * main

(in-package :spear-to-midi)

(defun spear-to-midi (input-file
		      &optional
			(output-file "output.mid")
			(velocity-skew 1)
			(vel-range '(0.0 1.0))
			(average-partials t)
			(bpm 60))
  ;; Set beat resolution: Pulses (ticks) per quarter note. Normally 480.
  (let ((file (get-pathname-dir input-file output-file))
	(lines '())
	(partials '())
	(events '()))
    (format t "~&--- SPEAR to MIDI ---")
    (format t "~&Reading file...")
    (with-open-file (f input-file :direction :input)
      (setf lines (loop for line = (read-line f nil nil)
                        while line
			;; Data lines are 57 characters or longer.
                        when (>= (length line) 57) collect line)))
    ;; get partial-data from lines
    (format t "~&Getting Partial Data...")
    (loop for line in lines
	  do (push (process-line line) partials))
    ;; loop through partials
    (if (null partials)
	(format t "~&No partial data found in ~a!" input-file)
	(setf events (partials-to-events partials average-partials velocity-skew vel-range)))
    ;; Call to lists-to-midi
    (format t "~&Generating Midi File...")
    (let ((notes '())
	  (durs '())
	  (starts '())
	  (vels '()))
      (loop for (note dur start vel) in events
	    do (push note notes)
	       (push dur durs)
	       (push start starts)
	       (push vel vels))
      (lists-to-midi notes durs starts
		     :velocity-list vels
		     :tempo bpm
		     :file file))
    (format t "~&Done")
    file))

;; EOF spear-to-midi.lsp
