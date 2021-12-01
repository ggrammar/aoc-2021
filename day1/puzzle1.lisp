; https://stackoverflow.com/questions/13359025/adding-to-the-end-of-list-in-lisp
; has some discussion on how these operators work
(set 'depth-measurements
     ; I want to run many commands, and return the result of the first one (dm)
     (prog1
       (set 'dm (list()))
       (set 'infile (open "puzzle1.input"))
         (when infile
	   (loop for line = (read-line infile nil)
		 while line do (push (parse-integer line) (cdr (last dm)))))))

; I created depth-measurements as list(), AKA nil. I created the list of measurements
; by appending each line of input, so we still need to remove the first element (nil)
; from the list. 
(set 'depth-measurements (remove (nth 0 depth-measurements) depth-measurements))

; We need to track the number of measurement increases. 
(set 'measurement-increase-count 0)

(loop for measurement in depth-measurements do

      ; skip the first element. this may not correctly handle the situation where
      ; a future measurement is the same as the first measurement, depending on how
      ; the lisp handles equality
      (if (eq measurement (nth 0 depth-measurements))
	(progn
	  (format T "this is the first measurement, so we skip comparison")(terpri))

	(if (> measurement previous-measurement)
	  (progn
	    (format T "measurement ~d > previous-measurement ~d" measurement previous-measurement)
	    (terpri)
	    (set 'measurement-increase-count (+ measurement-increase-count 1)))
	  (progn
	    (format T "measurement ~d =< previous-measurement ~d" measurement previous-measurement)
	    (terpri))
	)
      )

      ; save this element for future processing
      (set 'previous-measurement measurement)
      (format T "previous-measurement set to ~d" previous-measurement)(terpri)
)

(write measurement-increase-count)(terpri)

; misc notes
; terpri is AKA terminate printing - it flushes the output buffer and prints a new line. 
; prog1 and progn are useful for running many commands, and only returning the value of
;     one of the commands. I suspect there are more "lisp-y" ways of doing the things I 
;     want to do, but this is useful to get some work done in a familiar way. 
