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
(set 'previous-measurement 0)

(loop for measurement in depth-measurements do

      ; skip the first element. this may not correctly handle the situation where
      ; a future measurement is the same as the first measurement, depending on how
      ; the lisp handles equality
      (if (eq measurement (nth 0 depth-measurements))() ;do nothing
	(if (> measurement previous-measurement)
	  (set 'measurement-increase-count (+ measurement-increase-count 1))))

      ; save this element for future processing
      (set 'previous-measurement measurement)
      ; (format T "previous-measurement set to ~d" previous-measurement)(terpri)
)

(write measurement-increase-count)(terpri)

; misc notes
; terpri is AKA terminate printing - it flushes the output buffer and prints a new line. 
; prog1 and progn are useful for running many commands, and only returning the value of
;     one of the commands. I suspect there are more "lisp-y" ways of doing the things I 
;     want to do, but this is useful to get some work done in a familiar way. 

(defun measure-window (idx)
  ; for the 0th and 1st measurements, we don't have enough data to create a window.
  (if (<= idx 1)(+ 0 0)
    ; for the second-to-last and last measurements, we also don't have enough data. 
    (if (>= idx 1998)(+ 0 0)
      ; measure the window
      (+ 
	(nth    idx    depth-measurements)
	(nth (+ idx 1) depth-measurements)
	(nth (+ idx 2) depth-measurements)))))

(set 'loop-iterator 0)
(set 'window-increase 0)
(loop for measurement in depth-measurements do
      (set 'this-window (measure-window loop-iterator))
      (set 'next-window (measure-window (+ loop-iterator 1)))
      (if (> next-window this-window)
	(progn
	  ; (format T "~d > ~d" next-window this-window)(terpri)
	  (set 'window-increase (+ 1 window-increase))))

      (set 'loop-iterator (+ 1 loop-iterator))
)
; TODO: this returns 1310 instead of the correct answer, 1311. why? skipping a measurement?
(write window-increase)























