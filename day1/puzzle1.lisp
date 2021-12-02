; First, read in the file. 
; 
; I had previously been using a concoction with open, push, cdr, and last
; to gather the input values and append them to the depth-measurements list. 
; 
; This is a shorter way of reading a file into a list. Some notes:
;  - `with-open-file` is a CL macro, http://clhs.lisp.se/Body/m_w_open.htm
;    It uses `open` under the hood, and returns whatever we `collect`. 
;  - stream is a variable named stream, that is also a stream object
;  - collect is one of several accumulators, special loop clauses built
;    specifically for gathering values in a loop (that we later return) - see
;    https://www.cs.cmu.edu/Groups/AI/html/cltl/clm/node246.html
;  - I don't have to specifically create a list to append values to, so I 
;    don't have to go back later to pop the first `nil` off the list. The
;    list I want is created as a return value from `with-open-file`. 
(defvar depth-measurements)
(set 'depth-measurements
  (with-open-file (stream "puzzle1.input")  
    (loop for line = (read-line stream nil)
      while line
      collect (parse-integer line))))
; Also note that we `defvar` before `set`. SBCL throws warnings for undefined
; variables, and other Common Lisp implementations may throw errors or cause
; undefined behavior. 


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























