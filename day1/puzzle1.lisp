; Some variables we need access to:
(defvar depth-measurements)         ; the actual array of measurements
(defvar measurement-increase-count) ; for part 1, a count of the answer.
(defvar window-increase)            ; for part 2, a count of the answer. 
; We could re-use a variable to store the answer, but I like this for clarity. 

(defvar loop-iterator) ; generally useful, since I'm not using iter. 

; Read in the file. 
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
(set 'depth-measurements
  (with-open-file (stream "puzzle1.input")  
    (loop for line = (read-line stream nil)
      while line
      collect (parse-integer line))))
; Also note that we `defvar` before `set`. SBCL throws warnings for undefined
; variables, and other Common Lisp implementations may throw errors or cause
; undefined behavior. 


;
; PART 1
;
(set 'measurement-increase-count 0)
(set 'loop-iterator 0)

; I want to do something once for each entry in depth-measurements, but since
; I don't actually need the item from the list (since we're accessing the list
; directly by index), I can just `loop for nil` instead of `loop for measurement`.
(loop for nil in depth-measurements do

      ; skip the first element
      (if (eql 0 loop-iterator)() 
	; if this element is larger than the last one...
	(if (> (nth loop-iterator depth-measurements) (nth (- loop-iterator 1) depth-measurements))
	  ; ... increase our count by one. 
	  (set 'measurement-increase-count (+ measurement-increase-count 1))))

      ; iterate
      (set 'loop-iterator (+ 1 loop-iterator)))

(format T "~d measurements are larger than the previous measurement." measurement-increase-count)(terpri)

; misc notes
; terpri is AKA terminate printing - it flushes the output buffer and prints a new line. 
; prog1 and progn are useful for running many commands, and only returning the value of
;     one of the commands. I suspect there are more "lisp-y" ways of doing the things I 
;     want to do, but this is useful to get some work done in a familiar way. 

;
; PART 2
;

; measure a window of size 3, starting at index idx. So, we're looking forward. 
(defun measure-window (idx)
  ; for the second-to-last and last measurements, we don't have enough data 
  ; to measure a window. So, skip those (+ 0 0)
  (if (>= idx 1998)(+ 0 0)
    ; measure the window
    (+ 
      (nth    idx    depth-measurements)
      (nth (+ idx 1) depth-measurements)
      (nth (+ idx 2) depth-measurements))))

(set 'window-increase 0)
(set 'loop-iterator 0)
(loop for measurement in depth-measurements do
      ; I can use `let` here, since these variables are actually internal to
      ; the loop. I need to use `set` for loop-iterator, because it needs to
      ; persist from loop to loop. 
      (let ((this-window (measure-window    loop-iterator))
	    (next-window (measure-window (+ loop-iterator 1))))
        (if (> next-window this-window)
	  (set 'window-increase (+ 1 window-increase))))

      (set 'loop-iterator (+ 1 loop-iterator)))

; TODO: this returns 1310 instead of the correct answer, 1311. why? skipping a measurement?
; Found that, in measure-window, I was skipping any measurement that involved 
; the first two indexes, thinking that we couldn't measure any window that used
; those indexes. Of course, that's wrong - we can't take a window with just the
; first measurement or two, but we can take a window that involves those 
; measurements.
(format T "~d window measurements are larger than the previous window." window-increase)(terpri)

