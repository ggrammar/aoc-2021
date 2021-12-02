; the "asdf" package is where the uiop:split-string method comes from.
; I'm still not sure how the common lisp/sbcl package system works, so
; I arrived at this and `uiop:split-string` after some trial and error.
(require "asdf")

; Utility - both parts need to parse individual space-separated instructions.
; I originally had both parts iterating through all instructions and doing the
; parsing on their own, but it makes more sense to parse the file while we're
; reading it in. 
; https://stackoverflow.com/questions/37639171/how-to-handle-multiple-returns-in-common-lisp/37639240
(defun parse-instruction (instruction)
  (let ((instruction-list (uiop:split-string instruction :separator " ")))
    (list 
      (nth 0 instruction-list)
      (parse-integer (nth 1 instruction-list)))))

(defvar instructions)
(set 'instructions
  (with-open-file (stream "puzzle2.input")
    (loop for line = (read-line stream nil)
      while line
      collect (parse-instruction line))))

; part 1
; I was previously defining and setting horizontal-position and depth outside
; of the function, then using `set` to increase the values as appropriate. When
; I moved to binding the variables with `let`, I found that `set` no longer 
; worked to change the value. Instead, I can use incf and decf to increment and
; decrement the value of a "place". I'm not sure I really understand this, so
; from CLHS:
;   place n. 1. a form which is suitable for use as a generalized reference
;            2. the conceptual location referred to by such a place
;   generalized reference n. a reference to a location storing an object as if
;     to a variable. (Such a reference can be either to read or write the 
;     location.) 
; Then, from CLHS 5.1.1:
;   A generalized reference is the use of a form, sometimes called a place, as
;     if it were a variable that could be read and written. The value of a 
;     place is the object to which the place form evaluates. The value of a
;     place can be changed by using setf. 
; So, I guess I could have changed my `set` calls to `setf` calls, but the new
; implementation with incf/decf is cleaner anyway. 


; set variables for both part 1 and part 2, so we only iterate once. 
;       part 1
(let ((horizontal-position-1 0)
      (depth-1 0)
      ; part 2
      (horizontal-position-2 0)
      (depth-2 0)
      (aim-2 0))

  (loop for instruction in instructions do
    ; parse out the name and velocity of the instruction
    (let ((instruction-string (nth 0 instruction))
	  (instruction-count  (nth 1 instruction)))

      ; TODO: Can we do further parsing when reading in the file, and just use f/d/u?
      ; We really just need the first and last characters of each line. 
      (cond ((string= "f" (schar instruction-string 0))
	       ; part one :: add x to horizontal-position
	       (incf horizontal-position-1 instruction-count)
	       ; part two :: add x to horizontal-position, and adjust depth
	       (incf horizontal-position-2 instruction-count)
	       (setf depth-2 (+ depth-2 (* instruction-count aim-2))))

	    ((string= "d" (schar instruction-string 0))
	       ; part one :: adjust depth (increasing)
	       (incf depth-1 instruction-count)
	       ; part two :: adjust aim (increasing)
	       (incf aim-2 instruction-count))

	    ((string= "u" (schar instruction-string 0))
               ; part one :: adjust depth (decreasing)
	       (decf depth-1               instruction-count)
	       ; part two :: adjust aim (decreasing)
	       (decf aim-2 instruction-count)))))

  (format T "First calculation :: ~d" (* horizontal-position-1 depth-1))(terpri)
  (format T "Second calculation :: ~d" (* horizontal-position-2 depth-2))(terpri))

