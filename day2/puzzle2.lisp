; the "asdf" package is where the uiop:split-string method comes from.
; I'm still not sure how the common lisp/sbcl package system works, so
; I arrived at this and `uiop:split-string` after some trial and error.
(require "asdf")

(defvar instructions)
(set 'instructions
  (with-open-file (stream "puzzle2.input")
    (loop for line = (read-line stream nil)
      while line
      collect line)))

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
(let ((horizontal-position 0)
      (depth 0))
  (loop for instruction in instructions do
    ; parse out the name of the instruction, and velocity
    (let ((instruction-string (               nth 0 (uiop:split-string instruction :separator " ")))
	  (instruction-count  (parse-integer (nth 1 (uiop:split-string instruction :separator " ")))))

            ; forward adds x to horizontal-position
      (cond ((search "forward" instruction-string)(incf horizontal-position instruction-count))
            ; down adds x to depth
	    ((search "down"    instruction-string)(incf depth               instruction-count))
            ; up subtracts x from depth
	    ((search "up"      instruction-string)(decf depth               instruction-count)))))

  (format T "First calculation :: ~d" (* horizontal-position depth))(terpri))


; part 2
(let ((horizontal-position 0)
      (depth 0)
      (aim 0))

  (loop for instruction in instructions do
	; parse out the name of the instruction, and velocity
	(let ((instruction-string (               nth 0 (uiop:split-string instruction :separator " ")))
	      (instruction-count  (parse-integer (nth 1 (uiop:split-string instruction :separator " ")))))

	        ; down and up only adjust aim
	  (cond ((search "down"    instruction-string)(incf aim instruction-count))
		((search "up"      instruction-string)(decf aim instruction-count))
		; forward adds x to horizontal-position AND adjusts depth
		((search "forward" instruction-string)
		     (incf horizontal-position instruction-count)
		     (setf depth (+ depth (* instruction-count aim)))))))

  (format T "Second calculation :: ~d" (* horizontal-position depth))(terpri))





