; the "asdf" package is where the uiop:split-string method comes from.
; I'm still not sure how the common lisp/sbcl package system works, so
; I arrived at this and `uiop:split-string` after some trial and error.
(require "asdf")

(set 'instructions
  (prog1
    (set 'dm (list()))
      (set 'infile (open "puzzle2.input"))
        (when infile
	  (loop for line = (read-line infile nil)
	    while line do (push line (cdr (last dm)))))))

; because of the way I'm reading the file in, the first entry in the list is
; an empty list. pop that off, so we only have instructions. 
(set 'instructions (remove (nth 0 instructions) instructions))

; part 1
(set 'horizontal-position 0)
(set 'depth 0)
(loop for instruction in instructions do
      (set 'instruction-string 
	   (nth 0 (uiop:split-string instruction :separator " ")))
      (set 'instruction-count
	   (parse-integer (nth 1 (uiop:split-string instruction :separator " "))))

      ; forward adds x to horizontal-position
      ; down adds x to depth
      ; up subtracts x from depth
      (if (search "forward" instruction-string)(set 'horizontal-position (+ horizontal-position instruction-count))
	(if (search "down" instruction-string)(set 'depth (+ depth instruction-count))
	  (if (search "up" instruction-string)(set 'depth (- depth instruction-count))))))

; part 1 solution
(write (* horizontal-position depth))(terpri)


; part 2
(set 'horizontal-position 0)
(set 'depth 0)
(set 'aim 0)
(loop for instruction in instructions do
      (set 'instruction-string 
	   (nth 0 (uiop:split-string instruction :separator " ")))
      (set 'instruction-count
	   (parse-integer (nth 1 (uiop:split-string instruction :separator " "))))

      ; down and up only adjust aim
      (if (search "down" instruction-string)(set 'aim (+ aim instruction-count))
	(if (search "up" instruction-string)(set 'aim (- aim instruction-count))))

      ; forward adds count to horizontal-position
      ; forward also adds (forward-count * aim) to depth
      (if (search "forward" instruction-string)
	(progn
	  (set 'horizontal-position (+ horizontal-position instruction-count))
	  (set 'depth (+ depth (* instruction-count aim))))))

; part 2 solution
(write (* horizontal-position depth))(terpri)
