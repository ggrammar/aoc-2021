(require "asdf")

(set 'instructions
  (prog1
    (set 'dm (list()))
      (set 'infile (open "puzzle2.input"))
        (when infile
	  (loop for line = (read-line infile nil)
	    while line do (push line (cdr (last dm)))))))

; pop the first
(set 'instructions (remove (nth 0 instructions) instructions))

(set 'horizontal-position 0)
(set 'depth 0)
(loop for instruction in instructions do
      ; forward adds x to horizontal-position
      ; down adds x to depth
      ; up subtracts x from depth
      (set 'instruction-string 
	   (nth 0 (uiop:split-string instruction :separator " ")))
      (set 'instruction-count
	   (parse-integer (nth 1 (uiop:split-string instruction :separator " "))))

      (if (search "forward" instruction-string)(set 'horizontal-position (+ horizontal-position instruction-count))
	(if (search "down" instruction-string)(set 'depth (+ depth instruction-count))
	  (if (search "up" instruction-string)(set 'depth (- depth instruction-count))))))

(write (* horizontal-position depth))(terpri)


(set 'horizontal-position 0)
(set 'depth 0)
(set 'aim 0)
(loop for instruction in instructions do
      ; forward adds x to horizontal-position
      ; down adds x to depth
      ; up subtracts x from depth
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




(write (* horizontal-position depth))(terpri)
