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

(defvar horizontal-position)
(defvar depth)
; part 1
(set 'horizontal-position 0)
(set 'depth 0)
(loop for instruction in instructions do
      (let ((instruction-string (nth 0 (uiop:split-string instruction :separator " ")))
	    (instruction-count  (parse-integer (nth 1 (uiop:split-string instruction :separator " ")))))

        ; forward adds x to horizontal-position
        ; down adds x to depth
        ; up subtracts x from depth
        ; `cond` might be a better fit here: http://clhs.lisp.se/Body/m_cond.htm
        (if (search "forward" instruction-string)(set 'horizontal-position (+ horizontal-position instruction-count))
	  (if (search "down" instruction-string)(set 'depth (+ depth instruction-count))
	    (if (search "up" instruction-string)(set 'depth (- depth instruction-count)))))))

(format T "First calculation :: ~d" (* horizontal-position depth))(terpri)


; part 2
(set 'horizontal-position 0)
(set 'depth 0)

(defvar aim)
(set 'aim 0)
(loop for instruction in instructions do
  (let ((instruction-string (nth 0 (uiop:split-string instruction :separator " ")))
	(instruction-count  (parse-integer (nth 1 (uiop:split-string instruction :separator " ")))))

      ; down and up only adjust aim
    (if (search "down" instruction-string)(set 'aim (+ aim instruction-count))
      (if (search "up" instruction-string)(set 'aim (- aim instruction-count))))

        ; forward adds count to horizontal-position
        ; forward also adds (forward-count * aim) to depth
        (if (search "forward" instruction-string)
	  (progn
	    (set 'horizontal-position (+ horizontal-position instruction-count))
	    (set 'depth (+ depth (* instruction-count aim)))))))

(format T "Second calculation :: ~d" (* horizontal-position depth))(terpri)
