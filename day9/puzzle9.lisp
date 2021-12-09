
(defvar puzzle-input)
(set 'puzzle-input 
     (with-open-file (stream "puzzle9.input")
       (loop for line = (read-line stream nil)
	     while line
	     collect line)))

; (set 'puzzle-input '("2199943210" "3987894921" "9856789892" "8767896789" "9899965678"))

(defun map-value-at (x y)
  ; if the location is unreachable (too low or too high), return an impossibly large value
  (cond ((> 0 x) 99)
	((> 0 y) 99)
	((< (- (length (nth 0 puzzle-input)) 1) x) 99)
	((< (- (length puzzle-input) 1) y) 99)
	; else, return the value at that location. 
	((equal t t)(parse-integer (string (aref (nth y puzzle-input) x))))))

; search the x,y coord for a lower position
(defun is-lowest-position (x y)
  (let ((neighbor-up    (map-value-at x (- y 1)))
	(neighbor-down  (map-value-at x (+ y 1)))
	(neighbor-right (map-value-at (+ x 1) y))
	(neighbor-left  (map-value-at (- x 1) y))
	(this-position  (map-value-at x y)))
    ; need to check less-than-or-equal-to, not just less-than, because the 
    ; position needs to be lower than it's neighbors to count, not just
    ; similary low. 
    (cond ((<= neighbor-up this-position)())
	  ((<= neighbor-down this-position)())
	  ((<= neighbor-right this-position)())
	  ((<= neighbor-left this-position)())
	  ((equal t t)(eval t)))))


; find the low point
(defvar safety-value)
(set 'safety-value 0)
(loop for y from 0 upto (- (length puzzle-input) 1) do
      (loop for x from 0 upto (- (length (nth y puzzle-input)) 1) do
	    (if (is-lowest-position x y)
	      (set 'safety-value (+ 1 (map-value-at x y) safety-value))
	      ())))
(print safety-value)



