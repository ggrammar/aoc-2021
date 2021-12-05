(require "asdf")


(defvar ocean-map)
(set 'ocean-map (make-array '(1000 1000) :initial-element 0))

(defvar steam-vents)
(set 'steam-vents ())

(with-open-file (stream "puzzle5.input")
  (loop for line = (read-line stream nil)
	while line
	collect 
	(set 'steam-vents (append steam-vents (list (uiop:split-string line :separator " "))))))

; only needs to consider horizontal and vertical lines for now
(defun mark-steam-vent-on-map (steam-vent)
  (let ((start-x (parse-integer (nth 0 (uiop:split-string (nth 0 steam-vent) :separator ","))))
	(start-y (parse-integer (nth 1 (uiop:split-string (nth 0 steam-vent) :separator ","))))
	(end-x   (parse-integer (nth 0 (uiop:split-string (nth 2 steam-vent) :separator ","))))
	(end-y   (parse-integer (nth 1 (uiop:split-string (nth 2 steam-vent) :separator ",")))))
    ; if start-loc x != end-loc x AND start-loc y != end-loc y, do nothing, diagonal line. 
    (if (and
	     (not (equal start-x end-x))
	     (not (equal start-y end-y)))
      ; skip if the line is on a diagonal
      (format T "steam-vent ~s is on a diagonal, skipping" steam-vent)
      ; else, mark the line on the map
      (progn
	(format T "mapping ~s ..." steam-vent)
	(format T "~d,~d -> ~d,~d" start-x start-y end-x end-y)

	; if x remains the same, we need to iterate on y to adjust the map
	(if (equal start-x end-x)
	  (if (> start-y end-y)
	    (loop for y from end-y to start-y
		  do (incf (aref ocean-map y start-x)))
	    (loop for y from start-y to end-y
		  do (incf (aref ocean-map y start-x))))
	  ; y is the same, so loop on x
	  (if (> start-x end-x)
	    (loop for x from end-x to start-x
		  do (incf (aref ocean-map start-y x)))
	    (loop for x from start-x to end-x
		  do (incf (aref ocean-map start-y x)))))))
    (terpri)))


(loop for steam-vent in steam-vents
      do (mark-steam-vent-on-map steam-vent))

(let ((answer 0))
  (loop for x from 0 upto 999
      do (loop for y from 0 upto 999
	       do 
	       (if (> (aref ocean-map x y) 1)
		 (incf answer))))
  (print answer))


; (print ocean-map)
