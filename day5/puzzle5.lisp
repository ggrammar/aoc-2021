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


(defun mark-map-location (x y)
     ; ocean-map is a AxB array. my intuition for accessing map coordinate 
     ; (x, y), where x is the horizontal coordinate and y is the vertical
     ; coordinate, is to (aref ocean-map x y). 
     ; but, that's backwards! (aref ocean-map x) accesses a certain _row_ in
     ; the array, which in my mental spreadsheet of (x, y) coordinates is
     ; actually the vertical coordinate. so, we need to call (aref) backwards
     ; from what my intuition suggests.
     ; this doesn't actually affect anything for the solution - ocean-map is 
     ; just rotated if you (aref ocean-map x y) - but it helps if you need to 
     ; print the map and see what's going on. 
     (incf (aref ocean-map y x)))


(defun mark-steam-vent-on-map (steam-vent)
  ; should probably break this out into a function or a class
  (let ((start-x (parse-integer (nth 0 (uiop:split-string (nth 0 steam-vent) :separator ","))))
	(start-y (parse-integer (nth 1 (uiop:split-string (nth 0 steam-vent) :separator ","))))
	(end-x   (parse-integer (nth 0 (uiop:split-string (nth 2 steam-vent) :separator ","))))
	(end-y   (parse-integer (nth 1 (uiop:split-string (nth 2 steam-vent) :separator ",")))))

    ; We process diagonal lines differently than horizontal/vertical lines. 
    ; If x stays the same, it's horizontal. If y stays the same, it's vertical. 
    ; Otherwise (if both change), it's diagonal. 

    ; For horiz/vert, we run loops both up and down, to catch the cases when x is
    ; increasing/decreasing, or y is increasing/decreasing. As far as I
    ; can tell, CL doesn't have a way to loop in either direction (inc or
    ; dec), so we just run both. The loop that runs in the wrong direction
    ; just won't be executed. 

          ; x is the same, mark all y values
    (cond ((equal start-x end-x)
	    (loop for y from end-y to start-y
		  do (mark-map-location start-x y))
	    (loop for y from start-y to end-y
		  do (mark-map-location start-x y)))
          ; y is the same, mark all x values
	  ((equal start-y end-y)
	    (loop for x from end-x to start-x
		  do (mark-map-location x start-y))
	    (loop for x from start-x to end-x
		  do (mark-map-location x start-y)))
          ; diagonal - all other lines, so (cond ((eval t)) ())
	  ((eval t)
	   ; Both x and y changed, so this is a diagonal line. 
	   (progn 
	     ; (format T "mapping diagonal ~s ..." steam-vent)
	     (let ((vent-distance (abs (- start-x end-x))))
	       ; (format T "distance is ~d" vent-distance)
	       (loop for i from 0 upto vent-distance
		     do (cond
			  ; if x is increasing and y is increasing
			  ((and (> end-x start-x)(> end-y start-y))
			   (mark-map-location (+ start-x i)(+ start-y i)))
			  ; if x is increasing and y is decreasing
			  ((and (> end-x start-x)(< end-y start-y))
			   (mark-map-location (+ start-x i)(- start-y i)))
			  ; if x is decreasing and y is increasing
			  ((and (< end-x start-x)(> end-y start-y))
			   (mark-map-location (- start-x i)(+ start-y i)))
			  ; if x is decreasing and y is decreasing
			  ((and (< end-x start-x)(< end-y start-y))
			   (mark-map-location (- start-x i)(- start-y i)))))))))))


; mark all of our steam vents
(loop for steam-vent in steam-vents
      do (mark-steam-vent-on-map steam-vent))

; count locations with 2 or more vents (> count 1)
(let ((answer 0))
  (loop for x from 0 upto 999
      do (loop for y from 0 upto 999
	       do 
	       (if (> (aref ocean-map x y) 1)
		 (incf answer))))
  (print answer))

