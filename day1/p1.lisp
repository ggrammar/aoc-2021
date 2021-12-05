; Need to set some-var outside of the loop, so we can use it inside the loop.
(let ((some-var 9999) 
      (measurement-increase-count 0))
  (with-open-file (stream "puzzle1.input")
    (loop for line = (read-line stream nil)
	  while line
	  ; This only works if we do it in this order. Here is how I think this works:
	  ;  - first, retrieve the value of some-var
	  ;  - next, set some-var to the new vlaue
	  ;    - setf will return the new value 
	  ;  - do the comparison
	  do (if (< some-var (setf some-var (parse-integer line)))
	       (incf measurement-increase-count)
	       ())))
  (print measurement-increase-count))
