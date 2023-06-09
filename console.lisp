(defpackage :13l-console
  (:documentation "Single-player, console version of Thirteen Letters")
  (:nicknames :13lc)
  (:use :cl
	:13l)
  (:export #:play
	   #:menu))

(in-package :13lc)

(defun play (&optional (difficulty 0))
  "Play one round on the given difficulty level"
  (let* ((solution (get-random (nth difficulty *bucketed-words*)))
	 (scrambled (scramble solution)))
    (loop for tries upfrom 0
	  for attempt upfrom 1
	  with continue = t
	  with won = nil
	  with preamble = "Unscramble the following word"
	  while continue
	  do (format t
		     "~%~%~a, or enter 'q' to give up (attempt #~a):~%  ~a~a~%  ~a~%~%> "
		     preamble
		     attempt
		     (char-repeat #\Space tries)
		     (char-repeat #\_ (- *puzzle-length* tries))
		     scrambled)
	     (let* ((guess (read-line)))
	       (cond ((string= guess solution)
		      (format t
			      "~%~%Correct! Solved on attempt ~a (difficulty ~a).~%~%"
			      attempt
			      (1+ difficulty))
		      (setf continue nil)
		      (setf won t))
		     ((string= guess "q")
		      (format t "~%~%Better luck next time!~%~%")
		      (setf continue nil))
		     ((string= scrambled solution)
		      (format t "~%~%Too slow! Better luck next time!~%~%")
		      (setf continue nil))
		     (t
		      (unscramble scrambled solution tries)
		      (cond ((string= scrambled solution)
			     (format t "~%~%Too slow! The correct answer was: ~a~%~%" solution)
			     (setf continue nil))
			    (t
			     (format t "~%~%Nope! Guess again"))))))
	  finally (return (and won tries)))))

(defun menu ()
  "Show the title menu and prompt for difficulty level (or exit)"
  (format t
	  "
=== Thirteen Letters ===

 - Goal: Unscramble a thirteen-letter word in as few guesses as possible.
 - After each incorrect guess, one letter will be unscrambled.
 - There are ~a difficulty levels (1 being the easiest)~%~%"
	  *difficulty-buckets*)

  (loop with continue = t
	while continue
	do (format t
		   "Enter a difficulty level, 1 - ~a (or 'q' to quit):~%~%> "
		   *difficulty-buckets*)
	   (let* ((input (read-line))
		  (number (parse-integer input :junk-allowed t))
		  (difficulty (and number (>= number 1) (<= number *difficulty-buckets*) number)))
	     (cond ((string= "q" input)
		    (format t "~%~%So long!~%")
		    (setf continue nil))
		   (difficulty
		    (play (1- difficulty)))
		   (t
		    (format t "~%Invalid input!~%"))))))
