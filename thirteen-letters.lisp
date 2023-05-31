(defpackage :thirteen-letters
  (:documentation "Thirteen-letter word scramble game")
  (:nicknames :13l)
  (:use :cl)
  (:local-nicknames (:lp :lparallel))
  (:export #:play
	   #:menu))

(in-package :13l)

;;; General utility functions
(defun get-random (sequence)
  "Randomly selects an item from SEQUENCE"
  (nth (random (length sequence)) sequence))

(defun shuffle (sequence &rest rest &key &allow-other-keys)
  "Returns a copy of SEQUENCE (as an array) with its elements shuffled (note: extra arguments are passed to MAKE-ARRAY)"
  (let* ((length (length sequence))
	 (array (apply #'make-array length :initial-contents sequence rest)))
    (loop for i upfrom 0
	  for n downfrom length
	  while (> n 1)
	  do (let ((target (+ i (random n))))
	       (rotatef (aref array i) (aref array target))))
    array))

(defun shuffle-string (string)
  "Returns a copy of STRING with its characters shuffled"
  (shuffle string :element-type 'character))

(defun char-repeat (character times)
  "Returns a string with CHARACTER repeated TIMES times"
  (make-array times :element-type 'character :initial-element character))

;;; The actual game
(defparameter *puzzle-length* 13)
(defparameter *difficulty-buckets* 10)

(defmacro define-bucketed-words ()
  "Injects word list at compile time"
  (with-open-file (stream "data.lisp")
    `(defparameter *bucketed-words* ',(read stream))))

(define-bucketed-words)

(defun fully-scrambled-p (scrambled solution)
  "Returns NIL if any character in SCRAMBLED matches a letter in SOLUTION"
  (loop for scrambled-character across scrambled
	for solution-character across solution
	when (char-equal scrambled-character solution-character) do (return nil)
	finally (return t)))

(defun scramble (word)
  "Shuffles a word, ensuring that no character is in the correct place"
  (loop with scrambled = word
	until (fully-scrambled-p scrambled word)
	do (setf scrambled (shuffle-string scrambled))
	finally (return scrambled)))

(defun unscramble (scrambled solution index)
  "Unscrambles the letter at INDEX in SCRAMBLED"
  (let* ((c (aref solution index))
	 (target (position c scrambled :start index)))
    (rotatef (aref scrambled index) (aref scrambled target))))

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

;;; TODO
(defun TODO (&optional note)
  (format t "TODO: ~a~%" note))
  ;(error "TODO: fix!"))

;;; Server
(defparameter *round-time* 3 "Length of each round (in seconds)")
(defparameter *intermission-time* 1 "Length of time between rounds (in seconds)")

(defvar *done* t "True if the server should stop")
(defvar *queue* nil "Task queue")

;;; Server main worker logic
(defvar *solution* nil)
(defvar *scrambled* nil)
(defvar *round-end* nil)
(defvar *leaderboard* nil)

(defun broadcast-message (type object)
  "Broadcasts a message of type TYPE to all players"
  (let ((message (cons (cons :type type)
		       object)))
    (TODO (format nil "Broadcast: ~a" message))))

(defun broadcast-state ()
  "Broadcasts puzzle state to all players"
  (let ((state (list (cons :scrambled *scrambled*)
		     (cons :remaining (float (max 0 (/ (- *round-end* (get-internal-real-time)) internal-time-units-per-second))))
		     (cons :leaderboard *leaderboard*))))
    (broadcast-message :state state)))

(defun broadcast-result ()
  "Broadcasts result to all players"
  (let ((result (list (cons :solution *solution*)
		      (cons :leaderboard *leaderboard*))))
    (broadcast-message :result result)))

(defun round-start (&optional (difficulty 0))
  "Selects a new puzzle and broadcasts to all players"
  (setf *solution* (get-random (nth difficulty *bucketed-words*)))
  (setf *scrambled* (scramble *solution*))
  (setf *round-end* (+ (get-internal-real-time) *round-time*))
  (setf *leaderboard* nil)
  (broadcast-state))

(defun round-end ()
  "Broadcasts the results of the round that just ended"
  (broadcast-result))

(defun run-round()
  "Runs a single round"
  (lp:submit-task *queue* #'round-start)
  (sleep *round-time*)
  (lp:submit-task *queue* #'round-end)
  (sleep *intermission-time*))

(defun run-server ()
  "Runs the server"
  (setf *done* nil)
  (setf lp:*kernel* (lp:make-kernel 1))
  (setf *queue* (lp:make-channel))
    (loop while (not *done*)
	  do (run-round))
  (lp:end-kernel :wait t)
  (setf *queue* nil))
