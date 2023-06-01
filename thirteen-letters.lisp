(defpackage :thirteen-letters
  (:documentation "Thirteen-letter word scramble game")
  (:nicknames :13l)
  (:use :cl)
  (:local-nicknames (:lp :lparallel))
  (:export #:play
	   #:menu))

(in-package :13l)

;;; General utility functions
(defvar *verbose* nil)

(defmacro spew (&rest args)
  "Logs to the console, if *verbose* is non-nil"
  `(and *verbose* (format t ,@args)))

(defun get-time ()
  "Return a timestamp (in seconds, as a ratio)"
  (/ (get-internal-real-time) internal-time-units-per-second))

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

;;; Leaderboard entries
(defclass entry ()
  ((player-id :initarg :player-id)
   (word :initarg :word)
   (time :initform (get-time))))

(defgeneric entry-word-length (entry))

(defmethod entry-word-length ((entry entry))
  (length (slot-value entry 'word)))

(defmethod print-object ((object entry) stream)
  (print-unreadable-object (object stream :type t)
    (with-slots (player-id word time) object
      (format stream "~s: ~s (~f)" player-id word time))))

;;; TODO: Seems like a macro might make sense...
(defun entry< (a b)
  (let ((a-length (entry-word-length a))
	(b-length (entry-word-length b)))
    (or (> a-length b-length)
	(and (= a-length b-length)
	     (< (slot-value a 'time) (slot-value b 'time))))))

;;; Server

;;; WebSocket server
(defclass socket (hunchensocket:websocket-resource)
  ()
  (:documentation "WebSocket server/resource/socket")
  (:default-initargs :client-class 'client))

(defclass client (hunchensocket:websocket-client)
  ()
  (:documentation "WebSocket client"))

(defmethod hunchensocket:client-connected ((socket socket) client)
  (TODO "Client connected"))

(defmethod hunchensocket:client-disconnected ((socket socket) client)
  (TODO "Client disconnected"))

(defmethod hunchensocket:text-message-received ((socket socket) client message)
  (TODO (format nil "Received: ~a" message)))

(defun find-websocket-handler (request)
  (if (equal (hunchentoot:script-name request) *socket-path*)
      *socket*))

;;; TODO: Same port as HTTP? If so, rename
(defparameter *socket-port* 13131)
(defparameter *socket-path* "/ws/13l" "Path to WebSocket interface")

(defvar *socket* (make-instance 'socket))
(defvar *server* (make-instance 'hunchensocket:websocket-acceptor
				:address "127.0.0.1"
				:port *socket-port*))

;;; TODO: Plus or star?
(defparameter *round-time* 3 "Length of each round (in seconds)")
(defparameter *intermission-time* 1 "Length of time between rounds (in seconds)")

(defvar *done* t "True if the server should stop")
(defvar *queue* nil "Task queue")

;;; Server main worker logic
(defvar *solution* nil)
(defvar *scrambled* nil)
(defvar *round-end* nil)
(defvar *leaderboard* nil)

(defun broadcast (object)
  "Broadcasts a message to all players"
  (let ((text (prin1-to-string object)))
    (spew "Broadcast: ~a~%" text)
    (loop for client in (hunchensocket:clients *socket*)
	  do (hunchensocket:send-text-message client text))))

(defun broadcast-message (type object)
  "Broadcasts a message of type TYPE to all players"
  (let ((message (cons (cons :type type)
		       object)))
    (broadcast message)))

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

(defun run-round ()
  "Runs a single round"
  (lp:submit-task *queue* #'round-start)
  (sleep *round-time*)
  (lp:submit-task *queue* #'round-end)
  (sleep *intermission-time*))

(defun valid-word-p (word)
  "Returns true if WORD is a real word and uses letters from *SCRAMBLED*"
  (TODO "Test word is valid!")
  t)

(defun update-leaderboard (player-id word)
  "Updates the leaderboard (note: WORD is assumed valid"
  (setf *leaderboard* (stable-sort (cons (make-instance 'entry
							:player-id player-id
							:word word)
					 (delete-if #'(lambda (entry) (equal player-id (slot-value entry 'player-id))) *leaderboard*))
				   #'entry<)))

(defun handle-guess (player-name word)
  "Updates the current leaderboard with PLAYER-NAME's new score, if valid"
  (if (valid-word-p word) (update-leaderboard player-name word)))

(defun start-server ()
  "Runs the server"
  (setf *done* nil)
  (setf lp:*kernel* (lp:make-kernel 1))
  (setf *queue* (lp:make-channel))

  (setf hunchensocket:*websocket-dispatch-table* (list 'find-websocket-handler))
  (hunchentoot:start *server*)
  
  (loop while (not *done*)
	do (run-round))
  
  (hunchentoot:stop *server*)
  (setf hunchensocket:*websocket-dispatch-table* nil)
  
  (lp:end-kernel :wait t)
  (setf *queue* nil))

(defun stop-server ()
  "Stops the server"
  (setf *done* t))
