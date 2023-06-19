;(ql:quickload '(:websocket-driver-client :cl-json :scms))
;(load "shared.lisp")

(defpackage :chad-bot
  (:documentation "A dumb bot for Thirteen Letters")
  (:use :cl))

(in-package :chad-bot)

(defparameter *url* "wss://api.schemescape.com/ws/13l")
;(defparameter *play-time* 300)
(defparameter *play-time* 10)

(defvar *url-override* nil)
(defvar *done* nil)

;;; Helpers
(defclass word-info ()
  ((word :initarg :word)
   (length :initarg :length)
   (letter-counts :initarg :letter-counts))
  (:documentation "Information about a word"))

(defun get-word (line)
  "Gets the word part of a tab-separated word+frequency pair"
  (subseq line 0 (position #\Tab line)))

(defun for-most-frequent-words (count f)
  "Maps (at most) the COUNT most frequent words"
  (with-open-file (stream "count_1w.txt")
    (loop for n from 1 to count
	  do (funcall f (get-word (read-line stream))))))

(defun make-word-info (word)
  "Creates a WORD-INFO for WORD"
  (make-instance 'word-info
		 :word word
		 :length (length word)
		 :letter-counts (13l:get-letter-counts word)))

(defun make-vocabulary (count)
  "Creates a bot vocabularly using the COUNT most frequent words"
  (let ((word-infos (make-array count :element-type 'word-info))
	(i -1))
    (for-most-frequent-words count
			     #'(lambda (word) (setf (aref word-infos (incf i)) (make-word-info word))))
    word-infos))

(defun find-next-guess (word-infos letter-counts &key (start 0) (min-length 1) (tries 1000000))
  "Go through WORD-INFOS (starting at index START) TRIES times to find a matching word of length >= MIN-LENGTH"
  (loop for i from start below (min (+ start tries) (length word-infos))
	for word-info = (aref word-infos i)
	if (and (>= (slot-value word-info 'length) min-length)
		(13l:letter-subset-p letter-counts (slot-value word-info 'letter-counts)))
	  do (return (slot-value word-info 'word))
	finally (return nil)))

;;; TODO: Support recall < 100%
(defun make-guess-generator (word-infos letter-counts)
  "Creates a generator for guesses given the provided constraints"
  (let ((start 0)
	(min-length 3)
	(guess nil))
    (lambda (tries)
      (setf guess (find-next-guess word-infos
				   letter-counts
				   :start start
				   :min-length min-length
				   :tries tries))
      (if guess
	  (progn
	    (setf start 0)
	    (setf min-length (1+ (length guess))))
	  (setf start (+ start tries)))
      guess)))

;;; Bot implementation
(defclass bot ()
  ((lock :initform (bt:make-lock))
   (name :initarg :name)
   (url :initarg :url)
   (state :initform :between-rounds)
   (word-infos :initarg :word-infos)
   (think-period :initarg :think-period)
   (try-rate :initarg :try-rate)
   client)
  (:documentation "Framework for vocabulary-based bots"))

(defmethod initialize-instance :after ((bot bot) &key)
  (bt:with-lock-held ((slot-value bot 'lock))
    (let ((client (wsd:make-client (slot-value bot 'url))))
      (setf (slot-value bot 'client) client)
      (wsd:on :open client
	      (lambda ()
		(format t "Connected!~%")
		(send-json client (list (cons :type "rename")
					(cons :name (slot-value bot 'name))
					(cons :bot t)))))

      (wsd:on :close client
	      (lambda (&key code reason)
		(format t "Disconnected! ~a, ~a~%" code reason)))

      (wsd:on :message client
	      (lambda (json)
		(bot-handle-message bot (json:decode-json-from-string json))))

      (wsd:start-connection client))))

(defgeneric bot-handle-message (bot message)
  (:documentation "Handler for messages"))

;;; Note: These should be called under lock
(defgeneric bot-round-start (bot message)
  (:documentation "Called (under lock) when a round starts"))

(defgeneric bot-round-end (bot message)
  (:documentation "Called (under lock) when a roudn ends"))

(defun bot-close (bot)
  "Terminate a bot"
  (setf (slot-value bot 'state) :disconnected)
  (wsd:close-connection (slot-value bot 'client)))

(defun send-json (client message)
  "Encodes a message into JSON and sends it"
  (let ((json (json:encode-json-to-string message)))
    (format t "Sending: ~a~%" json)
    (wsd:send-text client json)))

(defun send-guess (client guess)
  "Sends a 'guess' message"
  (send-json client
	     (list (cons :type "guess")
		   (cons :word guess))))

(defun bot-send-guess (bot guess)
  "Sends a guess from the bot (note: this doesn't strictly avoid sending guesses between rounds"
  (if (eql (slot-value bot 'state) :in-round)
      (bt:with-lock-held ((slot-value bot 'lock))
	(send-guess (slot-value bot 'client) guess))))

(defmethod bot-handle-message ((bot bot) message)
  (bt:with-lock-held ((slot-value bot 'lock))
    (let ((message-type (cdr (assoc :type message :test 'equal))))
      (case (slot-value bot 'state)
	(:between-rounds
	 (cond
	   ((equal message-type "state")
	    (setf (slot-value bot 'state) :in-round)
	    (bot-round-start bot message))))
	(:in-round
	 (cond
	   ((equal message-type "result")
	    (setf (slot-value bot 'state) :between-rounds)
	    (bot-round-end bot message))))))))

(defmethod bot-round-start ((bot bot) message)
  "Start a 'guessing' thread for the bot"
  (with-slots (think-period try-rate) bot
    (let* ((tries-per-period (* think-period try-rate))
	   (scrambled (scms:alist-path message :scrambled))
	   (generator (make-guess-generator (slot-value bot 'word-infos)
					    (13l:get-letter-counts scrambled))))
      (bt:make-thread
       #'(lambda ()
	   (loop while (eql (slot-value bot 'state) :in-round)
		 do (sleep think-period)
		    (let ((guess (funcall generator tries-per-period)))
		      (if guess (bot-send-guess bot guess)))))
       :name "bot-thread"))))

(defmethod bot-round-end ((bot bot) message)
  (format t "Bot: round ended! ~a ~%" bot))

(defun make-bot-test (&key
			(name "Chad Bot")
			(vocabulary-size 1000)
			(think-period 5)
			(try-rate 10))
  "Makes a simpler bot for testing purposes"
  (make-instance 'bot
		 :name name
		 :url (or *url-override* *url*)
		 :word-infos (make-vocabulary vocabulary-size)
		 :think-period think-period
		 :try-rate try-rate))

(defparameter *names* '("Mike"
			"Chris"
			"Matt"
			"Josh"
			"Jake"
			"Nick"
			"Andrew"
			"Dan"
			"Tyler"
			"Joe"
			"Brandon"
			"David"
			"Jim"
			"Ryan"
			"John"
			"Zach"
			"Justin"
			"Bill"
			"Tony"
			"Rob"
			"Jessica"
			"Ashley"
			"Emily"
			"Sarah"
			"Samantha"
			"Amanda"
			"Brittany"
			"Elizabeth"
			"Taylor"
			"Megan"
			"Hannah"
			"Kayla"
			"Lauren"
			"Stephanie"
			"Rachel"
			"Jennifer"
			"Nicole"
			"Alexis"
			"Victoria"
			"Amber"))

(defun run-bot (bot seconds)
  "Lets a bot run for SECONDS seconds and then closes it"
  (sleep seconds)
  (format t "Stopping bot ~a~%" (slot-value bot 'name))
  (bot-close bot))

(defun spawn-bot ()
  "Spawns a bot that runs for some amount of time"
  (let* ((name (13l:get-random *names*))
	 (vocabulary-size (+ 500 (random 1500)))
	 (think-period (+ 3 (random 6)))
	 (try-rate (+ 5 (random 15)))
	 (play-time (+ *play-time* (random *play-time*)))
	 (bot (make-bot-test :name name
			     :vocabulary-size vocabulary-size
			     :think-period think-period
			     :try-rate try-rate)))
    (format t "Starting bot ~a for ~a seconds~%" name play-time)
    (bt:make-thread (lambda () (run-bot bot play-time))
		    :name (format nil "bot-~a" name))))

(defun start-bots ()
  (setf *done* nil)
  (bt:make-thread
   (lambda ()
     (loop while (not *done*)
	   do (spawn-bot)
	      (sleep (+ *play-time* (random *play-time*))))
     (format t "Orchestrator ended"))
   :name "bot-orchestrator"))

(defun stop-bots ()
  (setf *done* t))

;;; TODO: Consider sending pings:
;;       (loop do (sleep 60)
;; 	       (wsd:send-ping client)))))
