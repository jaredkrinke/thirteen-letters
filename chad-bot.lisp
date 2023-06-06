(ql:quickload '(:websocket-driver-client :cl-json))

(defpackage :chad-bot
  (:documentation "A dumb bot for Thirteen Letters")
  (:use :cl))

(in-package :chad-bot)

;;; TODO: gensym for event and handler at least
(defmacro with-web-socket ((client url &rest event-handlers) &body body)
  `(let ((,client (wsd:make-client ,url)))
     (loop for (event handler) on (list ,@event-handlers) by #'cddr
	   do (wsd:on event ,client handler))
     (unwind-protect
	  (progn
	    (wsd:start-connection ,client)	       
	    ,@body)
       (wsd:close-connection ,client))))

(defparameter *name* "Chad Bot")
(defparameter *guess* "set")
(defparameter *url* "wss://api.schemescape.com/ws/13l")

;;; TODO: For testing only!
(defun test ()
  (with-web-socket (client
		    *url*
		    :open (lambda () (format t "Opened!~%"))
		    :message (lambda (json) (format t "Received: ~a~%" json))
		    :close (lambda (&key &allow-other-keys) (format t "Closed!~%")))
    (sleep 60)))

(defun send-json (client message)
  "Encodes a message into JSON and sends it"
  (let ((json (json:encode-json-to-string message)))
    (format t "Sending: ~a~%" json)
    (wsd:send-text client json)))

(defun start ()
  "Run the Chad Bot!!!1"
  (let ((between-rounds t))
    (with-web-socket (client
		      *url*
		      :open (lambda ()
			      (format t "Connected!~%")
			      (send-json client (list (cons :type "rename")
						      (cons :name *name*))))
		      :message (lambda (json)
				 (format t "Received ~a~%" json)
				 (let* ((message (json:decode-json-from-string json))
					(message-type (cdr (assoc :type message :test 'equal))))
				   (if (and between-rounds (string-equal message-type "state"))
				       (progn ; A new round must have started!
					 (setf between-rounds nil)
					 (send-json client (list (cons :type "guess")
								 (cons :word *guess*)))))
				   (if (and (not between-rounds) (string-equal message-type "result"))
				       (setf between-rounds t))))
		      :close (lambda (&key code reason)
			       (format t "Disconnected! ~a, ~a~%" code reason)))
      (loop do (sleep 60)
	       (wsd:send-ping client)))))
