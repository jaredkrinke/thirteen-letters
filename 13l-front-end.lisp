(defpackage :13l-front-end
  (:documentation "HTML/JS/CSS generator for Thirteen Letters")
  (:nicknames :13lf)
  (:use :cl)
  (:import-from #:parenscript #:ps)
  (:local-nicknames (#:sp #:spinneret)))

(in-package :13lf)

(setf sp:*suppress-inserted-spaces* t)

(defmacro page ((&key title) &body body)
  "Outputs an HTML page"
  `(sp:with-html
     (:doctype)
     (:html
      (:head
       (:title ,title))
      (:body ,@body))))

(defparameter *intro* (sp:with-html-string ("**Thirteen Letters** is game where players compete to find the longest word that can be constructed from the given set of letters.

It's also a game that needs better documentation!!! I guess...")))

(defparameter *script* "")

(defun make-index-html ()
  "Outputs root HTML page"
  (page (:title "(thirteen-letters)")
    (:div :id "intro"
	  (:h2 "Welcome!")
	  (:raw *intro*)
	  (:input :id "name" :type "text")
	  (:br)
	  (:button :id "start" "Start"))
    (:div :id "main"
	  (:h1 "(thirteen-letters)")
	  (:div :id "letters"
		(dotimes (n 13)
		  (:span :id (format nil "l~d" n) "A")))
	  (:input :id "guess" :type "text"))
    (:div :id "top"
	  (:h3 "Leaderboard")
	  (:table (:thead (:th (:td "Name") (:td "Score")))
		  (:tbody)))
    (:div :id "result"
	  (:h2 "Results")
	  (:p :id "result" "The winner is " (:span :id "winner") "!"))
    (:script (:raw *script*))))

;;; generate index.html
(with-open-file (out "index.html" :direction :output
				  :if-exists :supersede)
  (setf sp:*html* out)
  (make-index-html))
