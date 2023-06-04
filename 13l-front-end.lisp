(defpackage :13l-front-end
  (:documentation "HTML/JS/CSS generator for Thirteen Letters")
  (:nicknames :13lf)
  (:use :cl)
  (:import-from #:parenscript #:ps)
  (:local-nicknames (#:sp #:spinneret)))

(in-package :13lf)

;;; JavaScript
;;; TODO: Path should be shared with back-end code
;(defparameter *web-socket-uri* "ws://127.0.0.1:13131/ws/13l")
(defparameter *web-socket-uri* "wss://api.schemescape.com/ws/13l")

(ps:defpsmacro TODO (message)
  `(debug ,message))

(ps:defpsmacro get-id (id)
  `((ps:chain document get-element-by-id) ,id))

(ps:defpsmacro watch (&body body)
  `(ps:try (progn ,@body) (:catch (e) (error (+ (ps:chain e message) " (" (ps:chain e line-number) ")")))))

(ps:defpsmacro clear-children (node)
  `(setf (ps:chain ,node inner-h-t-m-l) ""))

(ps:defpsmacro append-text (node text)
  `((ps:chain ,node append-child) ((ps:chain document create-text-node) ,text)))

(defparameter *script*
  (ps (defparameter *web-socket-uri* (ps:lisp *web-socket-uri*))
    (defun debug (message)
      (let ((debug-div ((ps:chain document get-element-by-id) "debug"))
	    (node ((ps:chain document create-element) "p")))
	(append-text node message)
	((ps:chain debug-div append-child) node)
	nil))
    (defun error (error-string)
      (debug (+ "ERROR: " error-string))
      nil)

    (watch
     (let ((socket)
	   (scrambled)
	   (intro-div (get-id "intro"))
	   (main-div (get-id "main"))
	   (top-div (get-id "top"))
	   (tbody (get-id "tbody"))
	   (result-div (get-id "result"))
	   (debug-div (get-id "debug"))
	   (name-input (get-id "name"))
	   (start-button (get-id "start"))
	   (guess-input (get-id "guess"))
	   (winner-span (get-id "winner"))
	   (winning-word-span (get-id "winning-word")))
       (defun hide (element) ((ps:chain element class-list add) "hidden"))
       (defun show (element)((ps:chain element class-list remove) "hidden"))
       
       (defun update-letters (word)
	 (dotimes (i (ps:chain word length))
	   (let ((letter-span (get-id (+ "l" i))))
	     (setf (ps:chain letter-span inner-text) (aref word i)))))
       
       (defun round-started (message)
	 (setf scrambled (ps:chain message scrambled))
	 (show guess-input)
	 (hide result-div)
	 (setf (ps:chain guess-input value) "")
	 ((ps:chain guess-input focus))
	 (update-letters scrambled))
       
       (defun round-ended (message)
	 (show result-div)
	 (hide guess-input)
	 (let* ((entries (ps:chain message leaderboard))
		(winning-entry (and entries (aref entries 0))))
	   (update-letters (ps:chain message solution))
	   (setf (ps:chain winner-span inner-text) (if winning-entry (ps:chain winning-entry name) "(no one)"))
	   (setf (ps:chain winning-word-span inner-text) (if winning-entry (ps:chain winning-entry word) "---"))))
       
       (defun update-leaderboard (message)
	 (let ((entries (ps:chain message leaderboard)))
	   (clear-children tbody)
	   (if entries
	       (dolist (entry entries)
		 (let ((row ((ps:chain document create-element) "tr"))
		       (col-name ((ps:chain document create-element) "td"))
		       (col-word ((ps:chain document create-element) "td")))
		   (append-text col-name (ps:chain entry name))
		   ((ps:chain row append-child) col-name)
		   (append-text col-word (ps:chain entry word))
		   ((ps:chain col-word class-list add) "right")
		   ((ps:chain col-word class-list add) "caps")
		   ((ps:chain row append-child) col-word)
		   ((ps:chain tbody append-child) row)))
	       (let ((row ((ps:chain document create-element) "tr"))
		     (col ((ps:chain document create-element) "td")))
		 ((ps:chain col set-attribute) "colspan" 2)
		 ((ps:chain col class-list add) "center")
		 (append-text col "(empty)")
		 ((ps:chain row append-child) col)
		 ((ps:chain tbody append-child) row)))))
       
       (defun handle-error ()
	 (error "WebSocket error!"))
       
       (defun handle-update (event)
	 (watch
	  (let* ((json (ps:chain event data))
		 (message ((ps:chain *JSON* parse) json)))
	    (case (ps:chain message type)
	      ("state"
	       (if (not (= scrambled (ps:chain message scrambled))) (round-started message))
	       (update-leaderboard message))
	      ("result"
	       (update-leaderboard message)
	       (round-ended message)))
	    nil)))
       
       (defun send (message)
	 (if socket
	     (let ((json ((ps:chain *json* stringify) message)))
	       ((ps:chain socket send) json))))
       
       (defun send-rename ()
	 (let* ((nameRaw (ps:chain name-input value))
		(name ((ps:chain nameRaw trim))))
	   (if name
	       (send (ps:create type "rename"
				name name)))))
       (defun handle-start ()
	 (watch
	  (hide intro-div)
	  (show main-div)
	  (show top-div)
	  (if (not socket)
	      (progn (setf socket (ps:new (-web-socket *web-socket-uri*)))
		     (setf (ps:chain socket onopen) send-rename)
		     (setf (ps:chain socket onmessage) handle-update)))
	  nil))
       
       (defun send-guess ()
	 (let* ((word-raw (ps:chain guess-input value))
		(word ((ps:chain word-raw to-lower-case)))
		(message (ps:create type "guess"
				    word word)))
	   (send message)
	   (setf (ps:chain guess-input value) "")))
       
       (defun handle-key-down (event)
	 (watch
	  (let ((key (ps:chain event key)))
	    (if (= key "Enter")
		(progn (send-guess))))))

       (setf (ps:chain start-button onclick) handle-start)
       (setf (ps:chain start-button onerror) handle-error)
       (setf (ps:chain guess-input onkeydown) handle-key-down)
       nil))))

;;; CSS
(defparameter *css*
  (cl-css:css
   '(("html, body" :margin 0)
     ("body, button, input" :font-family "'Cascadia Mono', 'Consolas', monospace")
     ("table" :width "100%")
     ("input" :font-size "100%")
     
     (".left" :text-align "left")
     (".center" :text-align "center")
     (".right" :text-align "right")
     (".caps" :text-transform "uppercase")
     
     (".main" :display "flex"
	      :flex-direction "column"
	      :align-items "center")
     (".top" :width "100%"
	     :display "flex"
	     :flex-direction "column"
	     :margin-top "1em")
     ("#root" :display "flex"
	      :flex-direction "column"
	      :align-items "center"
	      :max-width "40em"
	      :margin "auto"
	      :padding "1em")
     ("#guess, #result" :font-size "150%"
			:margin-top "1em")
     ("#start" :margin "1em"
	       :padding "0.25em 1em"
	       :font-size "150%"
	       :font-weight "bold")
     ("#letters" :margin "1em"
		 :text-align "center")
     
     ("#letters > div" :display "inline-block"
		       :margin "0.5em"
		       :padding "0.25em"
		       :border "0.1em solid"
		       :text-transform "uppercase")
     (.invisible :display "hidden")
     (.hidden :display "none"))))

;;; HTML
(defparameter *intro* (sp:with-html-string ("**Thirteen Letters** is a game where players compete to find the longest word that can be constructed from the given set of letters.

It's also a game that needs better documentation!")))

(defmacro page ((&key title) &body body)
  "Outputs an HTML page"
  `(sp:with-html
     (:doctype)
     (:html
      (:head
       (:title ,title)
       (:meta :name "viewport"
	      :content "width=device-width, initial-scale=1, shrink-to-fit=no"))
      (:style (:raw *css*))
      (:body ,@body))))

(defun make-index-html ()
  "Outputs root HTML page"
  (page (:title "(thirteen-letters)")
    (:div :id "root"
	  (:h1 "(thirteen-letters)")
	  (:div :id "intro"
		(:h2 "Welcome!")
		(:raw *intro*)
		(:p "Name: " (:input :id "name" :type "text"))
		(:p :class "center"
		    (:button :id "start" "Start")))
	  (:div :id "main" :class "hidden main"
		(:div :id "letters"
		      (dotimes (n 13)
			(:div :id (format nil "l~d" n) "?")))
		(:input :id "guess" :type "text"))
	  (:div :id "result" :class "hidden"
		"Winner: "
		(:span :id "winner")
		" ("
		(:span :id "winning-word" :class "caps")
		")")
	  (:div :id "top" :class "top hidden"
		(:h3 :class "center" "Leaderboard")
		(:table (:tbody :id "tbody")))
	  (:div :id "debug"))
    (:script (:raw *script*))))


;;; Generate index.html
(setf sp:*suppress-inserted-spaces* t)

(let ((*print-pretty* nil)
      (sp:*html-style* :tree))
  (with-open-file (out "index.html" :direction :output
				    :if-exists :supersede)
    (setf sp:*html* out)
    (make-index-html)))
