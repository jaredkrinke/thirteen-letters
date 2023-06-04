(defsystem #:13l-front-end
  :depends-on (#:spinneret
	       #:spinneret/cl-markdown
	       #:parenscript
	       #:cl-css)
  :serial t
  :components ((:file "13l-front-end")))
