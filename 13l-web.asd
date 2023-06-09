(defsystem #:13l-web
  :depends-on (#:spinneret
	       #:spinneret/cl-markdown
	       #:parenscript
	       #:cl-css)
  :serial t
  :components ((:file "web")))
