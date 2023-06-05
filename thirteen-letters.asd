(defsystem #:thirteen-letters
  :depends-on (#:lparallel
	       #:hunchentoot
	       #:hunchensocket
	       #:spinneret
	       #:cl-json)
  :serial t
  :components ((:file "thirteen-letters"))
  :build-operation program-op
  :build-pathname "thirteen-letters"
  :entry-point "thirteen-letters:menu")
