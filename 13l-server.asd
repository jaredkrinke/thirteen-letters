(defsystem #:13l-server
  :depends-on (#:lparallel
	       #:hunchentoot
	       #:hunchensocket
	       #:spinneret
	       #:cl-json)
  :serial t
  :components ((:file "shared")
	       (:file "server"))
  :build-operation program-op
  :build-pathname "13l-server"
  :entry-point "13ls:start-server")
