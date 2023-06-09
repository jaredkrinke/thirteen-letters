(defsystem #:13l-console
  :serial t
  :components ((:file "shared")
	       (:file "console"))
  :build-operation program-op
  :build-pathname "thirteen-letters"
  :entry-point "13lc:menu")
