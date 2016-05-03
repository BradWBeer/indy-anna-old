;;;; indy-anna.asd

(asdf:defsystem #:indy-anna
  :description "Describe indy-anna here"
  :author "Brad (warweasle) Beer"
  :license "MIT"
  :depends-on (#:clinch
               #:clinch-cairo
               #:clinch-pango
	       #:clinch-freeimage
	       #:sdl2-mixer
	       #:squirl)
  :serial t
  :components ((:file "package")
	       (:file "utils")
	       (:file "physics")
               (:file "indy-anna")))

