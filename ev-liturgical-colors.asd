;;;; ev-liturgical-colors.asd

(asdf:defsystem #:ev-liturgical-colors
  :serial t
  :description "A german protestant liturgical calender tool"
  :author "Christoph Finkensiep <chfin@freenet.de>"
  :license "MIT/X11"
  :depends-on (#:local-time #:alexandria)
  :components ((:file "package")
               (:file "ev-liturgical-colors")))

