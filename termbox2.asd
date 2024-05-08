(asdf:defsystem "termbox2"
  :author "garlic0x1"
  :depends-on (:cffi)
  :components ((:file "termbox2")))

(asdf:defsystem "termbox2/example"
  :depends-on (:termbox2)
  :components ((:file "example"))
  :build-operation "program-op"
  :build-pathname "termbox-example"
  :entry-point "termbox2/example::main")
