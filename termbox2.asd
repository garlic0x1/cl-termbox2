(asdf:defsystem "termbox2"
  :author "garlic0x1"
  :license "MIT"
  :depends-on (:cffi)
  :components ((:file "termbox2")))

(asdf:defsystem "termbox2/snake"
  :depends-on (:termbox2)
  :components ((:file "snake"))
  :build-operation "program-op"
  :build-pathname "snake-exe"
  :entry-point "termbox2/snake::main")

(asdf:defsystem "termbox2/example"
  :depends-on (:termbox2)
  :components ((:file "example"))
  :build-operation "program-op"
  :build-pathname "example-exe"
  :entry-point "termbox2/example::main")
