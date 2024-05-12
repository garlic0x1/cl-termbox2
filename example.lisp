(defpackage :termbox2/example
  (:use :cl :termbox2))
(in-package :termbox2/example)

(defun main ()
  (tb-init)
  (tb-print 0 0 +tb-cyan+ +tb-black+ "Hello, Termbox!")
  (tb-print 0 1 +tb-blue+ +tb-black+ "Press any key...")
  (tb-present)
  (tb-clear)
  (tb-printf 0 0 +tb-cyan+ +tb-black+ "~a" (tb-poll-event))
  (tb-present)
  (tb-peek-event 5000)
  (tb-shutdown))
