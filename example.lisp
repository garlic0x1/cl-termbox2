(defpackage :termbox2/example
  (:use :cl :termbox2))
(in-package :termbox2/example)

(defun main ()
  (tb-init)
  (tb-print 0 0 +tb_blue+ +tb_red+ "Hello")
  (tb-present)
  (let ((ev (tb-poll-event)))
    (tb-shutdown)
    (print ev)))
