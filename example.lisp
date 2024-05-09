(defpackage :termbox2/example
  (:use :cl :termbox2))
(in-package :termbox2/example)

(defvar res nil)

(defun main ()
  (tb-init)
  (cffi:with-foreign-object (w :uint32)
    (setf (cffi:mem-ref w :uint32) 2)
    (setf res (cons w (cffi:mem-ref w :uint32)))
    (tb-print-ex 0 0 +tb_blue+ +tb_red+ w "Hello"))
  (tb-present)
  (let ((ev (tb-poll-event)))
    (tb-shutdown)
    (print ev)
    (print res)))
