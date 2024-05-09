(defpackage :termbox2/snake
  (:use :cl :termbox2))
(in-package :termbox2/snake)

(defparameter +max-goals+ 8)

(defclass game ()
  ((snake
    :initform '((0 . 0))
    :accessor snake)
   (score
    :initform 1
    :accessor score)
   (goals
    :initform '()
    :accessor goals)
   (direction
    :initform :east
    :accessor direction)
   (exit
    :initform nil
    :accessor exit)))

(defun render (game)
  (tb-clear)
  (dolist (cell (snake game))
    (tb-print (car cell) (cdr cell) +tb_white+ +tb_red+ "s"))
  (dolist (goal (goals game))
    (tb-print (car goal) (cdr goal) +tb_cyan+ +tb_yellow+ "g"))
  (tb-present))

(defun spawn-goal (game)
  (let ((x (random (tb-width)))
        (y (random (tb-height))))
    (push (cons x y) (goals game))))

(defun move-snake (game)
  (flet ((relative-cell (cell direction)
           (case direction
             (:north (cons (car cell) (1- (cdr cell))))
             (:south (cons (car cell) (1+ (cdr cell))))
             (:east (cons (1+ (car cell)) (cdr cell)))
             (:west (cons (1- (car cell)) (cdr cell)))
             (otherwise (error "Bad direction."))))
         (valid-cell (cell)
           (and (< (car cell) (tb-width))
                (< (cdr cell) (tb-height))))
         (eat-cell (cell)
           (when (find cell (goals game) :test #'equal)
             (setf (score game) (1+ (score game)))
             (delete cell (goals game) :test #'equal))
           (setf (snake game) (last (append (snake game) (list cell)) (score game)))))
    (let ((next (relative-cell (car (last (snake game))) (direction game))))
      (if (valid-cell next)
          (eat-cell next)
          (setf (exit game) t)))))

(defparameter *last-ev* nil)
(defun game-loop (game)
  (when (> +max-goals+ (length (goals game)))
    (spawn-goal game))
  (move-snake game)
  (render game)
  (unless (exit game)
    (let ((k (tb-event-key (setf *last-ev* (tb-peek-event 1000)))))
      (setf (direction game)
            (cond ((= k +tb_key_arrow_up+) :north)
                  ((= k +tb_key_arrow_down+) :south)
                  ((= k +tb_key_arrow_right+) :east)
                  ((= k +tb_key_arrow_left+) :west)
                  (t (direction game)))))
    (game-loop game)))

(defun main ()
  (tb-init)
  (let ((game (make-instance 'game)))
    (game-loop game)
    (tb-shutdown)
    (describe game))
  (print *last-ev*))
