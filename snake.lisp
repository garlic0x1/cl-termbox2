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

(defun render (game &key (snake-bg +tb-red+))
  (tb-clear)
  (dolist (cell (snake game))
    (tb-print (car cell) (cdr cell) +tb-white+ snake-bg "s"))
  (dolist (goal (goals game))
    (tb-print (car goal) (cdr goal) +tb-cyan+ +tb-yellow+ "g"))
  (tb-present))

(defun relative-cell (cell direction)
  (case direction
    (:north (cons (car cell) (1- (cdr cell))))
    (:south (cons (car cell) (1+ (cdr cell))))
    (:east (cons (1+ (car cell)) (cdr cell)))
    (:west (cons (1- (car cell)) (cdr cell)))
    (otherwise (error "Bad direction."))))

(defun valid-cell (game cell)
  (and (< (car cell) (tb-width))
       (< (cdr cell) (tb-height))
       (<= 0 (car cell))
       (<= 0 (cdr cell))
       (not (find cell (snake game) :test #'equal))))

(defun eat-cell (game cell)
  (when (find cell (goals game) :test #'equal)
    (setf (score game) (1+ (score game))
          (goals game) (remove cell (goals game) :test #'equal)))
  (setf (snake game) (last (append (snake game) (list cell)) (score game))))

(defun move-snake (game)
  (let ((next (relative-cell (car (last (snake game))) (direction game))))
    (if (valid-cell game next)
        (eat-cell game next)
        (setf (exit game) t))))

(defun get-input (game)
  (setf (direction game)
        (let ((k (tb-event-key (tb-peek-event 1000))))
          (cond ((= k +tb-key-arrow-up+) :north)
                ((= k +tb-key-arrow-down+) :south)
                ((= k +tb-key-arrow-right+) :east)
                ((= k +tb-key-arrow-left+) :west)
                (t (direction game))))))

(defun spawn-goal (game)
  (when (> +max-goals+ (length (goals game)))
    (let ((x (random (tb-width)))
          (y (random (tb-height))))
      (push (cons x y) (goals game)))))

(defun game-loop (game)
  (spawn-goal game)
  (move-snake game)
  (render game)
  (unless (exit game)
    (get-input game)
    (game-loop game)))

(defun flash-game (game n)
  (dotimes (i n)
    (render game :snake-bg +tb-white+)
    (sleep 0.1)
    (render game)
    (sleep 0.1)))

(defun game-over (game)
  (tb-clear)
  (tb-print 0 0 +tb-red+ +tb-black+ "Game over.")
  (tb-printf 0 1 +tb-cyan+ +tb-black+ "Score: ~a" (score game))
  (tb-print 0 3 +tb-white+ +tb-black+ "Press any key...")
  (tb-present)
  (tb-poll-event))

(defun main ()
  (tb-init)
  (let ((game (make-instance 'game)))
    (game-loop game)
    (flash-game game 3)
    (game-over game)
    (tb-shutdown)))
