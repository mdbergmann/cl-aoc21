(in-package :cl-user)
(ql:quickload '(:fiveam :str :binding-arrows))

(defpackage :aoc21.day6-test
  (:use :cl :fiveam :str :binding-arrows)
  (:export #:run!
           #:all-tests
           #:nil))
(in-package :aoc21.day6-test)

(def-suite day6-tests
  :description "AoC 21 day6 tests")

(in-suite day6-tests)

(defun filter (pred lst)
  (mapcan (lambda (x)
            (if (funcall pred x)
                (list x)))
          lst))

;; ------------ demo -----------

(defparameter *demo-input* '(3 4 3 1 2))

(defun new-timer (timer)
  (cond
    ((= 0 timer) (cons 6 8))
    ((<= timer 8) (cons (1- timer) nil))))

(defun new-day (timers)
  (let ((new-timers (mapcar #'new-timer
                            timers)))
    (append
     (mapcar #'car new-timers)
     (filter (lambda (timer) timer) (mapcar #'cdr new-timers)))))

(defun generate-fish (initial days)
  (loop :repeat days
        :with result = initial
        :do (setf result (new-day result))
        :finally (return result)))

(test new-timer
  (is (equal '(2 . nil) (new-timer 3)))
  (is (equal '(1 . nil) (new-timer 2)))
  (is (equal '(0 . nil) (new-timer 1)))
  (is (equal '(6 . 8) (new-timer 0)))
  (is (equal '(5 . nil) (new-timer 6)))
  (is (equal '(7 . nil) (new-timer 8))))

(test new-day
  (is (equal '(2 3 2 0 1) (new-day '(3 4 3 1 2))))
  (is (equal '(1 2 1 6 0 8) (new-day '(2 3 2 0 1))))
  (is (equal '(0 1 0 5 6 7 8) (new-day '(1 2 1 6 0 8)))))

(test day6-1-demo
  (is (= 5934 (length (generate-fish *demo-input* 80)))))

(run! 'new-timer)
(run! 'new-day)
(run! 'day6-1-demo)

;; --------------- 1 ---------------

(defun parse-input ()
  (->> #P"day6-input.txt"
    (str:from-file)
    (str:split ",")
    (mapcar #'parse-integer)))

(test day6-1
  (let ((input (parse-input)))
    (is (= 390011 (length (generate-fish input 80))))))

(run! 'day6-1)
