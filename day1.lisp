;;(ql:quickload '(:fiveam :str :binding-arrows))

(defpackage :aoc21.day1-test
  (:use :cl :fiveam :str :binding-arrows)
  (:export #:run!
           #:all-tests
           #:nil))
(in-package :aoc21.day1-test)

(def-suite day1-tests
  :description "AoC 21 day1 tests")

(in-suite day1-tests)

(defun filter (pred lst)
  (mapcan (lambda (x) (if (funcall pred x)
                     (list x)))
          lst))

(defparameter *input1*
  (->> #P"~/Development/MySources/cl-aoc21/day1-input.txt"
    (str:from-file)
    (str:split #\NewLine)
    (filter (lambda (str) (> (length str) 0)))
    (mapcar #'parse-integer)))

(defun increased-depths (depths)
  (let ((increased 0))
    (loop :for i :in depths
          :with previous = nil
          :if (and previous (> i previous))
            :do (incf increased)
          :do (setf previous i))
    increased))

(test day1-1
  "day1-1"
  (is (= 1446 (increased-depths *input1*))))

(run! 'day1-1)
