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

(defun count-increased-depths (depths)
  (let ((increased 0))
    (loop :for i :in depths
          :with previous = nil
          :if (and previous (> i previous))
            :do (incf increased)
          :do (setf previous i))
    increased))

(test day1-1
  "day1-1"
  (is (= 1446 (count-increased-depths *input1*))))


(defparameter *input2-demo* '(199 200 208 210 200 207 240 269 260 263))

(defun count-increased-depth-sums (depths)
  (count-increased-depths 
   (loop :for i :from 0 :to (length depths)
         :for n1 = (nth i depths)
         :for n2 = (nth (+ i 1) depths)
         :for n3 = (nth (+ i 2) depths)
         :for sum = (+ (if n1 n1 0) (if n2 n2 0) (if n3 n3 0))
         :collect sum)))

(test day1-2-demo
  (is (= 5 (count-increased-depth-sums *input2-demo*))))

(test day1-2
  (is (= 1486 (count-increased-depth-sums *input1*))))

(run! 'day1-1)
(run! 'day1-2-demo)
(run! 'day1-2)
