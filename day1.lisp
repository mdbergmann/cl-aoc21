(in-package :cl-user)
(ql:quickload '(:fiveam :str :binding-arrows))

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
  (mapcan (lambda (x)
            (if (funcall pred x)
                (list x)))
          lst))

(defparameter *input1*
  (->> #P"day1-input.txt"
    (str:from-file)
    (str:split #\NewLine)
    (filter (lambda (str) (> (length str) 0)))
    (mapcar #'parse-integer)))

(defun count-increased (depths)
  (let ((increased 0))
    (loop :for i :in depths
          :with previous = nil
          :if (and previous (> i previous))
            :do (incf increased)
          :do (setf previous i))
    increased))

(test day1-1
  "day1-1"
  (is (= 1446 (count-increased *input1*))))


(defparameter *input2-demo* '(199 200 208 210 200 207 240 269 260 263))

(defun count-increased-depth-sums (depths)
  (count-increased 
   (loop :for i :from 0 :to (length depths)
         :for elems = (list
                       (or (nth i depths) 0)
                       (or (nth (+ i 1) depths) 0)
                       (or (nth (+ i 2) depths) 0))
         :for sum = (reduce #'+ elems :initial-value 0)
         :collect sum)))

(test day1-2-demo
  (is (= 5 (count-increased-depth-sums *input2-demo*))))

(test day1-2
  (is (= 1486 (count-increased-depth-sums *input1*))))

(run! 'day1-1)
(run! 'day1-2-demo)
(run! 'day1-2)
