(in-package :cl-user)
(ql:quickload '(:fiveam :str :binding-arrows))

(defpackage :aoc21.day5-test
  (:use :cl :fiveam :str :binding-arrows)
  (:export #:run!
           #:all-tests
           #:nil))
(in-package :aoc21.day5-test)

(def-suite day5-tests
  :description "AoC 21 day5 tests")

(in-suite day5-tests)

;; ------------- input ---------------

(defparameter *demo-input*
  "0,9 -> 5,9
8,0 -> 0,8
9,4 -> 3,4
2,2 -> 2,1
7,0 -> 7,4
6,4 -> 2,0
0,9 -> 2,9
3,4 -> 1,4
0,0 -> 8,8
5,5 -> 8,2")

(defun parse-point (point-string)
  (let ((xy (str:split "," point-string)))
    (cons (parse-integer (car xy)) (parse-integer (cadr xy)))))

(defun parse-line (line-string)
  (mapcar #'parse-point (str:split " -> " line-string)))

(defun parse-to-lines-from-string (input-string)
  (->> input-string
    (str:split #\NewLine)
    (mapcar #'parse-line)))

(test parse-input-to-list-of-lines
  (is (= 10 (length (parse-to-lines-from-string *demo-input*))))
  (is (equal '((0 . 9) (5 . 9)) (car (parse-to-lines-from-string *demo-input*))))
  (is (equal '((5 . 5) (8 . 2)) (car (last (parse-to-lines-from-string *demo-input*))))))

(run! 'parse-input-to-list-of-lines)

;; -------------- demo --------------

(defun point-on-line-p (grid-point line)
  (flet ((on-x-p (grid-x line)
           (and (>= grid-x (caar line))
                (< grid-x (caadr line))))
         (on-y-p (grid-y line)
           (and (>= grid-y (cdar line))
                (< grid-y (cdadr line)))))
    (or (on-x-p (car grid-point) line) 
        (on-y-p (cdr grid-point) line))))

(defun detect-overlap (grid-point lines)
  (length
   (loop :for line :in lines
         :collect (point-on-line-p grid-point line))))

(test detect-line-on-grid
  (is-true (point-on-line-p '(0 . 1) '((0 . 1) (0 . 2))))
  (is-false (point-on-line-p '(0 . 1) '((0 . 2) (0 . 2))))
  (is-false (point-on-line-p '(1 . 1) '((0 . 2) (0 . 2)))))

(test detect-overlappings
  (is (= 0 (detect-overlap '(1 . 1) '(((2 . 2) (2 . 2))))))
  (is (= 1 (detect-overlap '(1 . 1) '(((1 . 1) (2 . 1))))))
  (is (= 2 (detect-overlap '(1 . 1) '(((1 . 1) (2 . 1))
                                      ((1 . 1) (2 . 1)))))))

;; (defun detect-overlap (overlaps grid-size lines)
;;   (loop x :from 0 :to (car grid-size)
;;         :do (loop y :from 0 :to (cdr grid-size)
;;                   :do )
;;         ))

;; (test detect-2-overlap-on-demo-grid
;;   (let ((lines (parse-to-lines-from-string *demo-input*))
;;         (grid-size '(9 . 9)))
;;     (print (car lines))
;;     (is (= 5 (detect-overlap 2 grid-size lines)))))

(run! 'detect-line-on-grid)
(run! 'detect-overlappings)
;;(run! 'detect-2-overlap-on-demo-grid)
