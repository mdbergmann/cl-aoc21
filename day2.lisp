(in-package :cl-user)
(ql:quickload '(:fiveam :str :binding-arrows))

(defpackage :aoc21.day2-test
  (:use :cl :fiveam :str :binding-arrows)
  (:export #:run!
           #:all-tests
           #:nil))
(in-package :aoc21.day2-test)

(def-suite day2-tests
  :description "AoC 21 day2 tests")

(in-suite day2-tests)

(defun filter (pred lst)
  (mapcan (lambda (x)
            (if (funcall pred x)
                (list x)))
          lst))

(defun parse-input (input)
  (->> input
    (str:split #\NewLine)
    (filter (lambda (str) (> (length str) 0)))
    (mapcar (lambda (line) (let ((line-comps (str:split #\Space line)))
                        (cons (intern (string-upcase (first line-comps)))
                              (parse-integer (second line-comps))))))))

(defparameter *input1*
  (->> #P"day2-input.txt"
    (str:from-file)
    (parse-input)))

(defstruct sub
  (hor-pos 0)
  (depth 0))

(defun process-cmd (cmd sub)
  (ecase (car cmd)
    (forward (incf (sub-hor-pos sub) (cdr cmd)))
    (down (incf (sub-depth sub) (cdr cmd)))
    (up (incf (sub-depth sub) (- (cdr cmd)))))
  (cons (sub-hor-pos sub) (sub-depth sub)))

(defun process-cmd-seq (seq sub)
  (car (reverse (mapcar (lambda (cmd) (process-cmd cmd sub)) seq))))

(test day2-1-demo--move--single
  (let ((sub (make-sub)))
    (is (equal '(5 . 0) (process-cmd '(forward . 5) sub)))
    (is (equal '(8 . 0) (process-cmd '(forward . 3) sub)))
    (is (equal '(8 . 4) (process-cmd '(down . 4) sub)))
    (is (equal '(8 . 6) (process-cmd '(down . 2) sub)))
    (is (equal '(8 . 3) (process-cmd '(up . 3) sub)))))

(test day2-1-demo--move--sequence
  (let ((sub (make-sub)))
    (is (equal '(8 . 3) (process-cmd-seq
                         '((forward . 5)
                           (forward . 3)
                           (down . 4)
                           (down . 2)
                           (up . 3)) sub)))))

(test day2-1
  (let* ((sub (make-sub))
         (sub-state (process-cmd-seq *input1* sub)))
    (is (= 1488669 (* (car sub-state) (cdr sub-state))))))

(run! 'day2-1-demo--move--single)
(run! 'day2-1-demo--move--sequence)
(run! 'day2-1)
