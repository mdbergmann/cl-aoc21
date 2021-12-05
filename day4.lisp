(in-package :cl-user)
(ql:quickload '(:fiveam :str :binding-arrows))

(defpackage :aoc21.day4-test
  (:use :cl :fiveam :str :binding-arrows)
  (:export #:run!
           #:all-tests
           #:nil))
(in-package :aoc21.day4-test)

(def-suite day4-tests
  :description "AoC 21 day4 tests")

(in-suite day4-tests)

(defun filter (pred lst)
  (mapcan (lambda (x)
            (if (funcall pred x)
                (list x)))
          lst))

;; ------------- input --------------

(defun parse-input-number-line (line)
  (mapcar #'parse-integer (str:split "," line)))

(defun parse-input-board-line (line)
  (mapcar #'parse-integer (filter (lambda (n) (not (emptyp n)))
                                  (str:split #\Space line))))

(defun parse-input-boards (lines)
  (loop :for line :in lines
        :with board = '()
        :if (> (length line) 0)
          :do (setf board
                    (cons (parse-input-board-line line) board))
        :else
          :do (setf board '())
        :when (= 5 (length board))
          :collect (reverse board)))

(defun next-line (lines)
  (cdr lines))

(defun parse-input-from-file ()
  (let* ((file #P"day4-input.txt")
         (string (str:from-file file))
         (lines (str:split #\NewLine string))
         (numbers nil)
         (boards nil))
    (progn 
      (setf numbers (parse-input-number-line (car lines)))
      (setf lines (next-line lines)))
    (setf lines (next-line lines))
    (setf boards (parse-input-boards lines))
    (cons numbers boards)))

(test parse-input
  (let ((parsed (parse-input-from-file)))
    (is (typep (car parsed) 'list))
    (is-true (rest (car parsed)))
    (is-true (every #'numberp (car parsed)))
    ;;
    (is (typep (cdr parsed) 'list))
    (is-true (rest (cdr parsed)))
    (is-true (notevery #'null (cdr parsed)))
    (is-true (every (lambda (x) (= 5 (length x))) (cdr parsed)))))

(run! 'parse-input)
