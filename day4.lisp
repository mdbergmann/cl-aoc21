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
  (mapcar (lambda (field)
            (cons (parse-integer field) nil))
          (filter (lambda (n) (not (emptyp n)))
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

(defun parse-input-from-string (string)
  (let* ((lines (str:split #\NewLine string))
         (numbers nil)
         (boards nil))
    (progn 
      (setf numbers (parse-input-number-line (car lines)))
      (setf lines (next-line lines)))
    (setf lines (next-line lines))
    (setf boards (parse-input-boards lines))
    (cons numbers boards)))

(defun parse-input-from-file ()
  (->> #P"day4-input.txt"
    (str:from-file)
    (parse-input-from-string)))

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

;; ------------ demo --------------

(defparameter *demo-input*
  "7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1

22 13 17 11  0
 8  2 23  4 24
21  9 14 16  7
 6 10  3 18  5
 1 12 20 15 19

 3 15  0  2 22
 9 18 13 17  5
19  8  7 25 23
20 11 10 24  4
14 21 16 12  6

14 21 17 24  4
10 16 15  9 19
18  8 23 26 20
22 11 13  6  5
 2  0 12  3  7")

(defun mark-fields-in-board-line (line field-number)
  (mapcar (lambda (field)
            (if (= (car field) field-number)
                (cons (car field) t)
                (cons (car field) (cdr field))))
          line))

(defun mark-fields-in-board (board field-number)
  (mapcar (lambda (line) (mark-fields-in-board-line line field-number))
          board))

(defun mark-fields (boards field-number)
  (mapcar (lambda (board) (mark-fields-in-board board field-number))
          boards))

(defun board-contains-marked-number (board field-number)
  (some (lambda (line)
          (find-if (lambda (field)
                     (and (= (car field) field-number)
                          (not (null (cdr field)))))
                   line))
        board))

(defun marked-boards-with (boards field-number)
  (loop :for board :in boards
        :when (board-contains-marked-number board field-number)
          :collect board))

(defun board-has-fully-marked-row (board)
  (some (lambda (line)
          (every (lambda (field)
                   (cdr field)) line))
        board))

(defun board-has-fully-marked-col (board)
  (some (lambda (index)
          (every (lambda (line)
                   (cdr (nth index line))) board))
        '(0 1 2 3 4)))

(defun winner-board (boards)
  (loop :for board :in boards
        :for idx :from 0
        :when (or (board-has-fully-marked-row board)
                  (board-has-fully-marked-col board))
          :collect (cons (copy-list board) idx)))

(defun count-unmarked-fields-of (board)
  (reduce #'+
          (loop :for line :in board
                :append (mapcar #'car
                                (filter (lambda (field) (null (cdr field)))
                                        line)))))

(test mark-field
  (let* ((play-data (parse-input-from-string *demo-input*))
         (boards (cdr play-data)))
    (setf boards (mark-fields boards 11))
    (is (= 3 (length (marked-boards-with boards 11))))))

(test detect-row-win
  (let* ((play-data (parse-input-from-string *demo-input*))
         (boards (cdr play-data)))
    (loop :for number :in '(7 4 9 5 11 17 23 2 0 14 21 24)
          :do (setf boards (mark-fields boards number)))
    (is (= 1 (length (winner-board boards))))
    (is (= 188 (count-unmarked-fields-of (caar (winner-board boards)))))))

(test detect-col-win
  (let* ((play-data (parse-input-from-string *demo-input*))
         (boards (cdr play-data)))
    (loop :for number :in '(3 9 19 20 14)
          :do (setf boards (mark-fields boards number)))
    (is (= 1 (length (winner-board boards))))
    (is (= 259 (count-unmarked-fields-of (caar (winner-board boards)))))))

(run! 'mark-field)
(run! 'detect-row-win)
(run! 'detect-col-win)

;; -------------- 1 ----------------
(test day4-1
  (let* ((play-data (parse-input-from-file))
         (numbers (car play-data))
         (boards (cdr play-data)))
    (is (= 8136
           (loop :for number :in numbers
                 :with winners = nil
                 :do (progn
                       (setf boards (mark-fields boards number))
                       (setf winners (winner-board boards)))
                 :when (car winners)
                   :return (* number (count-unmarked-fields-of (caar winners))))))))

(run! 'day4-1)

;; -------------- 2 -----------------

(test day4-2
  (let* ((play-data (parse-input-from-file))
         (numbers (car play-data))
         (boards (cdr play-data)))
    (is (= 12738
           (loop :for number :in numbers
                 :with winners-idx = nil
                 :with winners = nil
                 :with last-winner-number = nil
                 :do (progn
                       (setf boards (mark-fields boards number))
                       (when (> (length (winner-board boards)) (length winners-idx))
                         (setf winners (winner-board boards))
                         (loop :for idx :in (mapcar #'cdr winners)
                               :when (not (member idx winners-idx))
                                 :do (setf winners-idx (cons idx winners-idx)))
                         (setf last-winner-number number)))
                 :finally (return
                            (* last-winner-number
                               (count-unmarked-fields-of
                                (car (find-if (lambda (board)
                                                (= (car winners-idx)
                                                   (cdr board)))
                                              winners))))))))))

(run! 'day4-2)
