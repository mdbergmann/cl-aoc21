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

(defun filter (pred lst)
  (mapcan (lambda (x)
            (if (funcall pred x)
                (list x)))
          lst))

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
5,5 -> 8,2
")

(defun parse-point (point-string)
  (let ((xy (str:split "," point-string)))
    (cons (parse-integer (car xy)) (parse-integer (cadr xy)))))

(defun parse-line (line-string)
  (mapcar #'parse-point (str:split " -> " line-string)))

(defun parse-to-lines-from-string (input-string)
  (->> input-string
    (str:split #\NewLine)
    (butlast)
    (mapcar #'parse-line)))

(test parse-input-to-list-of-lines
  (is (= 10 (length (parse-to-lines-from-string *demo-input*))))
  (is (equal '((0 . 9) (5 . 9)) (car (parse-to-lines-from-string *demo-input*))))
  (is (equal '((5 . 5) (8 . 2)) (car (last (parse-to-lines-from-string *demo-input*))))))

(run! 'parse-input-to-list-of-lines)

;; -------------- demo --------------

(defun operate-on-grid (grid-size collect-fun)
  (loop :for x :to (car grid-size)
        :append (loop :for y :to (cdr grid-size)
                      :collect (funcall collect-fun (cons x y)))))

(defun point-on-line-p (grid-point line)
  (let ((line-x1 (caar line))
        (line-x2 (caadr line))
        (line-y1 (cdar line))
        (line-y2 (cdadr line))
        (grid-x (car grid-point))
        (grid-y (cdr grid-point)))
    (flet ((on-x-p (grid-x)
             (and (or (and (>= grid-x line-x1)
                           (<= grid-x line-x2))
                      (and (<= grid-x line-x1)
                           (>= grid-x line-x2)))
                  (= grid-y line-y1 line-y2)))
           (on-y-p (grid-y)
             (and (or (and (>= grid-y line-y1)
                           (<= grid-y line-y2))
                      (and (<= grid-y line-y1)
                           (>= grid-y line-y2)))
                  (= grid-x line-x1 line-x2))))
      (or (on-x-p (car grid-point))
          (on-y-p (cdr grid-point))))))

(defun record-overlap-of-point (grid-point lines)
  (loop :for line :in lines
        :when (point-on-line-p grid-point line)
          :collect grid-point))

(defun record-overlap-all (grid-size lines)
  (operate-on-grid grid-size
                   (lambda (grid-point)
                     (record-overlap-of-point grid-point lines))))

(defun record-overlaps-in-records-by-point (grid-point records)
  (loop :for record :in records
        :when (> (length record) 0)
          :collect (count-if (lambda (r) (equal r grid-point))
                             record)))

(defun find-overlaps (n grid-size lines)
  (let ((records (record-overlap-all grid-size lines)))
    (reduce #'+
            (mapcar (lambda (find)
                      (count-if (lambda (x)
                                  (>= x n))
                                find))
                    (operate-on-grid grid-size
                                     (lambda (grid-point)
                                       (record-overlaps-in-records-by-point
                                        grid-point
                                        records)))))))

(defun find-grid-size-from-lines (lines)
  (loop :for line :in lines
        :with max-x = 0
        :with max-y = 0
        :for line-x1 = (caar line)
        :for line-x2 = (caadr line)
        :for line-y1 = (cdar line)
        :for line-y2 = (cdadr line)
        :do (setf max-x (max max-x line-x1 line-x2)
                  max-y (max max-y line-y1 line-y2))
        :finally (return (cons max-x max-y))))

(test detect-line-on-grid
  (is-true (point-on-line-p '(1 . 1) '((1 . 1) (2 . 1))))
  (is-true (point-on-line-p '(2 . 2) '((2 . 2) (2 . 1))))
  (is-true (point-on-line-p '(2 . 2) '((2 . 2) (2 . 2))))
  (is-true (point-on-line-p '(0 . 1) '((0 . 1) (0 . 2))))
  (is-true (point-on-line-p '(2 . 1) '((2 . 2) (2 . 1))))
  (is-false (point-on-line-p '(0 . 1) '((0 . 2) (0 . 2))))
  (is-false (point-on-line-p '(1 . 1) '((0 . 2) (0 . 2))))
  (is-false (point-on-line-p '(1 . 1) '((2 . 2) (2 . 2)))))

(test detect-overlappings
  (is (= 0 (length (record-overlap-of-point '(1 . 1) '(((2 . 2) (2 . 2)))))))
  (is (= 1 (length (record-overlap-of-point '(1 . 1) '(((1 . 1) (2 . 1)))))))
  (is (= 2 (length (record-overlap-of-point '(1 . 1) '(((1 . 1) (2 . 1))
                                                       ((1 . 1) (2 . 1))))))))

(test detect-2-overlap-on-demo-grid
  (let ((lines (parse-to-lines-from-string *demo-input*))
        (grid-size '(9 . 9)))
    (is (= 5 (find-overlaps 2 grid-size lines)))))

(test find-grid-size-from-lines
  (is (equal '(100 . 100) (find-grid-size-from-lines
                           '(((1 . 5) (5 . 1))
                             ((100 . 0) (0 . 100))))))
  )

(run! 'detect-line-on-grid)
(run! 'detect-overlappings)
(run! 'find-grid-size-from-lines)
(run! 'detect-2-overlap-on-demo-grid)

;; ------------- 1 ---------------

(defun day5-1 ()
  (let* ((lines (parse-to-lines-from-string
                 (str:from-file #P"day5-input.txt")))
         (less-lines (subseq lines 0 5))
         (grid-size (find-grid-size-from-lines less-lines)))
    (print grid-size)
    (find-overlaps 2 grid-size less-lines)
    ))

(test day5-1
  )

(run! 'day5-1)
