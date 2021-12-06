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

(defun line-vertical-p (line)
  (let ((line-x1 (caar line))
        (line-x2 (caadr line)))
    (= line-x1 line-x2)))
    
(defun line-horizontal-p (line)
  (let ((line-y1 (cdar line))
        (line-y2 (cdadr line)))
    (= line-y1 line-y2)))

(defun filter-hor-or-vert-lines (lines)
  (filter (lambda (line)
            (or (line-vertical-p line)
                (line-horizontal-p line)))
          lines))

(defun add-point (storage point)
  (let ((curr (gethash point storage 0)))
    (setf (gethash point storage) (1+ curr))))

(defun add-points-from-line (storage line)
  (if (line-horizontal-p line)
    (let* ((y (cdar line))
           (x1 (caar line))
           (x2 (caadr line))
           (x-min (min x1 x2))
           (x-max (max x1 x2)))
      (loop :for x :from x-min :to x-max
            :for point = (cons x y)
            :do (add-point storage point)))
    (let* ((x (caar line))
           (y1 (cdar line))
           (y2 (cdadr line))
           (y-min (min y1 y2))
           (y-max (max y1 y2)))
      (loop :for y :from y-min :to y-max
            :for point = (cons x y)
            :do (add-point storage point)))))

(test is-line-vertical
  (is-true (line-vertical-p '((0 . 0) (0 . 5))))
  (is-false (line-vertical-p '((1 . 0) (0 . 5)))))

(test is-line-horizontal
  (is-true (line-horizontal-p '((0 . 1) (5 . 1))))
  (is-false (line-horizontal-p '((0 . 1) (5 . 2)))))

(test filter-lines-for-hor-or-vert-only
  (let ((lines (parse-to-lines-from-string *demo-input*)))
    (is (= 10 (length lines)))
    (is (= 6 (length (filter-hor-or-vert-lines lines))))))

(test build-storage-from-line-points
  (let ((storage (make-hash-table :test #'equal :lock-free t)))
    (add-points-from-line storage '((0 . 0) (2 . 0)))
    (is (= 1 (gethash '(0 . 0) storage)))
    (is (= 1 (gethash '(1 . 0) storage)))
    (is (= 1 (gethash '(2 . 0) storage)))
    (is-false (gethash '(2 . 1) storage))
    (add-points-from-line storage '((0 . 0) (0 . 2)))
    (is (= 2 (gethash '(0 . 0) storage)))
    (is (= 1 (gethash '(0 . 1) storage)))
    (is (= 1 (gethash '(0 . 2) storage)))
    (is-false (gethash '(1 . 2) storage))))

(test day5-demo
  (let ((lines (filter-hor-or-vert-lines
                (parse-to-lines-from-string *demo-input*)))
        (storage (make-hash-table :test #'equal :lock-free t))
        (result 0))
    (dolist (line lines)
      (add-points-from-line storage line))
    (print storage)
    (maphash (lambda (k v) (if (>= v 2)
                          (incf result)))
             storage)
    (is (= 5 result))))

(run! 'is-line-vertical)
(run! 'is-line-horizontal)
(run! 'filter-lines-for-hor-or-vert-only)
(run! 'build-storage-from-line-points)
(run! 'day5-demo)

;; --------------- 1 ----------------

(test day5-1
  (let ((lines (filter-hor-or-vert-lines
                (parse-to-lines-from-string (str:from-file #P"day5-input.txt"))))
        (storage (make-hash-table :test #'equal :lock-free t))
        (result 0))
    (dolist (line lines)
      (add-points-from-line storage line))
    (print storage)
    (maphash (lambda (k v) (if (>= v 2)
                          (incf result)))
             storage)
    (is (= 5092 result))))

(run! 'day5-1)
