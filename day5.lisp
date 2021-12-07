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

(defun line-vertical-p (line)
  (let ((line-x1 (caar line))
        (line-x2 (caadr line)))
    (= line-x1 line-x2)))
    
(defun line-horizontal-p (line)
  (let ((line-y1 (cdar line))
        (line-y2 (cdadr line)))
    (= line-y1 line-y2)))

(defun line-diagonal-p (line)
  (let* ((line-y1 (cdar line))
         (line-y2 (cdadr line))
         (line-x1 (caar line))
         (line-x2 (caadr line))
         (diff-x (- line-x2 line-x1))
         (diff-y (- line-y2 line-y1)))
    (or (= diff-x diff-y)
        (= diff-x (- diff-y))
        (= (- diff-x) diff-y))))

(defun filter-hor-vert-lines (lines)
  (filter (lambda (line)
            (or (line-vertical-p line)
                (line-horizontal-p line)))
          lines))

(defun add-point (storage point)
  (let ((curr (gethash point storage 0)))
    (setf (gethash point storage) (1+ curr))))

(defun add-points-from-line (storage line)
  (let* ((y1 (cdar line))
         (y2 (cdadr line))
         (x1 (caar line))
         (x2 (caadr line))
         (x-min (min x1 x2))
         (x-max (max x1 x2))
         (y-min (min y1 y2))
         (y-max (max y1 y2)))
    (cond
      ((line-horizontal-p line)
       (loop :for x :from x-min :to x-max
             :for point = (cons x y1)
             :do (add-point storage point)))
      ((line-vertical-p line)
       (loop :for y :from y-min :to y-max
             :for point = (cons x1 y)
             :do (add-point storage point)))
      ((line-diagonal-p line)
       (if (and (>= x2 x1)
                (>= y2 y1))
           (loop :for x :from x-min :to x-max
                 :for y :from y-min :to y-max
                 :for point = (cons x y)
                 :do (add-point storage point))
           (loop :for x :from x-max :downto x-min
                 :for y :from y-min :to y-max
                 :for point = (cons x y)
                 :do (add-point storage point)))))))

(defun collect-overlaps (n storage)
  (let ((result 0))
    (maphash (lambda (k v)
               (declare (ignore k))
               (if (>= v n)
                   (incf result)))
             storage)
    result))

(test is-line-vertical
  (is-true (line-vertical-p '((0 . 0) (0 . 5))))
  (is-false (line-vertical-p '((1 . 0) (0 . 5)))))

(test is-line-horizontal
  (is-true (line-horizontal-p '((0 . 1) (5 . 1))))
  (is-false (line-horizontal-p '((0 . 1) (5 . 2)))))

(test filter-lines-for-hor-or-vert-only
  (let ((lines (parse-to-lines-from-string *demo-input*)))
    (is (= 10 (length lines)))
    (is (= 6 (length (filter-hor-vert-lines lines))))))

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

(test build-storage-from-diag-line-points
  (let ((storage (make-hash-table :test #'equal :lock-free t)))
    (add-points-from-line storage '((1 . 1) (3 . 3)))
    (is (= 1 (gethash '(1 . 1) storage)))
    (is (= 1 (gethash '(2 . 2) storage)))
    (is (= 1 (gethash '(3 . 3) storage)))
    (add-points-from-line storage '((9 . 7) (7 . 9)))
    (is (= 1 (gethash '(9 . 7) storage)))
    (is (= 1 (gethash '(8 . 8) storage)))
    (is (= 1 (gethash '(7 . 9) storage)))))

;; --------------- 1 demo ----------------

(test day5-demo
  (let ((lines (filter-hor-vert-lines
                (parse-to-lines-from-string *demo-input*)))
        (storage (make-hash-table :test #'equal :lock-free t)))
    (dolist (line lines)
      (add-points-from-line storage line))
    (is (= 5 (collect-overlaps 2 storage)))))

(run! 'is-line-vertical)
(run! 'is-line-horizontal)
(run! 'filter-lines-for-hor-or-vert-only)
(run! 'build-storage-from-line-points)
(run! 'build-storage-from-diag-line-points)
(run! 'day5-demo)

;; --------------- 1 ----------------

(test day5-1
  (let ((lines (filter-hor-vert-lines
                (parse-to-lines-from-string (str:from-file #P"day5-input.txt"))))
        (storage (make-hash-table :test #'equal :lock-free t)))
    (dolist (line lines)
      (add-points-from-line storage line))
    (is (= 5092 (collect-overlaps 2 storage)))))

(run! 'day5-1)

;; --------------- 2 demo -------------

(defun filter-hor-vert-diag-lines (lines)
  (filter (lambda (line)
            (or (line-vertical-p line)
                (line-horizontal-p line)
                (line-diagonal-p line)))
          lines))

(defun day5-2-demo ()
  (let ((lines (filter-hor-vert-diag-lines
                (parse-to-lines-from-string *demo-input*)))
        (storage (make-hash-table :test #'equal :lock-free t)))
    (dolist (line lines)
      (add-points-from-line storage line))
    (collect-overlaps 2 storage)))

(test diag-line
  (is-true (line-diagonal-p '((1 . 1) (3 . 3))))
  (is-true (line-diagonal-p '((9 . 7) (7 . 9)))))

(test day5-2-demo
    (is (= 12 (day5-2-demo))))

(run! 'diag-line)
(run! 'day5-2-demo)

;; ---------------- 2 -----------------

(test day5-2
  (let ((lines (filter-hor-vert-diag-lines
                (parse-to-lines-from-string (str:from-file #P"day5-input.txt"))))
        (storage (make-hash-table :test #'equal :lock-free t)))
    (dolist (line lines)
      (add-points-from-line storage line))
    (is (= 22066 (collect-overlaps 2 storage)))))

(run! 'day5-2)
