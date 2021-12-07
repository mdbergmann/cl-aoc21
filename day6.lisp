(in-package :cl-user)
(ql:quickload '(:fiveam :str :binding-arrows))

(defpackage :aoc21.day6-test
  (:use :cl :fiveam :str :binding-arrows)
  (:export #:run!
           #:all-tests
           #:nil))
(in-package :aoc21.day6-test)

(def-suite day6-tests
  :description "AoC 21 day6 tests")

(in-suite day6-tests)

(defun filter (pred lst)
  (mapcan (lambda (x)
            (if (funcall pred x)
                (list x)))
          lst))

;; ------------ demo -----------

(defparameter *demo-input* '(3 4 3 1 2))

(defun new-timer (timer)
  (cond
    ((= 0 timer) (cons 6 8))
    ((<= timer 8) (cons (1- timer) nil))))

(defun new-day (timers)
  (let ((new-timers (mapcar #'new-timer
                            timers)))
    (append
     (mapcar #'car new-timers)
     (filter (lambda (timer) timer) (mapcar #'cdr new-timers)))))

(defun new-day-2 (timers)
  (let ((to-append nil))
    (loop :for i :below (length timers)
          :for timer = (elt timers i)
          :for new-timer = (new-timer timer)
          :do (progn
                (setf (elt timers i) (car new-timer))
                (if (cdr new-timer)
                    (setf to-append (append to-append (list (cdr new-timer)))))))
    (dolist (n to-append)
      (vector-push-extend n timers))
    (length to-append)))

(defun generate-fish (initial days)
  (loop :repeat days
        :with result = initial
        :do (setf result (new-day result))
        :finally (return result)))

(defun generate-fish-2 (timers days)
  (loop :repeat days
        :do (new-day-2 timers)))

(test new-timer
  (is (equal '(2 . nil) (new-timer 3)))
  (is (equal '(1 . nil) (new-timer 2)))
  (is (equal '(0 . nil) (new-timer 1)))
  (is (equal '(6 . 8) (new-timer 0)))
  (is (equal '(5 . nil) (new-timer 6)))
  (is (equal '(7 . nil) (new-timer 8))))

(test new-day
  (is (equal '(2 3 2 0 1) (new-day '(3 4 3 1 2))))
  (is (equal '(1 2 1 6 0 8) (new-day '(2 3 2 0 1))))
  (is (equal '(0 1 0 5 6 7 8) (new-day '(1 2 1 6 0 8)))))

(test new-day-2
  (let ((timers (make-array 0 :adjustable t
                              :fill-pointer t)))
    (dolist (n '(3 4 3 1 2))
      (vector-push-extend n timers))
    (is (= 0 (new-day-2 timers)))
    (is (= 5 (length timers)))
    (is (= 1 (new-day-2 timers)))
    (is (= 6 (length timers)))
    (is (= 1 (new-day-2 timers)))
    (is (= 7 (length timers)))))

(defun day6-1-demo ()
  (generate-fish *demo-input* 80))

(test day6-1-demo
  (is (= 5934 (length (day6-1-demo)))))

(defun day6-1-2-demo ()
  (let ((timers (make-array 0 :adjustable t
                              :fill-pointer t)))
    (dolist (n *demo-input*)
      (vector-push-extend n timers))
    (generate-fish-2 timers 80)
    timers))

(test day6-1-2-demo
  (is (= 5934 (length (day6-1-2-demo)))))

(run! 'new-timer)
(run! 'new-day)
(run! 'new-day-2)
(time 
 (run! 'day6-1-demo))
(time 
 (run! 'day6-1-2-demo))

;; --------------- 1 ---------------

(defun parse-input ()
  (->> #P"day6-input.txt"
    (str:from-file)
    (str:split ",")
    (mapcar #'parse-integer)))

(defun day6-1 ()
  (let ((input (parse-input)))
    (generate-fish input 80)))

(test day6-1
  (is (= 390011 (length (day6-1)))))

(defun day6-1-2 ()
  (let ((timers (make-array 0 :adjustable t
                              :fill-pointer t)))
    (dolist (n (parse-input))
      (vector-push-extend n timers))
    (generate-fish-2 timers 80)
    timers))

(test day6-1-2
  (is (= 390011 (length (day6-1-2)))))

;;(time (run! 'day6-1))
;;(time (run! 'day6-1-2))

;; --------------- 2 ---------------

(defun day6-2 ()
  (let ((input (parse-input)))
    (generate-fish input 256)))

(test day6-2
  (is (= 390011 (length (day6-2)))))

(defun day6-2-2 ()
  (let ((timers (make-array 0 :adjustable t
                              :fill-pointer t)))
    (dolist (n (parse-input))
      (vector-push-extend n timers))
    (generate-fish-2 timers 256)
    timers))

(test day6-2-2
  (is (= 390011 (length (day6-2-2)))))

;;(time (run! 'day6-2))
;;(time (run! 'day6-2-2))

;; ------ version from Rainer Joswig, which is a 'bit' faster ----------

(defun fish-cycle (&optional (n 256))
  (let* ((all-fish (parse-input))
         (counted-gens (loop :for i :from 0 :to 8 :collect (count i all-fish))))
    (destructuring-bind (a0 a1 a2 a3 a4 a5 a6 a7 a8) counted-gens
      (labels ((cycle (n a0 a1 a2 a3 a4 a5 a6 a7 a8)
                 (format t "~a ~a ~a ~a ~a ~a ~a ~a ~a ~a~%"
                         n a0 a1 a2 a3 a4 a5 a6 a7 a8)
                 (if (zerop n)
                     (+ a0 a1 a2 a3 a4 a5 a6 a7 a8)
                     (cycle (1- n) a1 a2 a3 a4 a5 a6 (+ a7 a0) a8 a0))))
        (cycle n a0 a1 a2 a3 a4 a5 a6 a7 a8)))))
