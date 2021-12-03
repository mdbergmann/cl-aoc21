(in-package :cl-user)
(ql:quickload '(:fiveam :str :binding-arrows))

(defpackage :aoc21.day3-test
  (:use :cl :fiveam :str :binding-arrows)
  (:export #:run!
           #:all-tests
           #:nil))
(in-package :aoc21.day3-test)

(def-suite day3-tests
  :description "AoC 21 day3 tests")

(in-suite day3-tests)

;; (defun parse-to-command-seq (input)
;;   (->> input
;;     (str:split #\NewLine)
;;     (filter (lambda (str) (> (length str) 0)))
;;     (mapcar (lambda (line) (let ((line-comps (str:split #\Space line)))
;;                         (cons (intern (string-upcase (first line-comps)))
;;                               (parse-integer (second line-comps))))))))

;; (defparameter *input1*
;;   (->> #P"day2-input.txt"
;;     (str:from-file)
;;     (parse-to-command-seq)))

(defparameter *demo-input* '("00100"
                             "11110"
                             "10110"
                             "10111"
                             "10101"
                             "01111"
                             "00111"
                             "11100"
                             "10000"
                             "11001"
                             "00010"
                             "01010"))

(defun convert-string-to-bitvector (str)
  (coerce
   (mapcar (lambda (ch) (parse-integer (string ch))) (coerce str 'list))
   'bit-vector))

(test convert-string-to-bitvector
  (is (equalp #*00100 (convert-string-to-bitvector "00100"))))


(run! 'convert-string-to-bitvector)
