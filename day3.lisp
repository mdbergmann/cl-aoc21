(in-package :cl-user)
(ql:quickload '(:fiveam :str :binding-arrows :bit-smasher))

(defpackage :aoc21.day3-test
  (:use :cl :fiveam :str :binding-arrows :bit-smasher)
  (:export #:run!
           #:all-tests
           #:nil))
(in-package :aoc21.day3-test)

(def-suite day3-tests
  :description "AoC 21 day3 tests")

(in-suite day3-tests)

(defun string-to-bitvector (str)
  (coerce
   (map 'simple-bit-vector #'digit-char-p (coerce str 'list))
   'bit-vector))

(defparameter *demo-input*
  (mapcar #'string-to-bitvector
          '("00100"
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
            "01010")))

(defun most-common-bit (bitvec-seq index)
  (loop :for bitvec :in bitvec-seq
        :for bit = (elt bitvec index)
        :with ones = 0
        :with zeroes = 0
        :if (= 0 bit)
          :do (incf zeroes)
        :else
          :do (incf ones)
        :finally (return (if (> ones zeroes) 1 0))))

(defun most-common-bit-seq (bitvec-seq)
  (coerce
   (loop :for i :upto (- (length (car bitvec-seq)) 1)
         :collect (most-common-bit bitvec-seq i))
   'bit-vector))

(defun least-common-bit-seq (bitvec-seq)
  (bit-not (most-common-bit-seq bitvec-seq)))

(test string-to-bitvector
  (is (equalp #*00100 (string-to-bitvector "00100"))))

(test most-common-bit-from-bitvector
  (is (= 1 (most-common-bit *demo-input* 0)))
  (is (= 0 (most-common-bit *demo-input* 1)))
  (is (= 1 (most-common-bit *demo-input* 2)))
  (is (= 1 (most-common-bit *demo-input* 3)))
  (is (= 0 (most-common-bit *demo-input* 4))))

(test most-common-bit-seq
  (is (equalp #*10110 (most-common-bit-seq *demo-input*))))

(test least-common-bit-seq
  (is (equalp #*01001 (least-common-bit-seq *demo-input*))))

(test day3-1-demo
  (is (= 198 (* (int<- (most-common-bit-seq *demo-input*))
                (int<- (least-common-bit-seq *demo-input*))))))

(defparameter *input-1*
  (->> #P"day3-input.txt"
    (str:from-file)
    (str:split #\NewLine)
    (butlast)
    (mapcar #'string-to-bitvector)))

(test day3-1
  (is (= 3429254 (* (int<- (most-common-bit-seq *input-1*))
                    (int<- (least-common-bit-seq *input-1*))))))

(run! 'string-to-bitvector)
(run! 'most-common-bit-from-bitvector)
(run! 'most-common-bit-seq)
(run! 'least-common-bit-seq)
(run! 'day3-1-demo)
(run! 'day3-1)
