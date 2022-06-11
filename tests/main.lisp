(defpackage lincl/tests/main
  (:use :cl
        :lincl
        :rove))
(in-package :lincl/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :lincl)' in your Lisp.

(defparameter *mat-1x2* #2A((2 4)))
(defparameter *mat-2x2* #2A((2 4) (5 7)))
(defparameter *mat-3x2* #2A((2 4) (5 7) (3 2)))
(defparameter *mat-4x2* #2A((2 4) (5 7) (3 2) (9 0)))

(defparameter *mat-1x4* #2A((2 4 5 7)))
(defparameter *mat-2x4* #2A((2 4 5 7) (3 2 9 0)))
(defparameter *mat-3x4* #2A((2 4 5 7) (3 2 9 0) (8 5 3 1)))
(defparameter *mat-4x4* #2A((2 4 5 7) (3 2 9 0) (8 5 3 1) (0 1 2 3)))



(deftest matrix-conformable
  (testing "Are the two matrices conformable for product"
    (ok (conformable? *mat-2x4* *mat-4x2*) '(2 2))
    (ok (conformable? *mat-4x2* *mat-2x4*) '(2 2))
    (ng (conformable? *mat-3x4* *mat-4x2*))
    (ng (conformable? *mat-4x4* *mat-4x2*))))

(deftest matrix-product
  (testing "Is the product of two matrices correct"
    (ok (mm-equal (mm* *mat-4x4* *mat-4x4*) #2A((56 48 75 40) (84 61 60 30) (55 58 96 62) (19 15 21 11))))
    (ok (mm-equal (mm* *mat-2x4* *mat-4x2*) #2A((102 46) (43 44))))))
