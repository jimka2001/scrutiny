;; Copyright (c) 2018 EPITA Research and Development Laboratory
;;
;; Permission is hereby granted, free of charge, to any person obtaining
;; a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software without restriction,
;; including without limitation the rights to use, copy, modify, merge,
;; publish, distribute, sublicense, and/or sell copies of the Software,
;; and to permit persons to whom the Software is furnished to do so,
;; subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
;; LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
;; OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
;; WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.



(defpackage :scrutiny-test
  (:use :cl :scrutiny))

(in-package :scrutiny-test)

(define-test scrutiny-1
  (assert-false (= 1 2))
  (assert-false (= 1 3))
  (assert-true (= 1 1))
  (assert-true (= 2 2))
  (assert-false (= 2 3))
  (assert-error division-by-zero (/ 1 0))
  (let ((a 2)
	(b 1))
    (assert-false (< a b)))
  (let ((a "abc")
	(b "ABCD"))
    (assert-true (string-equal a b))
    (assert-true (string-equal (concatenate 'string a b)
			       (format nil "~A ~A" b a))))
  (assert-error error
		(error "some error"))
)
