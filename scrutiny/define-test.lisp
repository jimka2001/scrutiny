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

(in-package :scrutiny)

(defvar *tests* nil "list of tests, symbols which have been 
registered, presumably with DEFINE-TEST.")
(defvar *current-test* nil "The current test being run.")
(defvar *break-on-error* nil "Whether to break into the debugger (or 
default error handler) or whether to capture the error and treat it as 
a test failure.")

(defmacro define-test (test-name &body body)
  "Define a test.  A function of the same name with empty lambda list 
will be defined."
  (declare (type symbol test-name))
  `(progn
     (pushnew ',test-name *tests*)
     (defun ,test-name ()
       ,@body)))

(define-condition test-condition ()
  ((code :initarg :code
	 :reader test-condition-code
	 :initform nil)
   (test :initarg :test
	 :reader test-condition-test
	 :initform *current-test*
	 :type (or null symbol)))
  (:documentation "Parent condition for the conditions in the scrutiny package."))
(define-condition test-pass (test-condition)
  ())
(define-condition test-fail (test-condition)
  ((expected :reader test-condition-expected
	     :initarg :expected)
   (received :reader test-condition-received
	     :initarg :received)
   (arguments :reader test-condition-arguments
	      :initarg :arguments))
  (:documentation "Condition designating a failed assertion"))
(define-condition test-error (test-condition)
  ((error :initarg :error
	  :reader test-condition-error
	  :type error))
  (:documentation "Condition designating a non-asserted error."))

(defun test-report (tests-start-time num-passed failed errors)
  "Report the results of the tests--printed to stdout."
  (format t "------------------~%")
  (format t "Summary of tests:~%")
  (format t "PACKAGES: ~A~%" (let (packages)
			       (dolist (test *tests*)
				 (when (symbol-package test)
				   (pushnew (package-name (symbol-package test)) packages
					    :test #'string=)))
			       packages))
  (format t "TOTAL TESTS: ~D~%" (length *tests*))
  (format t "ASSERTIONS PASSED: ~D~%" num-passed)
  (format t "ASSERTIONS FAILED: ~D~%" (length failed))
  (let (tests-failed)
    (dolist (f failed)
      (pushnew (test-condition-test f) tests-failed))
    (dolist (f tests-failed)
      (let ((*package* (find-package :keyword)))
	(format t "  ~D failed assertions in ~S~%"
		(count f failed :key #'test-condition-test ) f))))
  (format t "ERRORS: ~D~%" (length errors))
  (dolist (f errors)
    (format t "  ~S~%" (test-condition-test f)))
  (format t "ELAPSED TIME: ")
  (let ((elapsed (- (get-universal-time) tests-start-time)))
    (cond ((< elapsed 60)
	   (format t "~D seconds~%" elapsed))
	  ((< elapsed (* 60 60))
	   (format t "~D minutes ~D seconds~%" (truncate elapsed 60) (mod elapsed 60)))
	  (t
	   (format t "~D hours~%" (/ elapsed 60.0 60.0))))))

(defun encode-time (&key (time (get-universal-time))
		    &aux (decoded-time (multiple-value-list (decode-universal-time time))))
  "Create a string similar to the UNIX date command: e.g., \"Thu Aug  3 10:39:18 2017\""
  (destructuring-bind (second minute hour date month year day-of-week ;; (0 = Monday)
                       daylight-savings-times ;; T (daylight savings times) or NIL (standard time)
                       timezone) decoded-time
    (declare (ignore timezone daylight-savings-times))
    (let ((day-of-week (aref #("Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun") day-of-week))
          (month (aref #("no-month" "Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec") month)))
      (with-output-to-string (str)
        (format str "~A ~A" day-of-week month)
        (format str " ~2D ~2D:~2,'0D:~2,'0D ~S" date hour minute second year)))))

(defun run-tests (&key ((:tests *tests*) *tests*) ((:break-on-error *break-on-error*) *break-on-error*))
  "Run all the defined tests, and print a report.  If :TESTS is
provided, only the specified tests will be run.  If :break-on-error is
t (nil is default), then an error evokes the normall error hanlder,
e.g., debugger, however by default errors are captured, which abandons
the current test, registers the error, and continues to the next
test."
  (let ((num-pass 0)
	(failed nil)
	(errors nil)
	(num-tests (length *tests*))
	(tests-start (get-universal-time))
	(test-num 0))
    (format t "Running tests from packages: ~A~%" (let (packages)
						    (dolist (test *tests*)
						      (when (symbol-package test)
							(pushnew (package-name (symbol-package test)) packages
								 :test #'string=)))
						    packages))
    (dolist (*current-test* *tests*)
      (block break
	(labels ((handle-assertion-error (e)
		   (declare (type test-error e))
		   (format t "  Error:  ~A~%" (test-condition-code e))
		   (format t "    Msg:  ~A~%" (test-condition-error e))
		   (push e errors)
		   ;; go to next test
		   (return-from break))
		 (handle-error (e)
		   (declare (type error e))
		   (unless *break-on-error*
		     (handle-assertion-error
		      (make-condition 'test-error
				      :error e
				      :code `(,*current-test*)))))
		 (handle-fail (f)
		   (declare (type test-fail f))
		   (format t "  Failed: ~S~%" (test-condition-code f))
		   (mapcar (lambda (operand arg)
			     (unless (equal operand arg)
			       (format t "      ~S~%" operand)
			       (format t "        => ~S~%" arg)))
			   (cdr (test-condition-code f))
			   (test-condition-arguments f))
		   (format t "    Expected: ~A~%" (test-condition-expected f))
		   (format t "    Got:      ~A~%" (test-condition-received f))
		   (push f failed))
		 (handle-pass (p)
		   (declare (type test-pass p)
			    (ignore p))
		   (incf num-pass)))
	  (handler-bind ((test-pass #'handle-pass)
			 (test-error #'handle-assertion-error)
			 (test-fail #'handle-fail)
			 (error #'handle-error))
	    (format t "Starting: ~A~%" (encode-time))
	    (let ((*package* (find-package :keyword)))
	      (format t "Running: ~D/~D ~S~%" (incf test-num) num-tests *current-test*))
	    (funcall *current-test*)
	    (format t "Finished: ~A~%" (encode-time))))))
    (test-report tests-start num-pass failed errors)))

(defun run-1-test (test-name &key ((:break-on-error *break-on-error*) *break-on-error*))
  "Run one test and print a report."
  (run-tests :tests (list test-name) :break-on-error *break-on-error*))

(defun run-package-tests (packages  &key ((:break-on-error *break-on-error*) *break-on-error*))
  "Run all the tests whose name is in one of the spedified packages.
PACAKGES is a package designator, compatible with CL:DO-SYMBOLS, or
list of package designators."
  (let (package-tests)
    (dolist (package (if (listp packages)
			 packages
			 (list packages)))
      (do-symbols (name package)
	(when (member name *tests*)
	  (pushnew name package-tests))))
    (run-tests :tests package-tests :break-on-error *break-on-error*)))

(defun test-for (expected test-function gen-arguments code)
  "Internal function used by ASSERT-TRUE and ASSERT-FALSE.
evaluates the arguments of the test expression, then applies
the testing function to the arguments.  Raises a condition
whose type a subtype TEST-CONDITION, depending on whether the
assertion passes, fails, or errors."
  (declare (type (member t nil) expected)
	   (type (function () list) gen-arguments)
	   (type function test-function))
  (let* ((arguments (handler-bind ((error (lambda (e)
					    (unless *break-on-error*
					      (signal 'test-error :error e :code code)
					      ;; exit the test because of error
					      (return-from test-for)))))
		      (funcall gen-arguments)))
	 (result (handler-bind ((error (lambda (e)
					 (unless *break-on-error*
					   (signal 'test-error :error e :code code)
					   ;; exit the test because of error
					   (return-from test-for)))))
		   (apply test-function arguments))))
    (cond
      ((and expected result)
       (signal 'test-pass :code code))
      ((or (and expected (not result))
	   (and (not expected) result))
       (signal 'test-fail :code code :arguments arguments :expected expected :received result))
      ((and (not expected) (not result))
       (signal 'test-pass :code code)))))

(defun non-null (object)
  (not (null object)))

(defmacro assert-true (code)
  "E.g. (assert-true (> 4 3))"
  (typecase code
    (cons
     `(test-for t
		(function ,(car code))
		(lambda ()
		  (list ,@(cdr code)))
		',code))
    (t
     `(test-for t
		#'not-null
		(lambda ()
		  (list ,code))
		',code))))
		


(defmacro assert-false (code)
  "E.g. (assert-false (< 4 3))"
  (typecase code
    (cons
     `(test-for nil
		(function ,(car code))
		(lambda ()
		  (list ,@(cdr code)))
		',code))
    (t
     `(test-for t
		#'null
		(lambda ()
		  (list ,code))
		',code))))

(defun raises (thunk)
  "Internal function which calls this given function, ignores its
return value, and returns the list of conditions its evaluation
raised."
  (declare (type (function () t) thunk))
  (let (conditions)
    (ignore-errors
     (handler-bind ((t (lambda (c)
			 (push c conditions))))
       (funcall thunk)))
    conditions))

(defmacro assert-error (error-type-specifier expr)
  "E.g., (assert-error division-by-zero (/ 3 0))"
  `(assert-true (find ',error-type-specifier
		      (raises (lambda ()
				,expr))
		      :test (lambda (type object)
			      (typep object type)))))

(defun shadow-all-symbols (&key package-from package-into (verbose nil))
  (declare (type (or package keyword) package-from package-into))
  (let ((package-into (or (find-package  package-into)
                          (error "cannot find package ~A" package-into)))
        (package-from (or (find-package  package-from)
                          (error "cannot find package ~A" package-from)))
        (*package* (find-package :keyword)))
    (let (symbols)
      (do-symbols (symbol package-from)
        (pushnew symbol symbols))
      (dolist (symbol (sort symbols #'string< ))
        (when (and (eq package-from (symbol-package symbol))
                   (or (not (find-symbol (symbol-name symbol) package-into))
                       (not (eq (find-symbol (symbol-name symbol) package-from)
                                (find-symbol (symbol-name symbol) package-into)))))
          (when verbose
	    (format t "importing name=~S into ~S " symbol package-into))
          (shadowing-import symbol package-into)
          (when verbose
	    (unless (equal '(:internal) (cdr (multiple-value-list (find-symbol (symbol-name symbol) package-into))))
	      (format t "~S~%" (cdr (multiple-value-list (find-symbol (symbol-name symbol) package-into)))))
	    (format t "~%"))
          )))
    (let (names)
      (do-symbols (symbol package-from)
        (pushnew (symbol-name symbol) names :test #'string=))
      (do-symbols (symbol package-into)
        (pushnew (symbol-name symbol) names :test #'string=))
      (setf names (sort names #'string<))
      (setf names (remove-if-not (lambda (name)
                                   (and (find-symbol name package-into)
                                        (find-symbol name package-from)
                                        (not (eq (find-symbol name package-into)
                                                 (find-symbol name package-from)))))
                                 names))
      (when names
        (error "The following distinct symbols exist in both packages ~A and ~A: ~A"
	       package-from package-into names)))))



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
  (assert nil)
  (assert-error error
		(error "some error"))
)
