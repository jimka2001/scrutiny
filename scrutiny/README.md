# SCRUTINY

## Synopsis

Slime-friendly Unit Testing package, based loosely on lisp-unit (https://github.com/OdonataResearchLLC/lisp-unit). 

## API
    
* `define-test` -- defines a test and a 0-ary function of the same name.  This and other tests may be run by calling `(run-tests)`
* `shadow-all-symbols` -- Import un-exported symbols from one package into another, typically into the test package.
```
(eval-when (:compile-toplevel :load-toplevel :execute)
  (shadow-all-symbols :package-from :cl-robdd-analysis
                      :package-into :cl-robdd-analysis-test))
```
* `assert-true` -- assert that a single expression returns non-nil.  E.g.,   
`(assert-true (= (+ a b) (- c d)))`
* `assert-false` -- assert that a single expression returns non-nil.  E.g.,   
`(assert-false (= (+ a b) (- c d)))`
* `assert-error` -- assert that evaluating a given expression signals a named condition   E.g.,   
`(assert-error error (= (f a b) (g c d)))`  
`(assert-error my-condition (format (f a b) (g c d)))`
* `run-tests` -- runs all loaded tests by default.  `:tests` specifies a test-name or list thereof to run. `:break-on-error` specifies to go into the debugger (or otherwise default error handler) if an error condition is triggered.  Otherwise test is simply marked as failed and reported later.
* `run-1-test` -- like `run-tests` but runs a single test. `:break-on-error` can be used as well
* `run-package-tests` -- run tests whose name is the designated package or list of packages.  E.g.,  
  `(run-package-tests "MY-PACKAGE")`  
  `(run-package-tests '(:pack1 :pack2) :break-on-error t)`

## Slime Support

scrutiny supports severel types of development using slime.
    
* `M-.` -- `slime-edit-definition` -- since tests are defined symbols as functions, `M-.` works normally, causing emacs to visit the file containing the defintion and to lookup the definition of the name at the point.
* `C-c C-c` -- `slime-compile-defun`  -- Compile the test at the point, but don't run the test.   Postfix arguments work exactly the same.  If there are compiler errors or warnings, they will be displayed in the buffer as you expect when compiling a normal function with `C-c C-c`.
* Backtrace -- running tests with `:break-on-error t`, makes the backtrace available so you can debug as you normally would.

### Debugging Tests

When running tests with either `run-tests` and friends, the report indicates
which tests failed as shown above.
You may then re-run a failing test using `run-1-test`, edit the code begin tested,
or the test itself, and continue re-running the test with `run-1-test`.
If a test failed because of an error (outside an `assert-true`, `assert-false`, or `assert-error`)
the error is normally supressed, so it can be reported.  To override this supression,
you may provide the argument `:break-on-error` as follows, to get a backtrace.
You can use normal slime debugging from that point.


````
MY-APP> (run-1-test 'scrutiny::scrutiny-1 :break-on-error t)

The assertion NIL failed.
   [Condition of type SIMPLE-ERROR]

Restarts:
 0: [CONTINUE] Retry assertion.
 1: [RETRY] Retry SLIME REPL evaluation request.
 2: [*ABORT] Return to SLIME's top level.
 3: [ABORT] abort thread (#<THREAD "repl-thread" RUNNING {10022C8003}>)

Backtrace:
  0: (SB-KERNEL:ASSERT-ERROR NIL NIL NIL NIL)
  1: (SCRUTINY::SCRUTINY-1)
  2: (SCRUTINY:RUN-TESTS :TESTS (SCRUTINY::SCRUTINY-1) :BREAK-ON-ERROR T)
  3: (SCRUTINY:RUN-1-TEST SCRUTINY::SCRUTINY-1 :BREAK-ON-ERROR T)
  4: (SB-INT:SIMPLE-EVAL-IN-LEXENV (SCRUTINY:RUN-1-TEST (QUOTE SCRUTINY::SCRUTINY-1) :BREAK-ON-ERROR T) #<NULL-LEXENV>)
  5: (EVAL (SCRUTINY:RUN-1-TEST (QUOTE SCRUTINY::SCRUTINY-1) :BREAK-ON-ERROR T))
````



### Note about Testing Macros

If a test references a macro, then when the code (implementation) of
the macro changes, the test MUST BE RECOMPLED to see the effect of the
change.  The semantics are exactly the same for normal functions.
scrutiny does absolutely nothing to circumvent the normal Lisp
evaluation rules of code being tested.  This is an important semantic
difference between scrutiny and lisp-unit.  lisp-unit attempts to
recompile your tests from the user given s-expression before running
the tests.

## Writing Tests
    
Tests are written by declaring functions using `define-test`. Each
test has a name which is a unique symbols.  Of course two symbols in
two different packages may share a `symbol-name`.  Within the body
of a test you typically call functions and assert things about their
return values or occasionally about which conditions they signal.
Use the macros `assert-true`, `assert-false`, and `assert-error`
to make such assertions.   The operand of `assert-true` and `assert-false`
is an expression which has call-by-value semantics.   Ie, 
`assert-true` and `assert-false` assumes an expression such as
`(my-equal arg1 (some-args) (some-more-args))` is a function call,
that `my-equal` names a function, and that `arg1`, `(some-args)`,
and `(some-more-args)` are expressions which can be evaluated in order.
The `assert-true` and `assert-false` macros deconstruct the call, evaluating
the arguments into temporary variables, then call the named function with those
saved arguments.  If the assertion fails, if the evaluation returns true when false
was expected or vice-versa, a message is generated which reports the operands,
unevaluated, along with evaluated values, the value which was returned and
what was expected, and the name of the compare function.

Tests are normally written in their own package, but scrutiny makes absolutely
no effort to enforce this.  A useful pattern is the following, defining an application
in a package, `"MY-APPLICATION"` and the tests in `"MY-APPLICATION-TEST"`.

````
(defpackage :my-application
  (:use :cl ...)
  (:export "FUN1" "FUN2" "FUN3"))

;; application code here
````

The definition of the `"MY-APPLICATION-TEST"` package typically goes
into a differnet file, but of course may go in the same file if you
like.

````
(defpackage :my-application-test
  (:use :cl :my-application))

(define-test test1
  (assert-true (= 42 (fun1 3)))
  (assert-false (< 12 (fun2 12))))

(let ((secret-value "xyzzy"))
  (define-test test2
    (assert-true (string< secret-value (fun3 "hello")))))

````

Sometimes you want to test the API of a package, and sometimes you
want to test the internals.  If you want to test the API, simply
include the name of the package being tested in the `:use` list of the
test package: `(:use :cl :my-application)`.


However, if you want to test the internal functions of the package,
you don't want to define the tests in `:my-application`, and you don't
want to use fully qualified names on all references to internal
symbols of `:my-application`.  In this case, call the function
`shadow-all-symbols`.  This imports all the internal symbols from one
package into another.


````
(defpackage :my-application-test2
  (:use :cl :my-application))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (shadow-all-symbols :package-from :my-application
                      :package-into :my-application-test2))

(define-test test1
  (assert-true (= 42 (fun-internal-1 3)))
  (assert-false (< 12 (fun-internal-2 12))))

(flet ((my-cmp (x y)
         (or (equal "" x)
             (string< x y) )))
  (define-test test2
    (assert-true (my-cmp "xyzzy" (fun-internal-3 "hello")))))

````


## Running the Tests

When the tests run, output such as the following is printed to `*standard-output*`:
```
Summary of tests:
PACKAGES: (SCRUTINY 2D-ARRAY-TEST DISPATCH-TEST CL-ROBDD-TEST LISP-TYPES-TEST
           LISP-TYPES-BAKER-ANALYSIS NDFA-TEST RTE-REGEXP-TEST RTE-TEST)
TOTAL TESTS: 181
ASSERTIONS PASSED: 1867
ASSERTIONS FAILED: 4
  1 failed assertions in LISP-TYPES-BAKER-ANALYSIS::BAKER/DECOMPOSE-5
  1 failed assertions in LISP-TYPES-BAKER-ANALYSIS::BAKER/DECOMPOSE-2
  2 failed assertions in SCRUTINY::SCRUTINY-1
ERRORS: 1
  SCRUTINY::SCRUTINY-1
ELAPSED TIME: 12 minutes 22 seconds
```
While tests are running, incremental output will be printed to `*standard-output*` such as the following.
```
Running tests from packages: (SCRUTINY 2D-ARRAY-TEST DISPATCH-TEST
                              CL-ROBDD-TEST LISP-TYPES-TEST
                              LISP-TYPES-BAKER-ANALYSIS NDFA-TEST
                              RTE-REGEXP-TEST RTE-TEST)
Starting: Thu Sep 20 12:06:59 2018
Running: 1/181 RTE-TEST::TEST/DESTRUCTURING-CASE-4
Finished: Thu Sep 20 12:06:59 2018
Starting: Thu Sep 20 12:06:59 2018
Running: 2/181 RTE-TEST::TEST/DESTRUCTURING-CASE-3
Finished: Thu Sep 20 12:06:59 2018
Starting: Thu Sep 20 12:06:59 2018
Running: 3/181 RTE-TEST::TEST/DESTRUCTURING-CASE-2-C
Finished: Thu Sep 20 12:06:59 2018
Starting: Thu Sep 20 12:06:59 2018
Running: 4/181 RTE-TEST::TEST/DESTRUCTURING-CASE-2-B
Finished: Thu Sep 20 12:06:59 2018
Starting: Thu Sep 20 12:06:59 2018
Running: 5/181 RTE-TEST::TEST/DESTRUCTURING-CASE-2-A
Finished: Thu Sep 20 12:06:59 2018
```

Here is an example of the output if a test assertion fails.
```
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
			       (format nil "~A~A" b a))))
  (assert-error error
		(error "some error")))
```
The test can be run with `(run-1-test 'scrutiny-1)` to see the following output.  Notice that when test assertions fail, not only the unevaluated arguments are shown but their values, if different.
```
Running tests from packages: (SCRUTINY)
Starting: Thu Sep 20 12:18:25 2018
Running: 1/1 SCRUTINY::SCRUTINY-1
  Failed: (STRING-EQUAL SCRUTINY::A SCRUTINY::B)
      SCRUTINY::A
        => "abc"
      SCRUTINY::B
        => "ABCD"
    Expected: T
    Got:      NIL
  Failed: (STRING-EQUAL (CONCATENATE 'STRING SCRUTINY::A SCRUTINY::B)
                        (FORMAT NIL "~A~A" SCRUTINY::B SCRUTINY::A))
      (CONCATENATE 'STRING SCRUTINY::A SCRUTINY::B)
        => "abcABCD"
      (FORMAT NIL "~A~A" SCRUTINY::B SCRUTINY::A)
        => "ABCDabc"
    Expected: T
    Got:      NIL
Finished: Thu Sep 20 12:18:25 2018
------------------
Summary of tests:
PACKAGES: (SCRUTINY)
TOTAL TESTS: 1
ASSERTIONS PASSED: 8
ASSERTIONS FAILED: 2
  2 failed assertions in SCRUTINY::SCRUTINY-1
ERRORS: 0
ELAPSED TIME: 0 seconds
```



## Motivation

I found my unit-testing needs were diverging from what was offered in lisp-unit, and the lisp-unit maintainers for whatever reason did not accomodate my pull requests.  On the other hand my unit-testing needs were pretty simply, so I built my own unit-testing package using the same API which I was using from lisp-unit.
