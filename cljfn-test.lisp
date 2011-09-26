
(defpackage #:cljfn-test
  (:use #:common-lisp #:cljfn
        #+sbcl :sb-rt #-sbcl :rt)
  (:export #:run-tests))

(in-package #:cljfn-test)
(rem-all-tests)

(cljfn:enable-fn-reader #\[ #\])

(deftest "take 1"
    (#[values %] 1)
  1)

(deftest "% and %1"
    (#[values % %1] 1)
  1 1)

(deftest "not using %2"
    (#[values % %3] 1 2 3)
  1 3)

(deftest "using %&"
    (#[length %&] 1 2 3 4 5)
  5)

(deftest "using %n and %&"
    (#[values % %3 %&] 1 2 3 4 5)
  1 3 (1 2 3 4 5))

(deftest "many arguments"
   (apply #[values (length %&) %100] (loop repeat 100 collect 100))
  100 100)

(deftest "error: too match arguments"
    (handler-case (#[+ % %2] 1 2 3)
      (error (c) :program-error))
  :program-error)

(deftest "error: less number of arguments"
    (handler-case (#[apply #'+ % %2 %3 %&] 1 2)
      (error (c) :program-error))
  :program-error)

(cljfn:disable-fn-reader)

(defun run-tests ()
  (do-tests))
