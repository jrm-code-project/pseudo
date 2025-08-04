;;; -*- Lisp -*-

(defpackage "PSEUDO"
  (:shadowing-import-from "FUNCTION" "COMPOSE")
  (:shadowing-import-from "NAMED-LET" "LET" "NAMED-LAMBDA")
  (:shadowing-import-from "SERIES" "DEFUN" "FUNCALL" "LET*" "MULTIPLE-VALUE-BIND")
  (:use "ALEXANDRIA" "CL" "FOLD" "FUNCTION" "NAMED-LET" "SERIES")
  (:export
   "+PSEUDO-MODEL+"
   "PSEUDO"
   "PSEUDEFUN"
   ))

(defpackage "PSEUDO-TESTS"
  (:shadowing-import-from "FUNCTION" "COMPOSE")
  (:shadowing-import-from "NAMED-LET" "LET" "NAMED-LAMBDA")
  (:shadowing-import-from "SERIES" "DEFUN" "FUNCALL" "LET*" "MULTIPLE-VALUE-BIND")
  (:use "ALEXANDRIA" "CL" "FOLD" "FUNCTION" "NAMED-LET" "PSEUDO" "SERIES")
  (:export
   ))
