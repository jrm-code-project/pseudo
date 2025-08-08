;;; -*- Lisp -*-

(defpackage "AUTODOC"
  (:shadowing-import-from "FUNCTION" "COMPOSE")
  (:shadowing-import-from "NAMED-LET" "LET" "NAMED-LAMBDA")
  (:shadowing-import-from "SERIES" "FUNCALL" "LET*" "MULTIPLE-VALUE-BIND")
  (:shadow "DEFCLASS"
           "DEFCONSTANT"
           "DEFGENERIC"
           "DEFMACRO"
           "DEFPARAMETER"
           "DEFUN"
           "DEFSTRUCT"
           "DEFVAR")
  (:use "ALEXANDRIA" "CL" "FOLD" "FUNCTION" "NAMED-LET" "SERIES")
  (:export "DEFCLASS"
           "DEFCONSTANT"
           "DEFGENERIC"
           "DEFMACRO"
           "DEFPARAMETER"
           "DEFUN"
           "DEFSTRUCT"
           "DEFVAR"))

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

(defpackage "AUTODOC-TESTS"
  (:shadowing-import-from "FUNCTION" "COMPOSE")
  (:shadowing-import-from "NAMED-LET" "LET" "NAMED-LAMBDA")
  (:shadowing-import-from "SERIES" "FUNCALL" "LET*" "MULTIPLE-VALUE-BIND")
  (:shadowing-import-from "AUTODOC"
                          "DEFCLASS"
                          "DEFCONSTANT"
                          "DEFGENERIC"
                          "DEFMACRO"
                          "DEFPARAMETER"
                          "DEFUN"
                          "DEFSTRUCT"
                          "DEFVAR")
  (:use "ALEXANDRIA" "CL" "FOLD" "FUNCTION" "NAMED-LET" "PSEUDO" "SERIES"))

(defpackage "PSEUDO-TESTS"
  (:shadowing-import-from "FUNCTION" "COMPOSE")
  (:shadowing-import-from "NAMED-LET" "LET" "NAMED-LAMBDA")
  (:shadowing-import-from "SERIES" "DEFUN" "FUNCALL" "LET*" "MULTIPLE-VALUE-BIND")
  (:use "ALEXANDRIA" "CL" "FOLD" "FUNCTION" "NAMED-LET" "PSEUDO" "SERIES")
  (:export
   ))
