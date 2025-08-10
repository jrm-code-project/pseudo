;;; -*- Lisp -*-

(in-package "AUTODOC-TESTS")

(defclass 3d-point ()
  ((x :initarg :x :initform 0)
   (y :initarg :y :initform 0)
   (z :initarg :z :initform 0)))

(defconstant +the-ultimate-answer+ 42)

(defgeneric quux (a b)
  (:method ((a integer) (b integer))
    (declare (ignore a b))
    0)
  (:method ((a string) (b string))
    (declare (ignore a b))
    "Hello, world!"))

(defmacro bar (a b)
  `(foo ,a ,b))

(defparameter *screen-width* 640)

(defstruct point
  (x 0)
  (y 0))

(defun foo (a b)
  (+ a b))

(defvar *current-foo* nil)
