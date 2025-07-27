;;; -*- Lisp -*-

(in-package "PSEUDO-TESTS")

(defun my-add (a b) (pseudo "add a and b"))

(pseudefun my-subtract (left right) "Subtract right from left.")

(pseudefun linear-regression (points)
   "Performs a linear regression on points.  Points are pairs of (x . y).  Return slope, intercept, and r-squared.")

(defun greet (person)
  (format t "~&Hello, ~a, how goes it?~%" person))

(pseudefun greet-all (&rest people)
  "Greet all the people.")
