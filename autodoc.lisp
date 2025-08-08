;;; -*- Lisp -*-

(in-package "AUTODOC")

(cl:defparameter +autodoc-model+ "gemini-2.5-flash")

(series::defun current-file-source-code ()
  "Return the text of the current file.  Language fence is included."
  (when (uiop:current-lisp-file-pathname)
    (with-open-file (stream (uiop:current-lisp-file-pathname) :direction :input)
      (do ((line (read-line stream nil) (read-line stream nil))
           (code (list "```lisp") (cons line code)))
          ((null line) (nreverse (cons "```" code)))))))

(cl:defparameter +general-instructions+
  (str:join
   #\newline
   '("You are a senior programmer with many years experience."
     "You write good technical documentation."
     "Your docstrings are concise, accurate, and easy to read.")))

(cl:defparameter +output-requirements+
  (str:join
   #\newline
   '("**Output Requirements**"
     ""
     "* Documentation will be in English text."
     "* Documentation will *NOT* include code."
     "* Do NOT include triple backtick.")))

(series::defun source-code-part (source-code)
  "Create a Gemini part for the source code."
  (when source-code
    (gemini:part
     (str:join
      #\newline
      `("**Source Code**"
        ""
        "The source code you are working with is:"
        ""
        ,@source-code)))))

(series::defun system-instruction (source-code)
  (gemini:content
   :parts (remove nil
                  (list (gemini:part +general-instructions+)
                        (gemini:part +output-requirements+)
                        (source-code-part source-code)))))

(series::defun autodoc-prompt (form)
  "Create a prompt for generating documentation for the given form."
  (with-output-to-string (stream)
    (format stream "Generate a brief documentation string for the following definition:~%")
    (format stream "```lisp~%")
    (pprint form stream)
    (format stream "```~%")))

(series::defun create-documentation (form source-code)
  "Generate documentation for the given form using Gemini."
  (let ((gemini:*include-thoughts* t)
        (gemini:*system-instruction* (system-instruction source-code))
        (gemini:*tools* nil)
        (start-time (get-universal-time)))
    (unwind-protect
         (str:trim (gemini:invoke-gemini (autodoc-prompt form) :model +autodoc-model+) :char-bag (list #\" #\' #\`))
      (let ((elapsed-time (- (get-universal-time) start-time)))
        (format *trace-output* "~&;; Documentation took ~A seconds.~%" elapsed-time)
        (finish-output *trace-output*)))))

(cl:defmacro defclass (&whole whole name supers slots &body extra)
  (let ((documentation (if (assoc :documentation extra)
                           nil
                           (list `(:documentation ,(create-documentation whole (current-file-source-code)))))))
    `(cl:defclass ,name ,supers ,slots ,@documentation ,@extra)))

(cl:defmacro defconstant (&whole whole name value &optional doc)
  (if doc
      `(cl:defconstant ,name ,value ,doc)
      `(cl:defconstant ,name ,value ,(create-documentation whole (current-file-source-code)))))

(cl:defmacro defgeneric (&whole whole name lambda-list &body extra)
  (let ((documentation (if (assoc :documentation extra)
                           nil
                           (list `(:documentation ,(create-documentation whole (current-file-source-code)))))))
    `(cl:defgeneric ,name ,lambda-list ,@documentation ,@extra)))

(cl:defmacro defmacro (&whole whole name lambda-list &body body)
  (let ((documentation (if (stringp (third whole))
                           nil
                           (list (create-documentation whole (current-file-source-code))))))
    `(cl:defmacro ,name ,lambda-list ,@documentation ,@body)))

(cl:defmacro defparameter (&whole whole name value &optional doc)
  (if doc
      `(cl:defparameter ,name ,value ,doc)
      `(cl:defparameter ,name ,value ,(create-documentation whole (current-file-source-code)))))

(cl:defmacro defun (&whole whole name lambda-list &body body)
  (let ((documentation (if (stringp (third whole))
                           nil
                           (list (create-documentation whole (current-file-source-code))))))
    `(series::defun ,name ,lambda-list ,@documentation ,@body)))

(cl:defmacro defstruct (&whole whole name-and-options &body body)
  (let ((documentation (if (stringp (third whole))
                           nil
                           (list (create-documentation whole (current-file-source-code))))))
    `(cl:defstruct ,name-and-options ,@documentation ,@body)))

(cl:defmacro defvar (&whole whole name value &optional doc)
  (if doc
      `(cl:defvar ,name ,value ,doc)
      `(cl:defvar ,name ,value ,(create-documentation whole (current-file-source-code)))))
