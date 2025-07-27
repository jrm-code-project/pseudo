;;; -*- Lisp -*-

(in-package "PSEUDO")

(defparameter +pseudo-model+ "gemini-2.5-pro"
  "The model name for the pseudocode generator.")

(defparameter +pseudo-temperature+ 0.1
  "The temperature setting for the pseudocode generator.")

(defparameter +pseudo-expression-prompt+ "Generate a Common Lisp expression from the following description: ")

(defun current-file-source-code ()
  "Return the text of the current file.  Language fence is included."
  (when (uiop:current-lisp-file-pathname)
    (with-open-file (stream (uiop:current-lisp-file-pathname) :direction :input)
      (do ((line (read-line stream nil) (read-line stream nil))
           (code (list "```lisp") (cons line code)))
          ((null line) (nreverse (cons "```" code)))))))

(defun lexical-variables (macro-environment)
  "Return a list of lexical variables from the macro environment."
  (and macro-environment
       (remove nil (mapcar #'car (sb-c::lexenv-vars macro-environment))
               :key #'symbol-package)))

(defun pseudocode-expression-prompt (description)
  "Generate a prompt for the pseudocode expression based on DESCRIPTION."
  (format nil "~A ~A." +pseudo-expression-prompt+ description))

(defparameter +general-instructions+
  (str:join
   #\newline
   `("You are a senior programmer with many years experience.  "
     "You write code that is clear, concise, correct, efficient, and idiomatic.  "
     "You are an expert in the Common Lisp language and its libraries.  "
     "You have extensive knowledge of the Common Lisp ecosystem, standard libraries, and commonly used third-party libraries, including but not limited to: "
     ""
     "*   **Data manipulation**: `alexandria`, `str`"
     "*   **Web development:** `hunchentoot`, `dexador`, `quri`, `cl-cookie`, `cl-smtp`, `cl-ppcre`"
     "*   **Data serialization:** `json`, `cl-yaml`, `cl-csv`"
     "*   **Concurrency:** `bordeaux-threads`"
     "*   **Cryptography:** `ironclad`"
     "*   **Dates and times:** `local-time`"
     "*   **Foreign Function Interface (FFI):** `cffi`")))

(defparameter +priorities+
  (str:join
   #\newline
   `("When generating code, prioritize:"
     ""
     "*   **Readability:** Use clear variable names and structure the code logically."
     "*   **Efficiency:** Choose appropriate algorithms and data structures.  Avoid unnecessary operations."
     "*   **Idiomatic Common Lisp:**  Leverage Common Lisp's features, `multiple-value-bind`, and macros, to write concise and expressive code."
     "*   **Error handling:**  Consider potential errors and include appropriate error handling mechanisms, such as `handler-case` or `ignore-errors`."
     "*   **Library Usage:** Prefer using appropriate third-party libraries to standard functions when libraries have a specific function for certain cases. For example, prefer `str:replace-all` to `substitute` if string replacement is what is needed."
     ""
     "You are provided with a list of preferred functions, other allowed functions, preferred global variables, and other allowed global variables.  You **MUST** use only functions and global variables from these lists.  You should prefer to use functions and global variables from the preferred lists.  When a function is available in both the `COMMON-LISP` package and another package, prefer the non-`COMMON-LISP` package's function.")))

(defparameter +expression-output-requirements+
  (str:join
   #\newline
   `("When generating code, ensure that the output is:"
     ""
     "*   **Valid Common Lisp:** The generated code must be a valid Common Lisp s-expression."
     "*   **Well-formed:** The generated code should not contain syntax errors."
     "*   **Complete:** The generated code should be a complete expression that can be evaluated in a Common Lisp environment."
     "*   **Self-contained:** The generated code should not rely on external files or resources unless explicitly allowed in the instructions."
     "*   **Efficient:** The generated code should be efficient in terms of time and space complexity."
     "*   **Idiomatic:** The generated code should follow Common Lisp idioms and best practices."
     ""
     "*   Your output **MUST** be a valid Common Lisp s-expression.  The code must be directly evaluable in a Common Lisp interpreter."
     "*   Do **NOT** include any surrounding text, explanations, or comments in your response. Only the s-expression."
     "*   Do **NOT** generate function definitions (e.g., with `defun`, `defmacro`).  The output should be a standalone s-expression that performs the requested task.  The caller will evaluate the expression.")))

(defun filter-external-symbols (package filter)
  "Return a list of external symbols in PACKAGE that match the FILTER function."
  (loop for sym being the external-symbols in package
        when (funcall filter sym)
          collect sym))

(defun external-functions (package)
  "Return a list of external functions defined in the given PACKAGE."
  (filter-external-symbols
   package
   (lambda (sym)
     (and (fboundp sym)
          (not (macro-function sym))
          (not (str:starts-with? "%" (symbol-name sym)))))))

(defun external-macros (package)
  "Return a list of external functions defined in the given PACKAGE."
  (filter-external-symbols
   package
   (lambda (sym)
     (and (fboundp sym)
          (macro-function sym)
          (not (str:starts-with? "%" (symbol-name sym)))))))

(defun external-variables (package)
  "Return a list of external functions defined in the given PACKAGE."
  (filter-external-symbols package 
                           (lambda (sym)
                             (and (boundp sym)
                                  (not (str:starts-with? "%" (symbol-name sym)))))))

(defun filter-package-list ()
  "Return a list of packages to consider for code generation."
  (sort (append
         (mappend (lambda (pn)
                    (let ((p (find-package pn)))
                      (when p (list p))))
                  '("SB-CLTL2"
                    "SB-MOP"))
         (remove-if
          (lambda (package)
            (let ((name (package-name package)))
              (or (equal name "KEYWORD")
                  (str:starts-with? "QL-" name)
                  (str:starts-with? "SB-" name)
                  (str:starts-with? "SLYNK" name)
                  (str:ends-with? "-ASD" name)
                  (str:ends-with? "-SYSTEM" name)
                  (str:ends-with? "-TEST" name)
                  (str:ends-with? "-TESTS" name)
                  (find #\/ name)
                  (find #\. name)
                  )))
          (list-all-packages)))
        #'string<
        :key #'package-name))

(defun get-top-level-functions ()
  "Return a list of top-level functions defined anywhere."
  (sort (remove-duplicates (mappend #'external-functions (filter-package-list)))
        #'string< :key #'symbol-name))

(defun get-top-level-macros ()
  "Return a list of top-level macros defined anywhere."
  (sort (remove-duplicates (mappend #'external-macros (filter-package-list)))
        #'string< :key #'symbol-name))

(defun get-top-level-variables ()
  "Return a list of top-level functions defined anywhere."
  (sort (remove-duplicates (mappend #'external-variables (filter-package-list)))
        #'string< :key #'symbol-name))

(defun preferred-functions-part (preferred-functions)
  "Create a Gemini part for preferred functions."
  (gemini:part
   (str:join
    #\newline
    `("**Preferred Functions**"
      ""
      "You should prefer to use the following functions:"
      ""
      ,@(mapcar (lambda (f) (format nil "* ~a" f))
                (sort preferred-functions
                      #'string< :key #'symbol-name))))))

(defun other-allowed-functions-part (allowed-functions)
  "Create a Gemini part for allowed functions."
  (gemini:part
   (str:join
    #\newline
    `("**Other Allowed Functions**"
      ""
      "You are allowed to use the following additional functions:"
      ""
      ,@(mapcar (lambda (f) (format nil "* ~a" f))
                (sort allowed-functions
                      #'string< :key #'symbol-name))))))

(defun preferred-macros-part (preferred-macros)
  "Create a Gemini part for preferred macros."
  (gemini:part
   (str:join
    #\newline
    `("**Preferred Macros**"
      ""
      "You should prefer to use the following macros:"
      ""
      ,@(mapcar (lambda (f) (format nil "* ~a" f))
                (sort preferred-macros
                      #'string< :key #'symbol-name))))))

(defun other-allowed-macros-part (allowed-macros)
  "Create a Gemini part for allowed macros."
  (gemini:part
   (str:join
    #\newline
    `("**Other Allowed Macros**"
      ""
      "You are allowed to use the following additional macros:"
      ""
      ,@(mapcar (lambda (f) (format nil "* ~a" f))
                (sort allowed-macros
                      #'string< :key #'symbol-name))))))

(defun preferred-variables-part (preferred-variables)
  "Create a Gemini part for preferred variables."
  (gemini:part
   (str:join
    #\newline
    `("**Preferred Macros**"
      ""
      "You should prefer to use the following global variables:"
      ""
      ,@(mapcar (lambda (f) (format nil "* ~a" f))
                (sort preferred-variables
                      #'string< :key #'symbol-name))))))

(defun other-allowed-variables-part (allowed-variables)
  "Create a Gemini part for allowed variables."
  (gemini:part
   (str:join
    #\newline
    `("**Other Allowed Variables**"
      ""
      "You are allowed to use the following additional variables:"
      ""
      ,@(mapcar (lambda (f) (format nil "* ~a" f))
                (sort allowed-variables
                      #'string< :key #'symbol-name))))))

(defun lexical-variables-part (lexical-variables)
  "Create a Gemini part for lexical variables."
  (gemini:part
   (str:join
    #\newline
    `("**Free Variables**"
      ""
      "Your code may have free references to the following lexical variables:"
      ""
      ,@(mapcar (lambda (v) (format nil "* ~a" v))
                (sort lexical-variables
                      #'string< :key #'symbol-name))))))

(defun source-code-part (source-code)
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

(defparameter *coding-style* :functional
  "Your preferred coding style.  One of :functional :pure :imperative :object-oriented.")

(defun style-instructions ()
  (ecase *coding-style*
    (:pure
     (gemini:part
      "You should strive for pure functional code that has no side effects.  Do not reassign variables.  Avoid mutating data. Make use of recursion and higher order functions."))
    (:functional
     (gemini:part
      "You prefer functional code that emphasizes immutability and first-class functions.  Use functional programming techniques to achieve this.  Avoid using global variables and mutable state.  Prefer recursion and higher-order functions."))
    (:imperative
     (gemini:part
      "You prefer imperative code that uses mutable state and side effects.  Use imperative programming techniques to achieve this.  Feel free to reassign variables and mutate data.  `TAGBODY` and `GO` are allowed for control flow."))
    (:object-oriented
     (gemini:part
      "You prefer imperative code, but only mutate state within objects.  You avoid mutating variables."))))

(defparameter *loop-affinity* :hate
  "Your affinity for using the LOOP macro.  One of :love :like :neutral :dislike :hate.")

(defun loop-instructions ()
  (ecase *loop-affinity*
    (:love
     (gemini:part
      "You should use the LOOP macro for iteration.  It is a powerful and expressive tool for iterating over collections, generating sequences, and performing complex operations in a concise manner.  Use it whenever possible."))
    (:like
     (gemini:part
      "You should consider using the LOOP macro for iteration.  It is a powerful and expressive tool for iterating over collections, generating sequences, and performing complex operations in a concise manner.  Use it when appropriate."))
    (:neutral
     (gemini:part
      "You may use the LOOP macro for iteration if you find it suitable.  It is a powerful and expressive tool for iterating over collections, generating sequences, and performing complex operations in a concise manner."))
    (:dislike
     (gemini:part
      "Avoid using the LOOP macro.  You may use the LOOP macro for iteration if you find it necessary, but prefer other iteration constructs like `dolist`, `dotimes`, or `map` when possible."))
    (:hate
     (gemini:part
      "Avoid using the LOOP macro if at all possible.  Prefer other iteration constructs like `dolist`, `dotimes`, or `map`.  Consider local iteration using LABELS or a NAMED-LET."))))

(defun system-instruction (lexical-variables source-code)
  "Create a system instruction for the Gemini API based on LEXICAL-VARIABLES and SOURCE-CODE."
  (let* ((preferred-functions
           (append (external-functions "COMMON-LISP")
                   (external-functions "SB-CLTL2")))

         (other-allowed-functions
           (set-difference (get-top-level-functions) preferred-functions))

         (preferred-macros
           (remove-if (lambda (macro)
                        (str:starts-with? "DEF" (symbol-name macro)))
                      (append (external-macros "COMMON-LISP")
                              (external-macros "SB-CLTL2"))))

         (other-allowed-macros
           (remove-if (lambda (macro)
                        (str:starts-with? "DEF" (symbol-name macro)))
                      (set-difference (get-top-level-macros) preferred-macros)))

         (preferred-variables
           (append (external-variables "COMMON-LISP")
                   (external-variables "SB-CLTL2")))

         (other-allowed-variables
           (set-difference (get-top-level-variables) preferred-variables)))

    (gemini:content
     :parts (remove nil
                    (list (gemini:part +general-instructions+)
                          (gemini:part +priorities+)
                          (gemini:part +expression-output-requirements+)
                          (preferred-functions-part preferred-functions)
                          (other-allowed-functions-part other-allowed-functions)
                          (preferred-macros-part preferred-macros)
                          (other-allowed-macros-part other-allowed-macros)
                          (preferred-variables-part preferred-variables)
                          (other-allowed-variables-part other-allowed-variables)
                          (loop-instructions)
                          (style-instructions)
                          (lexical-variables-part lexical-variables)
                          (source-code-part source-code))))))

(defun strip-code-block-fence (string)
  (str:join #\newline
            (remove-if (lambda (line) (str:starts-with? "```" (str:trim line)))
                       (str:split #\newline string))))

(defun process-text (text)
  "Process the text part of the Gemini response and return the generated expression."
  (let ((generated-code (strip-code-block-fence text)))
    (let ((*read-eval* nil))
      (read-from-string generated-code))))

(defun process-part (part)
  "Process a single part of the Gemini response and return the generated expression."
  (if (gemini:text-object? part)
      (process-text (gemini:get-text part))
      (error "Expected a text part, got ~s" part)))

(defun reflow-comment (lines)
  "Reflow a list of lines into a single string."
  (let iter ((lines lines)
             (words nil)
             (new-line ";;")
             (answer ""))
    (cond ((> (length new-line) 80)
           (iter lines words ";;" (concatenate 'string answer new-line "
")))
          ((null words)
           (cond ((null lines) (concatenate 'string answer new-line))
                 ((zerop (length (car lines)))
                  (iter (cdr lines) nil ";;" (concatenate 'string answer new-line "
")))
                 (t
                  (iter (cdr lines) (str:split #\Space (car lines)) ";;" (concatenate 'string answer new-line "
")))))
          (t (iter lines (cdr words) (if (zerop (length new-line))
                                         (car words)
                                         (concatenate 'string new-line " " (car words)))
               answer)))))

(defun process-thought (thought)
  (format *trace-output* "~&~a~%"
          (reflow-comment
           (mapcar #'str:trim
                   (str:split #\newline (gemini:get-text thought))))))

(defun process-thoughts (thoughts)
  (mapc #'process-thought thoughts))

(defun process-content (content)
  "Process the content of the Gemini response and return the generated expression."
  (if (equal (gemini:get-role content) "model")
      (let* ((parts (gemini:get-parts content))
             (thoughts (remove-if-not #'gemini:thought? parts))
             (results (remove-if #'gemini:thought? parts)))
        (process-thoughts thoughts)
        (if (gemini:singleton-list-of-parts? results)
            (process-part (first results))
            (error "Multiple results ~s" results)))
      (error "Expected content from model, got: ~s" content)))

(defun process-candidate (candidate)
  "Process a single candidate from the Gemini response and return the generated expression."
  (unless (equal (gemini:get-finish-reason candidate) "STOP")
    (error "Invalid finish reason: ~s" candidate))
  (process-content (gemini:get-content candidate)))

(defun process-response (response)
  "Process the response from the Gemini API and return the generated expression."
  (if (gemini:gemini-response? response)
      (let ((candidates (gemini:get-candidates response)))
        (if (gemini:singleton-list-of-candidates? candidates)
            (process-candidate (first candidates))
            (error "Multiple candidates found in response: ~A" candidates)))
      (error "Unrecognized Gemini response ~s" response)))

(defun pseudocode->expression (pseudocode lexical-variables source-code)
  "Generate a Common Lisp expression from PSEUDOCODE, LEXICAL-VARIABLES, and SOURCE-CODE."
  (let ((gemini:*include-thoughts* t)
        (gemini:*temperature* +pseudo-temperature+)
        (gemini:*output-processor* #'process-response)
        (gemini:*system-instruction* (system-instruction lexical-variables source-code)))
    (gemini:invoke-gemini +pseudo-model+ (pseudocode-expression-prompt pseudocode))))

(defun lambda-list->lexicals (lambda-list)
  "Convert a lambda list into a list of lexical variables."
  (mapcan (lambda (arg)
            (cond ((symbolp arg)
                   (if (member arg lambda-list-keywords)
                       nil
                       (list arg)))
                  ((and (consp arg)
                        (= (length arg) 3)
                        (symbolp (third arg)))
                   (cond ((symbolp (first arg)) (list (first arg) (third arg)))
                         ((and (consp (first arg))
                               (symbolp (first (first arg)))
                               (symbolp (second (first arg))))
                          (list (second (first arg)) (third arg)))))
                  ((consp arg)
                   (cond ((symbolp (first arg)) (list (first arg)))
                         ((and (consp (first arg))
                               (symbolp (first (first arg)))
                               (symbolp (second (first arg))))
                          (list (second (first arg))))
                         (t
                          (error "Unexpected argument in lambda list: ~s" arg))))
                  (t (error "Unexpected argument in lambda list: ~s" arg))))
             lambda-list))

(defun pseudocode->definition (pseudocode name arguments source-code)
  "Generate a Common Lisp definition from PSEUDOCODE, ARGUMENTS, and SOURCE-CODE."
  (let ((gemini:*include-thoughts* t)
        (gemini:*temperature* +pseudo-temperature+)
        (gemini:*output-processor* #'process-response)
        (gemini:*system-instruction* (system-instruction (lambda-list->lexicals arguments) source-code)))
    `(DEFUN ,name ,arguments
       ,pseudocode
       ,(gemini:invoke-gemini +pseudo-model+ (pseudocode-expression-prompt pseudocode)))))

(defmacro pseudo (pseudocode &environment macro-environment)
  "Generate a Common Lisp expression from PSEUDOCODE."
  (pseudocode->expression
   pseudocode
   (lexical-variables macro-environment)
   (current-file-source-code)))

(defmacro pseudefun (name arguments pseudocode)
  "Generate a Common Lisp function definition from NAME, ARGUMENTS, and PSEUDOCODE."
  (pseudocode->definition
   pseudocode
   name
   arguments
   (current-file-source-code)))
