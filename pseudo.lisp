;;; -*- Lisp -*-

(in-package "PSEUDO")

(defparameter +pseudo-model+ "gemini-2.5-flash"
  "The model name for the pseudocode generator.")

(defparameter +pseudo-temperature+ 0.1
  "The temperature setting for the pseudocode generator.")

(defparameter +pseudo-expression-prompt+ "Generate a Common Lisp expression from the following description: ")

(defparameter +prompt-includes-docstrings+ nil
  "Whether the prompt includes docstrings for functions and variables.")

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
     " *   **Data manipulation**: `alexandria`, `str`"
     " *   **Web development:** `hunchentoot`, `dexador`, `quri`, `cl-cookie`, `cl-smtp`, `cl-ppcre`"
     " *   **Data serialization:** `json`, `cl-yaml`, `cl-csv`"
     " *   **Concurrency:** `bordeaux-threads`"
     " *   **Cryptography:** `ironclad`"
     " *   **Dates and times:** `local-time`"
     " *   **Foreign Function Interface (FFI):** `cffi`")))

(defparameter +priorities+
  (str:join
   #\newline
   `("When generating code, prioritize:"
     " *   **Readability:** Use clear variable names and structure the code logically."
     " *   **Efficiency:** Choose appropriate algorithms and data structures.  Avoid unnecessary operations."
     " *   **Idiomatic Common Lisp:**  Leverage Common Lisp's features, `multiple-value-bind`, and macros, to write concise and expressive code."
     " *   **Error handling:**  Consider potential errors and include appropriate error handling mechanisms, such as `handler-case` or `ignore-errors`."
     " *   **Library Usage:** Prefer using appropriate third-party libraries to standard functions when libraries have a specific function for certain cases. For example, prefer `str:replace-all` to `substitute` if string replacement is what is needed."
     ""
     "You are provided with a list of preferred functions, other allowed functions, preferred global variables, and other allowed global variables.  You may be supplied with the source code of the file being compiled.  You **MUST** use only functions and global variables from these lists or from the source code.  You should prefer to use functions and global variables from the preferred lists and the source code.  When a function is available in both the `COMMON-LISP` package and another package, prefer the non-`COMMON-LISP` package's function.")))

(defparameter +expression-output-requirements+
  (str:join
   #\newline
   `("Your task is code generation.  Specifically, you will be expanding pseudocode into Common Lisp."
     "You are not attempting to interpret the pseudocode, but rather to generate a Common Lisp expression that implements the pseudocode."
     "Evaluating the expanded pseudocode should be equivalent to evaluating the original pseudocode."
     "When generating code, ensure that the output is:"
     " *   **Valid Common Lisp:** The generated code must be a valid Common Lisp s-expression."
     " *   **Balanced Parentheses:**  The generated code must have balanced parentheses."
     " *   **Well-formed:** The generated code must not contain syntax errors."
     " *   **Complete:** The generated code must be a complete expression that can be evaluated in a Common Lisp environment."
     " *   **Self-contained:** The generated code should not rely on external files or resources unless explicitly allowed in the instructions."
     " *   **Unquoted:** The generated code should not be quoted, backquoted, backticked, or quasiquoted unless it is a quoted form (e.g., a literal list or symbol).  The output should be a valid s-expression that can be evaluated directly."
     " *   **No Backtick:** Do *NOT* use a backtick immediately after the opening triple backticks.  Generated code **MUST NOT** begin with a backtick."
     " *   **Efficient:** The generated code should be efficient in terms of time and space complexity."
     " *   **Idiomatic:** The generated code should follow Common Lisp idioms and best practices."
     ""
     " *   Your output **MUST** be a valid Common Lisp s-expression.  The code must be directly evaluable in a Common Lisp interpreter."
     " *   Do **NOT** include any surrounding text, explanations, or comments in your response. Only the s-expression."
     " *   Do **NOT** generate function definitions (e.g., with `defun`, `defmacro`).  The output should be a standalone s-expression that performs the requested task.  The caller will evaluate the expression."
     " *   Be very careful when using recursion.  The generated code should not cause infinite recursion or stack overflow."
     )))

(defparameter +pseudo-control-requirements+
  (str:join
   #\newline
   `("You **MUST NOT** generate calls to the following functions:"
     " * `PSEUDO`"
     " * `PSEUDEFUN`"
     "")))

(defun preferred-functions-part (preferred-functions)
  "Create a Gemini part for preferred functions."
  (gemini:part
   (str:join
    #\newline
    `("**Preferred Functions**"
      "You should prefer to use the following functions:"
      ,@(mapcar (lambda (f)
                  (format nil " * `~(~s~)`~@[ - ~a~]" f (and +prompt-includes-docstrings+
                                                             (gemini:deflow (documentation f 'function)))))
                (sort preferred-functions
                      #'string< :key #'symbol-name))))))

(defun other-allowed-functions-part (allowed-functions)
  "Create a Gemini part for allowed functions."
  (gemini:part
   (str:join
    #\newline
    `("**Other Allowed Functions**"
      "You are allowed to use the following additional functions:"
      ,@(mapcar (lambda (f)
                  (format nil " * `~(~s~)`" f))
                (sort allowed-functions
                      #'string< :key #'symbol-name))))))

(defun preferred-macros-part (preferred-macros)
  "Create a Gemini part for preferred macros."
  (gemini:part
   (str:join
    #\newline
    `("**Preferred Macros**"
      "You should prefer to use the following macros:"
      ,@(mapcar (lambda (f)
                  (format nil " * `~(~s~)`~@[ - ~a~]" f (and +prompt-includes-docstrings+
                                                             (gemini:deflow (documentation f 'function)))))
                (sort preferred-macros
                      #'string< :key #'symbol-name))))))

(defun other-allowed-macros-part (allowed-macros)
  "Create a Gemini part for allowed macros."
  (gemini:part
   (str:join
    #\newline
    `("**Other Allowed Macros**"
      "You are allowed to use the following additional macros:"
      ,@(mapcar (lambda (f)
                  (format nil " * `~(~s~)`" f))
                (sort allowed-macros
                      #'string< :key #'symbol-name))))))

(defun preferred-variables-part (preferred-variables)
  "Create a Gemini part for preferred variables."
  (gemini:part
   (str:join
    #\newline
    `("**Preferred Variables**"
      "You should prefer to use the following global variables:"
      ,@(mapcar (lambda (v)
                  (format nil " * `~(~s~)`~@[ - ~a~]" v (and +prompt-includes-docstrings+
                                                             (gemini:deflow (documentation v 'variable)))))
                (sort preferred-variables
                      #'string< :key #'symbol-name))))))

(defun other-allowed-variables-part (allowed-variables)
  "Create a Gemini part for allowed variables."
  (gemini:part
   (str:join
    #\newline
    `("**Other Allowed Variables**"
      "You are allowed to use the following additional variables:"
      ,@(mapcar (lambda (v)
                  (format nil " * `~(~s~)`" v))
                (sort allowed-variables
                      #'string< :key #'symbol-name))))))

(defun lexical-variables-part (lexical-variables)
  "Create a Gemini part for lexical variables."
  (gemini:part
   (str:join
    #\newline
    `("**Free Variables**"
      "Your code may have free references to the following lexical variables:"
      ,@(mapcar (lambda (v)
                  (format nil " * ~(~a~)" v))
                (sort lexical-variables
                      #'string< :key #'symbol-name))))))

(defun source-code-part (source-code)
  "Create a Gemini part for the source code."
  (when source-code
    (gemini:part
     (str:join
      #\newline
      `("**Source Code**"
        "The source code you are working with is:"
        ,@source-code)))))

(defparameter *coding-style* :functional
  "Your preferred coding style.  One of :functional :pure :imperative :object-oriented.")

(defun style-instructions ()
  (ecase *coding-style*
    (:pure
     (gemini:part
      "You should strive for pure functional code that has no side effects.  Do not reassign variables.  Avoid mutating data. Make use of recursion and higher order functions.  Prefer `FOLD-LEFT` to `DOLIST`.  Prefer `FOLD-LEFT` to `REDUCE`. `IOTA` is inefficent for all but small numbers."))
    (:functional
     (gemini:part
      "You prefer functional code that emphasizes immutability and first-class functions.  Use functional programming techniques to achieve this.  Avoid using global variables and mutable state.  Prefer recursion and higher-order functions.  Prefer `FOLD-LEFT` to `DOLIST`.  Prefer `FOLD-LEFT` to `REDUCE`.  `IOTA` is inefficent for all but small numbers."))
    (:imperative
     (gemini:part
      "You prefer imperative code that uses mutable state and side effects.  Use imperative programming techniques to achieve this.  Feel free to reassign variables and mutate data.  Discourage use of higher order functions.  `TAGBODY` and `GO` are allowed for control flow.  Prefer `DOLIST` to `FOLD-LEFT`.  Prefer `DOLIST` to `REDUCE`."))
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

(defun pseudo-control-part ()
  "Create a Gemini part for pseudo control requirements."
  (gemini:part
   (str:join
    #\newline
    `("**Pseudo Control Requirements**"
      ,+pseudo-control-requirements+))))

(defun system-instruction (lexical-variables omit-functions source-code)
  "Create a system instruction for the Gemini API based on LEXICAL-VARIABLES and SOURCE-CODE."
  (let* ((preferred-functions (set-difference (gemini:visible-functions *package*) omit-functions))

         (other-allowed-functions
           (set-difference (gemini:get-top-level-functions) preferred-functions))

         (preferred-macros
           (remove-if (lambda (macro)
                        (str:starts-with? "DEF" (symbol-name macro)))
                      (gemini:visible-macros *package*)))

         (other-allowed-macros
           (remove-if (lambda (macro)
                        (str:starts-with? "DEF" (symbol-name macro)))
                      (set-difference (gemini:get-top-level-macros) preferred-macros)))

         (preferred-variables (gemini:visible-variables *package*))

         (other-allowed-variables
           (set-difference (gemini:get-top-level-variables) preferred-variables)))

    (gemini:content
     :parts (remove nil
                    (list (gemini:part +general-instructions+)
                          (gemini:part +priorities+)
                          (gemini:part +expression-output-requirements+)
                          (loop-instructions)
                          (style-instructions)
                          (pseudo-control-part)
                          (preferred-functions-part preferred-functions)
                          (other-allowed-functions-part other-allowed-functions)
                          (preferred-macros-part preferred-macros)
                          (other-allowed-macros-part other-allowed-macros)
                          (preferred-variables-part preferred-variables)
                          (other-allowed-variables-part other-allowed-variables)
                          (source-code-part source-code)
                          (lexical-variables-part lexical-variables)
                          )))))

(defun strip-code-block-fence (string)
  (str:join #\newline
            (remove "```" (str:split #\newline string)
                    :test #'str:starts-with? :key #'str:trim)))

(defun strip-leading-backtick (string)
  (if (str:starts-with? "`" string)
      (subseq string 1)
      string))

(defun process-text (text)
  "Process the text part of the Gemini response and return the generated expression."
  (let ((generated-code (strip-leading-backtick (strip-code-block-fence text))))
    (let ((*read-eval* nil))
      (let ((code (read-from-string generated-code)))
        (pprint code *trace-output*)
        code))))

(defun process-part (part)
  "Process a single part of the Gemini response and return the generated expression."
  (if (gemini:text-part? part)
      (process-text (gemini:get-text part))
      (error "Expected a text part, got ~s" part)))

(defun process-content (content)
  "Process the content of the Gemini response and return the generated expression."
  (if (equal (gemini:get-role content) "model")
      (let* ((parts (gemini:get-parts content))
             (thoughts (remove-if-not #'gemini:thought-part? parts))
             (results (remove-if #'gemini:thought-part? parts)))
        (gemini:process-thoughts thoughts)
        (if (gemini:singleton-list-of-parts? results)
            (process-part (first results))
            (error "Multiple results ~s" results)
            ))
      (error "Expected content from model, got: ~s" content)))

(defun process-candidate (candidate)
  "Process a single candidate from the Gemini response and return the generated expression."
  (unless (equal (gemini:get-finish-reason candidate) "STOP")
    (error "Invalid finish reason: ~s" candidate))
  (process-content (gemini:get-content candidate)))

(defun process-response (response)
  "Process the response from the Gemini API and return the generated expression."
  (if (gemini:gemini-response? response)
      (unwind-protect
           (let ((candidates (gemini:get-candidates response)))
             (if (gemini:singleton-list-of-candidates? candidates)
                 (process-candidate (first candidates))
                 (error "Multiple candidates found in response: ~A" candidates)))
        (let ((usage-metadata (gemini:get-usage-metadata response)))
          (when usage-metadata
            (gemini:process-usage-metadata usage-metadata))))
      (error "Unrecognized Gemini response ~s" response)))

(defun pseudocode->expression (pseudocode lexical-variables omit-functions source-code)
  "Generate a Common Lisp expression from PSEUDOCODE, LEXICAL-VARIABLES, and SOURCE-CODE."
  (let ((gemini:*include-thoughts* t)
        (gemini:*output-processor* #'process-response)
        (gemini:*system-instruction* (system-instruction lexical-variables omit-functions source-code))
        (gemini:*tools* nil)
        (start-time (get-universal-time)))
    (unwind-protect
         (let iter ((retry-count 0))
           (when (> retry-count 0)
             (format *trace-output* "~&;; Retrying code generation, attempt ~A...~%" retry-count))
           (if (> retry-count 3)
               ;; On the last retry, don't protect against errors so that we drop into the error handler.
               (let ((gemini:*temperature* (* +pseudo-temperature+ (+ retry-count 1))))
                 (gemini:invoke-gemini (pseudocode-expression-prompt pseudocode) :model +pseudo-model+))
               (restart-case
                   (let ((gemini:*temperature* (* +pseudo-temperature+ (+ retry-count 1))))
                     (gemini:invoke-gemini (pseudocode-expression-prompt pseudocode) :model +pseudo-model+))
                 (retry-generation ()
                  :report (lambda (s) (format s "Retry code generation"))
                   (iter (1+ retry-count))))))
      (let ((elapsed-time (- (get-universal-time) start-time)))
        (format *trace-output* "~&;; Code generation took ~A seconds.~%" elapsed-time)
        (finish-output *trace-output*)))))

(defmacro pseudo (pseudocode &optional omit &environment macro-environment)
  "Generate a Common Lisp expression from PSEUDOCODE."
  (pseudocode->expression
   pseudocode
   (lexical-variables macro-environment)
   (list omit)
   (current-file-source-code)))

(defmacro pseudefun (name arguments pseudocode)
  "Generate a Common Lisp function definition from NAME, ARGUMENTS, and PSEUDOCODE."
  `(PROGN
     (DEFUN ,name ,arguments
       ,pseudocode
       (PSEUDO ,pseudocode ,name))))
