(defsystem "pseudo"
  :description "A library for programming Common Lisp in pseudocode"
  :author "Joe Marshall"
  :license "MIT"
  :version "0.1.0"
  :components ((:file "autodoc" :depends-on ("package"))
               ;; (:file "autodoc-tests" :depends-on ("autodoc" "package" "pseudo"))
               (:file "package")
               (:file "pseudo"  :depends-on ("package"))
               ;; (:file "tests"   :depends-on ("autodoc" "package" "pseudo"))
               )
  :depends-on ("alexandria" "fiveam" "fold" "function" "gemini" "named-let" "series" "str"))
