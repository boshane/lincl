(defsystem #:lincl
  :version "0.1.0"
  :author "Dan Beauchesne"
  :license "MIT"
  :components ((:module "src"
                :components
                ((:file "package")
                 (:file "objects" :depends-on ("package"))
                 (:file "math" :depends-on ("objects"))
                 (:file "main" :depends-on ("math")))))
  :description ""
  :in-order-to ((test-op (test-op "lincl/tests"))))

(defsystem "lincl/tests"
  :author "Dan Beauchesne"
  :license "MIT"
  :depends-on ("lincl"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for lincl"
  :perform (test-op (op c) (symbol-call :rove :run c)))
