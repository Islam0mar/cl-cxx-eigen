#|
  This file is a part of lcm project.
  Copyright (c) 2018 Islam Omar (io1131@fayoum.edu.eg)
|#

(defpackage :cxx-eigen-test/system
  (:use :cl :asdf))

(in-package :cxx-eigen-test/system)

(defsystem :cxx-eigen-test
  :defsystem-depends-on (:prove-asdf)
  :author "Islam Omar"
  :license "MIT"
  :depends-on (:cxx-eigen
               :prove)
  :components ((:module "tests"
                :components
                ((:test-file "functions-test"))))
  :description "Test system for eigen"

  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run-test-system) :prove)
c)))
