#|
  This file is a part of cl-eigen project.
  Copyright (c) 2018 Islam Omar (io1131@fayoum.edu.eg)
|#


(defpackage :cxx-eigen/system
  (:use :cl :asdf))

(in-package :cxx-eigen/system)

(defsystem :cxx-eigen
  :version "1.0"
  :author "Islam Omar"
  :license "MIT"
  :depends-on (:cxx)
  :components ((:file "package")
               (:module "src"
                        :components ((:file "cxx-eigen"))))
  :description "Common Lisp eigen Interoperation"
  :long-description
  #.(with-open-file (stream (merge-pathnames
                             #p"README.markdown"
                             (or *load-pathname* *compile-file-pathname*))
                            :if-does-not-exist nil
                            :direction :input)
      (when stream
        (let ((seq (make-array (file-length stream)
                               :element-type 'character
                               :fill-pointer t)))
          (setf (fill-pointer seq) (read-sequence seq stream))
          seq)))
  :in-order-to ((test-op (test-op "cxx-eigen-test"))))
