(defpackage cxx/test
  (:use :cl
        :prove
        ))
(in-package :cxx/test)


(plan 1)

;; start here
(ok (eigen:init))

(eigen:m.print (eigen:create-mat2 3 3))
(eigen:m.print (eigen:m.set-identity (eigen:create-mat2 3 3)))

(ok (eigen:close))

(finalize)


