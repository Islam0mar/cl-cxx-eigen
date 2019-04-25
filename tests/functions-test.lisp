(defpackage cxx/test
  (:use :cl
        :prove
        :CXX-EIGEN
        ))
(in-package :cxx/test)

;; (ql:quickload :CXX-EIGEN)

(plan 2)

;; start here
(ok (init))

(defun test-m-loop ()
  (let ((x (create-matrix2 3 3))
        (d 0d0))
    (loop for i from 0 below 3
       do (loop for j from 0 below 3
             do (m.set-at-index x i j d)
               (setf d (1+ d))))
    x))
(defun test-m-identity ()
  (let ((x (create-matrix2 3 3)))
    (m.set-identity x)
    x))
(defun test-m-fill-lst ()
  (let ((x (create-matrix2 3 3)))
    (m.set-from-list x '(1d0 2d0 3d0 4d0 5d0 6d0 7d0 8d0 9d0 10d0) 5 2)
    x))

(m.print (test-m-loop))
(m.print (test-m-identity))
(m.print (test-m-fill-lst))

(ok (finish))

(finalize)


