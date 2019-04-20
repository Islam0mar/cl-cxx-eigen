
(in-package :cxx-eigen)

(defun init ()
  (pushnew (merge-pathnames #p"common-lisp/programs/cl-cxx-eigen/lib/"
                            (user-homedir-pathname))
           cffi:*foreign-library-directories*
           :test #'equal)

  (cffi:define-foreign-library my-lib
    (t (:default "libclcxx-eigen")))

  (cffi:use-foreign-library my-lib)
  (cxx:init)
  (let ((curr-pack (package-name *package*)))
    (unwind-protect
         (progn
           (eval `(in-package :cxx-eigen))
           (cxx::register-package "eigen" (foreign-symbol-pointer "EIGEN")))
      (eval `(in-package ,curr-pack)))))

(defun finish ()
  (cxx::remove-c-package "eigen")
  (cffi:close-foreign-library 'my-lib))

