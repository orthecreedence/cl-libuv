(in-package :cl-user)

(defpackage :cl-libuv/config
  (:use :common-lisp)
  (:export #:load-from-custom-path))

(in-package :cl-libuv/config)

(defmacro load-from-custom-path (path)
  "Define the path where libuv is to be found:
    (ql:quickload :cl-libuv/config)
    (cl-libuv-config:load-from-custom-path \"/opt/local/lib/libuv.so\")
    (ql:quickload :cl-libuv)"
  `(progn
     (cffi:define-foreign-library libuv (t ,path))
     (cffi:use-foreign-library libuv)))
