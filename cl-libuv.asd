(cl:eval-when (:load-toplevel :execute)
  (when (uiop:getenv "HOMEBREW_PREFIX")
    (pushnew :homebrew *features*))
  (asdf:operate 'asdf:load-op 'cffi-grovel))

(asdf:defsystem cl-libuv
  :author "Andrew Danger Lyon <orthecreedence@gmail.com>"
  :license "MIT"
  :version "0.1.6"
  :description "Low-level libuv bindings for Common Lisp."
  :depends-on (#:cffi #:alexandria)
  :serial t
  :components ((:file "package")
               (:file "lib")
               (:file "wrapper")
               (cffi-grovel:grovel-file "grovel")
               (:file "bindings")
               (:file "exports")
               (:file "accessors")
               (:file "util")))

