(cl:eval-when (:load-toplevel :execute)
  (asdf:operate 'asdf:load-op 'cffi-grovel))

(asdf:defsystem cl-libuv
  :author "Andrew Danger Lyon <orthecreedence@gmail.com>"
  :license "MIT"
  :version "0.1.5"
  :description "Low-level libuv bindings for Common Lisp."
  :depends-on (#:cffi #:alexandria)
  :components ((:file "package")
               (:file "lib" :depends-on ("package"))
               (cffi-grovel:grovel-file "grovel" :depends-on ("package"))
               (:file "wrapper" :depends-on ("package" "grovel"))
               (:file "bindings" :depends-on ("wrapper"))
               (:file "win-error" :depends-on ("wrapper"))
               (:file "exports" :depends-on ("bindings" "win-error"))
               (:file "accessors" :depends-on ("exports"))
               (:file "util" :depends-on ("accessors"))))

