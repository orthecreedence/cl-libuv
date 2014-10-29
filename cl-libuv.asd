(asdf:defsystem cl-libuv
  :author "Andrew Danger Lyon <orthecreedence@gmail.com>"
  :license "MIT"
  :version "0.1.5"
  :description "Low-level libuv bindings for Common Lisp."
  :depends-on (#:cffi)
  :components ((:file "libuv")
               (:file "wrapper" :depends-on ("libuv"))
               (:file "bindings" :depends-on ("wrapper"))
               (:file "win-error" :depends-on ("wrapper"))
               (:file "exports" :depends-on ("bindings" "win-error"))
               (:file "accessors" :depends-on ("exports"))
               (:file "util" :depends-on ("accessors"))))

