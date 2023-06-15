(cl:eval-when (:load-toplevel :execute)
  (when (uiop:getenv "HOMEBREW_PREFIX")
    (pushnew :homebrew *features*))
  (asdf:operate 'asdf:load-op 'cffi-grovel))

(asdf:defsystem cl-libuv-config
  :author "Andrew Danger Lyon <orthecreedence@gmail.com>"
  :license "MIT"
  :version "0.1.6"
  :description "Configure libuv to be used by cl-libuv."
  :serial t
  :components ((:file "config")))
