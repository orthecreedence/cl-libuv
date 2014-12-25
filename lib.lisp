(in-package :libuv)

(eval-when (:load-toplevel)
  (define-foreign-library libuv
    (:darwin (:or "libuv.dylib"))
    (:unix (:or "libuv.so"
                "/usr/lib/libuv.so"
                "/usr/local/lib/libuv.so"
                ; brew's install of libevent on Mac OX X
                "/usr/local/lib/libuv.dylib"))
    (:windows (:or "libuv.dll"))
    (t (:default "libuv")))
  (unless (foreign-library-loaded-p 'libuv)
    (use-foreign-library libuv)))

