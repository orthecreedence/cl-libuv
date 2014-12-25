(defpackage #:libuv-grovel
  (:use :cl :cffi-grovel))

(defpackage #:libuv
  (:use :cl :cffi :libuv-grovel)
  (:nicknames :uv)
  (:export #:+af-unspec+
           #:+af-unix+
           #:+af-inet+
           #:+af-inet-6+
           #:+sock-stream+
           #:+ipproto-tcp+

           #:errval
           #:alloc-uv-buf
           #:uv-buf-read
           #:free-uv-buf
           #:alloc-handle
           #:alloc-req
           #:free-handle
           #:free-req
           #:handle-type))

(defpackage #:libuv.accessors
  (:use :cl :cffi :libuv :libuv-grovel)
  (:nicknames :uv-a))

