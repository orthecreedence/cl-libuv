(in-package :libuv)

(defconstant +af-unspec+ 0)
(defconstant +af-unix+ 1)
(defconstant +af-inet+ 2)
(defconstant +af-inet-6+ 23)
(defconstant +sock-stream+ 1)
(defconstant +ipproto-tcp+ 6)

(defun errval (err)
  "Get an error constant value by its name keyword.

   So :etimedout gets the enum UV_ETIMEDOUT"
  (cffi:foreign-enum-value 'uv:uv-errno-t err))

(defparameter *handle-types*
  '(async
    check
    fs-event
    fs-poll
    handle
    idle
    named-pipe
    poll
    prepare
    process
    stream
    tcp
    timer
    tty
    udp
    signal)
  "Enumerates our handle classes.")

(defparameter *req-types*
  '(req
    connect
    write
    shutdown
    udp-send
    fs
    work
    getaddrinfo
    getnameinfo)
  "Enumerates our req classes.")

(defvar *handle-sizes* (make-hash-table :test 'eq)
  "Holds calculated size for uv handle objects.")

(defvar *req-sizes* (make-hash-table :test 'eq)
  "Holds calculated size for uv req objects.")

(defun alloc-uv-buf (pointer-to-c-buf size &optional uv-buf)
  "Allocate a ub_buf_t object. You'd think this was easy, but the commands that
   take a uv_buf_t expect pointers and the uv_buf_init() function returns a
   stack-allocated value. Don't know WTH is up with that (probably avoiding
   assuming we're using malloc or something).

   Anyway, we abstract it here."
  (let* ((type '(:struct uv:uv-buf-t))
         (buf (or uv-buf (cffi:foreign-alloc type))))
    (setf (foreign-slot-value buf type 'base) pointer-to-c-buf
          (foreign-slot-value buf type 'len) size)
    buf))

(defun uv-buf-read (uv-buf)
  "Returns two values: the c-buffer and the buffer len of a uv_buf_t object."
  (values (uv-a:uv-buf-t-base uv-buf)
          (uv-a:uv-buf-t-len uv-buf)))

(defun free-uv-buf (uv-buf)
  "Free an allocated uv_buf_t."
  (cffi:foreign-free uv-buf))

(defun alloc-handle (type)
  "Allocate a handle object (free with free-handle)."
  (let* ((size (gethash type *handle-sizes*))
         (handle (cffi:foreign-alloc :char :count size)))
    handle))

(defun alloc-req (type)
  "Allocate a req object (free with free-req)."
  (let* ((size (gethash type *req-sizes*))
         (req (cffi:foreign-alloc :char :count size)))
    req))

(defun free-handle (pointer)
  "Free a handle object created with alloc-handle."
  (when (cffi:pointerp pointer)
    (cffi:foreign-free pointer)))

(defun free-req (pointer)
  "Free a req object created with alloc-req."
  (when (cffi:pointerp pointer)
    (cffi:foreign-free pointer)))

(defun handle-type (handle-ptr)
  "Given a libuv handle, return its type."
  (uv-a:uv-handle-s-type handle-ptr))

(defun populate-sizes ()
  "Fill our hashes with size values for our handle/req classes."
  (dolist (key (mapcar #'alexandria:make-keyword *handle-types*))
    (let* ((enumval (cffi:foreign-enum-value 'uv:uv-handle-type key)))
      (setf (gethash key *handle-sizes*) (uv:uv-handle-size enumval))))
  (dolist (req *req-types*)
    (let ((key (intern (string-upcase (string req)) :keyword))
          (uvsym (intern (string req) :keyword)))
      (setf (gethash key *req-sizes*) (uv:uv-req-size (cffi:foreign-enum-value 'uv:uv-req-type uvsym))))))

(eval-when (:load-toplevel)
  (populate-sizes))
