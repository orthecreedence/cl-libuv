(in-package :libuv)

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
    signal
    file)
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

(defun alloc-uv-buf (pointer-to-c-buf size)
  "Allocate a ub_buf_t object. You'd think this was easy, but the commands that
   take a uv_buf_t expect pointers and the uv_buf_init() function returns a
   stack-allocated value. Don't know WTH is up with that (probably avoiding
   assuming we're using malloc or something).
   
   Anyway, we abstract it here."
  #+windows
    (let ((buf (cffi:foreign-alloc 'uv:uv-buf-t-win)))
      (setf (uv-a:uv-buf-t-win-base buf) pointer-to-c-buf
            (uv-a:uv-buf-t-win-len buf) size)
      buf)
  #-windows
    (let ((buf (cffi:foreign-alloc 'uv:uv-buf-t)))
      (setf (uv-a:uv-buf-t-base buf) pointer-to-c-buf
            (uv-a:uv-buf-t-len buf) size)
      buf))

(defun uv-buf-read (uv-buf)
  "Returns two values: the c-buffer and the buffer len of a uv_buf_t object."
  #+windows
    (values (uv-a:uv-buf-t-win-base uv-buf)
            (uv-a:uv-buf-t-win-len uv-buf))
  #-windows
    (values (uv-a:uv-buf-t-base uv-buf)
            (uv-a:uv-buf-t-len uv-buf)))

(defun alloc-handle (type)
  "Allocation a handle object (free with free-handle)."
  (let ((size (gethash type *handle-sizes*)))
    (cffi:foreign-alloc :char :count size)))

(defun alloc-req (type)
  "Allocation a req object (free with free-req)."
  (let ((size (gethash type *req-sizes*)))
    (cffi:foreign-alloc :char :count size)))

(defun free-handle (pointer)
  "Free a handle object created with alloc-handle."
  (when (cffi:pointerp pointer)
    (cffi:foreign-free pointer)))

(defun free-req (pointer)
  "Free a req object created with alloc-req."
  (when (cffi:pointerp pointer)
    (cffi:foreign-free pointer)))

(defun populate-sizes ()
  "Fill our hashes with size values for our handle/req classes."
  (dolist (handle *handle-types*)
    (let ((key (intern (string-upcase (string handle)) :keyword))
          (uvsym (intern (string-upcase (format nil "+uv-~a+" (string handle))) :keyword)))
      (setf (gethash key *handle-sizes*) (uv:uv-handle-size (cffi:foreign-enum-value 'uv:uv-handle-type uvsym)))))
  (dolist (req *req-types*)
    (let ((key (intern (string-upcase (string req)) :keyword))
          (uvsym (intern (string-upcase (format nil "+uv-~a+" (string req))) :keyword)))
      (setf (gethash key *req-sizes*) (uv:uv-req-size (cffi:foreign-enum-value 'uv:uv-req-type uvsym))))))

(eval-when (:load-toplevel)
  (populate-sizes))

