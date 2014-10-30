(in-package :libuv)

(defconstant +debug-mode+ nil)

;; directly exporting these
(defconstant +af-unspec+ 0)
(defconstant +af-unix+ 1)
(defconstant +af-inet+ 2)
(defconstant +af-inet-6+ 23)
(defconstant +sock-stream+ 1)
(defconstant +ipproto-tcp+ 6)

(defun errval (err)
  "Get an error constant value by its name keyword.
   
   So :etimedout gets the enum UV_ETIMEDOUT"
  (let ((sym (intern (format nil "+UV-~a+" (string-upcase (string err))) :keyword)))
    #+windows
    (cffi:foreign-enum-value 'uv:uv-errno-t-w sym)
    #-windows
    (cffi:foreign-enum-value 'uv:uv-errno-t sym)))

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

(defvar *handle-name-val* (make-hash-table :test 'eq)
  "Holds handle handle type -> enum mappings.")
(defvar *handle-val-name* (make-hash-table :test 'eq)
  "Holds handle enum -> handle type mappings.")

(defun alloc-uv-buf (pointer-to-c-buf size &optional uv-buf)
  "Allocate a ub_buf_t object. You'd think this was easy, but the commands that
   take a uv_buf_t expect pointers and the uv_buf_init() function returns a
   stack-allocated value. Don't know WTH is up with that (probably avoiding
   assuming we're using malloc or something).
   
   Anyway, we abstract it here."
  (let ((type #+windows '(:struct uv:uv-buf-t-win)
              #-windows '(:struct uv:uv-buf-t))
        (buf (or uv-buf (cffi:foreign-alloc type))))
    (setf (foreign-slot-value buf type 'base) pointer-to-c-buf
          (foreign-slot-value but type 'len) size)
    (when +debug-mode+ (format t "-- + buf aloc: ~s ~x (existing ~a)~%" size (cffi:pointer-address buf) (not (not uv-buf))))
    buf))

(defun uv-buf-read (uv-buf)
  "Returns two values: the c-buffer and the buffer len of a uv_buf_t object."
  #+windows
    (values (uv-a:uv-buf-t-win-base uv-buf)
            (uv-a:uv-buf-t-win-len uv-buf))
  #-windows
    (values (uv-a:uv-buf-t-base uv-buf)
            (uv-a:uv-buf-t-len uv-buf)))

(defun free-uv-buf (uv-buf)
  "Free an allocated uv_buf_t."
  (when +debug-mode+ (format t "-- + buf free: ~s ~x~%" size (cffi:pointer-address buf)))
  (cffi:foreign-free uv-buf))

(defun alloc-handle (type)
  "Allocation a handle object (free with free-handle)."
  (let* ((size (gethash type *handle-sizes*))
         (handle (cffi:foreign-alloc :char :count size)))
    (setf (uv-a:uv-handle-s-data handle) (cffi:make-pointer (handle-to-val type)))
    (when +debug-mode+ (format t "-- + handle aloc: ~s ~x~%" type (cffi:pointer-address handle)))
    handle))

(defun alloc-req (type)
  "Allocation a req object (free with free-req)."
  (let* ((size (gethash type *req-sizes*))
         (req (cffi:foreign-alloc :char :count size)))
    (when +debug-mode+ (format t "-- + req aloc: ~s ~x~%" type (cffi:pointer-address req)))
    req))

(defun free-handle (pointer)
  "Free a handle object created with alloc-handle."
  (when (cffi:pointerp pointer)
    (when +debug-mode+ (format t "-- - handle free: ~s ~x~%" (handle-type pointer) (cffi:pointer-address pointer)))
    (cffi:foreign-free pointer)))

(defun free-req (pointer)
  "Free a req object created with alloc-req."
  (when (cffi:pointerp pointer)
    (when +debug-mode+ (format t "-- - req free: ~x~%" (cffi:pointer-address pointer)))
    (cffi:foreign-free pointer)))

(defun handle-type (handle-ptr)
  "Given a libuv handle, return its type."
  (handle-from-val (cffi:pointer-address (uv-a:uv-handle-s-data handle-ptr))))

(defun handle-to-val (handle-keyword)
  "Get a handle enuma val from its name."
  (gethash handle-keyword *handle-name-val*))

(defun handle-from-val (enumval)
  "Get a handle name form its enuma val."
  (gethash enumval *handle-val-name*))

(defun populate-sizes ()
  "Fill our hashes with size values for our handle/req classes."
  (dolist (handle *handle-types*)
    (let* ((key (intern (string-upcase (string handle)) :keyword))
           (uvsym (intern (string-upcase (format nil "+uv-~a+" (string handle))) :keyword))
           (enumval (cffi:foreign-enum-value 'uv:uv-handle-type uvsym)))
      (setf (gethash key *handle-sizes*) (uv:uv-handle-size enumval))
      (setf (gethash key *handle-name-val*) enumval
            (gethash enumval *handle-val-name*) key)))
  (dolist (req *req-types*)
    (let ((key (intern (string-upcase (string req)) :keyword))
          (uvsym (intern (string-upcase (format nil "+uv-~a+" (string req))) :keyword)))
      (setf (gethash key *req-sizes*) (uv:uv-req-size (cffi:foreign-enum-value 'uv:uv-req-type uvsym))))))

(eval-when (:load-toplevel)
  (populate-sizes))

