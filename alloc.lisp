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

