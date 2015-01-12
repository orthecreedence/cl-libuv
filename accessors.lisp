(in-package :libuv.accessors)

(defmacro make-accessors (c-struct)
  (multiple-value-bind (slots error)
      (ignore-errors (foreign-slot-names `(:struct ,(intern (string c-struct) :libuv))))
    (when error
      (return-from make-accessors))
    (let ((_ptr (gensym "ptr")))
      `(progn
         ,@(loop for slot-name in slots
                 for accessor-name = (intern (concatenate 'string (symbol-name c-struct)
                                                          "-"
                                                          (symbol-name slot-name)))
                 append (list `(defmacro ,accessor-name (,_ptr)
                                 (list 'foreign-slot-value ,_ptr '(:struct ,(intern (string c-struct) :libuv)) ',slot-name))
                              `(import ',slot-name :libuv)
                              `(export ',slot-name :libuv)
                              `(export ',accessor-name :libuv.accessors)))))))

(defmacro do-class-symbols ()
  (let ((makers nil))
    (do-symbols (sym :libuv)
      (let ((str (string sym)))
        (when (and (zerop (or (search "UV-" str) -1))
                   (or (string= "-S" (subseq str (- (length str) 2)))
                       (string= "-T" (subseq str (- (length str) 2)))))
          (push
            `(handler-bind
                 ((error (lambda (e) e)))
               (progn (make-accessors ,sym)))
            makers))))
    `(progn ,@makers)))

(eval-when (:compile-toplevel :load-toplevel)
  (do-class-symbols)

  (make-accessors in-addr)
  (make-accessors in6-addr)
  (make-accessors sockaddr)
  (make-accessors sockaddr-in)
  (make-accessors sockaddr-in6)
  (make-accessors addrinfo))

