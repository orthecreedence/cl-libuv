(in-package :libuv.accessors)

(defmacro make-accessors (c-struct)
  (multiple-value-bind (slots error)
      (ignore-errors (foreign-slot-names `(:struct ,(intern (string c-struct) :libuv))))
    (when error
      (return-from make-accessors))
    `(progn
       ,@(loop for slot-name in slots
               for accessor-name = (intern (concatenate 'string (symbol-name c-struct)
                                                        "-"
                                                        (symbol-name slot-name)))
               append (list `(defmacro ,accessor-name (ptr)
                               (list 'foreign-slot-value ptr ''(:struct ,(intern (string c-struct) :libuv)) '',slot-name))
                            `(export ',accessor-name :libuv.accessors))))))

(defmacro do-class-symbols ()
  (let ((makers nil))
    (do-symbols (sym :libuv)
      (let ((str (string sym)))
        (when (and (zerop (or (search "UV-" str) -1))
                   (or (string= "-S" (subseq str (- (length str) 2)))
                       (string= "-T" (subseq str (- (length str) 2)))))
          (push
            `(handler-case
               (progn (make-accessors ,sym))
               ;; probably not a class, skip it
               (error () nil))
            makers))))
    `(progn ,@makers)))

(eval-when (:compile-toplevel :load-toplevel)
  (do-class-symbols))

