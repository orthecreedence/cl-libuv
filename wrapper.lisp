(in-package :libuv)

(defmacro defanonenum (&body enums)
  "Converts anonymous enums to defconstants."
  `(progn ,@(loop for value in enums
                        for index = 0 then (1+ index)
                        when (listp value) do (setf index (second value)
                                                          value (first value))
                        collect `(defconstant ,value ,index))))

(defun lispify (name flag &optional (package *package*))
  (labels ((helper (lst last rest &aux (c (car lst)))
             (cond
               ((null lst)
                rest)
               ((upper-case-p c)
                (helper (cdr lst) 'upper
                        (case last
                          ((lower digit) (list* c #\- rest))
                          (t (cons c rest)))))
               ((lower-case-p c)
                (helper (cdr lst)
                        'lower
                        (cons (case (readtable-case *readtable*)
                                (:preserve c)
                                (t (char-upcase c)))
                              rest)))
               ((digit-char-p c)
                (helper (cdr lst) 'digit
                        (case last
                          ((upper lower) (list* c #\- rest))
                          (t (cons c rest)))))
               ((char-equal c #\_)
                (helper (cdr lst) '_ (cons #\- rest)))
               (t
                (error "Invalid character: ~A" c))))
           (strip-prefix (prf str)
             (let ((l (length prf)))
               (if (and (eq flag 'enumvalue)
                        (< l (length str))
                        (string= (string-downcase prf) (string-downcase (subseq str 0 l))))
                   (subseq str l)
                   str)))
           (process (name)
             (case flag
               (classname-wrap (list :struct name))
               (t name))))
    (let ((fix (case flag
                 ((constant enumvalue) "+")
                 (variable "*")
                 (t ""))))
      (when (eq flag 'enumvalue)
        (setf fix ""))
      (process (intern (concatenate 'string fix (nreverse (helper (concatenate 'list (strip-prefix "uv_" name)) nil nil)) fix)
                       package)))))

(defun version<= (str-version str-cmp)
  (declare (ignore str-version str-cmp))
  ;; fuck it
  ;(asdf:version-satisfies (asdf:find-system :cffi) "0.10.7.0")
  t)

(defmacro cffi-type (type)
  (let ((version (asdf:component-version (asdf:find-system :cffi))))
    (if (version<= version "0.10.7.1")
        `(quote ,type)
        ``(:struct ,,type))))
