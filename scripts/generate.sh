#!/bin/sh

#
# This script can be used to regenerate the bindings.lisp file using
# SWIG. 
#

#swig -cffi -module bindings-win -noswig-lisp -o bindings-win.lisp scripts/bindings-win.i 
swig -cffi -module bindings -noswig-lisp -o bindings.lisp scripts/bindings-posix.i
sed -i 's|( *4095)|4095|' bindings.lisp
sed -i 's|uv_cpu_info_s_cpu_times|uv_cpu_info_s_cpu_times_s|' bindings.lisp
sed -i 's|uv_signal_s_tree_entry|uv_signal_s_tree_entry_s|' bindings.lisp

# ------------------------------------------------------------------------------
# make our exports 
# ------------------------------------------------------------------------------
echo -ne "(in-package :libuv)\n\n" > exports.lisp
cat bindings.lisp | \
    grep -e '^(\(cffi\|cl\):' | \
    sed 's|^(cffi:defcfun.*" \(#.(lispify[^)]\+)\).*|\1|' | \
    sed 's|^(cffi:defcenum.*\(#.(lispify[^)]\+)\).*|\1|' | \
    sed 's|^(cffi:defcunion.*\(#.(lispify[^)]\+)\).*|\1|' | \
    sed 's|^(cffi:defcstruct.*\(#.(lispify[^)]\+)\).*|\1|' | \
    sed 's|^(cl:defconstant.*\(#.(lispify[^)]\+)\).*|\1|' | \
    sed 's|^\(.*\)$|(export '"'"'\1)|' \
    >> exports.lisp

# ------------------------------------------------------------------------------
# make our accessors
# ------------------------------------------------------------------------------
cat <<-EOFMAC > accessors.lisp
(in-package :libuv.accessors)

(defmacro make-accessors (c-struct)
  \`(progn
     ,@(loop for slot-name in (foreign-slot-names \`(:struct ,(intern (string c-struct) :libuv)))
             for accessor-name = (intern (concatenate 'string (symbol-name c-struct)
                                                      "-"
                                                      (symbol-name slot-name)))
             append (list \`(defmacro ,accessor-name (ptr)
			 (list 'foreign-slot-value ptr ''(:struct ,(intern (string c-struct) :libuv)) '',slot-name))
                          \`(export ',accessor-name :libuv.accessors)))))

EOFMAC

cat bindings.lisp | \
    grep defcstruct | \
    sed 's|.*#\.(lispify|(make-accessors #.(libuv::lispify|g' | sed 's|$|)|' \
    >> accessors.lisp

