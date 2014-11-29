#!/bin/sh

#
# This script can be used to regenerate the bindings.lisp file using
# SWIG. 
#

#swig -cffi -module bindings-win -noswig-lisp -o bindings-win.lisp scripts/bindings-win.i 
swig \
	-cffi \
	-importall \
	-cpperraswarn \
	-w302 \
	-w305 \
	-Ic:/dev/MinGW/include \
	-I/usr/include \
	-I/usr/include/linux \
	-module bindings \
	-noswig-lisp \
	-o bindings.lisp \
	scripts/bindings.i || exit 1

# remove anonymous enums
cat bindings.lisp | \
	awk 'BEGIN{ RS = ""; FS = "\n" } { if ($1 !~ /defanonenum/) {print $0; print ""} }' \
	>> bindings2.lisp
mv bindings2.lisp bindings.lisp

sed -i 's|( *4095)|4095|' bindings.lisp
sed -i 's|uv_cpu_info_s_cpu_times|uv_cpu_info_s_cpu_times_s|' bindings.lisp
sed -i 's|uv_signal_s_tree_entry|uv_signal_s_tree_entry_s|' bindings.lisp
sed -i 's| #\.(lispify "\([0-9a-zA-Z_]\+\)" '"'"'classname))| #.(lispify "\1" '"'"'classname-wrap))|g' bindings.lisp

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

# ------------------------------------------------------------------------------
# make windows error codes
# ------------------------------------------------------------------------------
swig \
	-cffi \
	-module win-error \
	-noswig-lisp \
	-o win-error.lisp \
	scripts/win-error.i || exit 1
cat win-error.lisp | egrep '("uv_errno_t"|"UV_E)' > win-error.lisp.tmp
echo '(in-package :libuv)' > win-error.lisp
cat win-error.lisp.tmp >> win-error.lisp && rm win-error.lisp.tmp
sed -i 's|"uv_errno_t"|"uv_errno_t_w"|' win-error.lisp
sed -i 's|( *4095)|4095|' win-error.lisp

# win-error exports
cat win-error.lisp | \
    grep -e '^(\(cffi\|cl\):' | \
    sed 's|^(cffi:defcfun.*" \(#.(lispify[^)]\+)\).*|\1|' | \
    sed 's|^(cffi:defcenum.*\(#.(lispify[^)]\+)\).*|\1|' | \
    sed 's|^(cffi:defcunion.*\(#.(lispify[^)]\+)\).*|\1|' | \
    sed 's|^(cffi:defcstruct.*\(#.(lispify[^)]\+)\).*|\1|' | \
    sed 's|^(cl:defconstant.*\(#.(lispify[^)]\+)\).*|\1|' | \
    sed 's|^\(.*\)$|(export '"'"'\1)|' \
    >> exports.lisp
