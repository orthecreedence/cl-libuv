#!/bin/sh

#
# This script can be used to regenerate the bindings.lisp file using
# SWIG. 
#

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
sed -i 's|:long-long|size-t|g' bindings.lisp
sed -i 's|:unsigned-long-long|ssize-t|g' bindings.lisp
sed -i 's|uv_cpu_info_s_cpu_times|uv_cpu_info_s_cpu_times_s|' bindings.lisp
sed -i 's|uv_signal_s_tree_entry|uv_signal_s_tree_entry_s|' bindings.lisp
sed -i 's| #\.(lispify "\([0-9a-zA-Z_]\+\)" '"'"'classname))| #.(lispify "\1" '"'"'classname-wrap))|g' bindings.lisp

