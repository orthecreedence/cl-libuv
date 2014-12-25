(in-package :libuv-grovel)

#+windows
(progn
  (cc-flags "-Ic:/include/"
            "-Ic:/include/uv/"))

(include "uv.h")

