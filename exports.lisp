(in-package :libuv)

(export 'ssize-t)
(export 'size-t)
(export 'uint64-t)
(export 'uint32-t)
(export 'uint16-t)
(export 'uint8-t)

(export 'in-addr)
(export 'in6-addr)
(export 'sockaddr)
(export 'sockaddr-in)
(export 'sockaddr-in6)
(export 'addrinfo)

(eval-when (:compile-toplevel :load-toplevel)
  (do-symbols (sym :libuv)
    (when (zerop (or (search "UV-" (string sym) :test 'string-equal) -1))
      (export sym))))

