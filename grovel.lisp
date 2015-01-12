(in-package :libuv)

(cc-flags #+windows "-Ic:/include/"
          #+windows "-Ic:/include/uv/")

(include "uv.h")
(include "uv-errno.h")

;; -----------------------------------------------------------------------------
;; type mappings
;; -----------------------------------------------------------------------------
(ctype ssize-t "ssize_t")
(ctype size-t "size_t")
(ctype uint64-t "uint64_t")
(ctype uint32-t "uint32_t")
(ctype uint16-t "uint16_t")
(ctype uint8-t "uint8_t")
#+windows
(progn
  (ctype uv-uid-t "unsigned char")
  (ctype uv-gid-t "unsigned char")
  (ctype wchar "WCHAR")
  (ctype ulong "ULONG"))
#-windows
(progn
  (ctype uv-uid-t "uid_t")
  (ctype uv-gid-t "gid_t"))

;; -----------------------------------------------------------------------------
;; socket struct nastiness
;; -----------------------------------------------------------------------------
(cstruct in-addr "struct in_addr"
  (s-addr "s_addr" :type :unsigned-long))
(cstruct in6-addr "struct in6_addr"
  (s6-addr "s6_addr" :type :unsigned-char :count 16))

(cstruct sockaddr "struct sockaddr"
  (sa-family "sa_family" :type :unsigned-short)
  (sa-data "sa_data" :type :char :count 14))
(cstruct sockaddr-in "struct sockaddr_in"
  (sin-family "sin_family" :type :short)
  (sin-port "sin_port" :type :unsigned-short)
  (sin-addr "sin_addr" :type in-addr)
  (sin-zero "sin_zero" :type :char :count 8))
(cstruct sockaddr-in6 "struct sockaddr_in6"
  (sin6-family "sin6_family" :type :uint16)
  (sin6-port "sin6_port" :type :uint16)
  (sin6-flowinfo "sin6_flowinfo" :type :uint32)
  (sin6-addr "sin6_addr" :type in6-addr)
  (sin6-scope-id "sin6_scope_id" :type :uint32))
  
(cstruct addrinfo "struct addrinfo"
  (ai-flags "ai_flags" :type :int)
  (ai-family "ai_family" :type :int)
  (ai-socktype "ai_socktype" :type :int)
  (ai-protocol "ai_protocol" :type :int)
  (ai-addrlen "ai_addrlen" :type size-t)
  (ai-addr "ai_addr" :type :pointer)
  (ai-canonname "ai_canonname" :type :pointer)
  (ai-next "ai_next" :type :pointer))

;; -----------------------------------------------------------------------------
;; uv enums
;; -----------------------------------------------------------------------------
(cenum uv-errno-t
  ((:e2big "UV_E2BIG"))
  ((:eacces "UV_EACCES"))
  ((:eaddrinuse "UV_EADDRINUSE"))
  ((:eaddrnotavail "UV_EADDRNOTAVAIL"))
  ((:eafnosupport "UV_EAFNOSUPPORT"))
  ((:eagain "UV_EAGAIN"))
  ((:eai_addrfamily "UV_EAI_ADDRFAMILY"))
  ((:eai_again "UV_EAI_AGAIN"))
  ((:eai_badflags "UV_EAI_BADFLAGS"))
  ((:eai_badhints "UV_EAI_BADHINTS"))
  ((:eai_canceled "UV_EAI_CANCELED"))
  ((:eai_fail "UV_EAI_FAIL"))
  ((:eai_family "UV_EAI_FAMILY"))
  ((:eai_memory "UV_EAI_MEMORY"))
  ((:eai_nodata "UV_EAI_NODATA"))
  ((:eai_noname "UV_EAI_NONAME"))
  ((:eai_overflow "UV_EAI_OVERFLOW"))
  ((:eai_protocol "UV_EAI_PROTOCOL"))
  ((:eai_service "UV_EAI_SERVICE"))
  ((:eai_socktype "UV_EAI_SOCKTYPE"))
  ((:ealready "UV_EALREADY"))
  ((:ebadf "UV_EBADF"))
  ((:ebusy "UV_EBUSY"))
  ((:ecanceled "UV_ECANCELED"))
  ((:echarset "UV_ECHARSET"))
  ((:econnaborted "UV_ECONNABORTED"))
  ((:econnrefused "UV_ECONNREFUSED"))
  ((:econnreset "UV_ECONNRESET"))
  ((:edestaddrreq "UV_EDESTADDRREQ"))
  ((:eexist "UV_EEXIST"))
  ((:efault "UV_EFAULT"))
  ((:efbig "UV_EFBIG"))
  ((:ehostunreach "UV_EHOSTUNREACH"))
  ((:eintr "UV_EINTR"))
  ((:einval "UV_EINVAL"))
  ((:eio "UV_EIO"))
  ((:eisconn "UV_EISCONN"))
  ((:eisdir "UV_EISDIR"))
  ((:eloop "UV_ELOOP"))
  ((:emfile "UV_EMFILE"))
  ((:emsgsize "UV_EMSGSIZE"))
  ((:enametoolong "UV_ENAMETOOLONG"))
  ((:enetdown "UV_ENETDOWN"))
  ((:enetunreach "UV_ENETUNREACH"))
  ((:enfile "UV_ENFILE"))
  ((:enobufs "UV_ENOBUFS"))
  ((:enodev "UV_ENODEV"))
  ((:enoent "UV_ENOENT"))
  ((:enomem "UV_ENOMEM"))
  ((:enonet "UV_ENONET"))
  ((:enoprotoopt "UV_ENOPROTOOPT"))
  ((:enospc "UV_ENOSPC"))
  ((:enosys "UV_ENOSYS"))
  ((:enotconn "UV_ENOTCONN"))
  ((:enotdir "UV_ENOTDIR"))
  ((:enotempty "UV_ENOTEMPTY"))
  ((:enotsock "UV_ENOTSOCK"))
  ((:enotsup "UV_ENOTSUP"))
  ((:eperm "UV_EPERM"))
  ((:epipe "UV_EPIPE"))
  ((:eproto "UV_EPROTO"))
  ((:eprotonosupport "UV_EPROTONOSUPPORT"))
  ((:eprototype "UV_EPROTOTYPE"))
  ((:erange "UV_ERANGE"))
  ((:erofs "UV_EROFS"))
  ((:eshutdown "UV_ESHUTDOWN"))
  ((:espipe "UV_ESPIPE"))
  ((:esrch "UV_ESRCH"))
  ((:etimedout "UV_ETIMEDOUT"))
  ((:etxtbsy "UV_ETXTBSY"))
  ((:exdev "UV_EXDEV"))
  ((:unknown "UV_UNKNOWN"))
  ((:eof "UV_EOF"))
  ((:enxio "UV_ENXIO"))
  ((:emlink "UV_EMLINK")))

(cenum uv-req-type
  ((:unknown "UV_UNKNOWN_REQ"))
  ((:req "UV_REQ"))
  ((:connect "UV_CONNECT"))
  ((:write "UV_WRITE"))
  ((:shutdown "UV_SHUTDOWN"))
  ((:udp-send "UV_UDP_SEND"))
  ((:fs "UV_FS"))
  ((:work "UV_WORK"))
  ((:getaddrinfo "UV_GETADDRINFO"))
  ((:getnameinfo "UV_GETNAMEINFO")))

(cenum uv-handle-type
  ((:unknown "UV_UNKNOWN_HANDLE"))
  ((:async "UV_ASYNC"))
  ((:check "UV_CHECK"))
  ((:fs_event "UV_FS_EVENT"))
  ((:fs_poll "UV_FS_POLL"))
  ((:handle "UV_HANDLE"))
  ((:idle "UV_IDLE"))
  ((:pipe "UV_NAMED_PIPE"))
  ((:poll "UV_POLL"))
  ((:prepare "UV_PREPARE"))
  ((:process "UV_PROCESS"))
  ((:stream "UV_STREAM"))
  ((:tcp "UV_TCP"))
  ((:timer "UV_TIMER"))
  ((:tty "UV_TTY"))
  ((:udp "UV_UDP"))
  ((:signal "UV_SIGNAL")))

(cenum uv-tcp-flags
  ((:ipv6only "UV_TCP_IPV6ONLY")))

(cenum uv-udp-flags
  ((:ipv6only "UV_UDP_IPV6ONLY"))
  ((:parial "UV_UDP_PARTIAL"))
  ((:reuseaddr "UV_UDP_REUSEADDR")))

(cenum uv-fs-event
  ((:rename "UV_RENAME"))
  ((:change "UV_CHANGE")))

(cenum uv-fs-event-flags
  ((:watch-entry "UV_FS_EVENT_WATCH_ENTRY"))
  ((:event-stat "UV_FS_EVENT_STAT"))
  ((:recursive "UV_FS_EVENT_RECURSIVE")))

(cenum uv-fs-type
  ((:unknown "UV_FS_UNKNOWN"))
  ((:custom "UV_FS_CUSTOM"))
  ((:open "UV_FS_OPEN"))
  ((:close "UV_FS_CLOSE"))
  ((:read "UV_FS_READ"))
  ((:write "UV_FS_WRITE"))
  ((:sendfile "UV_FS_SENDFILE"))
  ((:stat "UV_FS_STAT"))
  ((:lstat "UV_FS_LSTAT"))
  ((:fstat "UV_FS_FSTAT"))
  ((:ftruncate "UV_FS_FTRUNCATE"))
  ((:utime "UV_FS_UTIME"))
  ((:futime "UV_FS_FUTIME"))
  ((:access "UV_FS_ACCESS"))
  ((:chmod "UV_FS_CHMOD"))
  ((:fchmod "UV_FS_FCHMOD"))
  ((:fsync "UV_FS_FSYNC"))
  ((:fdatasync "UV_FS_FDATASYNC"))
  ((:unlink "UV_FS_UNLINK"))
  ((:rmdir "UV_FS_RMDIR"))
  ((:mkdir "UV_FS_MKDIR"))
  ((:mkdtemp "UV_FS_MKDTEMP"))
  ((:rename "UV_FS_RENAME"))
  ((:scandir "UV_FS_SCANDIR"))
  ((:link "UV_FS_LINK"))
  ((:symlink "UV_FS_SYMLINK"))
  ((:readlink "UV_FS_READLINK"))
  ((:chown "UV_FS_CHOWN"))
  ((:fchow "UV_FS_FCHOWN")))

(cenum uv-poll-event
  ((:readable "UV_READABLE"))
  ((:writable "UV_WRITABLE")))

(cenum uv-process-flags
  ((:setuid "UV_PROCESS_SETUID"))
  ((:setgid "UV_PROCESS_SETGID"))
  ((:windows-verbatim-arguments "UV_PROCESS_WINDOWS_VERBATIM_ARGUMENTS"))
  ((:detached "UV_PROCESS_DETACHED"))
  ((:windows-hide "UV_PROCESS_WINDOWS_HIDE")))

(cenum uv-dirent-type-t
  ((:unknown "UV_DIRENT_UNKNOWN"))
  ((:file "UV_DIRENT_FILE"))
  ((:dir "UV_DIRENT_DIR"))
  ((:link "UV_DIRENT_LINK"))
  ((:fifo "UV_DIRENT_FIFO"))
  ((:socket "UV_DIRENT_SOCKET"))
  ((:char "UV_DIRENT_CHAR"))
  ((:block "UV_DIRENT_BLOCK")))

;; -----------------------------------------------------------------------------
;; uv helper structs
;; -----------------------------------------------------------------------------
(cstruct uv-buf-t "uv_buf_t"
  (base "base" :type :pointer)
  (len "len" :type #+windows ulong
                   #-windows size-t))

(cstruct uv-timespec-t "uv_timespec_t"
  (tv-sec "tv_sec" :type :long)
  (tv-nsec "tv_nsec" :type :long))

(cstruct uv-stat-t "uv_stat_t"
  (st-dev "st_dev" :type uint64-t)
  (st-mode "st_mode" :type uint64-t)
  (st-nlink "st_nlink" :type uint64-t)
  (st-uid "st_uid" :type uint64-t)
  (st-gid "st_gid" :type uint64-t)
  (st-rdev "st_rdev" :type uint64-t)
  (st-ino "st_ino" :type uint64-t)
  (st-size "st_size" :type uint64-t)
  (st-blksize "st_blksize" :type uint64-t)
  (st-blocks "st_blocks" :type uint64-t)
  (st-flags "st_flags" :type uint64-t)
  (st-gen "st_gen" :type uint64-t)
  (st-atim "st_atim" :type uv-timespec-t)
  (st-mtim "st_mtim" :type uv-timespec-t)
  (st-ctim "st_ctim" :type uv-timespec-t)
  (st-birthtim "st_birthtim" :type uv-timespec-t))

(cstruct uv-loop-s "struct uv_loop_s"
  (data "data" :type :pointer)
  (active-handles "active_handles" :type :unsigned-int)
  (handle-queue "handle_queue" :type :pointer)
  (active-reqs "active_reqs" :type :pointer)
  (stop-flag "stop_flag" :type :unsigned-int))

(cstruct uv-process-options-t "uv_process_options_t"
  (exit-cb "exit_cb" :type :pointer)
  (file "file" :type :string)
  (args "args" :type :pointer)
  (env "env" :type :pointer)
  (cwd "cwd" :type :string)
  (flags "flags" :type :unsigned-int)
  (stdio-count "stdio_count" :type :int)
  (stdio "stdio" :type :pointer)
  (uid "uid" :type uv-uid-t)
  (gid "gid" :type uv-gid-t))

(cstruct uv-dirent-s "struct uv_dirent_s"
  (name "name" :type :string)
  (type "type" :type uv-dirent-type-t))

;; TODO: figure out embedded struct/union syntax...
;(cstruct uv-stdio-container-t "uv_stdio_container_t"
;  ...)
;  
;(cstruct uv-cpu-info-s "struct uv_cpu_info_s"
;  ...)
;
;(cstruct uv-interface-address-s "struct uv_interface_address_s"
;  (name "name" :type :pointer)
;  (phys-addr "phys_addr" :type :char :count 6)
;  (is-internal "is_internal" :type :int)
;  (address "address" :type ...? ))

;; -----------------------------------------------------------------------------
;; reqs
;; -----------------------------------------------------------------------------
(cstruct uv-req-s "struct uv_req_s"
  (data "data" :type :pointer)
  (type "type" :type uv-req-type))

(cstruct uv-shutdown-s "struct uv_shutdown_s"
  (data "data" :type :pointer)
  (type "type" :type uv-req-type)
  (handle "handle" :type :pointer)
  (cb "cb" :type :pointer))

(cstruct uv-write-s "struct uv_write_s"
  (data "data" :type :pointer)
  (type "type" :type uv-req-type)
  (cb "cb" :type :pointer)
  (send-handle "send_handle" :type :pointer)
  (handle "handle" :type :pointer))

(cstruct uv-connect-s "struct uv_connect_s"
  (data "data" :type :pointer)
  (type "type" :type uv-req-type)
  (cb "cb" :type :pointer)
  (handle "handle" :type :pointer))

(cstruct uv-udp-send-s "struct uv_udp_send_s"
  (data "data" :type :pointer)
  (type "type" :type uv-req-type)
  (handle "handle" :type :pointer)
  (cb "cb" :type :pointer))

(cstruct uv-getaddrinfo-s "struct uv_getaddrinfo_s"
  (data "data" :type :pointer)
  (type "type" :type uv-req-type)
  (loop "loop" :type :pointer))

(cstruct uv-getnameinfo-s "struct uv_getnameinfo_s"
  (data "data" :type :pointer)
  (type "type" :type uv-req-type)
  (loop "loop" :type :pointer))

(cstruct uv-work-s "struct uv_work_s"
  (data "data" :type :pointer)
  (type "type" :type uv-req-type)
  (loop "loop" :type :pointer)
  (work-cb "work_cb" :type :pointer)
  (after-work-cb "after_work_cb" :type :pointer))

(cstruct uv-fs-s "struct uv_fs_s"
  (data "data" :type :pointer)
  (type "type" :type uv-req-type)
  (fs_type "fs_type" :type uv-fs-type)
  (loop "loop" :type :pointer)
  (cb "cb" :type :pointer)
  (result "result" :type ssize-t)
  (ptr "ptr" :type :pointer)
  (path "path" :type :string)
  (statbuf "statbuf" :type uv-stat-t))

;; -----------------------------------------------------------------------------
;; handles
;; -----------------------------------------------------------------------------
(cstruct uv-handle-s "struct uv_handle_s"
  (data "data" :type :pointer)
  (loop "loop" :type :pointer)
  (type "type" :type uv-handle-type))

(cstruct uv-stream-s "struct uv_stream_s"
  (data "data" :type :pointer)
  (loop "loop" :type :pointer)
  (type "type" :type uv-handle-type)
  (write-queue-size "write_queue_size" :type size-t)
  (alloc-cb "alloc_cb" :type :pointer)
  (read-cb "read_cb" :type :pointer))

(cstruct uv-tcp-s "struct uv_tcp_s"
  (data "data" :type :pointer)
  (loop "loop" :type :pointer)
  (type "type" :type uv-handle-type)
  (write-queue-size "write_queue_size" :type size-t)
  (alloc-cb "alloc_cb" :type :pointer)
  (read-cb "read_cb" :type :pointer))

(cstruct uv-udp-s "struct uv_udp_s"
  (data "data" :type :pointer)
  (loop "loop" :type :pointer)
  (type "type" :type uv-handle-type)
  (send-queue-size "send_queue_size" :type size-t)
  (send-queue-count "send_queue_count" :type size-t))

(cstruct uv-tty-s "struct uv_tty_s"
  (data "data" :type :pointer)
  (loop "loop" :type :pointer)
  (type "type" :type uv-handle-type)
  (write-queue-size "write_queue_size" :type size-t)
  (alloc-cb "alloc_cb" :type :pointer)
  (read-cb "read_cb" :type :pointer))

(cstruct uv-pipe-s "struct uv_pipe_s"
  (data "data" :type :pointer)
  (loop "loop" :type :pointer)
  (type "type" :type uv-handle-type)
  (write-queue-size "write_queue_size" :type size-t)
  (alloc-cb "alloc_cb" :type :pointer)
  (read-cb "read_cb" :type :pointer)
  (ipc "ipc" :type :int))

(cstruct uv-poll-s "struct uv_poll_s"
  (data "data" :type :pointer)
  (loop "loop" :type :pointer)
  (type "type" :type uv-handle-type)
  (poll-cb "poll_cb" :type :pointer))

(cstruct uv-prepare-s "struct uv_prepare_s"
  (data "data" :type :pointer)
  (loop "loop" :type :pointer)
  (type "type" :type uv-handle-type))

(cstruct uv-check-s "struct uv_check_s"
  (data "data" :type :pointer)
  (loop "loop" :type :pointer)
  (type "type" :type uv-handle-type))

(cstruct uv-idle-s "struct uv_idle_s"
  (data "data" :type :pointer)
  (loop "loop" :type :pointer)
  (type "type" :type uv-handle-type))

(cstruct uv-async-s "struct uv_async_s"
  (data "data" :type :pointer)
  (loop "loop" :type :pointer)
  (type "type" :type uv-handle-type))

(cstruct uv-timer-s "struct uv_timer_s"
  (data "data" :type :pointer)
  (loop "loop" :type :pointer)
  (type "type" :type uv-handle-type))

(cstruct uv-process-s "struct uv_process_s"
  (data "data" :type :pointer)
  (loop "loop" :type :pointer)
  (type "type" :type uv-handle-type)
  (exit-cb "exit_cb" :type :pointer)
  (pid "pid" :type :int))

(cstruct uv-fs-event-s "struct uv_fs_event_s"
  (data "data" :type :pointer)
  (loop "loop" :type :pointer)
  (type "type" :type uv-handle-type))

(cstruct uv-fs-poll-s "struct uv_fs_poll_s"
  (data "data" :type :pointer)
  (loop "loop" :type :pointer)
  (type "type" :type uv-handle-type))

(cstruct uv-signal-s "struct uv_signal_s"
  (data "data" :type :pointer)
  (loop "loop" :type :pointer)
  (type "type" :type uv-handle-type)
  (signal-cb "signal_cb" :type :pointer)
  (signum "signum" :type :int))

