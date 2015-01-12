(in-package :libuv-grovel)

(cc-flags #+windows "-Ic:/include/"
          #+windows "-Ic:/include/uv/")

(include "uv.h")

(ctype ssize-t "ssize_t")
(ctype size-t "size_t")
(ctype uint64-t "uint64_t")

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

(cenum uv-fs-type
  ((:uv-fs-unknown "UV_FS_UNKNOWN"))
  ((:uv-fs-custom "UV_FS_CUSTOM"))
  ((:uv-fs-open "UV_FS_OPEN"))
  ((:uv-fs-close "UV_FS_CLOSE"))
  ((:uv-fs-read "UV_FS_READ"))
  ((:uv-fs-write "UV_FS_WRITE"))
  ((:uv-fs-sendfile "UV_FS_SENDFILE"))
  ((:uv-fs-stat "UV_FS_STAT"))
  ((:uv-fs-lstat "UV_FS_LSTAT"))
  ((:uv-fs-fstat "UV_FS_FSTAT"))
  ((:uv-fs-ftruncate "UV_FS_FTRUNCATE"))
  ((:uv-fs-utime "UV_FS_UTIME"))
  ((:uv-fs-futime "UV_FS_FUTIME"))
  ((:uv-fs-access "UV_FS_ACCESS"))
  ((:uv-fs-chmod "UV_FS_CHMOD"))
  ((:uv-fs-fchmod "UV_FS_FCHMOD"))
  ((:uv-fs-fsync "UV_FS_FSYNC"))
  ((:uv-fs-fdatasync "UV_FS_FDATASYNC"))
  ((:uv-fs-unlink "UV_FS_UNLINK"))
  ((:uv-fs-rmdir "UV_FS_RMDIR"))
  ((:uv-fs-mkdir "UV_FS_MKDIR"))
  ((:uv-fs-mkdtemp "UV_FS_MKDTEMP"))
  ((:uv-fs-rename "UV_FS_RENAME"))
  ((:uv-fs-scandir "UV_FS_SCANDIR"))
  ((:uv-fs-link "UV_FS_LINK"))
  ((:uv-fs-symlink "UV_FS_SYMLINK"))
  ((:uv-fs-readlink "UV_FS_READLINK"))
  ((:uv-fs-chown "UV_FS_CHOWN"))
  ((:uv-fs-fchow "UV_FS_FCHOWN")))

;#define UV_REQ_FIELDS                                                         \
;  void* data;                                                                 \
;  uv_req_type type;                                                           \

(cstruct uv-req-t "struct uv_req_s"
  (data "data" :type :pointer)
  (type "type" :type uv-req-type))

(cstruct uv-shutdown-t "struct uv_shutdown_s"
  (data "data" :type :pointer)
  (type "type" :type uv-req-type)
  (handle "handle" :type :pointer)
  (cb "cb" :type :pointer))

(cstruct uv-write-t "struct uv_write_s"
  (data "data" :type :pointer)
  (type "type" :type uv-req-type)
  (cb "cb" :type :pointer)
  (send-handle "send_handle" :type :pointer)
  (handle "handle" :type :pointer))

(cstruct uv-connect-t "struct uv_connect_s"
  (data "data" :type :pointer)
  (type "type" :type uv-req-type)
  (cb "cb" :type :pointer)
  (handle "handle" :type :pointer))

(cstruct uv-udp-send-t "struct uv_udp_send_s"
  (data "data" :type :pointer)
  (type "type" :type uv-req-type)
  (handle "handle" :type :pointer)
  (cb "cb" :type :pointer))

(cstruct uv-getaddrinfo-t "struct uv_getaddrinfo_s"
  (data "data" :type :pointer)
  (type "type" :type uv-req-type)
  (loop "loop" :type :pointer))

(cstruct uv-getnameinfo-t "struct uv_getnameinfo_s"
  (data "data" :type :pointer)
  (type "type" :type uv-req-type)
  (loop "loop" :type :pointer))

(cstruct uv-work-t "struct uv_work_s"
  (data "data" :type :pointer)
  (type "type" :type uv-req-type)
  (loop "loop" :type :pointer)
  (work-cb "work_cb" :type :pointer)
  (after-work-cb "after_work_cb" :type :pointer))

(cstruct uv-fs-t "struct uv_fs_s"
  (data "data" :type :pointer)
  (type "type" :type uv-req-type)
  (fs_type "fs_type" :type uv-fs-type)
  (loop "loop" :type :pointer)
  (cb "cb" :type :pointer)
  (result "result" :type ssize-t)
  (ptr "ptr" :type :pointer)
  (path "path" :type :pointer)
  (statbuf "statbuf" :type uv-stat-t))

;#define UV_HANDLE_FIELDS                                                      \
;  void* data;                                                                 \
;  uv_loop_t* loop;                                                            \
;  uv_handle_type type;                                                        \

; #define UV_STREAM_FIELDS                                                      \
;   size_t write_queue_size;                                                    \
;   uv_alloc_cb alloc_cb;                                                       \
;   uv_read_cb read_cb;                                                         \

(cstruct uv-handle-t "struct uv_handle_s"
  (data "data" :type :pointer)
  (loop "loop" :type :pointer)
  (type "type" :type uv-handle-type))

(cstruct uv-stream-t "struct uv_stream_s"
  (data "data" :type :pointer)
  (loop "loop" :type :pointer)
  (type "type" :type uv-handle-type)
  (write-queue-size "write_queue_size" :type size-t)
  (alloc-cb "alloc_cb" :type :pointer)
  (read-cb "read_cb" :type :pointer))

(cstruct uv-tcp-t "struct uv_tcp_s"
  (data "data" :type :pointer)
  (loop "loop" :type :pointer)
  (type "type" :type uv-handle-type)
  (write-queue-size "write_queue_size" :type size-t)
  (alloc-cb "alloc_cb" :type :pointer)
  (read-cb "read_cb" :type :pointer))

(cstruct uv-udp-t "struct uv_udp_s"
  (data "data" :type :pointer)
  (loop "loop" :type :pointer)
  (type "type" :type uv-handle-type)
  (send-queue-size "send_queue_size" :type size-t)
  (send-queue-count "send_queue_count" :type size-t))

(cstruct uv-tty-t "struct uv_tty_s"
  (data "data" :type :pointer)
  (loop "loop" :type :pointer)
  (type "type" :type uv-handle-type)
  (write-queue-size "write_queue_size" :type size-t)
  (alloc-cb "alloc_cb" :type :pointer)
  (read-cb "read_cb" :type :pointer))

(cstruct uv-pipe-t "struct uv_pipe_s"
  (data "data" :type :pointer)
  (loop "loop" :type :pointer)
  (type "type" :type uv-handle-type)
  (write-queue-size "write_queue_size" :type size-t)
  (alloc-cb "alloc_cb" :type :pointer)
  (read-cb "read_cb" :type :pointer)
  (ipc "ipc" :type :int))

(cstruct uv-poll-t "struct uv_poll_s"
  (data "data" :type :pointer)
  (loop "loop" :type :pointer)
  (type "type" :type uv-handle-type)
  (poll-cb "poll_cb" :type :pointer))

(cstruct uv-prepare-t "struct uv_prepare_s"
  (data "data" :type :pointer)
  (loop "loop" :type :pointer)
  (type "type" :type uv-handle-type))

(cstruct uv-check-t "struct uv_check_s"
  (data "data" :type :pointer)
  (loop "loop" :type :pointer)
  (type "type" :type uv-handle-type))

(cstruct uv-idle-t "struct uv_idle_s"
  (data "data" :type :pointer)
  (loop "loop" :type :pointer)
  (type "type" :type uv-handle-type))

(cstruct uv-async-t "struct uv_async_s"
  (data "data" :type :pointer)
  (loop "loop" :type :pointer)
  (type "type" :type uv-handle-type))

(cstruct uv-timer-t "struct uv_timer_s"
  (data "data" :type :pointer)
  (loop "loop" :type :pointer)
  (type "type" :type uv-handle-type))

(cstruct uv-process-t "struct uv_process_s"
  (data "data" :type :pointer)
  (loop "loop" :type :pointer)
  (type "type" :type uv-handle-type)
  (exit-cb "exit_cb" :type :pointer)
  (pid "pid" :type :int))

(cstruct uv-fs-event-t "struct uv_fs_event_s"
  (data "data" :type :pointer)
  (loop "loop" :type :pointer)
  (type "type" :type uv-handle-type))

(cstruct uv-fs-poll-t "struct uv_fs_poll_s"
  (data "data" :type :pointer)
  (loop "loop" :type :pointer)
  (type "type" :type uv-handle-type))

(cstruct uv-signal-t "struct uv_signal_s"
  (data "data" :type :pointer)
  (loop "loop" :type :pointer)
  (type "type" :type uv-handle-type)
  (signal-cb "signal_cb" :type :pointer)
  (signum "signum" :type :int))

