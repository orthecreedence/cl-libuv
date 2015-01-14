%module bindings

%feature("intern_function", "lispify");

%insert("lisphead") %{
(in-package :libuv)
%}

// our c types (grovel gives us mappings for these)
%typemap(cin) ssize_t "ssize-t";
%typemap(cin) size_t "size-t";
%typemap(cin) uint64_t "uint64-t";
%typemap(cin) uint32_t "uint32-t";
%typemap(cin) uint16_t "uint16-t";
%typemap(cin) uint8_t "uint8-t";
%typemap(cin) WCHAR "wchar";
%typemap(cin) ULONG "ulong";
%typemap(cin) uv_uid_t "uv-uid-t";
%typemap(cin) uv_gid_t "uv-gid-t";

// setup types for the enums
%typemap(cin) uv_errno_t "uv-errno-t";
%typemap(cin) uv_req_type "uv-req-type";
%typemap(cin) uv_handle_type "uv-handle-type";
%typemap(cin) uv_tcp_flags "uv-tcp-flags";
%typemap(cin) uv_udp_flags "uv-udp-flags";
%typemap(cin) uv_fs_event "uv-fs-event";
%typemap(cin) uv_fs_event_flags "uv-fs-event-flags";
%typemap(cin) uv_fs_type "uv-fs-type";
%typemap(cin) uv_poll_event "uv-poll-event";
%typemap(cin) uv_process_flags "uv-process-flags";

// ignore our enums (grovel handles these)
%ignore "uv_errno_t";
%ignore "uv_req_type";
%ignore "uv_handle_type";
%ignore "uv_tcp_flags";
%ignore "uv_udp_flags";
%ignore "uv_fs_event";
%ignore "uv_fs_event_flags";
%ignore "uv_fs_type";
%ignore "uv_poll_event";
%ignore "uv_process_flags";

// ignore our structs (grovel handles these)
%ignore "uv_buf_t";
%ignore "uv_timespec_t";
%ignore "uv_stat_t";
%ignore "uv_loop_s";
%ignore "uv_process_options_t";
%ignore "uv_dirent_s";
%ignore "uv_stdio_container_t";
%ignore "uv_cpu_info_s";
%ignore "uv_interface_address_s";

%ignore "uv_req_s";
%ignore "uv_shutdown_s";
%ignore "uv_write_s";
%ignore "uv_connect_s";
%ignore "uv_udp_send_s";
%ignore "uv_getaddrinfo_s";
%ignore "uv_getnameinfo_s";
%ignore "uv_work_s";
%ignore "uv_fs_s";

%ignore "uv_handle_s";
%ignore "uv_stream_s";
%ignore "uv_tcp_s";
%ignore "uv_udp_s";
%ignore "uv_tty_s";
%ignore "uv_pipe_s";
%ignore "uv_poll_s";
%ignore "uv_prepare_s";
%ignore "uv_check_s";
%ignore "uv_idle_s";
%ignore "uv_async_s";
%ignore "uv_timer_s";
%ignore "uv_process_s";
%ignore "uv_fs_event_s";
%ignore "uv_fs_poll_s";
%ignore "uv_signal_s";

// function is worthless
%ignore "uv_buf_init";

%include "/usr/local/include/uv.h"

