%module bindings

%feature("intern_function", "lispify");

%insert("lisphead") %{
(in-package :libuv)
%}

#define AF_UNSPEC   0
#define AF_UNIX 1
#define AF_INET 2
#define AF_INET6 23
#define SOCK_STREAM 1
#define IPPROTO_TCP 6

typedef unsigned long size_t;
typedef unsigned long long uint64_t;
typedef unsigned int uint32_t;
typedef unsigned short uint16_t;
typedef unsigned short WCHAR;
typedef unsigned long long uint8_t;
typedef unsigned long ULONG;

struct addrinfo {
    int     ai_flags;
    int     ai_family;
    int     ai_socktype;
    int     ai_protocol;
    size_t  ai_addrlen;
    struct sockaddr  *ai_addr;
    char   *ai_canonname;
    struct evutil_addrinfo  *ai_next;
};

struct addrinfoW {
    int ai_flags;
    int ai_family;
    int ai_socktype;
    int ai_protocol;
    size_t ai_addrlen;
    WCHAR* ai_canonname;
    struct sockaddr* ai_addr;
    struct addrinfoW* ai_next;
};

struct sockaddr_in {
    short   sin_family;
    unsigned short sin_port;

    /* NOTE: should be unsigned long, but CFFI adds +4 to the offset if this
       is a long, which really shouldn't happen. unsigned int seems to work on
       all platforms, so until otherwise noted, this is how it's going to stay */
    unsigned int sin_addr;
    /*struct  in_addr sin_addr;*/
    /*char    sin_zero[8];*/
    char sin_zero_0;
    char sin_zero_1;
    char sin_zero_2;
    char sin_zero_3;
    char sin_zero_4;
    char sin_zero_5;
    char sin_zero_6;
    char sin_zero_7;
};

struct sockaddr_in6 {
    unsigned short sin6_family;
    unsigned short sin6_port;
    unsigned int sin6_flowinfo;
    /* struct sin6_addr */
    /* unsigned char s6_addr[16] */
    unsigned char sin6_addr_0;
    unsigned char sin6_addr_1;
    unsigned char sin6_addr_2;
    unsigned char sin6_addr_3;
    unsigned char sin6_addr_4;
    unsigned char sin6_addr_5;
    unsigned char sin6_addr_6;
    unsigned char sin6_addr_7;
    unsigned char sin6_addr_8;
    unsigned char sin6_addr_9;
    unsigned char sin6_addr_10;
    unsigned char sin6_addr_11;
    unsigned char sin6_addr_12;
    unsigned char sin6_addr_13;
    unsigned char sin6_addr_14;
    unsigned char sin6_addr_15;
    unsigned int sin6_scope_id;
};

typedef struct uv_buf_t {
  ULONG len;
  void* base;
} uv_buf_t_win;

typedef struct uv_buf_t {
  void* base;
  size_t len;
} uv_buf_t;

%include "/usr/local/include/uv/tree.h"
%include "/usr/local/include/uv/uv-errno.h"
%include "/usr/local/include/uv/uv-threadpool.h"
%include "/usr/local/include/uv/uv-unix.h"
%include "/usr/local/include/uv/uv.h"

