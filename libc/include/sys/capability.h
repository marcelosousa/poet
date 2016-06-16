#ifndef _SYS_CAPABILITY_H
#define _SYS_CAPABILITY_H

#include <klibc/extern.h>
#include <linux/capability.h>

// can we work it out with this definition?
typedef void *cap_t;
typedef int cap_value_t;

/*
 * Set identifiers
 */
typedef enum {
    CAP_EFFECTIVE=0,                        /* Specifies the effective flag */
    CAP_PERMITTED=1,                        /* Specifies the permitted flag */
    CAP_INHERITABLE=2                     /* Specifies the inheritable flag */
} cap_flag_t;

/*
 * These are the states available to each capability
 */
typedef enum {
    CAP_CLEAR=0,                            /* The flag is cleared/disabled */
    CAP_SET=1                                    /* The flag is set/enabled */
} cap_flag_value_t;


__extern cap_t   cap_dup(cap_t);
__extern int     cap_free(void *);
__extern cap_t   cap_init(void);
__extern int     cap_get_flag(cap_t, cap_value_t, cap_flag_t, cap_flag_value_t *);
__extern int     cap_set_flag(cap_t, cap_flag_t, int, const cap_value_t *, cap_flag_value_t);
__extern int     cap_clear(cap_t);
__extern int     cap_clear_flag(cap_t, cap_flag_t);
__extern cap_t   cap_get_fd(int);
__extern cap_t   cap_get_file(const char *);
__extern int     cap_set_fd(int, cap_t);
__extern int     cap_set_file(const char *, cap_t);
__extern cap_t   cap_get_proc(void);
__extern cap_t   cap_get_pid(pid_t);
__extern int     cap_set_proc(cap_t);
__extern int     cap_get_bound(cap_value_t);
__extern int     cap_drop_bound(cap_value_t);

#define CAP_IS_SUPPORTED(cap)  (cap_get_bound(cap) >= 0)

/* libcap/cap_extint.c */
__extern ssize_t cap_size(cap_t);
__extern ssize_t cap_copy_ext(void *, cap_t, ssize_t);
__extern cap_t   cap_copy_int(const void *);

/* libcap/cap_text.c */
__extern cap_t   cap_from_text(const char *);
__extern char *  cap_to_text(cap_t, ssize_t *);
__extern int     cap_from_name(const char *, cap_value_t *);
__extern char *  cap_to_name(cap_value_t);

#define CAP_DIFFERS(result, flag)  (((result) & (1 << (flag))) != 0)
__extern int     cap_compare(cap_t, cap_t);

/* system calls - look to libc for function to system call mapping */
__extern int capset(cap_user_header_t header, cap_user_data_t data);
__extern int capget(cap_user_header_t header, const cap_user_data_t data);

/* deprecated - use cap_get_pid() */
__extern int capgetp(pid_t pid, cap_t cap_d);

/* not valid with filesystem capability support - use cap_set_proc() */
__extern int capsetp(pid_t pid, cap_t cap_d);


#endif				/* _SYS_CAPABILITY_H */
