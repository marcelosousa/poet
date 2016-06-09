/*
 * stdarg.h
 *
 * This is just a wrapper for the gcc one, but defines va_copy()
 * even if gcc doesn't.
 */

/* Note: the _STDARG_H macro belongs to the gcc header... */
// #include_next <stdarg.h> -- let's ignore it for the moment -- Cesar

/* dummy implementation that type checks, poet doesn't support variadic
 * arguments anyway
 * -- Cesar
 */

// BEGIN of dummy implementationn
typedef __builtin_va_list va_list;
//typedef int va_list;

#define va_start(v,l)  __builtin_va_start(v,l)
#define va_end(v)	     __builtin_va_end(v)
#define va_arg(v,l)	  __builtin_va_arg(v,l)
#define va_copy(d,s)	  __builtin_va_copy(d,s)
// END of dummy implementation

/* Older gcc considers this an extension, so it's double underbar only */
#ifndef va_copy
#define va_copy(d,s) __va_copy(d,s)
#endif






