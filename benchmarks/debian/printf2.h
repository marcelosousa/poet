
#ifndef _PRINTF2_
#define _PRINTF2_

#include <stdio.h>
#include <stdint.h>
#include <string.h>

#ifdef __KLIBC__

#include <klibc/poet.h>
static inline void fmtint (char *buff, uintmax_t v, int base, int sgn)
{
   format_int2 (buff, 32, v, base, sgn);
}

#else
static inline void fmtint (char *buff, uintmax_t v, int base, int sgn)
{
   int l;

   switch (base)
   {
   case 16:
      l = sprintf (buff, "%lx", v);
      break;
   default :
      if (sgn)
         l = sprintf (buff, "%ld", v);
      else
         l = sprintf (buff, "%lu", v);
      break;
   }
   buff[l] = 0;
}
#define __libc_init_poet()
#endif // __KLIBC__


static inline int fputd (int d, FILE *f)
{
   char buff[32];
   fmtint (buff, d, 10, 1);
   return fputs (buff, f);
}

static inline int fputu (unsigned u, FILE *f)
{
   char buff[32];
   fmtint (buff, u, 10, 0);
   return fputs (buff, f);
}

static inline int fputld (long int d, FILE *f)
{
   char buff[32];
   fmtint (buff, d, 10, 1);
   return fputs (buff, f);
}

static inline int fputlu (long unsigned u, FILE *f)
{
   char buff[32];
   fmtint (buff, u, 10, 0);
   return fputs (buff, f);
}

static inline int fputx (unsigned d, FILE *f)
{
   char buff[32];
   fmtint (buff, d, 16, 0);
   return fputs (buff, f);
}

static inline int fputlx (long unsigned d, FILE *f)
{
   char buff[32];
   fmtint (buff, d, 16, 0);
   return fputs (buff, f);
}

#define putss(s) fputs (s, stdout)
#define putd(d)  fputd  (d, stdout)
#define putu(u)  fputu  (u, stdout)
#define putld(d) fputld (d, stdout)
#define putlu(u) fputlu (u, stdout)
#define putx(u)  fputx  (u, stdout)
#define putlx(u) fputlx (u, stdout)

// xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
#if 1
#define printf2(fmt,args...)             putss (fmt)
#define fprintf2(f,fmt,args...)          fputs (f,fmt)
#define sprintf2(buff,fmt,args...)       __sprintf2 (buff,fmt)
#define snprintf2(buff,size,fmt,args...) __snprintf2 (buff, size, fmt)

int __sprintf2 (char *buff, const char *fmt) { strcpy (buff,fmt); return strlen(fmt); }
int __snprintf2 (char *buff, size_t s, const char *fmt) { strncpy (buff,fmt,s); return strnlen(fmt,s); }

#else
#define printf2(fmt,args...)             printf (fmt, ##args)
#define fprintf2(f,fmt,args...)          fprintf (f, fmt, ##args)
#define sprintf2(buff,fmt,args...)       sprintf (buff, fmt, ##args)
#define snprintf2(buff,size,fmt,args...) snprintf (buff, size, fmt, ##args)
#endif
// xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx

#endif // _PRINTF2_

