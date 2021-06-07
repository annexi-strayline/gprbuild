/****************************************************************************
 *                                                                          *
 *                             GPR TECHNOLOGY                               *
 *                                                                          *
 *          Copyright (C) 1992-2020, Free Software Foundation, Inc.         *
 *                                                                          *
 * This library is free software;  you can redistribute it and/or modify it *
 * under terms of the  GNU General Public License  as published by the Free *
 * Software  Foundation;  either version 3,  or (at your  option) any later *
 * version. This library is distributed in the hope that it will be useful, *
 * but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- *
 * TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            *
 *                                                                          *
 * As a special exception under Section 7 of GPL version 3, you are granted *
 * additional permissions described in the GCC Runtime Library Exception,   *
 * version 3.1, as published by the Free Software Foundation.               *
 *                                                                          *
 * You should have received a copy of the GNU General Public License and    *
 * a copy of the GCC Runtime Library Exception along with this program;     *
 * see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    *
 * <http://www.gnu.org/licenses/>.                                          *
 *                                                                          *
 ****************************************************************************/

#ifdef __cplusplus
extern "C" {
#endif

#define _FILE_OFFSET_BITS 64
/* Defines that 64 bit file system interface shall be used in stat call below
 * even if it happens in 32 bit OS */

#ifdef IN_GCC
#include "auto-host.h"
#endif

#include <string.h>

/*  link_max is a conservative system specific threshold (in bytes) of the  */
/*  argument length passed to the linker which will trigger a file being    */
/*  used instead of the command line directly.                              */

/*  shared_libgcc_default gives the system dependent link method that       */
/*  be used by default for linking libgcc (shared or static)                */

/*  default_libgcc_subdir is the subdirectory name (from the installation   */
/*  root) where we may find a shared libgcc to use by default.              */

#define SHARED 'H'
#define STATIC 'T'

#if defined (__WIN32)
int __gnat_link_max = 30000;
char __gnat_shared_libgcc_default = STATIC;
const char *__gnat_default_libgcc_subdir = "lib";

#elif defined (__hpux__)
int __gnat_link_max = 5000;
char __gnat_shared_libgcc_default = STATIC;
const char *__gnat_default_libgcc_subdir = "lib";

#elif defined (__FreeBSD__)
int __gnat_link_max = 8192;
char __gnat_shared_libgcc_default = STATIC;
const char *__gnat_default_libgcc_subdir = "lib";

#elif defined (__APPLE__)
int __gnat_link_max = 262144;
char __gnat_shared_libgcc_default = SHARED;
const char *__gnat_default_libgcc_subdir = "lib";

#elif defined (linux) || defined(__GLIBC__)
int __gnat_link_max = 8192;
char __gnat_shared_libgcc_default = STATIC;
#if defined (__x86_64)
# if defined (__LP64__)
const char *__gnat_default_libgcc_subdir = "lib64";
# else
const char *__gnat_default_libgcc_subdir = "libx32";
# endif
#else
const char *__gnat_default_libgcc_subdir = "lib";
#endif

#elif defined (_AIX)
int __gnat_link_max = 15000;
char __gnat_shared_libgcc_default = STATIC;
const char *__gnat_default_libgcc_subdir = "lib";

#elif (HAVE_GNU_LD)
int __gnat_link_max = 8192;
char __gnat_shared_libgcc_default = STATIC;
const char *__gnat_default_libgcc_subdir = "lib";

#elif defined (sun)
int __gnat_link_max = 2147483647;
char __gnat_shared_libgcc_default = STATIC;
#if defined (__sparc_v9__) || defined (__sparcv9)
const char *__gnat_default_libgcc_subdir = "lib/sparcv9";
#elif defined (__x86_64)
const char *__gnat_default_libgcc_subdir = "lib/amd64";
#else
const char *__gnat_default_libgcc_subdir = "lib";
#endif

#elif defined (__svr4__) && defined (i386)
int __gnat_link_max = 2147483647;
char __gnat_shared_libgcc_default = STATIC;
const char *__gnat_default_libgcc_subdir = "lib";

#else
int __gnat_link_max = 2147483647;
char __gnat_shared_libgcc_default = STATIC;
const char *__gnat_default_libgcc_subdir = "lib";
#endif

#if defined (_WIN32)
#include <windows.h>
#else
#include <sys/types.h>
#include <sys/stat.h>
#include <limits.h>
#include <unistd.h>
#endif

  extern long long __gpr_file_time(char* name)
  {
    long long result;

    if (name == NULL) {
      return LLONG_MIN;
    }
    /* Number of seconds between <Jan 1st 1970> and <Jan 1st 2150>. */
    static const long long ada_epoch_offset = (136 * 365 + 44 * 366) * 86400LL;
#if defined (_WIN32)

    /* Number of 100 nanoseconds between <Jan 1st 1601> and <Jan 1st 2150>. */
    static const long long w32_epoch_offset =
       (11644473600LL + ada_epoch_offset) * 1E7;

    WIN32_FILE_ATTRIBUTE_DATA fad;
    union
    {
      FILETIME ft_time;
      long long ll_time;
    } t_write;

    if (!GetFileAttributesExA(name, GetFileExInfoStandard, &fad)) {
      return LLONG_MIN;
    }

    t_write.ft_time = fad.ftLastWriteTime;

    // return (t_write.ll_time - w32_epoch_offset) * 100;
    // with check overflow below

    if (__builtin_ssubll_overflow(t_write.ll_time, w32_epoch_offset, &result)) {
      return LLONG_MIN;
    }

    if (__builtin_smulll_overflow(result, 100, &result)) {
      return LLONG_MIN;
    }

#else
    struct stat sb;
    if (stat(name, &sb) != 0) {
      return LLONG_MIN;
    }

    //  return (sb.st_mtim.tv_sec - ada_epoch_offset) * 1E9
    //  + sb.st_mtim.tv_nsec;
    // with check overflow below

#if defined(__APPLE__) || defined(__NetBSD__)
#define st_mtim st_mtimespec
#endif

    if (__builtin_ssubll_overflow(sb.st_mtim.tv_sec, ada_epoch_offset, &result)) {
      return LLONG_MIN;
    }

    if (__builtin_smulll_overflow(result, 1E9, &result)) {
      return LLONG_MIN;
    }

    if (__builtin_saddll_overflow(result, sb.st_mtim.tv_nsec, &result)) {
      return LLONG_MIN;
    }

#endif
    return result;
  }

#ifdef __cplusplus
}
#endif
