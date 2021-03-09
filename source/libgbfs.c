//Torlus - dynamic definition of GBFS_SEARCH_LIMIT 

/* libgbfs.c
   access object in a GBFS file

Copyright 2002 Damian Yerrick

Permission is hereby granted, free of charge, to any person obtaining
a copy of this software and associated documentation files (the
"Software"), to deal in the Software without restriction, including
without limitation the rights to use, copy, modify, merge, publish,
distribute, sublicense, and/or sell copies of the Software, and to
permit persons to whom the Software is furnished to do so, subject to
the following conditions:

The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN
AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF
OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
IN THE SOFTWARE.

*/


/* This code assumes a LITTLE ENDIAN target.  It'll need a boatload
   of itohs and itohl calls if converted to run on Sega Genesis.  It
   also assumes that the target uses 16-bit short and 32-bit longs.
*/

typedef unsigned short u16;
typedef unsigned long u32;

#include <stdlib.h>
#include <string.h>
#include "gbfs.h"

/* change this to the end of your ROM, or to 0x02040000 for multiboot */

//Torlus
//#ifdef TARGET_PF_mb
//#define GBFS_SEARCH_LIMIT ((const u32 *)0x02040000)
//#else
#define GBFS_SEARCH_LIMIT ((const u32 *)0x0a000000)
//#endif


/* a power of two, less than or equal to the argument passed to
   padbin */
#define GBFS_ALIGNMENT  256

GBFS_FILE *find_first_gbfs_file(const void *start)
{
  /* align the pointer */
  const u32 *here = (const u32 *)
                      ((unsigned long)start & (-GBFS_ALIGNMENT));  
  
  const char rest_of_magic[] = "ightGBFS\r\n\032\n";

  /* while we haven't yet reached the end of the ROM space */
  while(here < GBFS_SEARCH_LIMIT)
  {
    /* We have to keep the magic code in two pieces; otherwise,
       this function will find itself and think it's a GBFS file.
       This obviously won't work if your compiler stores this
       numeric literal just before the literal string, but Devkit
       Advance seems to keep numeric constant pools separate enough
       from string pools for this to work.
    */
    if(*here == 0x456e6950)  /* ASCII code for "PinE" */
    {
      /* we're already after here;
         if the rest of the magic matches, then we're through */
      if(!memcmp(here + 1, rest_of_magic, 12))
        return (GBFS_FILE *)here;
    }
    here += GBFS_ALIGNMENT / sizeof(*here);
  }
  return 0;
}


void *skip_gbfs_file(const GBFS_FILE *file)
{
  return ((char *)file + file->total_len);
}


static int namecmp(const void *a, const void *b)
{
  return memcmp(a, b, 24);
}


void *gbfs_get_obj(const GBFS_FILE *file,
                         const char *name,
                         u32 *len)
{
  char key[24] = {0};

  GBFS_ENTRY *dirbase = (GBFS_ENTRY *)((char *)file + file->dir_off);
  size_t n_entries = file->dir_nmemb;
  GBFS_ENTRY *here;

  strncpy(key, name, 24);

  here = bsearch(key, dirbase,
                 n_entries, sizeof(GBFS_ENTRY),
                 namecmp);
  if(!here)
    return NULL;

  if(len)
    *len = here->len;
  return (char *)file + here->data_offset;
}


void *gbfs_copy_obj(void *dst,
                    const GBFS_FILE *file,
                    const char *name)
{
  u32 len;
  const void *src = gbfs_get_obj(file, name, &len);

  if(!src)
    return NULL;

  memcpy(dst, src, len);
  return dst;
}
