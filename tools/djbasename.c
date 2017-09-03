/* Copyright (C) 2002 Damian Yerrick
   licensed under GNU Lesser General Public License, version 2.1
   or later

   primary change: remove references to unconst, replacing them
   with a simpler c-style cast
   also removed references to unistd.h because it's not available
   on mingw
*/
/* Copyright (C) 1998 DJ Delorie, see COPYING.DJ for details */
/* Copyright (C) 1997 DJ Delorie, see COPYING.DJ for details */

char *
basename (const char *fname)
{
  const char *base = fname;

  if (fname && *fname)
  {
    if (fname[1] == ':')
    {
      fname += 2;
      base = fname;
    }

    while (*fname)
    {
      if (*fname == '\\' || *fname == '/')
	base = fname + 1;
      fname++;
    }
  }

  return (char *)base;
}
