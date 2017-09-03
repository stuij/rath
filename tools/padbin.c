/* padbin.c
   pad a binary to a given size boundary

Copyright (C) 2002  Damian Yerrick

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to 
  Free Software Foundation, Inc., 59 Temple Place - Suite 330,
  Boston, MA  02111-1307, USA.
GNU licenses can be viewed online at http://www.gnu.org/copyleft/

In addition, as a special exception, Damian Yerrick gives
permission to link the code of this program with import libraries
distributed as part of the plug-in development tools published by
the vendor of any image manipulation program, and distribute
linked combinations including the two.  You must obey the GNU
General Public License in all respects for all of the code used
other than the plug-in interface.  If you modify this file, you
may extend this exception to your version of the file, but you
are not obligated to do so.  If you do not wish to do so, delete
this exception statement from your version.

Visit http://www.pineight.com/ for more information.

*/

#include <stdio.h>
#include <stdlib.h>

char zeroz[1024];

int main(int argc, char **argv)
{
  FILE *fp;
  unsigned long overage;
  unsigned long factor;

  /* clear to 0xff for faster flash writing */
  for(factor = 0; factor < 1024; factor++)
    zeroz[factor] = 0xff;

  if(argc != 3)
  {
    fputs("pads a binary file to an integer multiple of a given number of bytes\n"
          "syntax: padbin FACTOR FILE\n"
          "FACTOR can be decimal (e.g. 256), octal (e.g. 0400), or hex (e.g. 0x100)\n", stderr);
    return 1;
  }

  factor = strtoul(argv[1], NULL, 0);
  if(factor < 2)
  {
    fputs("error: FACTOR must be greater than or equal to 2\n", stderr);
    return 1;
  }

  fp = fopen(argv[2], "rb+");
  if(!fp)
  {
    fputs("could not open", stderr);
    perror(argv[2]);
    return 1;
  }

  /* find the amount the file has over the limit */
  fseek(fp, 0, SEEK_END);
  overage = ftell(fp) % factor;
  if(overage != 0)
  {
    unsigned long extension = factor - overage;

    while(extension >= 1024)
    {
      fwrite(zeroz, 1, 1024, fp);
      extension -= 1024;
    }
    if(extension >= 0)
    {
      fwrite(zeroz, 1, extension, fp);
    }
  }
  fclose(fp);
  return 0;
}
