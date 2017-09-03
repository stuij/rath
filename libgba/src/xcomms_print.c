#include <stdarg.h>
#include <stdio.h>
#include <string.h>

#include "gba_sio.h"
#include "gba_video.h"
#include "xcomms_cmd.h"
#include "xcomms.h"

#define __DOUTBUFSIZE 256

unsigned char __outstr[__DOUTBUFSIZE];


//---------------------------------------------------------------------------------
void xcomms_dprintf(char *str, ...)
//---------------------------------------------------------------------------------
{
	va_list args;
	int len;
	char *string = __outstr;

	va_start(args, str);
	len=vsnprintf(__outstr,__DOUTBUFSIZE,str,args);
	va_end(args);

	u32 data = PRINT_CMD | len;
	xcomms_send(data);

	xcomms_sendblock(string,len);
}

