#include "tonc.h"

#include "AAS.h"
#include "AAS_Data.h"
#include "gbfs.h"

#ifdef LINK_UART
#include "circular_buffer.h"
#include "uart.h"
#endif

/* // 8x8 Font */
/* extern const u8 gba_font[]; */

// Space reserved for PandaForth
u8 user_area[256];
// This stack is used by Forth and C, so it needs to be large enough
// I could have made these two stacks distinct...
u8 ps_area[1024];
u8 rs_area[256];
u8 holdpad_area[40+88];

// Memory Map Information needed by PandaForth
u8 *forthInfo[] = { ps_area+128, rs_area+128, user_area, holdpad_area+40 };

// PandaForth entry point function
extern void boot(u8 *forthInfo[]);

// Source Files
int filesCount;
GBFS_FILE *gbfs;
GBFS_ENTRY *gbfs_entry;
u8 *sourcePos;
u32 sourceLen;

int main() {
  // Set up the interrupt handlers
  irq_init(NULL);
  // Enable Vblank Interrupt to allow VblankIntrWait
  irq_add(II_VBLANK, NULL);
  // the timer bits for the AAS engine
  // irq_add(II_TIMER1, AAS_FastTimer1InterruptHandler);

#ifdef LINK_UART
  init_circ_buff(&g_uart_rcv_buffer, g_rcv_buffer, UART_RCV_BUFFER_SIZE);
  init_uart(SIO_BAUD_115200);
  // Set uart interrupt handler
  irq_add(II_SERIAL, handle_uart_gbaser);
#endif

  // Initialise AAS
  AAS_SetConfig(
      AAS_CONFIG_MIX_32KHZ,
      AAS_CONFIG_CHANS_8,
      AAS_CONFIG_SPATIAL_STEREO,
      AAS_CONFIG_DYNAMIC_OFF );

  // Initialize sprites (outside of screen)
  OBJ_ATTR obj_attr = {160, 240, 0, 0};
  for(int i=0; i<128; i++) oam_mem[i] = obj_attr;
  
  // Find the embedded source, if any
  gbfs_entry = NULL;
  gbfs = find_first_gbfs_file(find_first_gbfs_file);
  sourceLen = 0;

  if (gbfs != NULL) {
    filesCount = gbfs->dir_nmemb-1;
    gbfs_entry = (GBFS_ENTRY *)((char *)gbfs + gbfs->dir_off);
    sourcePos = gbfs_get_obj(gbfs,gbfs_entry->name,&sourceLen);
  }

	// Boot up PandaForth
	boot(forthInfo);

	// Never reached
	return 0;
}

// C <-> Forth interface
int EWRAM_CODE service(int serv, int param) {
  int ch;
  if (serv == 6) {
    if (param != 0xff) {
      /* write_char(param); */
#ifndef LINK_NONE
      dputchar(param);
#endif
    } else {
      if (sourceLen > 0) {
        sourceLen--;
        ch = *sourcePos++;
        if ((sourceLen == 0) && (filesCount > 0)) {
          filesCount--;
          gbfs_entry++;
          sourcePos = gbfs_get_obj(gbfs,gbfs_entry->name,&sourceLen);
          if (sourcePos == NULL) sourceLen=0;
        }
        if (ch == '\t') return ' ';
        /*if (ch == '\n') for(i=0; i<30;i++) VBlankIntrWait();*/
        if (ch == '\r') return 0;
        return ch;
      } else {
#ifdef LINK_UART
        if(!circ_bytes_available(&g_uart_rcv_buffer)) {
          dputchar(0x1e);
        }
        ch = rcv_char();
#endif
#ifdef LINK_NONE
        while(1) VBlankIntrWait();
        return 0;
#else
        if (ch == '\r') return 0;
        return ch;
#endif
      }
    }
  } else if (serv == 1) {
    AAS_DoWork();
    while(param--) VBlankIntrWait();
    return 0;
  } else if (serv == 2) {
    irq_add(II_TIMER1, AAS_FastTimer1InterruptHandler);
    AAS_MOD_Play(AAS_DATA_MOD_bla);
    return 0;
  } else if ( serv == 3) {
    AAS_MOD_Stop();
    int countdown = 10;
    while(countdown--) {
      AAS_DoWork();
      VBlankIntrWait();
    }
  } else if ( serv == 4) {
    AAS_DoWork();
  }
  return 0;
}
