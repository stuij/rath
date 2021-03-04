#include "tonc.h"

#include "gbfs.h"
#ifdef LINK_UART
#include "uart.h"
#endif

// Useful definitions for handling background data, from Damian Yerrick
typedef u16 NAMETABLE[32][32];
#define MAP ((NAMETABLE *)0x06000000)

// 8x8 Font
extern const u8 gba_font[];

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

// Console Data
u16 console[20][30];
// Cursor position
int row,col;

// Source Files
int filesCount;
GBFS_FILE *gbfs;
GBFS_ENTRY *gbfs_entry;
u8 *sourcePos;
u32 sourceLen;

// Utilities
int display(u32 val, u16 *p);


int main() {
	int i;
	u16 *src, *dst;

	// Set up the interrupt handlers
	irq_init(NULL);
	// Enable Vblank Interrupt to allow VblankIntrWait
	irq_add(II_VBLANK, NULL);

	// Init Background Palette
	pal_bg_mem[0] = RGB15(0, 0, 0);
	pal_bg_mem[1] = RGB15(15, 15, 0);

	// Copy Font Data
	//memcpy((u16 *)CHAR_BASE_ADR(0),gba_font,256*16*2);
	src = (u16 *)gba_font;
  dst = (u16 *)tile_mem[0];
  for(i=0; i<256*16*2; i++) *dst++ = *src++;

	// Init Background 0
	REG_BG0HOFS = 0;
	REG_BG0VOFS = 0;
  // libgba eq:  CHAR_BASE               SCREEN_BASE
	REG_BGCNT[0] = (0 << 2) | BG_8BPP | (8 << 8) | BG_SIZE0;

	// Clear the background
	for(i=0; i<32*32; i++) MAP[8][0][i] = ' ';

	// Initialize sprites (outside of screen)
	OBJ_ATTR obj_attr = {160, 240, 0, 0};
	for(i=0; i<128; i++) oam_mem[i] = obj_attr;

	// Screen Mode & Background to display & Sprites
	REG_DISPCNT = DCNT_MODE0 | DCNT_BG0 | DCNT_OBJ | DCNT_OBJ_1D;

	// Initialize Console Data
	row = col = 0;
	for(i=0; i<30*20; i++) console[0][i] = ' ';

	// Find the embedded source, if any
	gbfs_entry = NULL;
    	gbfs = find_first_gbfs_file(find_first_gbfs_file);
    	sourceLen = 0;
    	    	
    	if (gbfs != NULL) {
    		filesCount = gbfs->dir_nmemb-1;
        	gbfs_entry = (GBFS_ENTRY *)((char *)gbfs + gbfs->dir_off);        
    		sourcePos = gbfs_get_obj(gbfs,gbfs_entry->name,&sourceLen);
    	}

#ifdef LINK_UART
  init_uart(SIO_BAUD_115200);
#endif

	// Boot up PandaForth
	boot(forthInfo);
	
	// Never reached
	return 0;
}

void write_char(int ch) {
	int lastrow = row;
	int x,y,i;
	if (ch >= 32) { 
		console[row][col++] = ch;		
		if (col == 30) {
			col=0;
			row++;
			if (row == 20) row = 0;
			// Clean the next colon
			for(x=0; x<30; x++) console[row][x] = ' ';
		}
	} else if (ch == '\n') {
		col=0;
		row++;
		if (row == 20) row = 0;		
		// Clean the next colon
		for(x=0; x<30; x++) console[row][x] = ' ';
	} else if (ch == 0x08) {
		console[row][col--] = ' ';
		if (col < 0) {
			col = 29;
			row--;
			if (row < 0) row = 19;
		}
	}
	
	
	if (lastrow != row) {
		// Update the whole screen
		y = row+1;
		if (y > 19) y=0;
		for(i=0; i<20; i++) {
			for(x=0; x<30; x++) {
				MAP[8][i][x] = console[y][x];
			}
			y++;
			if (y > 19) y=0;
		}
	} else {
		// Update only the current row
		for(x=0; x<30; x++) {
			MAP[8][19][x] = console[row][x];
		}
	}
}

int EWRAM_CODE service(int serv, int param) {
	int ch;
	if (serv == 6) {
		if (param != 0xff) {
			write_char(param);
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
		while(param--) VBlankIntrWait(); 	
		return 0;
	}
	return 0;
}


#define digit(x) ((x)<10?'0'+(x):'A'+(x)-10)

int display(u32 val, u16 *p) {
	int i;
	*p++ = digit( (val & 0xf0000000)>>28 );
	*p++ = digit( (val & 0x0f000000)>>24 );
	*p++ = digit( (val & 0x00f00000)>>20 );
	*p++ = digit( (val & 0x000f0000)>>16 );
	*p++ = digit( (val & 0x0000f000)>>12 );
	*p++ = digit( (val & 0x00000f00)>>8 );
	*p++ = digit( (val & 0x000000f0)>>4 );
	*p++ = digit( (val & 0x0000000f)>>0 );
	// Pause for two seconds
	for(i=0; i<30;i++) VBlankIntrWait(); 
	return 0;
}
