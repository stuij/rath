#include <stdarg.h>
#include <stdio.h>
#include <tonc.h>

#include "circular_buffer.h"
#include "console.h"

// Console Data
u16 console[20][30];

// Cursor position
s32 row,col;

void write_char(s32 ch) {
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

void write_console_line(const char* line) {
  while (*line)
    write_char((u32)(*line++));
}

void write_console_line_circ(struct circ_buff* buff) {
  char out;
  while (read_circ_char(buff, &out))
    write_char((u32)out);
}

// print to console
void printc (char* format, ...) {
  char buffer[256];
  va_list args;
  va_start (args, format);
  vsnprintf (buffer, sizeof(buffer), format, args);
  va_end (args);
  write_console_line(buffer);
}
