#ifndef CONSOLE_H
#define CONSOLE_H

#include <circular_buffer.h>
#include <tonc.h>

// Console Data
extern u16 console[20][30];

// Cursor position
extern s32 row,col;


// Useful definitions for handling background data, from Damian Yerrick
typedef u16 NAMETABLE[32][32];
#define MAP ((NAMETABLE *)0x06000000)

void write_char(s32 ch);
void write_console_line(const char*);
void write_console_line_circ(struct circ_buff* buff);
void printc (char* format, ...);

#endif // CONSOLE_H
