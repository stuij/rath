
@{{BLOCK(beany_sheet)

@=======================================================================
@
@	beany_sheet, 16x16@4, 
@	Transparent color : 00,00,00
@	+ palette 16 entries, not compressed
@	+ 4 tiles Metatiled by 1x2 not compressed
@	Total size: 32 + 128 = 160
@
@	Time-stamp: 2021-02-02, 00:31:06
@	Exported by Cearn's GBA Image Transmogrifier, v0.8.15
@	( http://www.coranac.com/projects/#grit )
@
@=======================================================================

	.section .rodata
	.align	2
	.global beany_sheetTiles		@ 128 unsigned chars
	.hidden beany_sheetTiles
beany_sheetTiles:
	.word 0x22222222,0x02666662,0x06536356,0x06666666,0x00651560,0x00666660,0x00066000,0x09999990
	.word 0x9AAAAAA9,0x9AA66AA9,0x99966999,0x0AAAAAA0,0x0DDDDDD0,0x05535530,0x05535530,0x00000000
	.word 0x22222222,0x02666662,0x06536356,0x06666666,0x60651560,0x90666660,0x90066000,0x99999990
	.word 0xAAAAAA99,0x0AAAAA99,0x0AAAAAA9,0x0AAAAAA9,0x0EEDDDD6,0x05535530,0x05535530,0x00000000

	.section .rodata
	.align	2
	.global beany_sheetPal		@ 32 unsigned chars
	.hidden beany_sheetPal
beany_sheetPal:
	.hword 0x0000,0x1884,0x1CA8,0x18EC,0x1D51,0x7FFF,0x329B,0x4F11
	.hword 0x1BDF,0x2B93,0x1AED,0x3646,0x1529,0x112A,0x1CE6,0x38E7

@}}BLOCK(beany_sheet)
