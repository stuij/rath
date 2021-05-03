hex

( mem map )

02000000 constant mem-ewram
03000000 constant mem-iwram
04000000 constant mem-io
05000000 constant mem-pal
06000000 constant mem-vram
07000000 constant mem-oam
08000000 constant mem-rom
0e000000 constant mem-sram

05000000 constant mem-pal-bg
05000200 constant mem-pal-obj
06000000 constant mem-vram-bg
06010000 constant mem-vram-obj

4000 constant cbb-size ( charblock size )
0800 constant sbb-size ( screenblock size )


( registers )

( display )

( reg-dispcnt )
04000000 constant reg-dispcnt  ( display control )

   0 constant dcnt-mode0  ( mode 0; bg 0-4: reg )
0001 constant dcnt-mode1  ( mode 1; bg 0-1: reg; bg 2: affine )
0002 constant dcnt-mode2  ( mode 2; bg 2-3: affine )
0003 constant dcnt-mode3  ( mode 3; bg2: 240x160@16 bitmap )
0004 constant dcnt-mode4  ( mode 4; bg2: 240x160@8 bitmap )
0005 constant dcnt-mode5  ( mode 5; bg2: 160x128@16 bitmap )
0008 constant dcnt-gb     ( gbc indicator )
0010 constant dcnt-page   ( page indicator )
0020 constant dcnt-o      ( allow oam updates in hblank )
   0 constant dcnt-obj-2d ( obj-vram as matrix )
0040 constant dcnt-obj-1d ( obj-vram as array )
0080 constant dcnt-blank  ( force screen blank )
0100 constant dcnt-bg0    ( enable bg 0 )
0200 constant dcnt-bg1    ( enable bg 1 )
0400 constant dcnt-bg2    ( enable bg 2 )
0800 constant dcnt-bg3    ( enable bg 3 )
1000 constant dcnt-obj    ( enable objects )
2000 constant dcnt-win0   ( enable window 0 )
4000 constant dcnt-win1   ( enable window 1 )
8000 constant dcnt-winobj ( enable object window )

0007 constant dcnt-mode-mask
   0 constant dcnt-mode-shift

1f00 constant dcnt-layer-mask
   8 constant dcnt-layer-shift

e000 constant dcnt-win-mask
  13 constant dcnt-win-shift

( reg-dipstat )
04000004 constant reg-dispstat ( display status )

0001 constant dstat-in-vbl  ( now in vblank )
0002 constant dstat-in-hbl  ( now in hblank )
0004 constant dstat-in-vct  ( now in set vcount )
0008 constant dstat-vbl-irq ( enable vblank irq )
0010 constant dstat-hbl-irq ( enable hblank irq )
0020 constant dstat-vct-irq ( enable vcount irq )

ff00 constant dstat-vct-mask
   8 constant dstat-vct-shift

( reg-vcount )
04000006 constant reg-vcount   ( scanline count )


( backgrounds )

( bg control registers)
04000008 constant reg-bg0cnt ( bg0 control )
0400000a constant reg-bg1cnt ( bg1 control )
0400000c constant reg-bg2cnt ( bg2 control )
0400000e constant reg-bg3cnt ( bg3 control )

( reg-bgXcnt )
0040 constant bg-mosaic      ( enable mosaic )
   0 constant bg-4bpp        ( 4bpp bg, no effect on affine bg)
0080 constant bg-8bpp        ( 8bpp bg, no effect on affine bg)
2000 constant bg-wrap        ( wrap around edges of affine bgs)
   0 constant bg-size0
4000 constant bg-size1
8000 constant bg-size2
c000 constant bg-size3
   0 constant bg-reg-32x32   ( reg bg, 32x32, 256x256 px )
4000 constant bg-reg-64x32   ( reg bg, 64x32, 512x256 px )
8000 constant bg-reg-32x64   ( reg bg, 32x64, 256x512 px )
c000 constant bg-reg-64x64   ( reg bg, 64x64, 512x512 px )
   0 constant bg-aff-16x16   ( affine bg, 16x16, 128x128 px )
4000 constant bg-aff-32x32   ( affine bg, 32x32, 256x256 px )
8000 constant bg-aff-64x64   ( affine bg, 64x64, 512x512 px )
c000 constant bg-aff-128x128 ( affine bg, 128x128, 1024x1024 px )

0003 constant bg-prio-mask
   0 constant bg-prio-shift

000c constant bg-cbb-mask
   2 constant bg-cbb-shift

1f00 constant bg-sbb-mask
   8 constant bg-sbb-shift

c000 constant bg-size-mask
  14 constant bg-size-shift

( bg scroll registers, write only )
04000010 constant reg-bg0hofs ( bg0 horizontal scroll )
04000012 constant reg-bg0vofs ( bg0 vertical scroll )
04000014 constant reg-bg1hofs ( bg1 horizontal scroll )
04000016 constant reg-bg1vofs ( bg1 vertical scroll )
04000018 constant reg-bg2hofs ( bg2 horizontal scroll )
0400001a constant reg-bg2vofs ( bg2 vertical scroll )
0400001c constant reg-bg3hofs ( bg3 horizontal scroll )
0400001e constant reg-bg3vofs ( bg3 vertical scroll )

( affine background parameters, write only! )
04000020 constant reg-bg2pa ( bg2 matrix.pa )
04000022 constant reg-bg2pb ( bg2 matrix.pb )
04000024 constant reg-bg2pc ( bg2 matrix.pc )
04000026 constant reg-bg2pd ( bg2 matrix.pd )
04000028 constant reg-bg2x  ( bg2 x scroll )
0400002c constant reg-bg2y  ( bg2 y scroll )
04000030 constant reg-bg3pa ( bg3 matrix.pa )
04000032 constant reg-bg3pb ( bg3 matrix.pb )
04000034 constant reg-bg3pc ( bg3 matrix.pc )
04000036 constant reg-bg3pd ( bg3 matrix.pd )
04000038 constant reg-bg3x  ( bg3 x scroll )
0400003c constant reg-bg3y  ( bg3 y scroll )


( effects )

( windowing registers )
04000040 constant reg-win0h  ( win0 right, left, 0xllrr )
04000042 constant reg-win1h  ( win1 right, left, 0xllrr )
04000044 constant reg-win0v  ( win0 bottom, top, 0xttbb )
04000046 constant reg-win1v  ( win1 bottom, top, 0xttbb )
04000048 constant reg-winin  ( win0, win1 control )
0400004a constant reg-winout ( winout, winobj control )

( alternate windowing register names )
04000040 constant reg-win0r ( win 0 right )
04000041 constant reg-win0l ( win 0 left )
04000042 constant reg-win1r ( win 1 right )
04000043 constant reg-win1l ( win 1 left )

04000044 constant reg-win0b ( win 0 bottom )
04000045 constant reg-win0t ( win 0 top )
04000046 constant reg-win1b ( win 1 bottom )
04000047 constant reg-win1t ( win 1 top )

04000048 constant reg-win0cnt   ( window 0 control )
04000049 constant reg-win1cnt   ( window 1 control )
0400004a constant reg-winoutcnt ( out window control )
0400004b constant reg-winobjcnt ( obj window control )

( window bit control )
0001 constant win-bg0 ( windowed bg 0 )
0002 constant win-bg1 ( windowed bg 1 )
0004 constant win-bg2 ( windowed bg 2 )
0008 constant win-bg3 ( windowed bg 3 )
0010 constant win-obj ( windowed objects )
001f constant win-all ( all layers in window )
0020 constant win-bld ( windowed blending )

003f constant win-layer-mask
   0 constant win-layer-shift

( mosaic )
0400004c constant reg-mosaic ( mosaic control )

000f constant mos-bh-mask
   0 constant mos-bh-shift

00f0 constant mos-bv-mask
   4 constant mos-bv-shift

0f00 constant mos-oh-mask
   8 constant mos-oh-shift

f000 constant mos-ov-mask
  12 constant mos-ov-shift

( blend control )
04000050 constant reg-bldcnt ( blend control )

0001 constant bld-bg0      ( blend bg 0 )
0002 constant bld-bg1      ( blend bg 1 )
0004 constant bld-bg2      ( blend bg 2 )
0008 constant bld-bg3      ( blend bg 3 )
0010 constant bld-obj      ( blend objects )
001f constant bld-all      ( all layers, except backdrop )
0020 constant bld-backdrop ( blend backdrop )
   0 constant bld-off      ( blend mode is off )
0040 constant bld-std      ( normal alpha blend, with reg-ev )
0080 constant bld-white    ( fade to white, with reg-y )
00c0 constant bld-black    ( fade to black, with reg-y )

003f constant bld-top-mask
   0 constant bld-top-shift

00c0 constant bld-mode-mask
   6 constant bld-mode-shift

3f00 constant bld-bot-mask
   8 constant bld-bot-shift

( blend weights )
04000052 constant reg-bldalpha ( blend alpha )

001f constant bld-eva-mask
   0 constant bld-eva-shift

1f00 constant bld-evb-mask
   8 constant bld-evb-shift

( fade levels )
04000054 constant reg-bldy

001f constant bldy-mask
   0 constant bldy-shift


( timer registers )
04000100 constant reg-tm ( timers as tmr-rec array )

04000100 constant reg-tm0d   ( timer 0 data )
04000102 constant reg-tm0cnt ( timer 0 control )
04000104 constant reg-tm1d   ( timer 1 data )
04000106 constant reg-tm1cnt ( timer 1 control )
04000108 constant reg-tm2d   ( timer 2 data )
0400010a constant reg-tm2cnt ( timer 2 control )
0400010c constant reg-tm3d   ( timer 3 data )
0400010e constant reg-tm3cnt ( timer 3 control )

   0 constant tm-freq-sys  ( system clock timer, 16.7 mhz )
   0 constant tm-freq-1    ( 1 cycle/tick, 16.7 mhz )
0001 constant tm-freq-64   ( 64 cycles/tick, 262 khz )
0002 constant tm-freq-256  ( 256 cycles/tick, 66 khz )
0003 constant tm-freq-1024 ( 1024 cycles/tick, 16 khz )
0004 constant tm-cascade   ( increment when preceding timer overflows )
0040 constant tm-irq       ( enable timer irq )
0080 constant tm-enable    ( enable timer )

0003 constant tm-freq-mask
   0 constant tm-freq-shift


( keys )

04000130 constant reg-keyinput ( key status, read only?? )
04000132 constant reg-keycnt ( key irq control )

0001 constant a
0002 constant b
0004 constant select
0008 constant start
0010 constant right
0020 constant left
0040 constant up
0080 constant down
0100 constant r
0200 constant l

0009 constant key-accept ( A or start )
030c constant key-reset  ( start + select + L + R )
00f0 constant key-dir    ( lef, right, up, down )
03ff constant key-any    ( any of them )

03ff constant key-mask

( adding these so I don't forget about them )
4000 constant kcnt-irq ( enable key irq )
   0 constant kcnt-or  ( interrupt on any of selected keys )
8000 constant kcnt-and ( interrupt on all of selected keys )

   
( interrupt / system registers )

04000200 constant reg-ie ( irq enable )
04000202 constant reg-if ( irq status/acknowledge )
04000204 constant reg-waitcnt ( waitstate control )
04000208 constant reg-ime ( irq master enable )
04000300 constant reg-pause ( pause system ? )


( sprites )

( attr0 )

   0 constant attr0-reg         ( regular object )
0100 constant attr0-aff         ( affine object )
0200 constant attr0-hide        ( inactive object )
0300 constant attr0-aff-dbl     ( double-size affine object )
0200 constant attr0-aff-dbl-bit
0400 constant attr0-blend       ( enable blend )
0800 constant attr0-window      ( use for object window )
1000 constant attr0-mosaic      ( enable mosaic )
   0 constant attr0-4bpp        ( 4bpp tiles )
2000 constant attr0-8bpp        ( 8bpp tiles )
   0 constant attr0-square      ( square shape )
4000 constant attr0-wide        ( tall shape )
8000 constant attr0-tall        ( wide shape )

00ff constant attr0-y-mask
   0 constant attr0-y-shift

0300 constant attr0-mode-mask
   8 constant attr0-mode-shift

c000 constant attr0-shape-mask
  14 constant attr0-shape-shift

( attr1 )

1000 constant attr1-hflip      ( horizontal flip, reg obj only )
2000 constant attr1-vflip      ( vertical flip, reg obj only )
( base sizes )
   0 constant attr1-size-8
4000 constant attr1-size-16
8000 constant attr1-size-32
c000 constant attr1-size-64
( square sizes )
   0 constant attr1-size-8x8      ( size flag for  8x8 px object )
4000 constant attr1-size-16x16 ( size flag for 16x16 px object )
8000 constant attr1-size-32x32 ( size flag for 32x32 px object )
c000 constant attr1-size-64x64 ( size flag for 64x64 px object )
( tall sizes )
   0 constant attr1-size-8x16     ( size flag for  8x16 px object )
4000 constant attr1-size-8x32  ( size flag for  8x32 px object )
8000 constant attr1-size-16x32 ( size flag for 16x32 px object )
c000 constant attr1-size-32x64 ( size flag for 32x64 px object )
( wide sizes )
   0 constant attr1-size-16x8     ( size flag for 16x8 px object )
4000 constant attr1-size-32x8  ( size flag for 32x8 px object )
8000 constant attr1-size-32x16 ( size flag for 32x16 px object )
c000 constant attr1-size-64x32 ( size flag for 64x64 px object )

01ff constant attr1-x-mask
   0 constant attr1-x-shift

3e00 constant attr1-aff-id-mask
   9 constant attr1-aff-id-shift

3000 constant attr1-flip-mask
  12 constant attr1-flip-shift

c000 constant attr1-size-mask
  14 constant attr1-size-shift


( attr2 )

03ff constant attr2-id-mask
   0 constant attr2-id-shift

0c00 constant attr2-prio-mask
  10 constant attr2-prio-shift

f000 constant attr2-palbank-mask
  12 constant attr2-palbank-shift


( helpers - a bunch, if not all, of these should travel to assembly )

: -rot rot rot ; ( a b c -- c a b )
: not 0= ; ( bool -- inverse-bool )
: 0> dup 0< swap 0= or not ; ( nr -- nr-positive? )
: <= 2dup = -rot swap > or ; ( a b - res )
: >= 2dup = -rot swap < or ; ( a b - res )

( wait for vblank interrupt )
: vsync 1 1 bdos drop ; ( -- )

( wait for vblank interrupt )
: start-music 1 2 bdos drop ; ( -- )
: stop-music 1 3 bdos drop ; ( -- )

( sprite handling )

02020000 constant spr-start ( shadow sprite start )
variable spr-head ( head of sprite linked list )
( if there have been more sprites dealloced than alloced this frame, )
( we need to erase the surplus in OAM. spr-deallocs saves the tally )
variable spr-deallocs

: attr0@ h@ ;     ( spr -- attr0 )
: attr1@ 2 + h@ ; ( spr -- attr1 )
: attr2@ 4 + h@ ; ( spr -- atr2 )
: attr0! h! ;     ( attr0 spr -- )
: attr1! 2 + h! ; ( attr1 spr -- )
: attr2! 4 + h! ; ( attr2 spr -- )

: spr-y@ c@ ;              ( spr -- y )
: spr-y! c! ;              ( y spr -- )
: spr-x@ attr1@ 01ff and ; ( spr -- x )
: spr-x! dup attr1@ fe00 and rot or swap attr1! ; ( x spr -- )

: spr-pal@ attr2@ c rshift ; ( spr -- pal )
: spr-pal! dup attr2@ 0fff and rot c lshift or swap ! ; ( pal spr -- )

: spr-z@ 7 + c@ ; ( spr - z-depth )
: spr-z! 7 + c! ; ( spr - z-depth )

: spr-i-to-addr 8 * spr-start + ;             ( index -- spr )
: spr-addr-to-i spr-start - 3 rshift ;        ( spr -- index )
: child-spr@ 6 + c@ spr-i-to-addr ;           ( spr -- child-spr )
: child-spr! swap spr-addr-to-i swap 6 + c! ; ( child-spr spr -- )

: clear-spr dup 0 swap ! 0 swap 4 + ! ;       ( spr -- )
: clear-oam-spr dup 0 swap ! 0 swap 2 + h! ;  ( spr -- )
: copy-spr 6 hmove ;                          ( spr addr -- )

( key handling)

( key index, for the bit-tribool )
4 constant ki-right
5 constant ki-left
6 constant ki-up
7 constant ki-down

variable key-curr
variable key-prev

: key-init 0 key-curr ! 0 key-prev ! ; ( -- )

( test: key-poll key-curr @ . key-prev @ . )
: key-poll ( -- )
  key-curr @ key-prev !
  reg-keyinput @ invert key-mask and key-curr ! ;

: key-status key-curr @ key-prev @ . . ; ( -- )

: key-hit ( key -- bool )
  key-curr @ key-prev @ or and ;

: bit-to-bool ( x bit -- bool )
  rshift 1 and ;

: bit-tribool ( minus plus x -- tri-state )
  tuck swap bit-to-bool -rot swap bit-to-bool - ;

: key-tri-horz ki-left ki-right key-curr @ bit-tribool ; ( -- +/- )
: key-tri-vert ki-up ki-down key-curr @ bit-tribool ; ( -- +/- )

( oam shadow list algos )

( free-spr-addr currently just finds the sprite address one above the currently )
( highest occupied address. this isn't very sophisticated. we should harvest )
( free addresses in the middle of our used address range as well. we'll do this )
( when we need to. )
: free-spr-addr ( -- spr )
  spr-head @ dup
  begin ( highest-spr curr-spr )
    child-spr@ 2dup < if nip dup then
    dup spr-start = until
  drop 8 + ;

( z-depth in => z-depth of head -> return 0, this will cover base case )
( z-depth => z-depth of curr, return higher-than-curr )
: find-spr-insert        ( z-depth -- spr )
  spr-head @             ( z-depth spr-head )
  dup spr-z@ rot tuck <=
  if                     ( spr-head z-depth )
    2drop 0
  else
    over child-spr@
    begin                ( higher-spr z-depth curr-spr )
      2dup spr-z@ >= not ( higher or equal to z-depth of current sprite )
    while                ( higher-spr z-depth curr-spr )
      rot drop dup -rot child-spr@
    repeat
    2drop
  then ;

: alloc-spr ( z-depth -- spr )
  free-spr-addr dup clear-spr 2dup spr-z! swap find-spr-insert ( free-addr top-spr-addr )
  dup 0= if
    drop ( free-addr ) ( no need to insert in top spr, set head to new addr )
    spr-head 2dup @ 2swap !
  else
    2dup child-spr@ 2swap child-spr!
  then ( free-addr bottom-spr-addr )
  over child-spr!
  -1 spr-deallocs +! ;

: spr-parent ( spr -- parent )
  dup spr-head @ =
  if drop 0
  else
    spr-head @ dup child-spr@
    begin ( target-spr curr-spr child-spr )
      rot 2dup = not
    while ( curr-spr child-spr target-spr )
      -rot nip dup child-spr@
    repeat 2drop
  then ;

: dealloc-spr ( spr -- )
  dup spr-parent dup if ( it's not 0, meaning not head )
    swap child-spr@ swap child-spr!
  else
    drop child-spr@ spr-head !
  then
  1 spr-deallocs +!
;


: spr-to-oam ( -- )
  spr-head @ mem-oam
  begin
    2dup copy-spr 8 +
    swap child-spr@ tuck spr-start =
  until nip ( mem-oam )
  spr-deallocs @ 0> if
    spr-deallocs @ 0 do dup clear-oam-spr 8 + loop
  then drop
  0 spr-deallocs ! ;

: init-spr-list ( -- )
  spr-start dup clear-spr spr-head !
  0 spr-deallocs !
;

variable beany

( game loop )
: game-loop ( -- )
  start-music
  begin
    vsync
    key-poll
    beany @ spr-x@ key-tri-horz + beany @ spr-x!
    beany @ spr-y@ key-tri-vert + beany @ spr-y!
    spr-to-oam
    select key-hit
  until
  stop-music ;

( testing )
: init
  init-spr-list
  10 alloc-spr beany !
  beany-tiles mem-vram-obj 32 move
  beany-pal mem-pal-obj 32 move
  0 beany @ spr-pal!
  attr0-tall 80 or beany @ attr0!
  95 beany @ attr1!
  key-init
  spr-to-oam
  game-loop ;

( attr1-size-16x32 95 or beany @ attr1! )
