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

mem-pal          constant mem-pal-bg
mem-pal 200 +    constant mem-pal-obj
mem-vram         constant mem-vram-bg
mem-vram 10000 + constant mem-vram-obj

4000 constant cbb-size ( charblock size )
0800 constant sbb-size ( screenblock size )

mem-ewram 20000 + constant ewram-free-start

( registers )

( display )

( reg-dispcnt )
mem-io constant reg-dispcnt  ( display control )

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
   d constant dcnt-win-shift

( reg-dipstat )
mem-io 0004 + constant reg-dispstat ( display status )

0001 constant dstat-in-vbl  ( now in vblank )
0002 constant dstat-in-hbl  ( now in hblank )
0004 constant dstat-in-vct  ( now in set vcount )
0008 constant dstat-vbl-irq ( enable vblank irq )
0010 constant dstat-hbl-irq ( enable hblank irq )
0020 constant dstat-vct-irq ( enable vcount irq )

ff00 constant dstat-vct-mask
   8 constant dstat-vct-shift

( reg-vcount )
mem-io 006 + constant reg-vcount   ( scanline count )


( backgrounds )

( bg control registers )
mem-io 008 + constant reg-bg0cnt ( bg0 control )
mem-io 00a + constant reg-bg1cnt ( bg1 control )
mem-io 00c + constant reg-bg2cnt ( bg2 control )
mem-io 00e + constant reg-bg3cnt ( bg3 control )

( reg-bgXcnt )
0040 constant bg-mosaic      ( enable mosaic )
   0 constant bg-4bpp        ( 4bpp bg, no effect on affine bg )
0080 constant bg-8bpp        ( 8bpp bg, no effect on affine bg )
2000 constant bg-wrap        ( wrap around edges of affine bgs )
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

: bg-cbb bg-cbb-shift lshift ; ( n -- n )

1f00 constant bg-sbb-mask
   8 constant bg-sbb-shift

: bg-sbb bg-sbb-shift lshift ; ( n -- n )

c000 constant bg-size-mask
   e constant bg-size-shift

: cbb-offs cbb-size * mem-vram-bg + ; ( index -- addr )
: sbb-offs sbb-size * mem-vram-bg + ; ( index -- addr )

( bg scroll registers, write only )
mem-io 010 + constant reg-bg0hofs ( bg0 horizontal scroll )
mem-io 012 + constant reg-bg0vofs ( bg0 vertical scroll )
mem-io 014 + constant reg-bg1hofs ( bg1 horizontal scroll )
mem-io 016 + constant reg-bg1vofs ( bg1 vertical scroll )
mem-io 018 + constant reg-bg2hofs ( bg2 horizontal scroll )
mem-io 01a + constant reg-bg2vofs ( bg2 vertical scroll )
mem-io 01c + constant reg-bg3hofs ( bg3 horizontal scroll )
mem-io 01e + constant reg-bg3vofs ( bg3 vertical scroll )

( affine background parameters, write only! )
mem-io 020 + constant reg-bg2pa ( bg2 matrix.pa )
mem-io 022 + constant reg-bg2pb ( bg2 matrix.pb )
mem-io 024 + constant reg-bg2pc ( bg2 matrix.pc )
mem-io 026 + constant reg-bg2pd ( bg2 matrix.pd )
mem-io 028 + constant reg-bg2x  ( bg2 x scroll )
mem-io 02c + constant reg-bg2y  ( bg2 y scroll )
mem-io 030 + constant reg-bg3pa ( bg3 matrix.pa )
mem-io 032 + constant reg-bg3pb ( bg3 matrix.pb )
mem-io 034 + constant reg-bg3pc ( bg3 matrix.pc )
mem-io 036 + constant reg-bg3pd ( bg3 matrix.pd )
mem-io 038 + constant reg-bg3x  ( bg3 x scroll )
mem-io 03c + constant reg-bg3y  ( bg3 y scroll )


( effects )

( windowing registers )
mem-io 040 + constant reg-win0h  ( win0 right, left, 0xllrr )
mem-io 042 + constant reg-win1h  ( win1 right, left, 0xllrr )
mem-io 044 + constant reg-win0v  ( win0 bottom, top, 0xttbb )
mem-io 046 + constant reg-win1v  ( win1 bottom, top, 0xttbb )
mem-io 048 + constant reg-winin  ( win0, win1 control )
mem-io 04a + constant reg-winout ( winout, winobj control )

( alternate windowing register names )
mem-io 040 + constant reg-win0r ( win 0 right )
mem-io 041 + constant reg-win0l ( win 0 left )
mem-io 042 + constant reg-win1r ( win 1 right )
mem-io 043 + constant reg-win1l ( win 1 left )

mem-io 044 + constant reg-win0b ( win 0 bottom )
mem-io 045 + constant reg-win0t ( win 0 top )
mem-io 046 + constant reg-win1b ( win 1 bottom )
mem-io 047 + constant reg-win1t ( win 1 top )

mem-io 048 + constant reg-win0cnt   ( window 0 control )
mem-io 049 + constant reg-win1cnt   ( window 1 control )
mem-io 04a + constant reg-winoutcnt ( out window control )
mem-io 04b + constant reg-winobjcnt ( obj window control )

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
mem-io 04c + constant reg-mosaic ( mosaic control )

000f constant mos-bh-mask
   0 constant mos-bh-shift

00f0 constant mos-bv-mask
   4 constant mos-bv-shift

0f00 constant mos-oh-mask
   8 constant mos-oh-shift

f000 constant mos-ov-mask
   c constant mos-ov-shift

( blend control )
mem-io 050 + constant reg-bldcnt ( blend control )

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

: bld-set ( mode top bot -- )
  bld-bot-shift lshift or or reg-bldcnt h! ;

( blend weights )
mem-io 052 + constant reg-bldalpha ( blend alpha )

001f constant bld-eva-mask
   0 constant bld-eva-shift

1f00 constant bld-evb-mask
   8 constant bld-evb-shift

: bldalpha-set ( bld-eva bld-evb -- )
  bld-evb-shift lshift or reg-bldalpha h! ;

( fade levels )
mem-io 054 + constant reg-bldy

001f constant bldy-mask
   0 constant bldy-shift

: bldy-set ( val -- )
  reg-bldy h! ;

( timer registers )
mem-io 100 + constant reg-tm ( timers as tmr-rec array )

mem-io 100 + constant reg-tm0d   ( timer 0 data )
mem-io 102 + constant reg-tm0cnt ( timer 0 control )
mem-io 104 + constant reg-tm1d   ( timer 1 data )
mem-io 106 + constant reg-tm1cnt ( timer 1 control )
mem-io 108 + constant reg-tm2d   ( timer 2 data )
mem-io 10a + constant reg-tm2cnt ( timer 2 control )
mem-io 10c + constant reg-tm3d   ( timer 3 data )
mem-io 10e + constant reg-tm3cnt ( timer 3 control )

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

mem-io 130 + constant reg-keyinput ( key status, read only?? )
mem-io 132 + constant reg-keycnt ( key irq control )

0001 constant key-a
0002 constant key-b
0004 constant select
0008 constant start
0010 constant right
0020 constant left
0040 constant up
0080 constant down
0100 constant key-r
0200 constant key-l

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

mem-io 200 + constant reg-ie ( irq enable )
mem-io 202 + constant reg-if ( irq status/acknowledge )
mem-io 204 + constant reg-waitcnt ( waitstate control )
mem-io 208 + constant reg-ime ( irq master enable )
mem-io 300 + constant reg-pause ( pause system ? )


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
   e constant attr0-shape-shift

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
   c constant attr1-flip-shift

c000 constant attr1-size-mask
   e constant attr1-size-shift


( attr2 )

03ff constant attr2-id-mask
   0 constant attr2-id-shift

0c00 constant attr2-prio-mask
   a constant attr2-prio-shift

f000 constant attr2-palbank-mask
   c constant attr2-palbank-shift


( helpers - a bunch, if not all, of these should travel to assembly )

: -rot rot rot ; ( a b c -- c a b )
: not 0= ; ( bool -- inverse-bool )
: 0> dup 0< swap 0= or not ; ( nr -- nr-positive? )
: <= 2dup = -rot swap > or ; ( a b - res )
: >= 2dup = -rot swap < or ; ( a b - res )

: set-reg h! ; ( val addr -- )

( wait for vblank interrupt )
: vsync 1 1 bdos drop ; ( -- )

( wait for vblank interrupt )
: start-music 1 2 bdos drop ; ( -- )
: play-ring 1 3 bdos drop ; ( -- )
: feed-music 1 4 bdos drop ; ( -- )
: init-music 1 5 bdos drop ; ( -- )

( sprite handling )

ewram-free-start constant spr-start ( shadow sprite start )
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
: spr-x@ attr1@ attr1-x-mask and ; ( spr -- x )
: spr-x! ( x spr -- )
  dup attr1@ attr1-x-mask invert and rot attr1-x-mask and or swap attr1! ;

: spr-pal@ attr2@ c rshift ; ( spr -- pal )
: spr-pal! dup attr2@ 0fff and rot c lshift or swap ! ; ( pal spr -- )
: spr-tid! ( tid spr -- )
  dup attr2@ attr2-id-mask invert and rot attr2-id-mask and or swap attr2! ;

: spr-hflip! ( flip-bool spr )
  swap if
    dup attr1@ attr1-hflip or swap attr1! ( set hflip )
  else
    dup attr1@ attr1-hflip invert and swap attr1! ( clear hflip )
  then ;

: spr-z@ 7 + c@ ; ( spr - z-depth )
: spr-z! 7 + c! ; ( spr - z-depth )

: spr-i-to-addr 8 * spr-start + ;             ( index -- spr )
: spr-addr-to-i spr-start - 3 rshift ;        ( spr -- index )
: child-spr@ 6 + c@ spr-i-to-addr ;           ( spr -- child-spr )
: child-spr! swap spr-addr-to-i swap 6 + c! ; ( child-spr spr -- )

: clear-spr dup 0 swap ! 0 swap 4 + ! ;       ( spr -- )
: clear-oam-spr dup 0 swap ! 0 swap 2 + h! ;  ( spr -- )
: copy-spr 6 hmove ;                          ( spr addr -- )

( key handling )

( key index, for the bit-tribool )
( TODO: i think we don't need these and we can just use the direction constants straight )
4 constant ki-right
5 constant ki-left
6 constant ki-up
7 constant ki-down

variable key-curr
variable key-prev

: key-init ( -- )
  0 key-curr !
  0 key-prev ! ;

( test: key-poll key-curr @ . key-prev @ . )
: key-poll ( -- )
  key-curr @ key-prev !
  reg-keyinput @ invert key-mask and key-curr ! ;

: key-status key-curr @ key-prev @ . . ; ( -- )

: key-is-down ( keys -- bool )
  key-curr @ and ;

: key-is-up ( keys -- bool )
  key-curr @ invert and ;

: key-was-down ( keys -- bool )
  key-prev @ and ;

: key-was-up ( keys -- bool )
  key-prev @ invert and ;


: key-transit ( keys -- transit-keys )
  key-curr @ key-prev @ or and ;

: key-held ( keys -- bool )
  key-curr @ key-prev @ and and ;

: key-released ( keys -- bool )
  key-curr @ invert key-prev @ and and ;

: key-hit ( keys -- bool )
  key-curr @ key-prev @ invert and and ;


: bit-to-bool ( x bit -- bool )
  rshift 1 and ;

: bit-tribool ( minus plus x -- tri-state )
  tuck swap bit-to-bool -rot swap bit-to-bool - ;

: key-tri-horz ki-left ki-right key-curr @ bit-tribool ; ( -- +/- )
: key-tri-horz-prev ki-left ki-right key-prev @ bit-tribool ; ( -- +/- )

: key-tri-vert ki-up ki-down key-curr @ bit-tribool ; ( -- +/- )
: key-tri-vert-prev ki-up ki-down key-prev @ bit-tribool ; ( -- +/- )


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
  1 spr-deallocs +! ;

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
  0 spr-deallocs ! ;


( sprite objects )

: obj-spr@ @ ; ( obj -- spr )
: obj-spr! ! ; ( spr obj -- )
: obj-coord cell + ; ( obj -- coord-addr )
: obj-coord@ cell + @ ; ( obj -- coord )
: obj-coord! cell + ! ; ( coord obj -- )
: obj-dir@ 2 cells + h@ ; ( obj -- dir )
: obj-dir! 2 cells + h! ; ( dir obj -- )
: obj-toi@ a + h@ ; ( obj -- toi )
: obj-toi! a + h! ; ( toi obj -- )
: obj-cb-offs 3 cells + ; ( obj -- coord ) ( object collision box offset )
: obj-cb-width 4 cells + ; ( obj -- width-addr )
: obj-cb-height 5 cells + ; ( obj -- height-addr )
: obj-tid-base@ 6 cells + @ ; ( obj -- tile-id-base )
: obj-tid-base! 6 cells + ! ; ( tile-id-base obj -- )
: obj-ticks@ 1e + h@ ; ( obj -- ticks )
: obj-ticks! 1e + h! ; ( ticks obj -- )
: obj-idle@ 8 cells + h@ ; ( obj -- idle )
: obj-idle! 8 cells + h! ; ( idle obj -- )
: obj-idle-override@ 22 + h@ ; ( obj -- idle-override )
: obj-idle-override! 22 + h! ; ( idle-override obj -- )

: set-spr-cb-8x16 ( obj --  )
  dup obj-cb-offs 0 1 rot store-xy
  dup obj-cb-width 1 swap !
  obj-cb-height 1 swap ! ;

: set-spr-cb-16x32 ( obj-cb-offs --  )
  dup obj-cb-offs 0 3 rot store-xy
  dup obj-cb-width 2 swap !
  obj-cb-height 2 swap ! ;

24 constant obj-size


( coordinate mapping, collision detection, things of interest )

( variables and helpers )

variable map-width
variable map-height
variable toi-map

4 array bg-coord
variable bg-x-max-clamp-mod
variable bg-y-max-clamp-mod

variable mov-delta
variable beany-equ-offs-x ( player sprite equilibrium x position )
variable beany-equ-offs-y ( player sprite equilibrium y position )

: x@ 2 + h@ ; ( coord-addr -- x )
: x! 2 + h! ; ( x coord-addr -- )
: y@ h@ ; ( coord-addr -- y )
: y! h! ; ( y coord-addr -- )
: store-xy tuck y! x! ; ( x y thing )
: x-get 10 rshift ; ( coord -- x )
: x-set ffff and swap 10 lshift or ; ( x coord -- coord )
: y-get ffff and ; ( coord -- y )
: y-set ffff0000 and or ; ( y coord -- coord )

: coord-to-tile 3 rshift ; ( coord-xy -- tile-size )

: get-x-tile-offs x-get swap x-get coord-to-tile + ; ( coord offs --  tile-x )
: get-y-tile-offs y-get swap y-get coord-to-tile + ; ( coord offs --  tile-y )

: get-offs-tile ( coord offs -- offs-tile )
  2dup get-x-tile-offs -rot get-y-tile-offs map-width @ * + ;

: new-x-delta key-tri-horz mov-delta @ * ; ( -- new-x )
: get-nxt-x x-get new-x-delta + ; ( coord -- )
: new-y-delta key-tri-vert mov-delta @ * ; ( -- new-y )
: get-nxt-y y-get new-y-delta + ; ( coord -- )
: get-nxt-coord dup get-nxt-x swap get-nxt-y x-set ; ( coord -- nxt-coord )

( things of interest )

: get-toi 2 * toi-map @ + h@ ; ( offs-tile -- toi )

( this fn uses width and height of a bounding box to check all the squares of )
( that box for points of interest, and will push them on the stack )
: check-toi-loop ( coord obj-cb-offs width height -- toi's.. )
  0 do ( 900060 3 2 )
    ( this is some truely horrible stack manipulation to make sure that we keep on )
    ( feeding the inner loop bounds, 2 in example, while pushing the result of get-toi )
    ( back on the stack. stack manipulation past 3 items becomes a serious chore )
    ( another option might be or-ing the values inline, but this )
    ( brings its own issues with having to know if this is the first value being pushed )
    ( or not. perhaps pass arguments in a struct, to circumvent stack manipulation, )
    ( but seeing traditional Forth doesn't do local variables, this isn't pretty either. )
    ( I'm guessing we'd need a global struct just for this fn. )
    dup 2swap rot ( 2 900060 3 2 )
    0 do ( 2 900060 3 )
      2dup dup x-get i + swap y-get j + x-set get-offs-tile get-toi
      ( 2 900060 3 3d2 ) 2swap swap >r ( 3 3d2 900060 ) rot ( 3d2 900060 3 ) r> -rot
    loop
    rot
    loop
  2drop drop ;

( check thing of interest tiles for sprite collision-box )
: check-toi-map ( coord obj-cb-offs width height -- toi )
  2dup * >r
  check-toi-loop
  ( w x h of toi's are now on the stack, compress them to one )
  r> 1 do or loop ;

( bg coordinates )

: update-shadow-bg-x bg-coord x! ; ( bg-x-coord -- )
: update-shadow-bg-y bg-coord y! ; ( bg-y-coord -- )
: update-shadow-bg-coord dup x-get update-shadow-bg-x y-get update-shadow-bg-y ; ( coord )

: update-bg-x bg-coord x@ reg-bg3hofs h! ; ( -- )
: update-bg-y bg-coord y@ reg-bg3vofs h! ; ( -- )
: update-bg-coord update-bg-x update-bg-y ; ( -- )

: clamp-bg-min dup 0 < if drop 0 then ; ( x-or-y-bg-coord )

: clamp-bg-x-max ( x-bg-coord )
  dup map-width @ 8 * f0 - bg-x-max-clamp-mod @ - tuck >
  if nip else drop then ;

: clamp-bg-y-max ( y-bg-coord )
  dup map-height @ 8 * a0 - bg-y-max-clamp-mod @ - tuck >
  if nip else drop then ;

: spr-to-x-bg-coord ( coord -- x-bg-coord )
  x-get beany-equ-offs-x @ - clamp-bg-min clamp-bg-x-max ;

: spr-to-y-bg-coord ( coord -- y-bg-coord )
  y-get beany-equ-offs-y @ - clamp-bg-min clamp-bg-y-max ;

: beany-to-bg-coord ( new-coord -- bg-coord )
  dup spr-to-x-bg-coord swap spr-to-y-bg-coord x-set ;

: update-x-spr-coord ( obj -- )
  dup obj-coord@ x-get
  bg-coord @ x-get ( obj x-obj-coord x-bg-coord ) -
  swap obj-spr@ spr-x! ;

: update-y-spr-coord ( obj -- )
  dup obj-coord@ y-get
  bg-coord @ y-get ( obj y-obj-coord y-bg-coord ) -
  swap obj-spr@ spr-y! ;

: update-spr-coord ( obj -- )
  dup update-x-spr-coord update-y-spr-coord ;

: update-coord ( obj -- ) ( currently the only thing of interest is a collision )
  dup obj-coord@ get-nxt-coord 2dup swap ( obj nxt-coord nxt-coord obj )
  dup obj-cb-offs @ over obj-cb-width @ 1 + rot obj-cb-height @
  check-toi-map dup >r rot dup r> swap obj-toi! -rot
  1 and not ( obj nxt-coord !toi )
  if
    2dup ( obj nxt-coord obj nxt-coord )
    swap ( obj nxt-coord nxt-coord obj )
    obj-coord! ( obj nxt-coord )
    beany-to-bg-coord update-shadow-bg-coord
    update-spr-coord
  else
    2drop
  then ;


( effects )

: fade-to-black ( shift -- )
  bld-black bld-all 0 bld-set
  dup 16 swap lshift 0 do
    dup 1 swap lshift i swap mod not if
      dup i swap rshift bldy-set else
    then
    vsync
  loop
  drop
  1f bldy-set ;

: fade-from-black ( shift -- )
  bld-black bld-all 0 bld-set
  dup 16 swap lshift 0 do
    dup 1 swap lshift i swap mod not if
      dup i swap rshift 1f swap - bldy-set else
    then
    vsync
  loop
  drop
  0 bldy-set ;

( animation )

: obj-ticks-inc ( obj -- )
  dup obj-ticks@ 1+ swap obj-ticks! ;

: new-obj-dir ( dir obj )
  dup 0 swap obj-ticks!
  dup 0 swap obj-idle!
  obj-dir! ;

: update-dir ( obj -- )
  key-dir key-transit
  if
    ( if we point to same direction as before we don't change direction, )
    ( so we face the same side when transitioning to/from diagonals. )
    ( we only support 4 directions for now )
    ( just update ticks )
    dup obj-idle-override@ if
      dup 0 swap obj-ticks!
      dup 0 swap obj-idle-override!
      1 swap obj-idle!
    else
      dup obj-dir@ key-is-down
      if
        dup obj-idle@ if
          dup 0 swap obj-ticks!
          0 swap obj-idle!
        else
          obj-ticks-inc
        then
      else
        ( else point to direction in preset order of preference )
        left  key-is-down if left  swap new-obj-dir else
        right key-is-down if right swap new-obj-dir else
        up    key-is-down if up    swap new-obj-dir else
        down  key-is-down if down  swap new-obj-dir else
        ( no key has been pressed. we're idle and we weren't before. )
        dup 0 swap obj-ticks!
        1 swap obj-idle!
        then then then then
      then
    then
  else
    obj-ticks-inc
  then ;

: obj-set-spr-tid ( tid-offs obj )
  tuck obj-tid-base@ + 2 *
  swap obj-spr@ spr-tid! ;

( 16x32 sprite sheet layout: )
( tiles per sprite: 8 )
( animations:
( walking: 3 per direction, 4 per loop: left-leg rest1 right-leg rest1 )
( standing: let's start off with 2 frames, per direction )
( so that's 4 frames in total, packed like: left-leg, right-leg, rest1, rest2 )
( directions packing: down, up, left.. right is left mirrored )

8 constant tiles-per-sprite
4 constant frames-per-dir
tiles-per-sprite frames-per-dir * constant tot-dir-tiles

5 constant tick-shift-idle ( remove bottom tick ranges )
4 constant tick-shift-move ( remove bottom tick ranges )

: tid-offs ( obj -- tid-offs ) ( general tid offset into direction )
  dup obj-ticks@ swap obj-idle@ if
    ( tick-shift rshifts: strobing 32 ticks ~= on-off every half second )
    ( the addition of 2 will mean that a val of 0 will be rest1 and a val of 1 )
    ( will match rest2 )
    tick-shift-idle rshift 7 and ( here we're cutting our range into 4 regions )
    dup 0 = if drop 2 else ( rest1 )
    dup 1 = if drop 2 else ( rest1 )
    dup 2 = if drop 3 else ( rest2 )
    dup 3 = if drop 2 else ( rest1 )
    dup 4 = if drop 2 else ( rest1 )
    dup 5 = if drop 2 else ( rest1 )
    dup 6 = if drop 2 else ( rest2 )
    dup 7 = if drop 2 else ( rest1 )
    drop ( shouldn't happen ) then then then then then then then then
  else
    tick-shift-move rshift 3 and
    dup 0 = if drop 0 else ( left-leg, we're done. we're already at pos 0 )
    dup 1 = if drop 2 else ( rest1 )
    dup 2 = if drop 1 else ( right-leg )
    dup 3 = if drop 2 else ( rest1 )
    drop ( shouldn't happen ) then then then then
  then
  tiles-per-sprite * ; ( multiply by tiles per sprite )

: spr-dir-left-right! ( mirror? obj )
  dup
  dup tid-offs tot-dir-tiles 2 * + swap obj-set-spr-tid ( set spr tid )
  obj-spr@ spr-hflip! ; ( set spr mirror )

: spr-dir! ( obj )
  dup obj-dir@
  dup down  = if drop dup tid-offs swap obj-set-spr-tid else
  dup up    = if drop dup tid-offs tot-dir-tiles + swap obj-set-spr-tid else
  dup left  = if drop 0 swap spr-dir-left-right! else
  dup right = if drop 1 swap spr-dir-left-right! else
  2drop then then then then ;

: update-spr-frame ( obj -- )
  dup update-dir
  spr-dir! ;


( higher logic general )

( continuation of game logic )
variable main-loop-continuation
variable timed-event-continuation

( beany == player character sprite )
obj-size array beany

variable covid-date
variable timed-event-counter

( text )

variable font-tiles-base
variable font-map-base
variable font-bg-map-base

variable print-x-base
variable print-x-len
variable print-y-base
variable print-y-len
variable print-x-cur
variable print-y-cur

1e 14 * array print-map

: print-pos 1e * + print-map + ; ( x y - pos )
: font-map-pos 20 * + 2 * font-map-base @ + ; ( x y )
: font-bg-map-pos 20 * + 2 * font-bg-map-base @ + ; ( x y )

: print-x-inc print-x-cur dup @ 1+ swap ! ; ( -- )
: print-y-inc print-y-cur dup @ 1+ swap ! ; ( -- )

: print-wrap-y ( y-old -- y-new )
  dup print-y-base @ print-y-len @ + =
  if drop print-y-base @ then ;

: print-wrap-y-set ( -- )
  print-y-cur @ print-wrap-y print-y-cur ! ;

: print-clear-curr-line ( -- )
  print-y-cur @ print-x-base @ print-x-len @ + print-x-base @ do
    dup i swap print-pos bl swap c! loop drop ;

: print-check-x-bounds ( -- )
  print-x-cur @ print-x-base @ print-x-len @ 1- + = ;

: print-set-char ( c -- )
  print-x-cur @ print-y-cur @ print-pos c! ;

: print-nxt-line ( -- )
  print-x-base @ print-x-cur !
  print-y-inc
  print-wrap-y-set
  ( print-clear-curr-line ) ;

: print-set-next-free-pos ( -- )
  print-check-x-bounds
  if print-nxt-line
  else print-x-inc then ;

: print-char ( c -- )
  dup 0a =
  if drop print-nxt-line
     ( feed-music )
     print-x-base @ 1- print-x-cur !
  else
    ( feed-music )
    print-set-next-free-pos
    print-set-char
  then ;

: print-dialog
  print-y-cur @ 1+ print-wrap-y
  print-y-base @ dup print-y-len @ + swap do
    print-x-base @ dup print-x-len @ + swap do
      dup
      vsync ( we want to slow down text showing )
      ( i 2 mod not if vsync then )
      i swap print-pos c@
      i j font-map-pos h!
    loop
    1+ print-wrap-y
  loop
  drop ;

: clear-dialog-text
  print-y-cur @ 1+ print-wrap-y
  print-y-base @ dup print-y-len @ + swap do
    print-x-base @ dup print-x-len @ + swap do
      dup
      ( i 4 mod not if feed-music then )
      i swap print-pos c@
      i j font-map-pos h!
    loop
    1+ print-wrap-y
  loop
  drop ;

: draw-txt-bg ( tile -- )
  print-y-base @ 1- dup print-y-len @ 2 + + swap do
    ( print-x-base @ dup print-x-len @ + 1+ swap do )
    1e 0 do
      dup i j font-bg-map-pos h!
      ( i 4 mod not if feed-music then )
    loop
  loop drop ;

: reset-dialog-pos ( -- )
  print-x-base @ 1- print-x-cur !
  print-y-base @ print-y-cur ! ;

: set-dialog-dimensions
  1  print-x-base !
  1c print-x-len !
  1 print-y-base !
  4 print-y-len !
  reset-dialog-pos ;

: clear-dialog ( -- )
  print-y-base @ dup print-y-len @ + swap do
    i print-y-cur !
    ( feed-music )
    print-clear-curr-line
  loop
  clear-dialog-text
  reset-dialog-pos ;

: dissolve-dialog ( -- )
  clear-dialog
  0 draw-txt-bg ;

: exit-dialog ( -- )
   dissolve-dialog
   ['] in-default main-loop-continuation ! ;

: in-dialog ( -- )
  key-a key-hit if
    exit-dialog
  then ;

: str-print ( addr n -- )
  0 do
    dup
    i + c@ print-char
  loop drop ;

: nr-to-str ( u -- addr n )
  0 <# #s #> ;

: set-transp-txt-bg ( -- )
  bld-std bld-bg1 bld-bg2 bld-bg3 or bld-obj or bld-set
  8 2 bldalpha-set ;


( actions/things of interest )

variable apt-seq-iter
variable fridge-opened
variable friend-called
variable exit-apt-seq

0 constant collisions
1 constant kitchen
2 constant sink
3 constant toilet
4 constant bath
5 constant couch
6 constant tv
7 constant front-door
8 constant desk
9 constant sleep
a constant closet
b constant clothes
c constant gamecube
d constant fridge
e constant poster

: intro-str1       s" It's " ;
: intro-str2       s" days since the\nstart of lockdown.\n\nYou've got a splitting headache, and you wonder why you just woke up lying on the kitchen floor.\n\n\n\nPress A to interact with things and to exit dialogs." ;

: couch-start-str  s" The couch is calling you. 'Lay down', it says, 'you deserve it'.\n\nYou wipe away the crisps, get comfy and you think: I'll just take a tiny nap.. Just 15 minutes max!" ;
: couch-wake1-str  s" It's " ;
: couch-wake2-str  s" days since the\nstart of lockdown.\n\nYou pluck some stale pieces of crisps from your body, eat them, and regret it.\n\nBut you're ready to take on this new day!! Or perhaps a little doze couldn't hurt.. To ease you into it..\n\nCouch says: Yes!!" ;

: bed-start-str    s" How can you resist?.." ;
: bed-wake1-str    s" It's " ;
: bed-wake2-str    s" days since the\nstart of lockdown. Another day gone.\n\nBut you had a nice dream.\n\nYou dreamt that you were alone in your apartment.\n\nNo responsibilities, no goals, no plans.\n\nWait, that wasn't a nice dream at all!!\n\nWas it even a dream..\n\nAre you awake?" ;

: kitchen-str      s" Tonight you will make vegan tiramisu with brocolli.\n\nIt's a bit of a risky combination, but you are sure you can pull it off.\n\nIf only you could find that blue spatula.." ;

: sink-str         s" You turn on the hot water, and let it run over your wrists for a while.\n\nIt's a soothing feeling. You feel refreshed." ;

: toilet-str       s" You ascend your throne, whip out your Game Boy Advance, and settle down for another three hour session of Elf Bowling. Nr 2 this time." ;

: bath-str         s" The bath just reminds you of that day rubber ducky went out to get a pack of smokes." ;

: tv-str           s" You flick the TV to a news program.\n\n'... estimate that the zombie hordes might now have reached Nebraska. Perhaps Florida too, but there it's hard to tell them apart from the population ...'\n\nYou sigh.. Always the same old, same old." ;

: front-door-str   s" You shall not pass!!\n\nWell, it's not that dramatic. But you haven't opened the door in months.\n\nIn fact you kinda lost your keys. Last week, a search didn't show up anything.\n\nPerhaps you SHOULD be worried." ;

: desk-str         s" Your desk. It's so tidy. Not a thing out of place. There's a film of dust on the laptop keys.\n\nYour eyes try to avoid the scene. You don't want to be here." ;

: closet-str       s" You open the right closet door,snuggle up in the corner and rock your body back and forth for a bit.\n\nYou get out again, and close the door gently. You're fine!" ;

: clothes-str      s" You wore proper clothes back then. They made you look respectable. They made you fit in. Now you don't fit them anymore.\n\n And why should you! Pyjama's stretch to fit YOU. They hug you always just as much as you need. Your best friends till the end." ;

: gamecube-str     s" Looking at your Gamecube, your eyes well up. It is serenading you with the whizzy sounds of its CD drive. Like a beautiful whale song.\n\nIt is surely sentient, and it is trying to tell you something... If only you could speak Gamecube.." ;

: fridge-str       s" You open the fridge. It is stacked from top to bottom with mushrooms.. Individual mushrooms!\n\nStrange! You have no memory of this whatsoever.\n\nYou feel a panic attack coming up, and you smother it by shoving mushrooms into your mouth. They taste weird." ;

: poster-str       s" It's a really nice poster of some art-house movie.\n\nYou haven't seen the movie.\n\nYou would like to, but you are afraid that you won't like the movie and then you're stuck with a poster that reminds you of a shitty movie." ;

: call-init-str s" RING RING!!!" ;

: set-in-dialog ( -- )
  ['] in-dialog main-loop-continuation ! ; ( ai, circular dependency.. ok when cross-compiling, but not when Forthing on GBA )

: print-msg ( addr size dia-len -- )
  print-y-len !
  str-print
  8 draw-txt-bg
  print-dialog ;

: print-dispatch ( addr size dia-len -- )
  key-a key-hit if print-msg set-in-dialog else 2drop drop then ;

: print-covid-date ( -- )
  bl print-char
  covid-date @ nr-to-str str-print
  bl print-char ;

: print-covid-msg ( addr-msg2 n-msg2 addr-msg1 n-msg1 y-len -- )
  print-y-len !
  str-print
  print-covid-date
  str-print
  8 draw-txt-bg
  print-dialog ;

: sleep-seq ( xt x-coord y-coord -- )
  dissolve-dialog
  2 fade-to-black
  beany obj-coord store-xy
  beany dup down swap obj-dir!
  dup update-coord
  update-anim
  vsync
  update-vblank-hard
  covid-date mem-1+
  2 fade-from-black
  set-transp-txt-bg
  main-loop-continuation ! ;

( bed dialog )
: bed-wake-dialog ( -- )
  bed-wake2-str bed-wake1-str 12 print-covid-msg set-in-dialog ;

: bed-seq ( -- )
  key-a key-hit if
    ['] bed-wake-dialog 18e 40 sleep-seq
  then ;

: bed-dispatch ( addr size dia-len -- )
  key-a key-hit if
    print-msg
    ['] bed-seq main-loop-continuation !
  else 2drop drop then ;

( couch dialog )
: couch-wake-dialog ( -- )
  couch-wake2-str couch-wake1-str d print-covid-msg set-in-dialog ;

: couch-seq ( -- )
  key-a key-hit if
    ['] couch-wake-dialog 110 3a sleep-seq
  then ;

: couch-dispatch ( addr size dia-len -- )
  key-a key-hit if
    print-msg
    ['] couch-seq main-loop-continuation !
  else 2drop drop then ;

( dispatchers )

: gamecube-dialog ( addr size dia-len -- )
  b print-y-len !
  gamecube-str str-print
  space print-char d print-char e print-char
  space print-char d print-char e print-char
  8 draw-txt-bg
  print-dialog
  set-in-dialog ;

: fridge-dialog
  fridge-str d print-msg set-in-dialog
  1 fridge-opened !
  friend-called @ if
    0 timed-event-counter !
    ['] sinister-call timed-event-continuation !
  then ;

: kitchen-dialog kitchen-str 9 print-msg set-in-dialog ;
: bed-dialog bed-start-str 1 print-msg ['] bed-seq main-loop-continuation ! ;
: sink-dialog sink-str 6 print-msg set-in-dialog ;
: toilet-dialog toilet-str 5 print-msg set-in-dialog ;
: bath-dialog bath-str 3 print-msg set-in-dialog ;
: couch-dialog couch-start-str 8 print-msg ['] couch-seq main-loop-continuation ! ;
: tv-dialog tv-str c print-msg set-in-dialog ;
: front-door-dialog front-door-str c print-msg set-in-dialog ;
: desk-dialog desk-str 8 print-msg set-in-dialog ;
: closet-dialog closet-str 8 print-msg set-in-dialog ;
: clothes-dialog clothes-str b print-msg set-in-dialog ;
: poster-dialog poster-str b print-msg set-in-dialog ;

: toi-cont ( cont-xt -- )
  key-a key-hit if
    1 beany obj-idle-override!
    main-loop-continuation !
  else
    drop
  then ;

: dispatch-toi ( toi -- )
  dup 1 kitchen    lshift and if drop ['] kitchen-dialog    toi-cont else
  dup 1 sleep      lshift and if drop ['] bed-dialog        toi-cont else
  dup 1 sink       lshift and if drop ['] sink-dialog       toi-cont else
  dup 1 toilet     lshift and if drop ['] toilet-dialog     toi-cont else
  dup 1 bath       lshift and if drop ['] bath-dialog       toi-cont else
  dup 1 couch      lshift and if drop ['] couch-dialog      toi-cont else
  dup 1 tv         lshift and if drop ['] tv-dialog         toi-cont else
  dup 1 front-door lshift and if drop ['] front-door-dialog toi-cont else
  dup 1 desk       lshift and if drop ['] desk-dialog       toi-cont else
  dup 1 closet     lshift and if drop ['] closet-dialog     toi-cont else
  dup 1 clothes    lshift and if drop ['] clothes-dialog    toi-cont else
  dup 1 gamecube   lshift and if drop ['] gamecube-dialog   toi-cont else
  dup 1 fridge     lshift and if drop ['] fridge-dialog     toi-cont else
  dup 1 poster     lshift and if drop ['] poster-dialog     toi-cont else
  drop then then then then then then then then then then then then then then ;


( timed events )

900 constant friend-call-wait-thresh
500 constant suggest-wait-thresh
500 constant sinister-call-wait-thresh
500 constant woozy-wait-thresh
500 constant end-seq-thresh

: timed-events-idle ( -- )
  ( nop) ;

: timed-events-end ( -- )
  ( this should hold some dramatic end sequence, nop for now ) ;

: timed-dialog-seq ( addr n size cont -- )
  key-a key-hit if
    dissolve-dialog
    print-msg
    main-loop-continuation !
  else 2drop 2drop
  then ;

: bg-phone-show ( -- )
  reg-dispcnt dup @ dcnt-bg2 or swap ! ;

: bg-phone-hide ( -- )
  reg-dispcnt dup @ dcnt-bg2 invert and swap ! ;

( end sequence )

: end-seq-splash ( -- )
  key-a key-hit if
    dissolve-dialog
    mem-pal-bg dup @ invert swap !
    2 fade-to-black
    0 timed-event-counter !
    1 exit-apt-seq !
  then ;

: end-seq-start ( -- )
  s" Your feet give out and you tumble over. Your arms refuse to break your fall and your head hits the ground..\n\nHard." 7 print-msg
  set-in-dialog
  ['] end-seq-splash main-loop-continuation ! ;

: end-seq ( -- )
  timed-event-counter @ end-seq-thresh > if
    1 beany obj-idle-override!
    ['] end-seq-start main-loop-continuation !
    ['] timed-events-idle timed-event-continuation !
  then ;

: palette-invert
  apt-pal-len 0 do
    mem-pal-bg i + dup @ invert swap !
  4 +loop ;

: woozy-start ( -- )
  s" You're starting feel a bit woozy, and your eyes seem to be loosing their grip on what colors should look like. This isn't good..." 5 print-msg
  palette-invert
  0 timed-event-counter !
  set-in-dialog
  ['] end-seq timed-event-continuation ! ;

: woozy-seq ( -- )
  timed-event-counter @ woozy-wait-thresh > if
    1 beany obj-idle-override!
    ['] woozy-start main-loop-continuation !
    ['] timed-events-idle timed-event-continuation !
  then ;

( sinister sequence )

: sinister-hang-up ( -- )
  key-a key-hit if
    dissolve-dialog
    bg-phone-hide
    0 timed-event-counter !
    ['] in-default main-loop-continuation !
    ['] woozy-seq timed-event-continuation !
  then ;

: sinister-reply ( -- )
  ['] sinister-hang-up
  s" you: s'cuse me. Who is this...\n\nLooks like they hung up.\n\nThat was.. frightening to be honest. You're trying to shake it off, without much luck.\n\nYou need to do something pronto, but what?" c
  timed-dialog-seq ;

: sinister-intro ( -- )
  ['] sinister-reply
  s" voice: You!! Finally! Hi.. This is me. I mean you. Us! Please trust me.. Those mushrooms in your fridge.. Don't eat them! Just don't! Don't tell me you ate them.. You ate them!?!?! Listen, you're stuck in a..\n\n<disturbing garbled sound>" a
  timed-dialog-seq ;

: sinister-pick-up ( -- )
  ['] sinister-intro
  s" you: Ok, listen for a bit. Don't just hang up again like the ass you can be. Got something to tell you.." 4
  timed-dialog-seq ;

: sinister-call-start ( -- )
  play-ring
  bg-phone-show
  call-init-str 1 print-msg
  ['] sinister-pick-up main-loop-continuation ! ;

: sinister-call ( -- )
  timed-event-counter @ sinister-call-wait-thresh > if
    1 beany obj-idle-override!
    ['] sinister-call-start main-loop-continuation !
    ['] timed-events-idle timed-event-continuation !
  then ;

( suggest sequence )
: suggest-end
  key-a key-hit if
    dissolve-dialog
    bg-phone-hide
    0 timed-event-counter !
    ['] in-default main-loop-continuation !

    fridge-opened @ if
      ['] sinister-call timed-event-continuation !
    then
  then ;

: suggest-start
  s" You're very hungry. Your tummy growls. You wonder if there's still some of that squash soup left in the fridge." 5 print-msg
  ['] suggest-end main-loop-continuation ! ;

: suggest-seq ( -- )
  timed-event-counter @ suggest-wait-thresh > if
    fridge-opened @ if
      ['] sinister-call timed-event-continuation !
    else
      1 beany obj-idle-override!
      ['] suggest-start main-loop-continuation !
      ['] timed-events-idle timed-event-continuation !
    then
 then ;

( friend sequence )

: friend-hang-up ( -- )
  key-a key-hit if
    dissolve-dialog
    bg-phone-hide
    1 friend-called !
    0 timed-event-counter !
    ['] in-default main-loop-continuation !

    fridge-opened @ if
      ['] sinister-call timed-event-continuation !
    else
      ['] suggest-seq timed-event-continuation !
    then
  then ;

: friend-talk-over ( -- )
  ['] friend-hang-up
  s" friend: Ok, yea, sure.. Listen, I'm a bit short on time. Call you back in a bit. Hang in there tiger!\n\nYour friend hung up already.\n\nYou sigh.. He's really fun to be around, but he is such a freeloader and he doesn't listen ever." b
  timed-dialog-seq ;

: friend-reply ( -- )
  ['] friend-talk-over
  s" This is a bit of a reality check. You're not sure if you can still speak after months of silence.\n\nYou croak: Yea,.. maybe.. I kinda lost my keys.. Ehh.. How are you?.." 8
  timed-dialog-seq ;

: friend-intro ( -- )
  ['] friend-reply
  apt-seq-iter @
  dup 0 = if drop
    s" friend: Hey, been ages. I'm sure you're fine. Yea so d'ya hear, lockdown is over in a couple of days probably. Wanna come to Crimson Park on Friday? At 2. Oh, and could you take me there?" 8
  else dup 1 = if drop
    s" friend: Where were you on Friday. Not cool man. Not cool. I ended up riding with Eileen. So embaressing. Anyways, can I come over in a bit? I'm out of smokes, and it looks like lockdown is going to end for real now. And hardly any zombies about." a
  else drop
    s" klomp: What! You're still here? Respect. But we're fresh out of content for this GBA Jam 2021 demo.\n\nHope you liked it.\n\nMaybe one day I'll have time to make a proper game out of this. For now hang around for as long as you want. The place is all yours." c
  then then
  timed-dialog-seq ;

: friend-pick-up ( -- )
  ['] friend-intro
  s" you: y'hullo?" 1
  timed-dialog-seq ;

: friend-call-start
  play-ring
  bg-phone-show
  call-init-str 1 print-msg
  ['] friend-pick-up main-loop-continuation ! ;

: friend-call ( -- )
  timed-event-counter @ friend-call-wait-thresh > if
    1 beany obj-idle-override!
    ['] friend-call-start main-loop-continuation !
    ['] timed-events-idle timed-event-continuation !
  then ;

( timed events driver )

: timed-events
  timed-event-continuation @ execute ;


( high-level update functions )

: update-anim ( obj -- )
  update-spr-frame ;

( updates that need to happen within hard boundry of vblank, aka graphics updates )
: update-vblank-hard ( -- )
  update-bg-coord
  spr-to-oam ;

: update-actions ( -- )
  beany obj-toi@ dispatch-toi ;

: in-default ( -- )
  timed-events
  beany dup update-coord
  update-actions
  update-anim ;

: intro-out ( -- )
  key-a key-hit if
    1 attr2-prio-shift lshift beany @ attr2@ or beany @ attr2!
    exit-dialog
  then ;

: intro ( -- )
  vsync
  2 fade-from-black
  vsync

  set-transp-txt-bg
  intro-str2 intro-str1 c print-covid-msg
  ['] intro-out main-loop-continuation ! ;

: mem-1+ ( addr )
  dup @ 1+ swap ! ;

( update things that are less time-sensitive )
: update-loose ( -- )
  timed-event-counter mem-1+
  main-loop-continuation @ execute
;

: update-world
  update-vblank-hard
  update-loose ;


( game loop )

: gloop ( -- )
  begin
    key-poll
    update-world
    ( music buffer stuffing happens in C before vsync irq call )
    vsync
    ( select key-hit )
    exit-apt-seq @ 1 =
  until ;


( initialization )

: font-init
  2 cbb-offs font-tiles-base !
  font-tiles font-tiles-base @ font-len move
  1d sbb-offs font-map-base !
  1c sbb-offs font-bg-map-base !
  set-dialog-dimensions ;

: phone-init
  ( palette is shared with apt palette, automatically cause of setup )
  phone-tiles 1 cbb-offs phone-len move
  phone-map 1b sbb-offs phone-map-len move ;

: beany-init ( -- )
  1 mov-delta !
  78 beany-equ-offs-x !
  3a beany-equ-offs-y !

  beany
  dup set-spr-cb-16x32
  dup 0 swap obj-ticks!
  dup 1 swap obj-idle!
  dup 0 swap obj-idle-override!
  dup down swap obj-dir!
  dup 0 swap obj-tid-base! ( so basically mem-vram-obj )
  obj-coord 90 60 rot store-xy

  snaggle-tiles mem-vram-obj 1800 move
  snaggle-pal mem-pal-obj 200 move

  init-spr-list
  10 alloc-spr beany !
  0 beany @ spr-pal!
  attr0-tall attr0-8bpp or beany-equ-offs-y @ or beany @ attr0!
  beany-equ-offs-x @ attr1-size-16x32 or beany @ attr1! ;

: set-to-black ( -- )
  bld-black bld-all 0 bld-set
  1f bldy-set ;

: apt-graphics-mode-init ( -- )
  ( set up dispcnt and bg control regs for apartment scene )
  dcnt-obj dcnt-obj-1d or dcnt-mode0 or
  dcnt-bg0 or
  dcnt-bg1 or
  ( dcnt-bg2 or )
  dcnt-bg3 or
  reg-dispcnt set-reg

  2 bg-cbb 1d bg-sbb or bg-8bpp or bg-reg-32x32 or 0 or reg-bg0cnt set-reg ( txt )
  0 bg-cbb 1c bg-sbb or bg-8bpp or bg-reg-32x32 or 0 or reg-bg1cnt set-reg ( txt bg )
  1 bg-cbb 1b bg-sbb or bg-8bpp or bg-reg-32x32 or 1 or reg-bg2cnt set-reg ( phone )
  0 bg-cbb 1e bg-sbb or bg-8bpp or bg-reg-64x32 or 1 or reg-bg3cnt set-reg ( apt ) ;


: apt-bg-init ( -- )
  40 map-width !
  20 map-height !
  apt-toi toi-map !

  0  bg-x-max-clamp-mod !
  1d bg-y-max-clamp-mod !

  ( set up bg graphics )
  apt-tiles 0 cbb-offs apt-tiles-len move
  apt-pal mem-pal-bg apt-pal-len move
  apt-map 1e sbb-offs apt-map-len move ;

: timed-event-init
  0 timed-event-counter !
  ['] ( end-seq ) friend-call timed-event-continuation ! ;

: apt-misc-init
  0 fridge-opened !
  0 friend-called !
  0 exit-apt-seq ! ;

( init all )
: apt ( -- )
  key-init
  beany-init
  font-init
  phone-init
  apt-bg-init
  apt-misc-init
  timed-event-init
  ['] in-default main-loop-continuation !
  update-world
  apt-graphics-mode-init
  ['] intro main-loop-continuation ! ( uncomment for intro )
  gloop ;


( splash screen )

: splash-screen-init
  splash 0 cbb-offs splash-len move
  dcnt-mode3 dcnt-bg2 or reg-dispcnt set-reg ;

: splash-loop ( -- )
  begin
    vsync
    key-poll
    key-a key-hit
  until ;

: splash-init ( -- )
  splash-screen-init
  1 fade-from-black ( this one is for the re-entry )
  splash-loop
  1 fade-to-black ;


( end screen )

: end-screen-init
  end 0 cbb-offs end-len move
  dcnt-mode3 dcnt-bg2 or reg-dispcnt set-reg ;

: end-loop ( -- )
  2 fade-from-black
  begin
    vsync
    key-poll
    key-a key-hit if
      2 fade-to-black
      1
    else
      0
    then
  until ;

: fill-txt-phone-tile-indexes
  1c sbb-offs sbb-size 2 * 0 wfill ;

: main-meta-loop ( -- )
  begin
    fill-txt-phone-tile-indexes
    apt
    end-screen-init
    end-loop
    apt-seq-iter mem-1+
    covid-date mem-1+
    0
  until ;

( forth main )

: init
  ( init-spr )
  1000 covid-date !
  0 apt-seq-iter !
  init-music
  start-music
  splash-init ( uncomment for intro )
  main-meta-loop ;

( implement this at some point: commercial rom WAITCNT settings )
( REG_WAITCNT = 0x4317 )
