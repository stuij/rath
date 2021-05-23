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
  13 constant dcnt-win-shift

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
  14 constant bg-size-shift

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
  12 constant mos-ov-shift

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

( blend weights )
mem-io 052 + constant reg-bldalpha ( blend alpha )

001f constant bld-eva-mask
   0 constant bld-eva-shift

1f00 constant bld-evb-mask
   8 constant bld-evb-shift

( fade levels )
mem-io 054 + constant reg-bldy

001f constant bldy-mask
   0 constant bldy-shift


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

: set-reg h! ; ( val addr -- )

( wait for vblank interrupt )
: vsync 1 1 bdos drop ; ( -- )

( wait for vblank interrupt )
: start-music 1 2 bdos drop ; ( -- )
: stop-music 1 3 bdos drop ; ( -- )

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
: spr-x@ attr1@ 01ff and ; ( spr -- x )
: spr-x! dup attr1@ fe00 and rot 01ff and or swap attr1! ; ( x spr -- )

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

( key handling )

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
  key-curr @ key-prev @ and and ;

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


( sprite objects )

: obj-spr@ @ ; ( obj -- spr )
: obj-spr! ! ; ( spr obj -- )
: obj-coord cell + ; ( obj -- coord-addr )
: obj-coord@ cell + @ ; ( obj -- coord )
: obj-coord! cell + ! ; ( obj -- coord )
: obj-dir@ 2 cells + h@ ; ( obj -- dir )
: obj-dir! 2 cells + h! ; ( dir obj -- )
: obj-actions@ a + h@ ; ( obj -- actions )
: obj-actions! a + h! ; ( actions obj -- )
: obj-cb-offs 3 cells + ; ( obj -- coord ) ( object collision box offset = offset x 4 )
: obj-frame@ 7 cells + h@ ; ( obj -- frame )
: obj-frame! 7 cells + h! ; ( frame obj -- )

1e constant obj-size

( a collision box offset is counted in tiles. it has both an x and a y component, )
( both of which are a halfword long. so they too can be extracted with the x and y accessors )
: cb-offs-ul ;           ( cb-coords -- ul ) ( return upper left collision box offset )
: cb-offs-ur cell + ;    ( cb-coords -- ur ) ( return upper right collision box offset )
: cb-offs-ll 2 cells + ; ( cb-coords -- ll ) ( return lower left collision box offset )
: cb-offs-lr 3 cells + ; ( cb-coords -- lr ) ( return lower right collision box offset )

obj-size array beany ( beany obj ) ( beany == player character )


( coordinate mapping, collision detection, things of interest )

variable mov-delta
4 array bg-coord
variable map-width
variable map-height
variable toi-map

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

: get-toi toi-map @ + c@ ; ( offs-tile )

( check thing of interest tiles for sprite collision-box - max 2x2 collision box tiles!! )
: check-toi-map ( coord cb-offs-addr -- toi )
  4 0 do 2dup i 4 * + @ get-offs-tile get-toi -rot loop
  2drop ( toi toi toi toi ) or or or ;

: new-x-delta key-tri-horz mov-delta @ * ; ( -- new-x )
: get-nxt-x x-get new-x-delta + ; ( coord -- )
: new-y-delta key-tri-vert mov-delta @ * ; ( -- new-y )
: get-nxt-y y-get new-y-delta + ; ( coord -- )
: get-nxt-coord dup get-nxt-x swap get-nxt-y x-set ; ( coord -- nxt-coord )

: update-bg-x dup bg-coord x! reg-bg0hofs h! ; ( bg-x-coord -- )
: update-bg-y dup bg-coord y! reg-bg0vofs h! ; ( bg-y-coord -- )
: update-bg-coord dup x-get update-bg-x y-get update-bg-y ; ( coord )

: spr-to-x-bg-coord  x-get swap spr-x@ - ; ( spr coord -- x-bg-coord )
: spr-to-y-bg-coord  y-get swap spr-y@ - ; ( spr coord -- y-bg-coord )

( we are here )
: beany-to-bg-coord  ( new-coord obj -- bg-coord )
  obj-spr@ swap 2dup spr-to-x-bg-coord -rot spr-to-y-bg-coord x-set ;

: update-coord  ( obj -- ) ( currently the only thing of interest is a collision )
  dup obj-coord@ get-nxt-coord 2dup swap obj-cb-offs check-toi-map not ( obj nxt-coord !toi )
  if
    2dup swap obj-coord!
    swap beany-to-bg-coord update-bg-coord
  else
    2drop
  then ;


( game loop )

: gloop ( -- )
  start-music
  begin
    vsync
    key-poll
    beany update-coord
    spr-to-oam
    select key-hit
  until
  stop-music ;


( initialization )

: set-cb-offs-8x16 ( obj-cb-offs --  ) ( todo )
  dup cb-offs-ul 0 1 rot store-xy
  dup cb-offs-ur 0 2 rot store-xy
  dup cb-offs-ll 1 1 rot store-xy
  cb-offs-lr 1 2 rot store-xy ;

: set-cb-offs-16x32 ( obj-cb-offs --  )
  dup cb-offs-ul 0 3 rot store-xy
  dup cb-offs-ur 0 4 rot store-xy
  dup cb-offs-ll 2 3 rot store-xy
  cb-offs-lr 2 4 rot store-xy ;

: beany-init ( -- )
  beany
  dup obj-cb-offs
  set-cb-offs-16x32

  dup 0 swap obj-frame!
  dup 0 swap obj-dir!
  obj-coord 90 60 rot store-xy

  snaggle-tiles mem-vram-obj 100 move
  snaggle-pal mem-pal-obj 200 move

  init-spr-list
  10 alloc-spr beany !
  0 beany @ spr-pal!
  attr0-tall 50 or beany @ attr0!
  78 attr1-size-16x32 or beany @ attr1!
  spr-to-oam ;

: init-graphics ( -- )
  ( set up dispcnt and bg control regs )
  dcnt-obj dcnt-obj-1d or dcnt-mode0 or dcnt-bg0 or reg-dispcnt set-reg
  0 bg-cbb 1e bg-sbb or bg-8bpp or bg-reg-64x32 or reg-bg0cnt set-reg

  ( set up bg graphics )
  apt-tiles mem-vram-bg apt-tiles-len move
  apt-pal mem-pal-bg apt-pal-len move
  apt-map sbb-size 1e * mem-vram + apt-map-len move ;

( init all )
: init ( -- )
  1 mov-delta !
  40 map-width !
  20 map-height !
  apt-toi toi-map !

  init-graphics
  beany-init
  key-init
  gloop ;
