hex

( helpers - a bunch, if not all, of these should travel to assembly )

: -rot rot rot ; ( a b c -- c a b )
: not 0= ; ( bool -- inverse-bool )
: 0> dup 0< swap 0= or not ; ( nr -- nr-positive? )
: <= 2dup = -rot swap > or ; ( a b - res )
: >= 2dup = -rot swap < or ; ( a b - res )

( mem map )

02000000 constant mem-ewram
03000000 constant mem-iwram
04000000 constant mem-io
05000000 constant mem-pal
06000000 constant mem-vram
07000000 constant mem-oam
08000000 constant mem-rom
0e000000 constant mem-sram

4000 constant cbb-size ( charblock size )
0800 constant sbb-size ( screenblock size )

mem-vram         constant mem-vram-bg
mem-vram 10000 + constant mem-vram-obj
mem-pal          constant mem-pal-bg
mem-pal 200 +    constant mem-pal-obj


( registers )

( display )
04000000 constant reg-dispcnt ( display control )
04000004 constant reg-dispstat ( display status )
04000006 constant reg-vcount ( scanline count )

( bg control registers)
04000008 constant reg-bg0cnt ( bg0 control )
0400000a constant reg-bg1cnt ( bg1 control )
0400000c constant reg-bg2cnt ( bg2 control )
0400000e constant reg-bg3cnt ( bg3 control )

( bg scroll registers, write only )
04000010 constant reg-bg0hofs ( bg0 horizontal scroll )
04000012 constant reg-bg0vofs ( bg0 vertical scroll )
04000014 constant reg-bg1hofs ( bg1 horizontal scroll )
04000016 constant reg-bg1vofs ( bg1 vertical scroll )
04000018 constant reg-bg2hofs ( bg2 horizontal scroll )
0400001a constant reg-bg2vofs ( bg2 vertical scroll )
0400001c constant reg-bg3hofs ( bg3 horizontal scroll )
0400001e constant reg-bg3vofs ( bg3 vertical scroll )

( graphic effects bg/sprites )
0400004c constant reg-mosaic ( mosaic control )
04000050 constant reg-bldcnt ( alpha control )
04000052 constant reg-bldalpha ( fade level )
04000054 constant reg-bldy ( blend levels )

( timer registers )
04000100 constant reg-tm0d ( timer 0 data )
04000102 constant reg-tm0cnt ( timer 0 control )
04000104 constant reg-tm1d ( timer 1 data )
04000106 constant reg-tm1cnt ( timer 1 control )
04000108 constant reg-tm2d ( timer 2 data )
0400010a constant reg-tm2cnt ( timer 2 control )
0400010c constant reg-tm3d ( timer 3 data )
0400010e constant reg-tm3cnt ( timer 3 control )

( keypad registers )
04000130 constant reg-keyinput ( key status, read only?? )
04000132 constant reg-keycnt ( key irq control )

( interrupt / system registers )
04000200 constant reg-ie ( irq enable )
04000202 constant reg-if ( irq status/acknowledge )
04000204 constant reg-waitcnt ( waitstate control )
04000208 constant reg-ime ( irq master enable )
04000300 constant reg-pause ( pause system ? )


( sprite handling )

02001000 constant spr-start ( shadow sprite start )
variable spr-head ( head of sprite linked list )
( if there have been more sprites dealloced than alloced this frame, )
( we need to erase the surplus in OAM. spr-deallocs saves the tally )
variable spr-deallocs

: attr0@ h@ ;     ( spr -- attr0 )
: attr1@ 2 + h@ ; ( spr -- attr1 )
: attr2@ 4 + h@ ; ( spr -- atr2 )
: attr0! h! ;     ( attr0 spr -- attr0 )
: attr1! 2 + h! ; ( attr1 spr -- attr1 )
: attr2! 4 + h! ; ( attr2 spr -- atr2 )

: spr-y@ c@ ; ( spr -- y )
: spr-y! c! ; ( y spr -- )
: spr-x@ attr1@ 01ff and ; ( spr -- x )
: spr-x! dup attr1@ fe00 and rot or swap attr1! ; ( x spr -- )

: spr-pal@ attr2@ c rshift ; ( spr -- pal )
: spr-pal! dup attr2@ 0fff and rot c lshift or swap ! ; ( pal spr -- )

: spr-z@ 7 + c@ ; ( spr - z-depth )
: spr-z! 7 + c! ; ( spr - z-depth )

: spr-i-to-addr 8 * spr-start + ; ( index -- spr )
: spr-addr-to-i spr-start - 3 rshift ; ( spr -- index )
: child-spr@ 6 + c@ spr-i-to-addr ; ( spr -- child-spr )
: child-spr! swap spr-addr-to-i swap 6 + c! ; ( child-spr spr -- )

: clear-spr dup 0 swap ! 0 swap 4 + ! ; ( spr -- )
: clear-oam-spr dup 0 swap ! 0 swap 2 + h! ; ( spr -- )
: copy-spr 6 hmove ; ( spr addr -- )

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
: find-spr-insert ( z-depth -- spr )
  spr-head @ ( z-depth spr-head )
  dup spr-z@ rot tuck <=
  if ( spr-head z-depth )
    2drop 0
  else
    over child-spr@
    begin ( higher-spr z-depth curr-spr )
      2dup spr-z@ >= not ( higher or equal to z-depth of current sprite )
    while ( higher-spr z-depth curr-spr )
      rot drop dup -rot child-spr@
    repeat
    2drop
  then ;

: alloc-spr ( z-depth -- spr )
  free-spr-addr dup clear-spr 2dup spr-z! swap find-spr-insert ( free-addr top-spr-addr )
  dup 0= if
    drop ( free-addr ) ( no need to insert in top spr, set head to new addr)
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


( wait for vblank interrupt )
: vsync 1 1 bdos drop ; ( -- )


( keys )

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
4000 constant kcnt_irq ( enable key irq )
0000 constant kcnt_or  ( interrupt on any of selected keys )
8000 constant kcnt_and ( interrupt on all of selected keys )

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

: bit-to-bool ( x bit -- bool)
  rshift 1 and ;

: bit-tribool ( minus plus x -- tri-state )
  tuck swap bit-to-bool -rot swap bit-to-bool - ;

: key-tri-horz ki-left ki-right key-curr @ bit-tribool ; ( -- +/- )
: key-tri-vert ki-up ki-down key-curr @ bit-tribool ; ( -- +/- )


( testing )
init-spr-list
10 alloc-spr constant beany
beany-tiles mem-vram-obj 32 move
beany-pal mem-pal-obj 32 move
0 beany spr-pal!
95 beany spr-x! 80 beany spr-y!
key-init
spr-to-oam

( game loop )
: game-loop ( -- )
  begin
    vsync
    key-poll
    beany spr-x@ key-tri-horz + beany spr-x!
    beany spr-y@ key-tri-vert + beany spr-y!
    spr-to-oam
    select key-hit
  until ;

game-loop
