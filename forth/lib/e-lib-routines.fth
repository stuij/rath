hex

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
  95 beany @ spr-x! 80 beany @ spr-y!
  key-init
  spr-to-oam
  game-loop ;

init

