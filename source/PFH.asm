# SYSTEM VARIABLES & CONSTANTS ==================

#C BL      -- char            an ASCII space
    head bl,2,"bl",docon,storedest
        .word 0x20

#Z tibsize  -- n         size of TIB
    head tibsize,7,"tibsize",docon,bl
        .word 124          /* 2 chars safety zone */

#X tib     -- a-addr     Terminal Input Buffer
#  HEX 82 CONSTANT TIB   CP/M systems: 126 bytes
#  HEX -80 USER TIB      others: below user area
    head tib,3,"tib",dovar,tibsize
.space 128

#Z u0      -- a-addr       current user area adrs
    codeh u0,2,"u0",tib
	str	r1, [sp, #-4]!	/* push TOS */
        mov	r1, r2
        next

#C >IN     -- a-addr        holds offset into TIB
    head toin,3,">in",douser,u0
        .word 4

#C BASE    -- a-addr       holds conversion radix
    head base,4,"base",douser,toin
        .word 8

#C STATE   -- a-addr       holds compiler state
    head state,5,"state",douser,base
        .word 12

#Z dp      -- a-addr       holds dictionary ptr
    head dp,2,"dp",douser,state
    	.word 16

#Z 'source  -- a-addr      two cells: len, adrs
    head ticksource,7,"'source",douser,dp
    	.word 20

#Z latest    -- a-addr     last word in dict.
    head latest,6,"latest",douser,ticksource
    	.word 28

#Z hp       -- a-addr     HOLD pointer
    head hp,2,"hp",douser,latest
    	.word 32

#Z LP       -- a-addr     Leave-stack pointer
    head lp,2,"lp",douser,hp
    	.word 36

#Z s0       -- a-addr     end of parameter stack
    codeh s0,2,"s0",lp
    	str	r1, [sp, #-4]!	/* push old TOS */
	ldr	r1, [r10]
    	next

#X PAD       -- a-addr    user PAD buffer
#                         = end of hold area!
    codeh pad,3,"pad",s0
    	str	r1, [sp, #-4]!	/* push old TOS */
	ldr	r1, [r10, #12]
    	next

#Z l0       -- a-addr     bottom of Leave stack
    head l0,2,"l0",dovar,pad
.space 128

#Z r0       -- a-addr     end of return stack
    codeh r0,2,"r0",l0
    	str	r1, [sp, #-4]!	/* push old TOS */
	ldr	r1, [r10, #4]
    	next

#Z uinit    -- addr  initial values for user area
    head uinit,5,"uinit",docreate,r0
        .word 0x12345678,0,10,0     /* reserved (UNUSED),>IN,BASE,STATE */
        .word enddict      /* DP */
        .word 0,0          /* SOURCE init'd elsewhere */
        .word lastword     /* LATEST */
        .word 0            /* HP init'd elsewhere */

#Z #init    -- n    #bytes of user area init data
    head ninit,5,"#init",docon,uinit
        .word 36

# ARITHMETIC OPERATORS ==========================

#C S>D    n -- d          single -> double prec.
#   DUP 0< ;
    head stod,3,"s>d",docolon,ninit
        .word dup,zeroless,exit

#Z ?NEGATE  n1 n2 -- n3  negate n1 if n2 negative
#_   0< IF NEGATE THEN ;        ...a common factor
    head qnegate,7,"?negate",docolon,stod
        .word zeroless,qbranch,qneg1,negate
qneg1:  .word exit

#C ABS     n1 -- +n2     absolute value
#   DUP ?NEGATE ;
    head abs,3,"abs",docolon,qnegate
        .word dup,qnegate,exit

#X DNEGATE   d1 -- d2     negate double precision
#   SWAP INVERT SWAP INVERT 1 M+ ;
    head dnegate,7,"dnegate",docolon,abs
        .word swap,invert,swap,invert,lit,1,mplus
        .word exit

#Z ?DNEGATE  d1 n -- d2   negate d1 if n negative
#_   0< IF DNEGATE THEN ;       ...a common factor
    head qdnegate,8,"?dnegate",docolon,dnegate
        .word zeroless,qbranch,dneg1,dnegate
dneg1:  .word exit

#X DABS     d1 -- +d2    absolute value dbl.prec.
#   DUP ?DNEGATE ;
    head dabs,4,"dabs",docolon,qdnegate
        .word dup,qdnegate,exit

#C M*     n1 n2 -- d    signed 16*16->32 multiply
#_   2DUP XOR >R        carries sign of the result
#   SWAP ABS SWAP ABS UM*
#   R> ?DNEGATE ;
    head mstar,2,"m*",docolon,dabs
        .word twodup,xor,tor
        .word swap,abs,swap,abs,umstar
        .word rfrom,qdnegate,exit

#C SM/REM   d1 n1 -- n2 n3   symmetric signed div
#_   2DUP XOR >R              sign of quotient
#   OVER >R                  sign of remainder
#   ABS >R DABS R> UM/MOD
#   SWAP R> ?NEGATE
#   SWAP R> ?NEGATE ;
# Ref. dpANS-6 section 3.2.2.1.
    head smslashrem,6,"sm/rem",docolon,mstar
        .word twodup,xor,tor,over,tor
        .word abs,tor,dabs,rfrom,umslashmod
        .word swap,rfrom,qnegate,swap,rfrom,qnegate
        .word exit

#C FM/MOD   d1 n1 -- n2 n3   floored signed div'n
#   DUP >R              save divisor
#   SM/REM
#   DUP 0< IF           if quotient negative,
#       SWAP R> +         add divisor to rem'dr
#       SWAP 1-           decrement quotient
#   ELSE R> DROP THEN ;
# Ref. dpANS-6 section 3.2.2.1.
    head fmslashmod,6,"fm/mod",docolon,smslashrem
        .word dup,tor,smslashrem
        .word dup,zeroless,qbranch,fmmod1
        .word swap,rfrom,plus,swap,oneminus
        .word branch,fmmod2
fmmod1: .word rfrom,drop
fmmod2: .word exit

#C *      n1 n2 -- n3       signed multiply
#   M* DROP ;
    head star,1,"*",docolon,fmslashmod
        .word mstar,drop,exit

#C /MOD   n1 n2 -- n3 n4    signed divide/rem'dr
#   >R S>D R> FM/MOD ;
    head slashmod,4,"/mod",docolon,star
        .word tor,stod,rfrom,fmslashmod,exit

#C /      n1 n2 -- n3       signed divide
#   /MOD nip ;
    head slash,1,"/",docolon,slashmod
        .word slashmod,nip,exit

#C MOD    n1 n2 -- n3       signed remainder
#   /MOD DROP ;
    head mod,3,"mod",docolon,slash
        .word slashmod,drop,exit

#C */MOD  n1 n2 n3 -- n4 n5    n1*n2/n3, rem&quot
#   >R M* R> FM/MOD ;
    head ssmod,5,"*/mod",docolon,mod
        .word tor,mstar,rfrom,fmslashmod,exit

#C */     n1 n2 n3 -- n4        n1*n2/n3
#   */MOD nip ;
    head starslash,2,"*/",docolon,ssmod
        .word ssmod,nip,exit

#C MAX    n1 n2 -- n3       signed maximum
#_   2DUP < IF SWAP THEN DROP ;
    head max,3,"max",docolon,starslash
        .word twodup,less,qbranch,max1,swap
max1:   .word drop,exit

#C MIN    n1 n2 -- n3       signed minimum
#_   2DUP > IF SWAP THEN DROP ;
    head min,3,"min",docolon,max
        .word twodup,greater,qbranch,min1,swap
min1:   .word drop,exit

# DOUBLE OPERATORS ==============================

#C 2@    a-addr -- x1 x2    fetch 2 cells
#   DUP CELL+ @ SWAP @ ;
#   the lower address will appear on top of stack
    head twofetch,2,"2@",docolon,min
        .word dup,cellplus,fetch,swap,fetch,exit

#C 2!    x1 x2 a-addr --    store 2 cells
#   SWAP OVER ! CELL+ ! ;
#   the top of stack is stored at the lower adrs
    head twostore,2,"2!",docolon,twofetch
        .word swap,over,store,cellplus,store,exit

#C 2DROP  x1 x2 --          drop 2 cells
#   DROP DROP ;
    head twodrop,5,"2drop",docolon,twostore
        .word drop,drop,exit

#C 2DUP   x1 x2 -- x1 x2 x1 x2   dup top 2 cells
#   OVER OVER ;
    head twodup,4,"2dup",docolon,twodrop
        .word over,over,exit

#C 2SWAP  x1 x2 x3 x4 -- x3 x4 x1 x2  per diagram
#   ROT >R ROT R> ;
    head twoswap,5,"2swap",docolon,twodup
        .word rot,tor,rot,rfrom,exit

#C 2OVER  x1 x2 x3 x4 -- x1 x2 x3 x4 x1 x2
#   >R >R 2DUP R> R> 2SWAP ;
    head twoover,5,"2over",docolon,twoswap
        .word tor,tor,twodup,rfrom,rfrom
        .word twoswap,exit

# INPUT/OUTPUT ==================================

#C COUNT   c-addr1 -- c-addr2 u  counted->adr/len
#   DUP CHAR+ SWAP C@ ;
    head count,5,"count",docolon,twoover
        .word dup,charplus,swap,cfetch,exit

#C CR      --               output newline
#_   0D EMIT 0A EMIT ;
    head cr,2,"cr",docolon,count
        .word lit,0x0a,emit,exit

#C SPACE   --               output a space
#   BL EMIT ;
    head space,5,"space",docolon,cr
        .word bl,emit,exit

#C SPACES   n --            output n spaces
#   BEGIN DUP WHILE SPACE 1- REPEAT DROP ;
    head spaces,6,"spaces",docolon,space
spcs1:  .word dup,qbranch,spcs2
        .word space,oneminus,branch,spcs1
spcs2:  .word drop,exit

#Z umin     u1 u2 -- u      unsigned minimum
#_   2DUP U> IF SWAP THEN DROP ;
    head umin,4,"umin",docolon,spaces
        .word twodup,ugreater,qbranch,umin1,swap
umin1:  .word drop,exit

#Z umax    u1 u2 -- u       unsigned maximum
# _  2DUP U< IF SWAP THEN DROP ;
    head umax,4,"umax",docolon,umin
        .word twodup,uless,qbranch,umax1,swap
umax1:  .word drop,exit

#C ACCEPT  c-addr +n -- +n'  get line from term'l
#   OVER + 1- OVER      -- sa ea a
#   BEGIN KEY           -- sa ea a c
#   DUP 0D <> WHILE
#       DUP EMIT        -- sa ea a c
#       DUP 8 = IF  DROP 1-    >R OVER R> UMAX
#             ELSE  OVER C! 1+ OVER UMIN
#       THEN            -- sa ea a
#   REPEAT              -- sa ea a c
#   DROP NIP SWAP - ;
    head accept,6,"accept",docolon,umax
        .word over,plus,oneminus,over
acc1:   .word key,dup,lit,0x0a,notequal,qbranch,acc5
        .word dup,emit,dup,lit,8,equal,qbranch,acc3
        .word drop,oneminus,tor,over,rfrom,umax
        .word branch,acc4
acc3:   .word over,cstore,oneplus,over,umin
acc4:   .word branch,acc1
acc5:   .word drop,nip,swap,minus,exit

#C TYPE    c-addr +n --     type line to term'l
#   ?DUP IF
#     OVER + SWAP DO I C@ EMIT LOOP
#   ELSE DROP THEN ;
    head type,4,"type",docolon,accept
        .word qdup,qbranch,typ4
        .word over,plus,swap,xdo
typ3:   .word ii,cfetch,emit,xloop,typ3
        .word branch,typ5
typ4:   .word drop
typ5:   .word exit

#Z (S")     -- c-addr u   run-time code for S"
#   R> COUNT 2DUP + ALIGNED >R  ;
    head xsquote,4,"(s\")",docolon,type
        .word rfrom,count,twodup,plus,aligned
        .word tor
        .word exit

#C S"       --         compile in-line string
#   COMPILE (S")  [ HEX ]
#   22 WORD C@ 1+ ALIGNED ALLOT ; IMMEDIATE
    immed squote,2,"s\"",docolon,xsquote
        .word lit,xsquote,commaxt
        .word lit,0x22,word,cfetch,oneplus
        .word aligned,allot,exit

#C ."       --         compile string to print
#   POSTPONE S"  POSTPONE TYPE ; IMMEDIATE
    immed dotquote,2,".\"",docolon,squote
        .word squote
        .word lit,type,commaxt
        .word exit

# NUMERIC OUTPUT ================================
# Numeric conversion is done l.s.digit first, so
# the output buffer is built backwards in memory.

# Some double-precision arithmetic operators are
# needed to implement ANSI numeric conversion.

#Z UD/MOD   ud1 u2 -- u3 ud4   32/16->32 divide
#   >R 0 R@ UM/MOD  ROT ROT R> UM/MOD ROT ;
    head udslashmod,6,"ud/mod",docolon,dotquote
        .word tor,lit,0,rfetch,umslashmod,rot,rot
        .word rfrom,umslashmod,rot,exit

#Z UD*      ud1 d2 -- ud3      32*16->32 multiply
#   DUP >R UM* DROP  SWAP R> UM* ROT + ;
    head udstar,3,"ud*",docolon,udslashmod
        .word dup,tor,umstar,drop
        .word swap,rfrom,umstar,rot,plus,exit

#C HOLD  char --        add char to output string
#   -1 HP +!  HP @ C! ;
    head hold,4,"hold",docolon,udstar
        .word lit,-1,hp,plusstore
        .word hp,fetch,cstore,exit

#C <#    --             begin numeric conversion
#   PAD HP ! ;          (initialize Hold Pointer)
    head lessnum,2,"<#",docolon,hold
        .word pad,hp,store,exit

#Z >digit   n -- c      convert to 0..9A..Z
#   [ HEX ] DUP 9 > 7 AND + 30 + ;
    head todigit,6,">digit",docolon,lessnum
        .word dup,lit,0x9,greater,lit,0x27,and,plus
        .word lit,0x30,plus,exit

#C #     ud1 -- ud2     convert 1 digit of output
#   BASE @ UD/MOD ROT >digit HOLD ;
    head num,1,"#",docolon,todigit
        .word base,fetch,udslashmod,rot,todigit
        .word hold,exit

#C #S    ud1 -- ud2     convert remaining digits
#   BEGIN # 2DUP OR 0= UNTIL ;
    head nums,2,"#s",docolon,num
nums1:  .word num,twodup,or,zeroequal,qbranch,nums1
        .word exit

#C #>    ud1 -- c-addr u    end conv., get string
#   2DROP HP @ PAD OVER - ;
    head numgreater,2,"#>",docolon,nums
        .word twodrop,hp,fetch,pad,over,minus
        .word exit

#C SIGN  n --           add minus sign if n<0
#_   0< IF 2D HOLD THEN ;
    head sign,4,"sign",docolon,numgreater
        .word zeroless,qbranch,sign1,lit,0x2d,hold
sign1:  .word exit

#C U.    u --           display u unsigned
#   <# 0 #S #> TYPE SPACE ;
    head udot,2,"u.",docolon,sign
        .word lessnum,lit,0,nums,numgreater,type
        .word space,exit

#C .     n --           display n signed
#   <# DUP ABS 0 #S ROT SIGN #> TYPE SPACE ;
    head dot,1,".",docolon,udot
        .word lessnum,dup,abs,lit,0,nums
        .word rot,sign,numgreater
        .word type,space,exit

#C DECIMAL  --      set number base to decimal
#   10 BASE ! ;
    head decimal,7,"decimal",docolon,dot
        .word lit,10,base,store,exit

#X HEX     --       set number base to hex
#   16 BASE ! ;
    head hex,3,"hex",docolon,decimal
        .word lit,16,base,store,exit

# DICTIONARY MANAGEMENT =========================

#C HERE    -- addr      returns dictionary ptr
#   DP @ ;
    head here,4,"here",docolon,hex
        .word dp,fetch,exit

#C ALLOT   n --         allocate n bytes in dict
#   DP +! ;
    head allot,5,"allot",docolon,here
        .word dp,plusstore,exit

# Note: , and C, are only valid for combined
# Code and Data spaces.

#C ,    x --           append cell to dict
#   HERE ! 1 CELLS ALLOT ;
    head comma,1,",",docolon,allot
        .word here,store,lit,1,cells,allot,exit

#C C,   char --        append char to dict
#   HERE C! 1 CHARS ALLOT ;
    head ccomma,2,"c,",docolon,comma
        .word here,cstore,lit,1,chars,allot,exit

# INTERPRETER ===================================
# Note that NFA>LFA, NFA>CFA, IMMED?, and FIND
# are dependent on the structure of the Forth
# header.  This may be common across many CPUs,
# or it may be different.

#C SOURCE   -- adr n    current input buffer
#   'SOURCE 2@ ;        length is at lower adrs
    head source,6,"source",docolon,ccomma
        .word ticksource,twofetch,exit

#X /STRING  a u n -- a+n u-n   trim string
#   ROT OVER + ROT ROT - ;
    head slashstring,7,"/string",docolon,source
        .word rot,over,plus,rot,rot,minus,exit

#Z >counted  src n dst --     copy to counted str
#   2DUP C! CHAR+ SWAP CMOVE ;
    head tocounted,8,">counted",docolon,slashstring
        .word twodup,cstore,charplus,swap,cmove,exit

#C WORD   char -- c-addr n   word delim'd by char
#   DUP  SOURCE >IN @ /STRING   -- c c adr n
#   DUP >R   ROT SKIP           -- c adr' n'
#   OVER >R  ROT SCAN           -- adr" n"
#   DUP IF CHAR- THEN        skip trailing delim.
#   R> R> ROT -   >IN +!        update >IN offset
#   TUCK -                      -- adr' N
#   HERE >counted               --
#   HERE                        -- a
#   BL OVER COUNT + C! ;    append trailing blank
    head word,4,"word",docolon,tocounted
        .word dup,source,toin,fetch,slashstring
        .word dup,tor,rot,skip
        .word over,tor,rot,scan
        .word dup,qbranch,word1,charminus
word1:  .word rfrom,rfrom,rot,minus,toin,plusstore
        .word tuck,minus
        .word here,tocounted,here
        .word bl,over,count,plus,cstore,exit

#Z NFA>LFA   nfa -- lfa    name adr -> link field
#_   3 - ;
# Here -8
    head nfatolfa,7,"nfa>lfa",docolon,word
        .word lit,8,minus,exit

#Z NFA>CFA   nfa -- cfa    name adr -> code field
#   COUNT 7F AND + ;       mask off 'smudge' bit
# Needs testing
    head nfatocfa,7,"nfa>cfa",docolon,nfatolfa
        .word count,lit,0x7f,and,plus
        .word aligned
        .word exit

#Z IMMED?    nfa -- f      fetch immediate flag
#_   1- C@ ;                     nonzero if immed
# Here -4
    head immedq,6,"immed?",docolon,nfatocfa
        .word lit,4,minus,fetch,exit

#C FIND   c-addr -- c-addr 0   if not found
#C                  xt  1      if immediate
#C                  xt -1      if "normal"
#   LATEST @ BEGIN             -- a nfa
#       2DUP OVER C@ CHAR+     -- a nfa a nfa n+1
#       S=                     -- a nfa f
#       DUP IF
#           DROP
#           NFA>LFA @ DUP      -- a link link
#       THEN
#_   0= UNTIL                   -- a nfa  OR  a 0
#   DUP IF
#       NIP DUP NFA>CFA        -- nfa xt
#       SWAP IMMED?            -- xt iflag
#_       0= 1 OR                -- xt 1/-1
#   THEN ;
    head find,4,"find",docolon,immedq
        .word latest,fetch
find1:  .word twodup,over,cfetch,charplus
        .word sequal,dup,qbranch,find2
        .word drop,nfatolfa,fetch,dup
find2:  .word zeroequal,qbranch,find1
        .word dup,qbranch,find3
        .word nip,dup,nfatocfa
        .word swap,immedq,zeroequal,lit,1,or
find3:  .word exit

#C LITERAL  x --        append numeric literal
#   STATE @ IF ['] LIT ,XT , THEN ; IMMEDIATE
# This tests STATE so that it can also be used
# interpretively.  (ANSI doesn't require this.)
    immed literal,7,"literal",docolon,find
        .word state,fetch,qbranch,liter1
        .word lit,lit,commaxt,comma
liter1: .word exit

#Z DIGIT?   c -- n -1   if c is a valid digit
#Z            -- x  0   otherwise
#   [ HEX ] DUP 39 > 100 AND +     silly looking
#   DUP 140 > 107 AND -   30 -     but it works!
#   DUP BASE @ U< ;
    head digitq,6,"digit?",docolon,literal
        .word dup,lit,0x39,greater,lit,0x100,and,plus
        .word dup,lit,0x160,greater,lit,0x127,and
        .word minus,lit,0x30,minus
        .word dup,base,fetch,uless,exit

#Z ?SIGN   adr n -- adr' n' f  get optional sign
#Z  advance adr/n if sign; return NZ if negative
#   OVER C@                 -- adr n c
#_   2C - DUP ABS 1 = AND    -- +=-1, -=+1, else 0
#   DUP IF 1+               -- +=0, -=+2
#       >R 1 /STRING R>     -- adr' n' f
#   THEN ;
    head qsign,5,"?sign",docolon,digitq
        .word over,cfetch,lit,0x2c,minus,dup,abs
        .word lit,1,equal,and,dup,qbranch,qsign1
        .word oneplus,tor,lit,1,slashstring,rfrom
qsign1: .word exit

#C >NUMBER  ud adr u -- ud' adr' u'
#C                      convert string to number
#   BEGIN
#   DUP WHILE
#       OVER C@ DIGIT?
#_       0= IF DROP EXIT THEN
#       >R 2SWAP BASE @ UD*
#       R> M+ 2SWAP
#_       1 /STRING
#   REPEAT ;
    head tonumber,7,">number",docolon,qsign
tonum1: .word dup,qbranch,tonum3
        .word over,cfetch,digitq
        .word zeroequal,qbranch,tonum2,drop,exit
tonum2: .word tor,twoswap,base,fetch,udstar
        .word rfrom,mplus,twoswap
        .word lit,1,slashstring,branch,tonum1
tonum3: .word exit

#Z ?NUMBER  c-addr -- n -1      string->number
#Z                 -- c-addr 0  if convert error
#   DUP  0 0 ROT COUNT      -- ca ud adr n
#   ?SIGN >R  >NUMBER       -- ca ud adr' n'
#   IF   R> 2DROP 2DROP 0   -- ca 0   (error)
#   ELSE 2DROP NIP R>
#       IF NEGATE THEN  -1  -- n -1   (ok)
#   THEN ;
    head qnumber,7,"?number",docolon,tonumber
        .word dup,lit,0,dup,rot,count
        .word qsign,tor,tonumber,qbranch,qnum1
        .word rfrom,twodrop,twodrop,lit,0
        .word branch,qnum3
qnum1:  .word twodrop,nip,rfrom,qbranch,qnum2,negate
qnum2:  .word lit,-1
qnum3:  .word exit

#Z INTERPRET    i*x c-addr u -- j*x
#Z                      interpret given buffer
# This is a common factor of EVALUATE and QUIT.
# ref. dpANS-6, 3.4 The Forth Text Interpreter
#   'SOURCE 2!  0 >IN !
#   BEGIN
#   BL WORD DUP C@ WHILE        -- textadr
#       FIND                    -- a 0/1/-1
#       ?DUP IF                 -- xt 1/-1
#_           1+ STATE @ 0= OR    immed or interp?
#           IF EXECUTE ELSE ,XT THEN
#       ELSE                    -- textadr
#           ?NUMBER
#           IF POSTPONE LITERAL     converted ok
#           ELSE COUNT TYPE 3F EMIT CR ABORT  err
#           THEN
#       THEN
#   REPEAT DROP ;
    head interpret,9,"interpret",docolon,qnumber
        .word ticksource,twostore,lit,0,toin,store
inter1: .word bl,word,dup,cfetch,qbranch,inter9
        .word find
        .word qdup,qbranch,inter4
        .word oneplus,state,fetch
        .word zeroequal,or
        .word qbranch,inter2
        .word execute
        .word branch,inter3
inter2: .word commaxt
inter3: .word branch,inter8
inter4: .word qnumber,qbranch,inter5
        .word literal,branch,inter6
inter5: .word count,type,lit,0x3f,emit,cr,abort
inter6:
inter8: .word branch,inter1
inter9: .word drop,exit

#C EVALUATE  i*x c-addr u -- j*x  interprt string
#   'SOURCE 2@ >R >R  >IN @ >R
#   INTERPRET
#   R> >IN !  R> R> 'SOURCE 2! ;
    head evaluate,8,"evaluate",docolon,interpret
        .word ticksource,twofetch,tor,tor
        .word toin,fetch,tor,interpret
        .word rfrom,toin,store,rfrom,rfrom
        .word ticksource,twostore,exit

#C QUIT     --    R: i*x --    interpret from kbd
#   L0 LP !  R0 RP!   0 STATE !
#   BEGIN
#       TIB DUP TIBSIZE ACCEPT  SPACE
#       INTERPRET
#       STATE @ 0= IF CR ." OK" THEN
#   AGAIN ;
    head quit,4,"quit",docolon,evaluate
        .word l0,lp,store
        .word r0,rpstore,lit,0,state,store
quit1:  .word tib,dup,tibsize,accept,space
        .word interpret
        .word state,fetch,zeroequal,qbranch,quit2
        .word cr,xsquote
        .byte 3
        .ascii "ok "
        .align
        /* .align would add 4 bytes here */
        .word type
        /* .word lit,0x1e,emit      drop serial receive queue */
quit2:  .word branch,quit1

#C ABORT    i*x --   R: j*x --   clear stk & QUIT
#   S0 SP!  QUIT ;
    head abort,5,"abort",docolon,quit
        /* .word lit,0x1e,emit      drop serial receive queue */
        .word s0,spstore,quit   /* quit never returns */

#Z ?ABORT   f c-addr u --      abort & print msg
#   ROT IF TYPE ABORT THEN 2DROP ;
    head qabort,6,"?abort",docolon,abort
        .word rot,qbranch,qabo1,type,abort
qabo1:  .word twodrop,exit

#C ABORT"  i*x 0  -- i*x   R: j*x -- j*x  x1=0
#C         i*x x1 --       R: j*x --      x1<>0
#   POSTPONE S" POSTPONE ?ABORT ; IMMEDIATE
    immed abortquote,6,"abort\"",docolon,qabort
        .word squote
        .word lit,qabort,commaxt
        .word exit

#C '    -- xt           find word in dictionary
#   BL WORD FIND
#_   0= ABORT" ?" ;
    head tick,1,"'",docolon,abortquote
        .word bl,word,find,zeroequal,xsquote
        .byte 1
        .ascii "?"
        .align
        .word qabort,exit

#C CHAR   -- char           parse ASCII character
#   BL WORD 1+ C@ ;
    head char,4,"char",docolon,tick
        .word bl,word,oneplus,cfetch,exit

#C [CHAR]   --          compile character literal
#   CHAR  ['] LIT ,XT  , ; IMMEDIATE
    immed bracchar,6,"[char]",docolon,char
        .word char
        .word lit,lit,commaxt
        .word comma,exit

#C (    --                     skip input until )
#   [ HEX ] 29 WORD DROP ; IMMEDIATE
    immed paren,1,"(",docolon,bracchar
        .word lit,0x29,word,drop,exit

# COMPILER ======================================

#C CREATE   --      create an empty definition
#   LATEST @ , 0 C,         link & immed field
#   HERE LATEST !           new "latest" link
#   BL WORD C@ 1+ ALLOT         name field
#   docreate ,CF                code field
# It's a bit different there
    head create,6,"create",docolon,paren
        .word latest,fetch,comma,lit,0,comma
        .word here,latest,store
        .word bl,word,cfetch,oneplus,aligned,allot
        .word lit,docreate,commacf
        .word exit

#Z (DOES>)  --      run-time action of DOES>
#   R>              adrs of headless DOES> def'n
#   LATEST @ NFA>CFA    code field to fix up
#   !CF ;
    head xdoes,7,"(does>)",docolon,create
        .word rfrom,latest,fetch,nfatocfa,storecf
        .word exit

#C DOES>    --      change action of latest def'n
#   COMPILE (DOES>)
#   dodoes ,CF ; IMMEDIATE
    immed does,5,"does>",docolon,xdoes
        .word lit,xdoes,commaxt
# compiles "mov r3, lr"
        .word lit,0xe1a0300e,comma
        .word lit,dodoes,commacf,exit

#C RECURSE  --      recurse current definition
#   LATEST @ NFA>CFA ,XT ; IMMEDIATE
    immed recurse,7,"recurse",docolon,does
        .word latest,fetch,nfatocfa,commaxt,exit

#C [        --      enter interpretive state
#_   0 STATE ! ; IMMEDIATE
    immed leftbracket,1,"[",docolon,recurse
        .word lit,0,state,store,exit

#C ]        --      enter compiling state
#_   -1 STATE ! ;
    head rightbracket,1,"]",docolon,leftbracket
        .word lit,-1,state,store,exit

#Z HIDE     --      "hide" latest definition
#   LATEST @ DUP C@ 80 OR SWAP C! ;
    head hide,4,"hide",docolon,rightbracket
        .word latest,fetch,dup,cfetch,lit,0x80,or
        .word swap,cstore,exit

#Z REVEAL   --      "reveal" latest definition
#   LATEST @ DUP C@ 7F AND SWAP C! ;
    head reveal,6,"reveal",docolon,hide
        .word latest,fetch,dup,cfetch,lit,0x7f,and
        .word swap,cstore,exit

#C IMMEDIATE   --   make last def'n immediate
#_   1 LATEST @ 1- C! ;   set immediate flag
# It's a bit different there
    head immediate,9,"immediate",docolon,reveal
        .word lit,1,latest,fetch,lit,4,minus,store
        .word exit

#C :        --      begin a colon definition
#   CREATE HIDE ] !COLON ;
    head colon,1,":",docolon,immediate
        .word create,hide,rightbracket,storcolon
        .word exit

#C ;
#   REVEAL  ,EXIT
#   POSTPONE [  ; IMMEDIATE
#    immed SEMICOLON,1,";",docolon,COLON
.align
.word link_colon
.word 1
link_semicolon:
.byte 1
.byte 0x3b
.align
semicolon:
	bl 	docolon
        .word reveal,cexit
        .word leftbracket,exit

#C [']  --         find word & compile as literal
#   '  ['] LIT ,XT  , ; IMMEDIATE
# When encountered in a colon definition, the
# phrase  ['] xxx  will cause   LIT,xxt  to be
# compiled into the colon definition
# (where xxt is the execution token of word xxx).
# When the colon definition executes, xxt will
# be put on the stack.  (All xt's are one cell.)
    immed bractick,3,"[']",docolon,semicolon
        .word tick               /* get xt of 'xxx' */
        .word lit,lit,commaxt    /* append lit action */
        .word comma,exit         /* append xt literal */

#C POSTPONE  --   postpone compile action of word
#   BL WORD FIND
#   DUP 0= ABORT" ?"
#_   0< IF   -- xt  non immed: add code to current
#                  def'n to compile xt later.
#       ['] LIT ,XT  ,      add "LIT,xt,COMMAXT"
#       ['] ,XT ,XT         to current definition
#   ELSE  ,XT      immed: compile into cur. def'n
#   THEN ; IMMEDIATE
    immed postpone,8,"postpone",docolon,bractick
        .word bl,word,find,dup,zeroequal,xsquote
        .byte 1
        .ascii "?"
        .align
        .word qabort,zeroless,qbranch,post1
        .word lit,lit,commaxt,comma
        .word lit,commaxt,commaxt,branch,post2
post1:  .word commaxt
post2:  .word exit

#Z COMPILE   --   append inline execution token
#   R> DUP CELL+ >R @ ,XT ;
# The phrase ['] xxx ,XT appears so often that
# this word was created to combine the actions
# of LIT and ,XT.  It takes an inline literal
# execution token and appends it to the dict.
    head compile,7,"compile",docolon,postpone
        .word rfrom,dup,cellplus,tor
        .word fetch,commaxt,exit
# N.B.: not used in the current implementation

# CONTROL STRUCTURES ============================

#C IF       -- adrs    conditional forward branch
#   ['] qbranch ,BRANCH  HERE DUP ,DEST ;
#   IMMEDIATE
    immed if,2,"if",docolon,compile
        .word lit,qbranch,commabranch
        .word here,dup,commadest,exit

#C THEN     adrs --        resolve forward branch
#   HERE SWAP !DEST ; IMMEDIATE
    immed then,4,"then",docolon,if
        .word here,swap,storedest,exit

#C ELSE     adrs1 -- adrs2    branch for IF..ELSE
#   ['] branch ,BRANCH  HERE DUP ,DEST
#   SWAP  POSTPONE THEN ; IMMEDIATE
    immed else,4,"else",docolon,then
        .word lit,branch,commabranch
        .word here,dup,commadest
        .word swap,then,exit

#C BEGIN    -- adrs        target for bwd. branch
#   HERE ; IMMEDIATE
    immed begin,5,"begin",docolon,else
        .word here,exit

#C UNTIL    adrs --   conditional backward branch
#   ['] qbranch ,BRANCH  ,DEST ; IMMEDIATE
#   conditional backward branch
    immed until,5,"until",docolon,begin
        .word lit,qbranch,commabranch
        .word commadest,exit

#X AGAIN    adrs --      uncond'l backward branch
#   ['] branch ,BRANCH  ,DEST ; IMMEDIATE
#   unconditional backward branch
    immed again,5,"again",docolon,until
        .word lit,branch,commabranch
        .word commadest,exit

#C WHILE    -- adrs         branch for WHILE loop
#   POSTPONE IF ; IMMEDIATE
    immed while,5,"while",docolon,again
        .word if,exit

#C REPEAT   adrs1 adrs2 --     resolve WHILE loop
#   SWAP POSTPONE AGAIN POSTPONE THEN ; IMMEDIATE
    immed repeat,6,"repeat",docolon,while
        .word swap,again,then,exit

#Z >L   x --   L: -- x        move to leave stack
#   CELL LP +!  LP @ ! ;      (L stack grows up)
    head tol,2,">l",docolon,repeat
        .word cell,lp,plusstore,lp,fetch,store,exit

#Z L>   -- x   L: x --      move from leave stack
#   LP @ @  CELL NEGATE LP +! ;
    head lfrom,2,"l>",docolon,tol
        .word lp,fetch,fetch
        .word cell,negate,lp,plusstore,exit

#C DO       -- adrs   L: -- 0
#   ['] xdo ,XT   HERE     target for bwd branch
#_   0 >L ; IMMEDIATE           marker for LEAVEs
    immed do,2,"do",docolon,lfrom
        .word lit,xdo,commaxt,here
        .word lit,0,tol,exit

#Z ENDLOOP   adrs xt --   L: 0 a1 a2 .. aN --
#   ,BRANCH  ,DEST                backward loop
#   BEGIN L> ?DUP WHILE POSTPONE THEN REPEAT ;
#                                 resolve LEAVEs
# This is a common factor of LOOP and +LOOP.
    head endloop,7,"endloop",docolon,do
        .word commabranch,commadest
loop1:  .word lfrom,qdup,qbranch,loop2
        .word then,branch,loop1
loop2:  .word exit

#C LOOP    adrs --   L: 0 a1 a2 .. aN --
#   ['] xloop ENDLOOP ;  IMMEDIATE
    immed loop,4,"loop",docolon,endloop
        .word lit,xloop,endloop,exit

#C +LOOP   adrs --   L: 0 a1 a2 .. aN --
#   ['] xplusloop ENDLOOP ;  IMMEDIATE
    immed plusloop,5,"+loop",docolon,loop
        .word lit,xplusloop,endloop,exit

#C LEAVE    --    L: -- adrs
#   ['] UNLOOP ,XT
#   ['] branch ,BRANCH   HERE DUP ,DEST  >L
#   ; IMMEDIATE      unconditional forward branch
    immed leave,5,"leave",docolon,plusloop
        .word lit,unloop,commaxt
        .word lit,branch,commabranch
        .word here,dup,commadest,tol,exit

# OTHER OPERATIONS ==============================

#X WITHIN   n1|u1 n2|u2 n3|u3 -- f   n2<=n1<n3?
#  OVER - >R - R> U< ;          per ANS document
    head within,6,"within",docolon,leave
        .word over,minus,tor,minus,rfrom,uless,exit

#C MOVE    addr1 addr2 u --     smart move
#             VERSION FOR 1 ADDRESS UNIT = 1 CHAR
#  >R 2DUP SWAP DUP R@ +     -- ... dst src src+n
#  WITHIN IF  R> CMOVE>        src <= dst < src+n
#       ELSE  R> CMOVE  THEN ;          otherwise

#    head move,4,"move",docolon,within
#        .word tor,twodup,swap,dup,rfetch,plus
#        .word within,qbranch,move1
#        .word rfrom,cmoveup,branch,move2
#move1:  .word rfrom,cmove
#move2:  .word exit

#C MOVE    addr1 addr2 u --     assembly move
    codeh move,4,"move",within
	ldr	r4, [sp], #4	/* addr2 */
	ldr	r5, [sp], #4	/* addr1 */
    cmp r1, #0
    ble move2
move1:
    ldr r6, [r5], #4
	str	r6, [r4], #4
    subs r1, r1, #4
    bgt move1
move2:
	ldr	r1, [sp], #4	/* pop new TOS */
	next

#C DEPTH    -- +n        number of items on stack
#   SP@ S0 SWAP - 2/ ;   16-BIT VERSION!
#_ 32-bit version
    head depth,5,"depth",docolon,move
        .word spfetch,s0,swap,minus,twoslash,twoslash,exit

#C ENVIRONMENT?  c-addr u -- false   system query
#                         -- i*x true
#_   2DROP 0 ;       the minimal definition!
    head environmentq,12,"environment?",docolon,depth
        .word twodrop,lit,0,exit

# UTILITY WORDS AND STARTUP =====================

#X WORDS    --          list all words in dict.
#   LATEST @ BEGIN
#       DUP COUNT TYPE SPACE
#       NFA>LFA @
#   DUP 0= UNTIL
#   DROP ;
    head words,5,"words",docolon,environmentq
        .word latest,fetch
wds1:   .word dup,count,type,space,nfatolfa,fetch
        .word dup,zeroequal,qbranch,wds1
        .word drop,exit

#X .S      --           print stack contents
#   SP@ S0 - IF
#       SP@ S0 2 - DO I @ U. -2 +LOOP
#   THEN ;
#_ 32-bit version
    head dots,2,".s",docolon,words
        .word spfetch,s0,minus,qbranch,dots2
        .word spfetch,s0,lit,4,minus,xdo
dots1:  .word ii,fetch,udot,lit,-4,xplusloop,dots1
dots2:  .word exit

#Z COLD     --      cold start Forth system
#   UINIT U0 #INIT CMOVE      init user area
#   80 COUNT INTERPRET       interpret CP/M cmd
#   ." Z80 CamelForth etc."
#   ABORT ;
    head cold,4,cold,docolon,dots
        .word uinit,u0,ninit,cmove
        .word xsquote
        .byte 28
        .ascii "type forth my friend...    "
        .byte 0x0a
        .align
        .word type,s0,spstore,quit       /* abort never returns */
