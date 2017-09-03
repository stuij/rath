# SYSTEM VARIABLES & CONSTANTS ==================

#C BL      -- char            an ASCII space
    head BL,2,"BL",docon,STOREDEST
        .word 0x20

#Z tibsize  -- n         size of TIB
    head TIBSIZE,7,"TIBSIZE",docon,BL
        .word 124          /* 2 chars safety zone */

#X tib     -- a-addr     Terminal Input Buffer
#  HEX 82 CONSTANT TIB   CP/M systems: 126 bytes
#  HEX -80 USER TIB      others: below user area
    head TIB,3,"TIB",dovar,TIBSIZE
.space 128

#Z u0      -- a-addr       current user area adrs
    codeh U0,2,"U0",TIB
	str	r1, [sp, #-4]!	/* push TOS */
        mov	r1, r2
        next

#C >IN     -- a-addr        holds offset into TIB
    head TOIN,3,">IN",douser,U0
        .word 4

#C BASE    -- a-addr       holds conversion radix
    head BASE,4,"BASE",douser,TOIN
        .word 8

#C STATE   -- a-addr       holds compiler state
    head STATE,5,"STATE",douser,BASE
        .word 12

#Z dp      -- a-addr       holds dictionary ptr
    head DP,2,"DP",douser,STATE
    	.word 16

#Z 'source  -- a-addr      two cells: len, adrs
    head TICKSOURCE,7,"'SOURCE",douser,DP
    	.word 20

#Z latest    -- a-addr     last word in dict.
    head LATEST,6,"LATEST",douser,TICKSOURCE
    	.word 28

#Z hp       -- a-addr     HOLD pointer
    head HP,2,"HP",douser,LATEST
    	.word 32

#Z LP       -- a-addr     Leave-stack pointer
    head LP,2,"LP",douser,HP
    	.word 36

#Z s0       -- a-addr     end of parameter stack
    codeh S0,2,"S0",LP
    	str	r1, [sp, #-4]!	/* push old TOS */
	ldr	r1, [r10]
    	next
    	
#X PAD       -- a-addr    user PAD buffer
#                         = end of hold area!
    codeh PAD,3,"PAD",S0
    	str	r1, [sp, #-4]!	/* push old TOS */
	ldr	r1, [r10, #12]
    	next

#Z l0       -- a-addr     bottom of Leave stack
    head L0,2,"L0",dovar,PAD
.space 128

#Z r0       -- a-addr     end of return stack
    codeh R0,2,"R0",L0
    	str	r1, [sp, #-4]!	/* push old TOS */
	ldr	r1, [r10, #4]
    	next

#Z uinit    -- addr  initial values for user area
    head UINIT,5,"UINIT",docreate,R0
        .word 0x12345678,0,10,0     /* reserved (UNUSED),>IN,BASE,STATE */
        .word enddict      /* DP */
        .word 0,0          /* SOURCE init'd elsewhere */
        .word lastword     /* LATEST */
        .word 0            /* HP init'd elsewhere */

#Z #init    -- n    #bytes of user area init data
    head NINIT,5,"#INIT",docon,UINIT
        .word 36

# ARITHMETIC OPERATORS ==========================

#C S>D    n -- d          single -> double prec.
#   DUP 0< ;
    head STOD,3,"S>D",docolon,NINIT
        .word DUP,ZEROLESS,EXIT

#Z ?NEGATE  n1 n2 -- n3  negate n1 if n2 negative
#_   0< IF NEGATE THEN ;        ...a common factor
    head QNEGATE,7,"?NEGATE",docolon,STOD
        .word ZEROLESS,QBRANCH,QNEG1,NEGATE
QNEG1:  .word EXIT

#C ABS     n1 -- +n2     absolute value
#   DUP ?NEGATE ;
    head ABS,3,"ABS",docolon,QNEGATE
        .word DUP,QNEGATE,EXIT

#X DNEGATE   d1 -- d2     negate double precision
#   SWAP INVERT SWAP INVERT 1 M+ ;
    head DNEGATE,7,"DNEGATE",docolon,ABS
        .word SWAP,INVERT,SWAP,INVERT,LIT,1,MPLUS
        .word EXIT

#Z ?DNEGATE  d1 n -- d2   negate d1 if n negative
#_   0< IF DNEGATE THEN ;       ...a common factor
    head QDNEGATE,8,"?DNEGATE",docolon,DNEGATE
        .word ZEROLESS,QBRANCH,DNEG1,DNEGATE
DNEG1:  .word EXIT

#X DABS     d1 -- +d2    absolute value dbl.prec.
#   DUP ?DNEGATE ;
    head DABS,4,"DABS",docolon,QDNEGATE
        .word DUP,QDNEGATE,EXIT

#C M*     n1 n2 -- d    signed 16*16->32 multiply
#_   2DUP XOR >R        carries sign of the result
#   SWAP ABS SWAP ABS UM*
#   R> ?DNEGATE ;
    head MSTAR,2,"M*",docolon,DABS
        .word TWODUP,XOR,TOR
        .word SWAP,ABS,SWAP,ABS,UMSTAR
        .word RFROM,QDNEGATE,EXIT

#C SM/REM   d1 n1 -- n2 n3   symmetric signed div
#_   2DUP XOR >R              sign of quotient
#   OVER >R                  sign of remainder
#   ABS >R DABS R> UM/MOD
#   SWAP R> ?NEGATE
#   SWAP R> ?NEGATE ;
# Ref. dpANS-6 section 3.2.2.1.
    head SMSLASHREM,6,"SM/REM",docolon,MSTAR
        .word TWODUP,XOR,TOR,OVER,TOR
        .word ABS,TOR,DABS,RFROM,UMSLASHMOD
        .word SWAP,RFROM,QNEGATE,SWAP,RFROM,QNEGATE
        .word EXIT

#C FM/MOD   d1 n1 -- n2 n3   floored signed div'n
#   DUP >R              save divisor
#   SM/REM
#   DUP 0< IF           if quotient negative,
#       SWAP R> +         add divisor to rem'dr
#       SWAP 1-           decrement quotient
#   ELSE R> DROP THEN ;
# Ref. dpANS-6 section 3.2.2.1.
    head FMSLASHMOD,6,"FM/MOD",docolon,SMSLASHREM
        .word DUP,TOR,SMSLASHREM
        .word DUP,ZEROLESS,QBRANCH,FMMOD1
        .word SWAP,RFROM,PLUS,SWAP,ONEMINUS
        .word BRANCH,FMMOD2
FMMOD1: .word RFROM,DROP
FMMOD2: .word EXIT

#C *      n1 n2 -- n3       signed multiply
#   M* DROP ;
    head STAR,1,"*",docolon,FMSLASHMOD
        .word MSTAR,DROP,EXIT

#C /MOD   n1 n2 -- n3 n4    signed divide/rem'dr
#   >R S>D R> FM/MOD ;
    head SLASHMOD,4,"/MOD",docolon,STAR
        .word TOR,STOD,RFROM,FMSLASHMOD,EXIT

#C /      n1 n2 -- n3       signed divide
#   /MOD nip ;
    head SLASH,1,"/",docolon,SLASHMOD
        .word SLASHMOD,NIP,EXIT

#C MOD    n1 n2 -- n3       signed remainder
#   /MOD DROP ;
    head MOD,3,"MOD",docolon,SLASH
        .word SLASHMOD,DROP,EXIT

#C */MOD  n1 n2 n3 -- n4 n5    n1*n2/n3, rem&quot
#   >R M* R> FM/MOD ;
    head SSMOD,5,"*/MOD",docolon,MOD
        .word TOR,MSTAR,RFROM,FMSLASHMOD,EXIT

#C */     n1 n2 n3 -- n4        n1*n2/n3
#   */MOD nip ;
    head STARSLASH,2,"*/",docolon,SSMOD
        .word SSMOD,NIP,EXIT

#C MAX    n1 n2 -- n3       signed maximum
#_   2DUP < IF SWAP THEN DROP ;
    head MAX,3,"MAX",docolon,STARSLASH
        .word TWODUP,LESS,QBRANCH,MAX1,SWAP
MAX1:   .word DROP,EXIT

#C MIN    n1 n2 -- n3       signed minimum
#_   2DUP > IF SWAP THEN DROP ;
    head MIN,3,"MIN",docolon,MAX
        .word TWODUP,GREATER,QBRANCH,MIN1,SWAP
MIN1:   .word DROP,EXIT

# DOUBLE OPERATORS ==============================

#C 2@    a-addr -- x1 x2    fetch 2 cells
#   DUP CELL+ @ SWAP @ ;
#   the lower address will appear on top of stack
    head TWOFETCH,2,"2@",docolon,MIN
        .word DUP,CELLPLUS,FETCH,SWAP,FETCH,EXIT

#C 2!    x1 x2 a-addr --    store 2 cells
#   SWAP OVER ! CELL+ ! ;
#   the top of stack is stored at the lower adrs
    head TWOSTORE,2,"2!",docolon,TWOFETCH
        .word SWAP,OVER,STORE,CELLPLUS,STORE,EXIT

#C 2DROP  x1 x2 --          drop 2 cells
#   DROP DROP ;
    head TWODROP,5,"2DROP",docolon,TWOSTORE
        .word DROP,DROP,EXIT

#C 2DUP   x1 x2 -- x1 x2 x1 x2   dup top 2 cells
#   OVER OVER ;
    head TWODUP,4,"2DUP",docolon,TWODROP
        .word OVER,OVER,EXIT

#C 2SWAP  x1 x2 x3 x4 -- x3 x4 x1 x2  per diagram
#   ROT >R ROT R> ;
    head TWOSWAP,5,"2SWAP",docolon,TWODUP
        .word ROT,TOR,ROT,RFROM,EXIT

#C 2OVER  x1 x2 x3 x4 -- x1 x2 x3 x4 x1 x2
#   >R >R 2DUP R> R> 2SWAP ;
    head TWOOVER,5,"2OVER",docolon,TWOSWAP
        .word TOR,TOR,TWODUP,RFROM,RFROM
        .word TWOSWAP,EXIT

# INPUT/OUTPUT ==================================

#C COUNT   c-addr1 -- c-addr2 u  counted->adr/len
#   DUP CHAR+ SWAP C@ ;
    head COUNT,5,"COUNT",docolon,TWOOVER
        .word DUP,CHARPLUS,SWAP,CFETCH,EXIT

#C CR      --               output newline
#_   0D EMIT 0A EMIT ;
    head CR,2,"CR",docolon,COUNT
        .word LIT,0x0d,EMIT,LIT,0x0a,EMIT,EXIT

#C SPACE   --               output a space
#   BL EMIT ;
    head SPACE,5,"SPACE",docolon,CR
        .word BL,EMIT,EXIT

#C SPACES   n --            output n spaces
#   BEGIN DUP WHILE SPACE 1- REPEAT DROP ;
    head SPACES,6,"SPACES",docolon,SPACE
SPCS1:  .word DUP,QBRANCH,SPCS2
        .word SPACE,ONEMINUS,BRANCH,SPCS1
SPCS2:  .word DROP,EXIT

#Z umin     u1 u2 -- u      unsigned minimum
#_   2DUP U> IF SWAP THEN DROP ;
    head UMIN,4,"UMIN",docolon,SPACES
        .word TWODUP,UGREATER,QBRANCH,UMIN1,SWAP
UMIN1:  .word DROP,EXIT

#Z umax    u1 u2 -- u       unsigned maximum
# _  2DUP U< IF SWAP THEN DROP ;
    head UMAX,4,"UMAX",docolon,UMIN
        .word TWODUP,ULESS,QBRANCH,UMAX1,SWAP
UMAX1:  .word DROP,EXIT

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
    head ACCEPT,6,"ACCEPT",docolon,UMAX
        .word OVER,PLUS,ONEMINUS,OVER
ACC1:   .word KEY,DUP,LIT,0x0d,NOTEQUAL,QBRANCH,ACC5
        .word DUP,EMIT,DUP,LIT,8,EQUAL,QBRANCH,ACC3
        .word DROP,ONEMINUS,TOR,OVER,RFROM,UMAX
        .word BRANCH,ACC4
ACC3:   .word OVER,CSTORE,ONEPLUS,OVER,UMIN
ACC4:   .word BRANCH,ACC1
ACC5:   .word DROP,NIP,SWAP,MINUS,EXIT

#C TYPE    c-addr +n --     type line to term'l
#   ?DUP IF
#     OVER + SWAP DO I C@ EMIT LOOP
#   ELSE DROP THEN ;
    head TYPE,4,"TYPE",docolon,ACCEPT
        .word QDUP,QBRANCH,TYP4
        .word OVER,PLUS,SWAP,XDO
TYP3:   .word II,CFETCH,EMIT,XLOOP,TYP3
        .word BRANCH,TYP5
TYP4:   .word DROP	
TYP5:   .word EXIT

#Z (S")     -- c-addr u   run-time code for S"
#   R> COUNT 2DUP + ALIGNED >R  ;
    head XSQUOTE,4,"(S\")",docolon,TYPE
        .word RFROM,COUNT,TWODUP,PLUS,ALIGNED
        .word TOR
        .word EXIT

#C S"       --         compile in-line string
#   COMPILE (S")  [ HEX ]
#   22 WORD C@ 1+ ALIGNED ALLOT ; IMMEDIATE
    immed SQUOTE,2,"S\"",docolon,XSQUOTE
        .word LIT,XSQUOTE,COMMAXT
        .word LIT,0x22,WORD,CFETCH,ONEPLUS
        .word ALIGNED,ALLOT,EXIT

#C ."       --         compile string to print
#   POSTPONE S"  POSTPONE TYPE ; IMMEDIATE
    immed DOTQUOTE,2,".\"",docolon,SQUOTE
        .word SQUOTE
        .word LIT,TYPE,COMMAXT
        .word EXIT
        
# NUMERIC OUTPUT ================================
# Numeric conversion is done l.s.digit first, so
# the output buffer is built backwards in memory.

# Some double-precision arithmetic operators are
# needed to implement ANSI numeric conversion.

#Z UD/MOD   ud1 u2 -- u3 ud4   32/16->32 divide
#   >R 0 R@ UM/MOD  ROT ROT R> UM/MOD ROT ;
    head UDSLASHMOD,6,"UD/MOD",docolon,DOTQUOTE
        .word TOR,LIT,0,RFETCH,UMSLASHMOD,ROT,ROT
        .word RFROM,UMSLASHMOD,ROT,EXIT

#Z UD*      ud1 d2 -- ud3      32*16->32 multiply
#   DUP >R UM* DROP  SWAP R> UM* ROT + ;
    head UDSTAR,3,"UD*",docolon,UDSLASHMOD
        .word DUP,TOR,UMSTAR,DROP
        .word SWAP,RFROM,UMSTAR,ROT,PLUS,EXIT

#C HOLD  char --        add char to output string
#   -1 HP +!  HP @ C! ;
    head HOLD,4,"HOLD",docolon,UDSTAR
        .word LIT,-1,HP,PLUSSTORE
        .word HP,FETCH,CSTORE,EXIT

#C <#    --             begin numeric conversion
#   PAD HP ! ;          (initialize Hold Pointer)
    head LESSNUM,2,"<#",docolon,HOLD
        .word PAD,HP,STORE,EXIT

#Z >digit   n -- c      convert to 0..9A..Z
#   [ HEX ] DUP 9 > 7 AND + 30 + ;
    head TODIGIT,6,">DIGIT",docolon,LESSNUM
        .word DUP,LIT,9,GREATER,LIT,7,AND,PLUS
        .word LIT,0x30,PLUS,EXIT

#C #     ud1 -- ud2     convert 1 digit of output
#   BASE @ UD/MOD ROT >digit HOLD ;
    head NUM,1,"#",docolon,TODIGIT
        .word BASE,FETCH,UDSLASHMOD,ROT,TODIGIT
        .word HOLD,EXIT

#C #S    ud1 -- ud2     convert remaining digits
#   BEGIN # 2DUP OR 0= UNTIL ;
    head NUMS,2,"#S",docolon,NUM
NUMS1:  .word NUM,TWODUP,OR,ZEROEQUAL,QBRANCH,NUMS1
        .word EXIT

#C #>    ud1 -- c-addr u    end conv., get string
#   2DROP HP @ PAD OVER - ;
    head NUMGREATER,2,"#>",docolon,NUMS
        .word TWODROP,HP,FETCH,PAD,OVER,MINUS
        .word EXIT

#C SIGN  n --           add minus sign if n<0
#_   0< IF 2D HOLD THEN ;
    head SIGN,4,"SIGN",docolon,NUMGREATER
        .word ZEROLESS,QBRANCH,SIGN1,LIT,0x2D,HOLD
SIGN1:  .word EXIT

#C U.    u --           display u unsigned
#   <# 0 #S #> TYPE SPACE ;
    head UDOT,2,"U.",docolon,SIGN
        .word LESSNUM,LIT,0,NUMS,NUMGREATER,TYPE
        .word SPACE,EXIT

#C .     n --           display n signed
#   <# DUP ABS 0 #S ROT SIGN #> TYPE SPACE ;
    head DOT,1,".",docolon,UDOT
        .word LESSNUM,DUP,ABS,LIT,0,NUMS
        .word ROT,SIGN,NUMGREATER
        .word TYPE,SPACE,EXIT

#C DECIMAL  --      set number base to decimal
#   10 BASE ! ;
    head DECIMAL,7,"DECIMAL",docolon,DOT
        .word LIT,10,BASE,STORE,EXIT

#X HEX     --       set number base to hex
#   16 BASE ! ;
    head HEX,3,"HEX",docolon,DECIMAL
        .word LIT,16,BASE,STORE,EXIT

# DICTIONARY MANAGEMENT =========================

#C HERE    -- addr      returns dictionary ptr
#   DP @ ;
    head HERE,4,"HERE",docolon,HEX
        .word DP,FETCH,EXIT

#C ALLOT   n --         allocate n bytes in dict
#   DP +! ;
    head ALLOT,5,"ALLOT",docolon,HERE
        .word DP,PLUSSTORE,EXIT

# Note: , and C, are only valid for combined
# Code and Data spaces.

#C ,    x --           append cell to dict
#   HERE ! 1 CELLS ALLOT ;
    head COMMA,1,",",docolon,ALLOT
        .word HERE,STORE,LIT,1,CELLS,ALLOT,EXIT

#C C,   char --        append char to dict
#   HERE C! 1 CHARS ALLOT ;
    head CCOMMA,2,"C,",docolon,COMMA
        .word HERE,CSTORE,LIT,1,CHARS,ALLOT,EXIT

# INTERPRETER ===================================
# Note that NFA>LFA, NFA>CFA, IMMED?, and FIND
# are dependent on the structure of the Forth
# header.  This may be common across many CPUs,
# or it may be different.

#C SOURCE   -- adr n    current input buffer
#   'SOURCE 2@ ;        length is at lower adrs
    head SOURCE,6,"SOURCE",docolon,CCOMMA
        .word TICKSOURCE,TWOFETCH,EXIT

#X /STRING  a u n -- a+n u-n   trim string
#   ROT OVER + ROT ROT - ;
    head SLASHSTRING,7,"/STRING",docolon,SOURCE
        .word ROT,OVER,PLUS,ROT,ROT,MINUS,EXIT

#Z >counted  src n dst --     copy to counted str
#   2DUP C! CHAR+ SWAP CMOVE ;
    head TOCOUNTED,8,">COUNTED",docolon,SLASHSTRING
        .word TWODUP,CSTORE,CHARPLUS,SWAP,CMOVE,EXIT

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
    head WORD,4,WORD,docolon,TOCOUNTED
        .word DUP,SOURCE,TOIN,FETCH,SLASHSTRING
        .word DUP,TOR,ROT,SKIP
        .word OVER,TOR,ROT,SCAN
        .word DUP,QBRANCH,WORD1,CHARMINUS
WORD1:  .word RFROM,RFROM,ROT,MINUS,TOIN,PLUSSTORE
        .word TUCK,MINUS
        .word HERE,TOCOUNTED,HERE
        .word BL,OVER,COUNT,PLUS,CSTORE,EXIT

#Z NFA>LFA   nfa -- lfa    name adr -> link field
#_   3 - ;
# Here -8
    head NFATOLFA,7,"NFA>LFA",docolon,WORD
        .word LIT,8,MINUS,EXIT

#Z NFA>CFA   nfa -- cfa    name adr -> code field
#   COUNT 7F AND + ;       mask off 'smudge' bit
# Needs testing
    head NFATOCFA,7,"NFA>CFA",docolon,NFATOLFA
        .word COUNT,LIT,0x7F,AND,PLUS
        .word ALIGNED
        .word EXIT

#Z IMMED?    nfa -- f      fetch immediate flag
#_   1- C@ ;                     nonzero if immed
# Here -4
    head IMMEDQ,6,"IMMED?",docolon,NFATOCFA
        .word LIT,4,MINUS,FETCH,EXIT

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
    head FIND,4,"FIND",docolon,IMMEDQ
        .word LATEST,FETCH
FIND1:  .word TWODUP,OVER,CFETCH,CHARPLUS
        .word SEQUAL,DUP,QBRANCH,FIND2
        .word DROP,NFATOLFA,FETCH,DUP
FIND2:  .word ZEROEQUAL,QBRANCH,FIND1
        .word DUP,QBRANCH,FIND3
        .word NIP,DUP,NFATOCFA
        .word SWAP,IMMEDQ,ZEROEQUAL,LIT,1,OR
FIND3:  .word EXIT

#C LITERAL  x --        append numeric literal
#   STATE @ IF ['] LIT ,XT , THEN ; IMMEDIATE
# This tests STATE so that it can also be used
# interpretively.  (ANSI doesn't require this.)
    immed LITERAL,7,"LITERAL",docolon,FIND
        .word STATE,FETCH,QBRANCH,LITER1
        .word LIT,LIT,COMMAXT,COMMA
LITER1: .word EXIT

#Z DIGIT?   c -- n -1   if c is a valid digit
#Z            -- x  0   otherwise
#   [ HEX ] DUP 39 > 100 AND +     silly looking
#   DUP 140 > 107 AND -   30 -     but it works!
#   DUP BASE @ U< ;
    head DIGITQ,6,"DIGIT?",docolon,LITERAL
        .word DUP,LIT,0x39,GREATER,LIT,0x100,AND,PLUS
        .word DUP,LIT,0x140,GREATER,LIT,0x107,AND
        .word MINUS,LIT,0x30,MINUS
        .word DUP,BASE,FETCH,ULESS,EXIT

#Z ?SIGN   adr n -- adr' n' f  get optional sign
#Z  advance adr/n if sign; return NZ if negative
#   OVER C@                 -- adr n c
#_   2C - DUP ABS 1 = AND    -- +=-1, -=+1, else 0
#   DUP IF 1+               -- +=0, -=+2
#       >R 1 /STRING R>     -- adr' n' f
#   THEN ;
    head QSIGN,5,"?SIGN",docolon,DIGITQ
        .word OVER,CFETCH,LIT,0x2C,MINUS,DUP,ABS
        .word LIT,1,EQUAL,AND,DUP,QBRANCH,QSIGN1
        .word ONEPLUS,TOR,LIT,1,SLASHSTRING,RFROM
QSIGN1: .word EXIT

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
    head TONUMBER,7,">NUMBER",docolon,QSIGN
TONUM1: .word DUP,QBRANCH,TONUM3
        .word OVER,CFETCH,DIGITQ
        .word ZEROEQUAL,QBRANCH,TONUM2,DROP,EXIT
TONUM2: .word TOR,TWOSWAP,BASE,FETCH,UDSTAR
        .word RFROM,MPLUS,TWOSWAP
        .word LIT,1,SLASHSTRING,BRANCH,TONUM1
TONUM3: .word EXIT

#Z ?NUMBER  c-addr -- n -1      string->number
#Z                 -- c-addr 0  if convert error
#   DUP  0 0 ROT COUNT      -- ca ud adr n
#   ?SIGN >R  >NUMBER       -- ca ud adr' n'
#   IF   R> 2DROP 2DROP 0   -- ca 0   (error)
#   ELSE 2DROP NIP R>
#       IF NEGATE THEN  -1  -- n -1   (ok)
#   THEN ;
    head QNUMBER,7,"?NUMBER",docolon,TONUMBER
        .word DUP,LIT,0,DUP,ROT,COUNT
        .word QSIGN,TOR,TONUMBER,QBRANCH,QNUM1
        .word RFROM,TWODROP,TWODROP,LIT,0
        .word BRANCH,QNUM3
QNUM1:  .word TWODROP,NIP,RFROM,QBRANCH,QNUM2,NEGATE
QNUM2:  .word LIT,-1
QNUM3:  .word EXIT

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
    head INTERPRET,9,"INTERPRET",docolon,QNUMBER
        .word TICKSOURCE,TWOSTORE,LIT,0,TOIN,STORE
INTER1: .word BL,WORD,DUP,CFETCH,QBRANCH,INTER9
        .word FIND
        .word QDUP,QBRANCH,INTER4
        .word ONEPLUS,STATE,FETCH
        .word ZEROEQUAL,OR
        .word QBRANCH,INTER2
        .word EXECUTE
        .word BRANCH,INTER3
INTER2: .word COMMAXT
INTER3: .word BRANCH,INTER8
INTER4: .word QNUMBER,QBRANCH,INTER5
        .word LITERAL,BRANCH,INTER6
INTER5: .word COUNT,TYPE,LIT,0x3F,EMIT,CR,ABORT
INTER6: 
INTER8: .word BRANCH,INTER1
INTER9: .word DROP,EXIT

#C EVALUATE  i*x c-addr u -- j*x  interprt string
#   'SOURCE 2@ >R >R  >IN @ >R
#   INTERPRET
#   R> >IN !  R> R> 'SOURCE 2! ;
    head EVALUATE,8,"EVALUATE",docolon,INTERPRET
        .word TICKSOURCE,TWOFETCH,TOR,TOR
        .word TOIN,FETCH,TOR,INTERPRET
        .word RFROM,TOIN,STORE,RFROM,RFROM
        .word TICKSOURCE,TWOSTORE,EXIT

#C QUIT     --    R: i*x --    interpret from kbd
#   L0 LP !  R0 RP!   0 STATE !
#   BEGIN
#       TIB DUP TIBSIZE ACCEPT  SPACE
#       INTERPRET
#       STATE @ 0= IF CR ." OK" THEN
#   AGAIN ;
    head QUIT,4,"QUIT",docolon,EVALUATE
        .word L0,LP,STORE
        .word R0,RPSTORE,LIT,0,STATE,STORE
QUIT1:  .word TIB,DUP,TIBSIZE,ACCEPT,SPACE
        .word INTERPRET
        .word STATE,FETCH,ZEROEQUAL,QBRANCH,QUIT2
        .word CR,XSQUOTE
        .byte 3
        .ascii "OK "
        /* .align would add 4 bytes here */
        .word TYPE
QUIT2:  .word BRANCH,QUIT1

#C ABORT    i*x --   R: j*x --   clear stk & QUIT
#   S0 SP!  QUIT ;
    head ABORT,5,"ABORT",docolon,QUIT
        .word S0,SPSTORE,QUIT   /* QUIT never returns */

#Z ?ABORT   f c-addr u --      abort & print msg
#   ROT IF TYPE ABORT THEN 2DROP ;
    head QABORT,6,"?ABORT",docolon,ABORT
        .word ROT,QBRANCH,QABO1,TYPE,ABORT
QABO1:  .word TWODROP,EXIT

#C ABORT"  i*x 0  -- i*x   R: j*x -- j*x  x1=0
#C         i*x x1 --       R: j*x --      x1<>0
#   POSTPONE S" POSTPONE ?ABORT ; IMMEDIATE
    immed ABORTQUOTE,6,"ABORT\"",docolon,QABORT
        .word SQUOTE
        .word LIT,QABORT,COMMAXT
        .word EXIT

#C '    -- xt           find word in dictionary
#   BL WORD FIND
#_   0= ABORT" ?" ;
    head TICK,1,"'",docolon,ABORTQUOTE
        .word BL,WORD,FIND,ZEROEQUAL,XSQUOTE
        .byte 1
        .ascii "?"
        .align
        .word QABORT,EXIT

#C CHAR   -- char           parse ASCII character
#   BL WORD 1+ C@ ;
    head CHAR,4,"CHAR",docolon,TICK
        .word BL,WORD,ONEPLUS,CFETCH,EXIT

#C [CHAR]   --          compile character literal
#   CHAR  ['] LIT ,XT  , ; IMMEDIATE
    immed BRACCHAR,6,"[CHAR]",docolon,CHAR
        .word CHAR
        .word LIT,LIT,COMMAXT
        .word COMMA,EXIT

#C (    --                     skip input until )
#   [ HEX ] 29 WORD DROP ; IMMEDIATE
    immed PAREN,1,"(",docolon,BRACCHAR
        .word LIT,0x29,WORD,DROP,EXIT

# COMPILER ======================================

#C CREATE   --      create an empty definition
#   LATEST @ , 0 C,         link & immed field
#   HERE LATEST !           new "latest" link
#   BL WORD C@ 1+ ALLOT         name field
#   docreate ,CF                code field
# It's a bit different there
    head CREATE,6,"CREATE",docolon,PAREN
        .word LATEST,FETCH,COMMA,LIT,0,COMMA
        .word HERE,LATEST,STORE
        .word BL,WORD,CFETCH,ONEPLUS,ALIGNED,ALLOT
        .word LIT,docreate,COMMACF
        .word EXIT

#Z (DOES>)  --      run-time action of DOES>
#   R>              adrs of headless DOES> def'n
#   LATEST @ NFA>CFA    code field to fix up
#   !CF ;
    head XDOES,7,"(DOES>)",docolon,CREATE
        .word RFROM,LATEST,FETCH,NFATOCFA,STORECF
        .word EXIT

#C DOES>    --      change action of latest def'n
#   COMPILE (DOES>)
#   dodoes ,CF ; IMMEDIATE
    immed DOES,5,"DOES>",docolon,XDOES
        .word LIT,XDOES,COMMAXT
# compiles "mov r3, lr"        
        .word LIT,0xE1A0300E,COMMA
        .word LIT,dodoes,COMMACF,EXIT

#C RECURSE  --      recurse current definition
#   LATEST @ NFA>CFA ,XT ; IMMEDIATE
    immed RECURSE,7,"RECURSE",docolon,DOES
        .word LATEST,FETCH,NFATOCFA,COMMAXT,EXIT

#C [        --      enter interpretive state
#_   0 STATE ! ; IMMEDIATE
    immed LEFTBRACKET,1,"[",docolon,RECURSE
        .word LIT,0,STATE,STORE,EXIT

#C ]        --      enter compiling state
#_   -1 STATE ! ;
    head RIGHTBRACKET,1,"]",docolon,LEFTBRACKET
        .word LIT,-1,STATE,STORE,EXIT

#Z HIDE     --      "hide" latest definition
#   LATEST @ DUP C@ 80 OR SWAP C! ;
    head HIDE,4,"HIDE",docolon,RIGHTBRACKET
        .word LATEST,FETCH,DUP,CFETCH,LIT,0x80,OR
        .word SWAP,CSTORE,EXIT

#Z REVEAL   --      "reveal" latest definition
#   LATEST @ DUP C@ 7F AND SWAP C! ;
    head REVEAL,6,"REVEAL",docolon,HIDE
        .word LATEST,FETCH,DUP,CFETCH,LIT,0x7F,AND
        .word SWAP,CSTORE,EXIT

#C IMMEDIATE   --   make last def'n immediate
#_   1 LATEST @ 1- C! ;   set immediate flag
# It's a bit different there
    head IMMEDIATE,9,"IMMEDIATE",docolon,REVEAL
        .word LIT,1,LATEST,FETCH,LIT,4,MINUS,STORE
        .word EXIT

#C :        --      begin a colon definition
#   CREATE HIDE ] !COLON ;
    head COLON,1,":",docolon,IMMEDIATE
        .word CREATE,HIDE,RIGHTBRACKET,STORCOLON
        .word EXIT

#C ;
#   REVEAL  ,EXIT
#   POSTPONE [  ; IMMEDIATE
#    immed SEMICOLON,1,";",docolon,COLON
.align
.word link_COLON
.word 1
link_SEMICOLON:
.byte 1
.byte 0x3b
.align
SEMICOLON: 
	bl 	docolon
        .word REVEAL,CEXIT
        .word LEFTBRACKET,EXIT

#C [']  --         find word & compile as literal
#   '  ['] LIT ,XT  , ; IMMEDIATE
# When encountered in a colon definition, the
# phrase  ['] xxx  will cause   LIT,xxt  to be
# compiled into the colon definition (where
# (where xxt is the execution token of word xxx).
# When the colon definition executes, xxt will
# be put on the stack.  (All xt's are one cell.)
    immed BRACTICK,3,"[']",docolon,SEMICOLON
        .word TICK               /* get xt of 'xxx' */
        .word LIT,LIT,COMMAXT    /* append LIT action */
        .word COMMA,EXIT         /* append xt literal */

#C POSTPONE  --   postpone compile action of word
#   BL WORD FIND
#   DUP 0= ABORT" ?"
#_   0< IF   -- xt  non immed: add code to current
#                  def'n to compile xt later.
#       ['] LIT ,XT  ,      add "LIT,xt,COMMAXT"
#       ['] ,XT ,XT         to current definition
#   ELSE  ,XT      immed: compile into cur. def'n
#   THEN ; IMMEDIATE
    immed POSTPONE,8,"POSTPONE",docolon,BRACTICK
        .word BL,WORD,FIND,DUP,ZEROEQUAL,XSQUOTE
        .byte 1
        .ascii "?"
        .align
        .word QABORT,ZEROLESS,QBRANCH,POST1
        .word LIT,LIT,COMMAXT,COMMA
        .word LIT,COMMAXT,COMMAXT,BRANCH,POST2
POST1:  .word COMMAXT
POST2:  .word EXIT

#Z COMPILE   --   append inline execution token
#   R> DUP CELL+ >R @ ,XT ;
# The phrase ['] xxx ,XT appears so often that
# this word was created to combine the actions
# of LIT and ,XT.  It takes an inline literal
# execution token and appends it to the dict.
    head COMPILE,7,"COMPILE",docolon,POSTPONE
        .word RFROM,DUP,CELLPLUS,TOR
        .word FETCH,COMMAXT,EXIT
# N.B.: not used in the current implementation

# CONTROL STRUCTURES ============================

#C IF       -- adrs    conditional forward branch
#   ['] qbranch ,BRANCH  HERE DUP ,DEST ;
#   IMMEDIATE
    immed IF,2,"IF",docolon,COMPILE
        .word LIT,QBRANCH,COMMABRANCH
        .word HERE,DUP,COMMADEST,EXIT

#C THEN     adrs --        resolve forward branch
#   HERE SWAP !DEST ; IMMEDIATE
    immed THEN,4,"THEN",docolon,IF
        .word HERE,SWAP,STOREDEST,EXIT

#C ELSE     adrs1 -- adrs2    branch for IF..ELSE
#   ['] branch ,BRANCH  HERE DUP ,DEST
#   SWAP  POSTPONE THEN ; IMMEDIATE
    immed ELSE,4,"ELSE",docolon,THEN
        .word LIT,BRANCH,COMMABRANCH
        .word HERE,DUP,COMMADEST
        .word SWAP,THEN,EXIT

#C BEGIN    -- adrs        target for bwd. branch
#   HERE ; IMMEDIATE
    immed BEGIN,5,"BEGIN",docolon,ELSE
        .word HERE,EXIT

#C UNTIL    adrs --   conditional backward branch
#   ['] qbranch ,BRANCH  ,DEST ; IMMEDIATE
#   conditional backward branch
    immed UNTIL,5,"UNTIL",docolon,BEGIN
        .word LIT,QBRANCH,COMMABRANCH
        .word COMMADEST,EXIT

#X AGAIN    adrs --      uncond'l backward branch
#   ['] branch ,BRANCH  ,DEST ; IMMEDIATE
#   unconditional backward branch
    immed AGAIN,5,"AGAIN",docolon,UNTIL
        .word LIT,BRANCH,COMMABRANCH
        .word COMMADEST,EXIT

#C WHILE    -- adrs         branch for WHILE loop
#   POSTPONE IF ; IMMEDIATE
    immed WHILE,5,"WHILE",docolon,AGAIN
        .word IF,EXIT

#C REPEAT   adrs1 adrs2 --     resolve WHILE loop
#   SWAP POSTPONE AGAIN POSTPONE THEN ; IMMEDIATE
    immed REPEAT,6,"REPEAT",docolon,WHILE
        .word SWAP,AGAIN,THEN,EXIT
        
#Z >L   x --   L: -- x        move to leave stack
#   CELL LP +!  LP @ ! ;      (L stack grows up)
    head TOL,2,">L",docolon,REPEAT
        .word CELL,LP,PLUSSTORE,LP,FETCH,STORE,EXIT

#Z L>   -- x   L: x --      move from leave stack
#   LP @ @  CELL NEGATE LP +! ;
    head LFROM,2,"L>",docolon,TOL
        .word LP,FETCH,FETCH
        .word CELL,NEGATE,LP,PLUSSTORE,EXIT

#C DO       -- adrs   L: -- 0
#   ['] xdo ,XT   HERE     target for bwd branch
#_   0 >L ; IMMEDIATE           marker for LEAVEs
    immed DO,2,"DO",docolon,LFROM
        .word LIT,XDO,COMMAXT,HERE
        .word LIT,0,TOL,EXIT

#Z ENDLOOP   adrs xt --   L: 0 a1 a2 .. aN --
#   ,BRANCH  ,DEST                backward loop
#   BEGIN L> ?DUP WHILE POSTPONE THEN REPEAT ;
#                                 resolve LEAVEs
# This is a common factor of LOOP and +LOOP.
    head ENDLOOP,7,"ENDLOOP",docolon,DO
        .word COMMABRANCH,COMMADEST
LOOP1:  .word LFROM,QDUP,QBRANCH,LOOP2
        .word THEN,BRANCH,LOOP1
LOOP2:  .word EXIT

#C LOOP    adrs --   L: 0 a1 a2 .. aN --
#   ['] xloop ENDLOOP ;  IMMEDIATE
    immed LOOP,4,"LOOP",docolon,ENDLOOP
        .word LIT,XLOOP,ENDLOOP,EXIT

#C +LOOP   adrs --   L: 0 a1 a2 .. aN --
#   ['] xplusloop ENDLOOP ;  IMMEDIATE
    immed PLUSLOOP,5,"+LOOP",docolon,LOOP
        .word LIT,XPLUSLOOP,ENDLOOP,EXIT

#C LEAVE    --    L: -- adrs
#   ['] UNLOOP ,XT
#   ['] branch ,BRANCH   HERE DUP ,DEST  >L
#   ; IMMEDIATE      unconditional forward branch
    immed LEAVE,5,"LEAVE",docolon,PLUSLOOP
        .word LIT,UNLOOP,COMMAXT
        .word LIT,BRANCH,COMMABRANCH
        .word HERE,DUP,COMMADEST,TOL,EXIT

# OTHER OPERATIONS ==============================

#X WITHIN   n1|u1 n2|u2 n3|u3 -- f   n2<=n1<n3?
#  OVER - >R - R> U< ;          per ANS document
    head WITHIN,6,"WITHIN",docolon,LEAVE
        .word OVER,MINUS,TOR,MINUS,RFROM,ULESS,EXIT

#C MOVE    addr1 addr2 u --     smart move
#             VERSION FOR 1 ADDRESS UNIT = 1 CHAR
#  >R 2DUP SWAP DUP R@ +     -- ... dst src src+n
#  WITHIN IF  R> CMOVE>        src <= dst < src+n
#       ELSE  R> CMOVE  THEN ;          otherwise
    head MOVE,4,"MOVE",docolon,WITHIN
        .word TOR,TWODUP,SWAP,DUP,RFETCH,PLUS
        .word WITHIN,QBRANCH,MOVE1
        .word RFROM,CMOVEUP,BRANCH,MOVE2
MOVE1:  .word RFROM,CMOVE
MOVE2:  .word EXIT

#C DEPTH    -- +n        number of items on stack
#   SP@ S0 SWAP - 2/ ;   16-BIT VERSION!
#_ 32-bit version
    head DEPTH,5,"DEPTH",docolon,MOVE
        .word SPFETCH,S0,SWAP,MINUS,TWOSLASH,TWOSLASH,EXIT

#C ENVIRONMENT?  c-addr u -- false   system query
#                         -- i*x true
#_   2DROP 0 ;       the minimal definition!
    head ENVIRONMENTQ,12,"ENVIRONMENT?",docolon,DEPTH
        .word TWODROP,LIT,0,EXIT

# UTILITY WORDS AND STARTUP =====================

#X WORDS    --          list all words in dict.
#   LATEST @ BEGIN
#       DUP COUNT TYPE SPACE
#       NFA>LFA @
#   DUP 0= UNTIL
#   DROP ;
    head WORDS,5,"WORDS",docolon,ENVIRONMENTQ
        .word LATEST,FETCH
WDS1:   .word DUP,COUNT,TYPE,SPACE,NFATOLFA,FETCH
        .word DUP,ZEROEQUAL,QBRANCH,WDS1
        .word DROP,EXIT

#X .S      --           print stack contents
#   SP@ S0 - IF
#       SP@ S0 2 - DO I @ U. -2 +LOOP
#   THEN ;
#_ 32-bit version
    head DOTS,2,".S",docolon,WORDS
        .word SPFETCH,S0,MINUS,QBRANCH,DOTS2
        .word SPFETCH,S0,LIT,4,MINUS,XDO
DOTS1:  .word II,FETCH,UDOT,LIT,-4,XPLUSLOOP,DOTS1
DOTS2:  .word EXIT

#Z COLD     --      cold start Forth system
#   UINIT U0 #INIT CMOVE      init user area
#   80 COUNT INTERPRET       interpret CP/M cmd
#   ." Z80 CamelForth etc."
#   ABORT ;
    head COLD,4,COLD,docolon,DOTS
        .word UINIT,U0,NINIT,CMOVE
        .word XSQUOTE
        .byte 30
        .ascii "-=[ PandaForth ]=- by Torlus"
        .byte 0x0d,0x0a
        .align	
        .word TYPE,ABORT       /* ABORT never returns */

