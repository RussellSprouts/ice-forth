
: '\n' 10 ;
: BL 32 ;

: SPACE BL EMIT ;

: NEGATE 0 SWAP - ;

: TRUE 0 1 - ;
: FALSE 0 ;
: NOT 0= ;

: JSR, 32 C, , ;

\ Recursively call the current word
: RECURSE IMMEDIATE
  LATEST @
  >CFA
  JSR,
;

\ Takes the next word and compiles it even if it's immediate
: [COMPILE] IMMEDIATE
  WORD
  FIND
  >CFA
  JSR,
;

: 2- 2 - ;
: 2+ 2 + ;

16 BASE !
: lda.i 0A9 C, C, ;
: lda.zx 0B5 C, C, ;
: sta.zx 095 C, C, ;
: beq 0F0 C, C, ;
: bne 0D0 C, C, ;
: ora.zx 015 C, C, ;
: pop 0E8E8 , ;
: dex;dex 0CACA , ;
: clv;bvc 050B8 , C, ;
: rts 060 C, ;

: DEBUG IMMEDIATE 0FF C, ;
: DEBUG_END IMMEDIATE 0FE C, ;

: STACK 8 ;
0A BASE !

\ Save branch instruction address
: IF IMMEDIATE
  \ [COMPILE] DEBUG
  pop
  STACK 2- lda.zx
  STACK 1- ora.zx
  CHERE @
  0 beq
;

: UNLESS IMMEDIATE
  ['] NOT JSR,
  [COMPILE] IF
;

\ Write the branch target to here.
: THEN IMMEDIATE
  DUP
  CHERE @ SWAP - 2-
  SWAP 1+ C! 
;

: ELSE IMMEDIATE
  CHERE @ 1+
  SWAP
  0 clv;bvc
  DUP
  CHERE @ SWAP - 2-
  SWAP 1+ C!
;

: TEST IF EMIT ELSE 1+ EMIT THEN ;

: BEGIN IMMEDIATE
  \ [COMPILE] DEBUG
  CHERE @
;

\ ( branch-target -- )
: UNTIL IMMEDIATE
  pop
  STACK 2- lda.zx
  STACK 1- ora.zx
  CHERE @ - 2- beq
;

: WHILE
  pop
  STACK 2- lda.zx
  STACK 1- ora.zx
  CHERE @ - 2- bne
;

: LITERAL IMMEDIATE
  dex;dex
  DUP
  LO lda.i
  STACK sta.zx
  HI lda.i
  STACK 1+ sta.zx
;

: '(' [ CHAR ( ] LITERAL ;
: ')' [ CHAR ) ] LITERAL ;

: ( IMMEDIATE
  1
  BEGIN
    KEY
    DUP '(' = IF
      DROP
      1+
    ELSE
      ')' = IF
        1-
      THEN
    THEN
  DUP 0= UNTIL
  DROP
;

( Now I can write comments using (nested) parens )

: ALLOT
  VHERE +!
;

( Declares a constant value. Use like `10 CONSTANT VariableName`)
: CONSTANT IMMEDIATE
  WORD
  CREATE
  [COMPILE] LITERAL
  rts
;

( Declares an uninitialized variable, giving it space
  after VHERE )
: VARIABLE IMMEDIATE
  VHERE @
  2 ALLOT
  [COMPILE] CONSTANT
;

( Takes a dictionary entry and prints the name of the word )
: ID.
  4 +    ( Skip the pointers )
  DUP C@ ( get the length )
  31 AND ( Mask the flags )
  
  BEGIN
    SWAP 1+ ( addr len -- len addr+1 )
    DUP C@ ( len addr -- len addr char )
    EMIT
    SWAP 1- ( len addr -- addr len-1 )

    DUP 0=
  UNTIL 
  DROP
  DROP
;

: ?HIDDEN
  4 +
  C@
  32 AND
;

: ?IMMEDIATE
  4 +
  C@
  128 AND
;

: WORDS
  LATEST @ ( read latest entry )
  BEGIN
    DUP ?HIDDEN NOT IF
      DUP ID.
      SPACE
    THEN
    @ ( read previous pointer )
    DUP 0=
  UNTIL
  DROP ( drop null pointer )
  CR
;
