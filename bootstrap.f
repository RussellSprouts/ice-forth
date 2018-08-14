
: stack 8 ;

\ special case these instructions
: TXA 138 c, ;
: TYA 152 c, ;
: TXS 154 c, ;

: IF  chere @ ;
: IFEQ  0 BEQ IF ;
: IFNE  0 BNE IF ;
: IFCC  0 BCC IF ;
: IFCS  0 BCS IF ;
: IFVC  0 BVC IF ;
: IFVS  0 BVS IF ;
: IFPL  0 BPL IF ;
: IFMI  0 BMI IF ;

: THEN
  dup
  chere @ swap -
  swap 1- c! 
;

: test [
  stack LDA.ZX
  32    CMP.#
  IFEQ
    stack INC.ZX
  THEN
] ;

\ a b -- a b a
: over [
  DEX
  DEX
  stack 4 + LDA.ZX
  stack     STA.ZX
  stack 5 + LDA.ZX
  stack 1 + STA.ZX  ] ;

\ ( a b c -- b c a )
: rot [
  stack     LDA.ZX
  PHA
  stack 2 + LDY.ZX
  stack 4 + LDA.ZX
  stack     STA.ZX
  stack 4 + STY.ZX
  PLA
  stack 2 + STA.ZX
  stack 1 + LDA.ZX
  PHA
  stack 3 + LDY.ZX
  stack 5 + LDA.ZX

  stack 1 + STA.ZX
  stack 5 + STY.ZX
  PLA
  stack 3 + STA.ZX
] ;

: and [
  stack     LDA.ZX
  stack 2 + AND.ZX
  stack 2 + STA.ZX
  
  stack 1+  LDA.ZX
  stack 3 + AND.ZX
  stack 3 + STA.ZX
  
  INX INX
] ;

: or [
  stack     LDA.ZX
  stack 2 + ORA.ZX
  stack 2 + STA.ZX

  stack 1+  LDA.ZX
  stack 3 + ORA.ZX
  stack 3 + STA.ZX

  INX INX
] ;

: xor [
  stack     LDA.ZX
  stack 2 + EOR.ZX
  stack 2 + STA.ZX

  stack 1+  LDA.ZX
  stack 3 + EOR.ZX
  stack 3 + STA.ZX

  INX INX
] ;

\ Logical shift right
\ ( u -- u )
: lsr [
  stack 1+ LSR.ZX
  stack    ROR.ZX
] ;

\ Arithmetic shift right
\ ( i -- i )
: asr [
  stack 1+ LDA.ZX
  128      CMP.#
  stack 1+ ROR.ZX
  stack    ROR.ZX
] ;

\ Arithmetic shift left
: asl [
  stack    ASL.ZX
  stack 1+ ROL.ZX
] ;

: emit [
  stack LDA.ZX
  16412 STA \ $401C
  INX
  INX
] ;

: c>r always-inline [
  stack LDA.ZX
  PHA
  INX
  INX
] ;

: cr> always-inline [
  DEX
  DEX
  PLA
  stack    STA.ZX
  0        LDA.#
  stack 1+ STA.ZX
] ;

: >r always-inline [
  stack 1+ LDA.ZX
  PHA
  stack    LDA.ZX
  PHA
  INX
  INX
] ;

: r> always-inline [
  DEX
  DEX
  PLA
  stack    STA.ZX
  PLA
  stack 1+ STA.ZX
] ;

\ ( r: a -- )
: rdrop always-inline [
  PLA
  PLA
] ;

\ ( -- sp )
: dsp@ [
  DEX
  DEX
  TXA
  stack    STA.ZX
  0        LDY.#
  stack 1+ STY.ZX
] ;

\ ( sp -- )
: dsp! [
  stack LDA.ZX
  TAX
] ;

: ['] word find ;

: '\n' 10 ;
: bl 32 ;

: space bl emit ;

: negate 0 swap - ;

: true 0 1 - ;
: false 0 ;
: not 0= ;

: hex 16 base ! ;
: decimal 10 base ! ;

: jsr, 32 c, , ;
: jmp, 76 c, , ;

\ Recursively call the current word
: recurse immediate
  latest @
  jsr,
;

: recurse-tail immediate
  latest @
  jmp,
;

\ Takes the next word and compiles it even if it's immediate
: [compile] immediate
  word
  find
  jsr,
;

: 2- 2 - ;
: 2+ 2 + ;

hex
  \ todo -- integrate the interpreter to autogenerate these rules
  : POP INX INX ;

decimal

\ Save branch instruction address
: if immediate
  \ [compile] debug
  POP
  stack 2- LDA.ZX
  stack 1- ORA.ZX
  chere @
  0 BEQ
;

: unless immediate
  ['] not jsr,
  [compile] if
;

\ Write the branch target to here.
: then immediate
  dup
  chere @ swap - 2-
  swap 1+ c! 
;

: else immediate
  chere @ 1+
  swap
  CLV 0 BVC
  dup
  chere @ swap - 2-
  swap 1+ c!
;

: begin immediate
  \ [compile] debug
  chere @
;

\ ( branch-target -- )
: until immediate
  POP
  stack 2- LDA.ZX
  stack 1- ORA.ZX
  chere @ - 2- BEQ
;

: while
  POP
  stack 2- LDA.ZX
  stack 1- ORA.ZX
  chere @ - 2- BNE
;

: literal immediate
  DEX DEX
  dup
  <byte LDA.#
  stack STA.ZX
  >byte LDA.#
  stack 1+ STA.ZX
;

: '(' [ char ( ] literal ;
: ')' [ char ) ] literal ;
: '"' [ char " ] literal ;


: ( immediate
  1
  begin
    key
    dup '(' = if
      drop
      1+
    else
      ')' = if
        1-
      then
    then
  dup 0= until
  drop
;

( Now I can write comments using (nested) parens )

: allot
  vhere +!
;

( Declares a constant value. Use like `10 constant VariableName`)
: constant immediate
  word
  create
  [compile] literal
  RTS
;

1 constant version

( Declares an uninitialized variable, giving it space
  after vhere )
: variable immediate
  vhere @
  2 allot
  [compile] constant
;

( xt -- impl )
: >impl
  dict::impl + @
;

( Takes a dictionary entry and prints the name of the word )
: id.
  dict::len +    ( Skip the pointers )
  dup c@ ( get the length )
  31 and ( Mask the flags )
  
  begin
    swap 1+ ( addr len -- len addr+1 )
    dup c@ ( len addr -- len addr char )
    emit
    swap 1- ( len addr -- addr len-1 )

    dup 0=
  until 
  drop
  drop
;

: ?hidden
  dict::len +
  c@
  32 and
;

: ?immediate
  dict::len +
  c@
  128 and
;

( Returns the next dictionary entry. Assumes execute is the last )
: next
  dup [ ['] execute literal ] = if
    drop
    0
  else
    dup dict::len + c@ 63 and + dict::name +
  then
;

: words
  latest @ ( read latest entry )
  begin
    dup ?hidden not if
      dup id.
      space
    then
    next
    dup 0=
  until
  drop ( drop null pointer )
  cr
;

: compiling? state @ ;

( -- )
: ." immediate
  compiling? if
    [ ['] (.') ] literal jsr, ( compile jsr (.") )

    begin
      key
      dup '"' <> if
        c,
        0
      then
    until
    0 c,
  else
    begin
      key
      dup '"' <> if
        emit
        0
      then
    until
  then
;

: welcome
  ." Welcome to Forth!" cr
;

welcome

( A jump to 0 is treated as a signal to
  the emulator to stop execution and freeze
  the ROM )
: freeze
  [ 0 jsr, ] 
;

( A variable which, when called, pushes its value instead of its address )
: val
  vhere @ ( get the variable address )
  2 allot ( allot two variables )
  word
  create ( create a new dictionary entry )
  DEX DEX
  dup LDA ( load the value from the variable and push it to the stack )
  stack STA.ZX
  dup 1+ LDA
  stack 1+ STA.ZX 
  RTS

  ( initialize val )
  !

  [ ['] always-inline literal ] execute drop
;

( Gets the address of a val )
: val-addr
  word
  find
  dup 0= if
    drop
    drop
    drop
    ." Cannot get address of unknown val." cr
    quit
  then
  >impl 3 + @ ( read variable address from val impl )
;

( Writes a value to a `val` variable )
: to immediate
  val-addr 
  compiling? if
    dup
    stack LDA.ZX
    STA
    stack 1+ LDA.ZX
    1+ STA
    INX INX
  else
    !
  then
;

hex
( xt -- )
: set-reset! 0FFFC ! ;
( xt -- )
: set-nmi! 0FFFA ! ;
( xt -- )
: set-irq! 0FFFE ! ;

( Ends an interrupt handler definiton )
: ;int immediate
  40 c, \ append rti
  latest @ hidden \ unhide
  [compile] [
;
decimal

: int-handle ;int

['] int-handle set-nmi!
['] int-handle set-irq!

( new-xt old-xt -- )
( Redefines old as new, so that all calls to old
  will instead call new. Doesn't rePLAce inlined calls )
: monkey-patch
  dict::impl + !
;

: inline,
  1-
  begin
    1+ dup
    c@ dup c,
    96 =
  until
  drop
  \ undo writing the rts
  chere @ 1- chere !
;

( a simple inline which just copies the impl until hitting an rts.
  It will be confused by any 0x60 byte )
: [inline] immediate
  word find >impl inline,
;

: disas
  20
  begin
    swap
    see
    swap 1-
    dup 0=
  until
  drop
  drop
;

: do immediate
  \ inline code to push the loop bound and inDEX onto the return stack
  [ ['] c>r >impl literal ]
  dup inline,
      inline,

  \ save the address of the beginning of the loop
  chere @
;

: loop immediate
  \ inline code to pull the loop bounds from the return stack
  PLA
  0 STA.Z
  0 INC.Z
  PLA
  0 CMP.Z
  6 BEQ
  PHA
  0 LDA.Z
  PHA
  chere @ - 2- BNE
;

: i always-inline
  [ PLA
  DEX DEX
  stack STA.ZX
  PHA
  0 LDA.#
  stack 1+ STA.ZX ]
;
