
<tmp> definitions:

  \ Define addresses of the stack and tmp variables.
  : stack 8 ;
  : tmp 0 ;
  : io-port 16412 ;


  \ Macro to move the top of stack into tmp.
  : >TMP
    stack    LDA.ZX
    tmp      STA.Z
    stack 1+ LDA.ZX
    tmp 1+   STA.Z
  ;

  \ special case these instructions
  : TXA 138 c, ;
  : TYA 152 c, ;
  : TXS 154 c, ;

<perm> definitions:

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

<tmp> definitions:

  \ Define the inline assembly language IF and THEN constructs.
  \ Rather than using labeled branches, we can do structured
  \ control flow. IFEQ, for example, will branch to the matching
  \ THEN if the Z flag is non-zero.
  : IF  chere @ ;
  : IFEQ  0 BNE IF ;
  : IFNE  0 BEQ IF ;
  : IFCC  0 BCS IF ;
  : IFCS  0 BCC IF ;
  : IFVC  0 BVS IF ;
  : IFVS  0 BVC IF ;
  : IFPL  0 BMI IF ;
  : IFMI  0 BPL IF ;

  : THEN
    dup
    chere @ swap -
    swap 1- c! 
  ;

  : ELSE
    CLV
    IFVS
    swap
    THEN
  ;

  \ Define the assembly language looping constructs.
  \ A BEGIN..UNTIL loop will continue looping until
  \ the condition code given is set.
  : BEGIN  chere @ ;
  : UNTIL  chere @ - 2 - ;
  : UNTILEQ  UNTIL BNE ;
  : UNTILNE  UNTIL BEQ ;
  : UNTILCC  UNTIL BCS ;
  : UNTILCS  UNTIL BCC ;
  : UNTILVC  UNTIL BVS ;
  : UNTILVS  UNTIL BVC ;
  : UNTILPL  UNTIL BMI ;
  : UNTILMI  UNTIL BPL ;

  : WHILE  chere @ ;
  : WHILEEQ  0 BNE WHILE ;
  : WHILENE  0 BEQ WHILE ;
  : WHILECC  0 BCS WHILE ;
  : WHILECS  0 BCC WHILE ;
  : WHILEVC  0 BVS WHILE ;
  : WHILEVS  0 BVC WHILE ;
  : WHILEPL  0 BMI WHILE ;
  : WHILEMI  0 BPL WHILE ;

  : REPEAT
    CLV
    swap chere @ - 2 - BVC \ bra to start of loop
    dup
    chere @ swap -
    swap 1- c!
  ;

  : flags  dhere @ dict::len + ;

  : immediate  flags @ 128 xor flags ! ;
  immediate \ mark the word immediate as immediate

  : always-inline immediate
    flags @ 64 xor flags ! ;

  : .s [
    BEGIN
      TXA
      79 CMP.#
    WHILENE
      ] . [
    REPEAT
  ] ;
 
<perm> definitions:

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

  \ Executes the xt on the stack 
  : execute [
    >TMP
    tmp JMP.I
  ] ;

  : = [
    INX INX
    stack 2 - LDA.ZX
    stack     CMP.ZX
    IFNE
      0        LDA.#
      stack    STA.ZX
      stack 1+ STA.ZX
      RTS
    THEN
    0        LDY.#
    stack 1- LDA.ZX
    stack 1+ CMP.ZX
    IFEQ
      DEY
    THEN
    stack    STY.ZX
    stack 1+ STY.ZX
  ] ;

  : <> [
    INX INX
    stack 2 - LDA.ZX
    stack     CMP.ZX
    IFNE
     255      LDA.#
     stack    STA.ZX
     stack 1+ STA.ZX
     RTS
   THEN
   0 LDY.#
   stack 1- LDA.ZX
   stack 1+ CMP.ZX
   IFNE
     DEY
   THEN
   stack    STY.ZX
   stack 1+ STY.ZX
   ] ;

  : 0= [
    0        LDY.#
    stack    LDA.ZX
    stack 1+ ORA.ZX
    IFEQ
      DEY
    THEN
    stack    STY.ZX
    stack 1+ STY.ZX
  ] ;

  : 0> [
    0        LDY.#
    stack 1+ LDA.ZX
    IFPL
      DEY
    THEN
    stack    STY.ZX
    stack 1+ STY.ZX
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

  : >byte [
    stack 1+ LDA.ZX
    stack    STA.ZX
    0        LDA.#
    stack 1+ STA.ZX
  ] ;

  : <byte always-inline [
    0 LDA.#
    stack 1+ STA.ZX
  ] ;

  : +! [
    >TMP

    0         LDY.#
    tmp       LDA.IY
    CLC
    stack 2 + ADC.ZX
    tmp       STA.IY

    INY
    tmp       LDA.IY
    stack 3 + ADC.ZX
    tmp       STA.IY

    INX INX
    INX INX
  ] ;

  : -! [
    >TMP

    0         LDY.#
    tmp       LDA.IY
    SEC
    stack 2 + SBC.ZX
    tmp       STA.IY

    INY
    tmp       LDA.IY
    stack 3 + SBC.ZX
    tmp       STA.IY

    INX INX
    INX INX
  ] ;

<tmp> definitions:

  : emit [
    stack LDA.ZX
    io-port STA \ $401C
    INX
    INX
  ] ;

  : cr
    10 emit ;

<perm> definitions:

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
  dhere @
  jsr,
;

: recurse-tail immediate
  dhere @
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
: repeat immediate
  CLV
  chere @ - 2- BVC
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

: char
  word
  [
    INX INX \ drop word length
    stack    LDA.XI
    stack    STA.ZX
    0        LDA.#
    stack 1+ STA.ZX
  ]
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

: ?end
  dict::flags +
  c@
  128 and
;

: ?immediate
  dict::len +
  c@
  128 and
;

( Returns the next dictionary entry. )
: next
  dup ?end if
    drop
    0
  else
    dup dict::len + c@ 63 and + dict::name +
  then
;

( Prints all the words in the dictionary )
( dict-start -- )
: words'
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

( Prints all the words in all the dictionaries )
: words
  dicts
  begin
    words'
  dup 0= until
  drop
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

: c-val
  vhere @ ( get the variable address )
  1 allot
  word
  create ( create a new dictionary entry )
  DEX DEX
  dup LDA ( load the value and push it )
  stack STA.ZX
  0 LDA.#
  stack 1+ STA.ZX
  RTS
  ( initialize )
  !
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

: c-to immediate
  val-addr 
  compiling? if
    stack LDA.ZX
    STA
    INX INX
  else
    c!
  then
;

hex
( xt -- )
: set-reset! 0FFFC ! ;
( xt -- )
: set-nmi! 0FFFA ! ;
( xt -- )
: set-irq! 0FFFE ! ;


['] thaw set-reset!

( Ends an interrupt handler definiton )
: ;int immediate
  40 c, \ append rti
  dhere @ hidden \ unhide
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
  \ inline code to put the loop bound then 
  c-sp      LDY.Z
  DEY
  DEY
  c-sp      STY.Z
  stack     LDA.ZX
  cstack    STA.Y
  stack  2+ LDA.ZX
  cstack 1+ STA.Y
  INX
  INX
  INX
  INX \ drop the values on the stack

  \ save the address of the beginning of the loop
  chere @
;

: loop immediate
  c-sp   LDY.Z
  cstack LDA.Y
  CLC
  1      ADC.#
  cstack 1+ CMP.Y
  cstack STA.Y
  UNTILEQ 
;


: i [
  DEX
  DEX
  0 LDA.#
  stack 1+ STA.ZX
  c-sp LDY.Z
  cstack LDA.Y
  stack STA.ZX
] ;

: save-for-interrupt always-inline [
  DEX DEX \ make room for the red zone
  PHA
  TYA
  PHA
  tmp LDA.Z
  PHA
  tmp 1+  LDA.Z
  PHA
  tmp 2 + LDA.Z
  PHA
  tmp 3 + LDA.Z
  PHA
  tmp 4 + LDA.Z
  PHA
  tmp 5 + LDA.Z
  PHA
  tmp 6 + LDA.Z
  PHA
  tmp 7 + LDA.Z
  PHA
] ;

: restore-for-interrupt always-inline [
  PLA
  tmp 7 + STA.Z
  PLA
  tmp 6 + STA.Z
  PLA
  tmp 5 + STA.Z
  PLA
  tmp 4 + STA.Z
  PLA
  tmp 3 + STA.Z
  PLA
  tmp 2 + STA.Z
  PLA
  tmp 1+  STA.Z
  PLA
  tmp     STA.Z
  
  PLA
  TAY
  PLA
  INX INX \ remove the red zone
] ;

hex
\ Wait a few frames for the PPU to stabilize on power-on
: wait-for-ppu [
  BEGIN
    2002 BIT
  UNTILMI
  BEGIN
    2002 BIT
  UNTILMI
  BEGIN
    2002 BIT
  UNTILMI
  BEGIN
    2002 BIT
  UNTILMI
] ;

['] nmi set-nmi!


