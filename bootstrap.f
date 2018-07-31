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

\ Recursively call the current word
: recurse immediate
  latest @
  jsr,
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
  : lda.i 0A9 c, c, ;
  : lda.a 0AD c, , ;
  : sta.a 08D c, , ;
  : lda.zx 0B5 c, c, ;
  : lda.z  0A5 c, c, ;
  : sta.zx 095 c, c, ;
  : sta.z 085 c, c, ;
  : inc.zx 0F6 c, c, ;
  : inc.z  0E6 c, c, ;
  : cmp.zx 0D5 c, c, ;
  : cmp.z  0C5 c, c, ;
  : beq 0F0 c, c, ;
  : bne 0D0 c, c, ;
  : ora.zx 015 c, c, ;
  : pop 0E8E8 , ;
  : dex;dex 0CACA , ;
  : inx;inx 0E8E8 , ;
  : clv;bvc 050B8 , c, ;
  : rts 060 c, ;
  : pla 068 c, ;
  : pha 048 c, ;

  : debug_start immediate 0FF c, ;
  : debug_end immediate 0FE c, ;

  : stack 8 ;
decimal

\ Save branch instruction address
: if immediate
  \ [compile] debug
  pop
  stack 2- lda.zx
  stack 1- ora.zx
  chere @
  0 beq
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
  0 clv;bvc
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
  pop
  stack 2- lda.zx
  stack 1- ora.zx
  chere @ - 2- beq
;

: while
  pop
  stack 2- lda.zx
  stack 1- ora.zx
  chere @ - 2- bne
;

: literal immediate
  dex;dex
  dup
  <byte lda.i
  stack sta.zx
  >byte lda.i
  stack 1+ sta.zx
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
  rts
;

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

: words
  latest @ ( read latest entry )
  begin
    dup ?hidden not if
      dup id.
      space
    then
    dict::prev + @ ( read previous pointer )
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
  dex;dex
  dup lda.a ( load the value from the variable and push it to the stack )
  stack sta.zx
  dup 1+ lda.a
  stack 1+ sta.zx 
  rts

  ( initialize val )
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
    stack lda.zx
    sta.a
    stack 1+ lda.zx
    1+ sta.a
    inx;inx
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
  will instead call new. Doesn't replace inlined calls )
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
  \ inline code to push the loop bound and index onto the return stack
  [ ['] c>r >impl literal ]
  dup inline,
      inline,

  \ save the address of the beginning of the loop
  chere @
;

: loop immediate
  \ inline code to pull the loop bounds from the return stack
  pla
  0 sta.z
  0 inc.z
  pla
  0 cmp.z
  6 beq
  pha
  0 lda.z
  pha
  chere @ - 2- bne
;

: i always-inline
  [ pla
  dex;dex
  stack sta.zx
  pha
  0 lda.i
  stack 1+ sta.zx ]
;

: t
  0 10 do
    ." LOOK AROUND YOU! "
  loop
;

['] t >impl disas
." And:"
['] t >impl 28 + disas

