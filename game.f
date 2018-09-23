<perm> definitions:

hex

0 val colors
0 val addr

0 c-val spr
80 c-val Y

20 val ballvx
20 val ballvy
500 val ballx
800 val bally

( x y -- )
: draw| 
  [ char | bl - literal ]
  0
  spr oam-spr to spr ;

( x y -- )
: draw-paddle
  over over 8 +
  draw| draw|
;

( x y -- )
: draw-ball
  [ char . bl - literal ] 0 spr oam-spr to spr 
;

: frame
    \ colors 1+ 7 and to colors
    0 to spr

    joy1 btnU and if
      Y 4 - to Y
    then
    joy1 btnD and if
      Y 4 + to Y
    then

    ballvx ballx + to ballx
    ballvy bally + to bally

    ballx asr asr asr asr
    dup 100 > if
      1000 to ballx
      ballvx negate to ballvx
    else dup 0 < if
      0 to ballx
      ballvx negate to ballvx
    then then

    bally asr asr asr asr
    dup E0 > if
      E00 to bally
      ballvy negate to ballvy
    else dup 0 < if
      0 to bally
      ballvy negate to ballvy
    then then
    draw-ball

    10 \ x
    Y \ y
    draw-paddle

    E0 10 draw-paddle

    addr 1+ 1FFF and to addr

    vppu-mask 1F and
    colors asl asl asl asl asl or to vppu-mask
    
    vppu-mask
    [
      stack LDA.ZX
      FE AND.#
      2001 STA
    ]
    drop
;

: init
  font 0000 mv>ppu
  font 1000 mv>ppu
  19 to vppu-mask
;

: done
  freeze \ stop emulation and write NES ROM
  wait-for-ppu
  \ Disable rendering
  [ 0 LDA.#
    2000 STA ]
  init
  \ Enable NMIs
  [ 80 LDA.#
    2000 STA ]
  \ Loop forever and call the frame function each frame
  begin
    ppu-wait-nmi
    frame
  0 until
;

: print9
  c-sp @ .
  dsp@ .
  0A 0 do
    900 i + .
    yield
  loop
;

: print8
  c-sp @ .
  dsp@ .
  0A 0 do
    800 i + .
    yield
  loop
;

: printany
  c-sp @ .
  dsp@ .
  0A 0 do
    dup i + .
    yield
  loop
  drop
;

' print9 co-start .
' print8 co-start .
500 ' printany co-start >co

400 printany

0 15 pal-col!
1 16 pal-col!
2 30 pal-col!
3 31 pal-col!
4 pal-bright
80 to vppu-ctrl
