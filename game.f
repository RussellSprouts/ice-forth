hex

0 c-val vppu-ctrl
0 c-val vppu-mask

0700 constant sprites

( Palette addresses:
  3F01-3: bg 0
  3F05-7: bg 1
  3F09-B: bg 2
  3F0D-F: bg 3
  3F11-3: sp 0
  3F15-7: sp 1
  3F19-B: sp 2
  3F1D-F: sp 3

3F10: universal background color )

\ Define 25 (base 10) bytes of space for
\ palette entries.
variable pal-buf 17 allot

: write-palettes [
  3F LDA.#
  2006 STA
  01 LDA.#
  2006 STA \ set ppu address to first entry
  
  pal-buf     LDA
  2007        STA
  pal-buf 1+  LDA
  2007        STA
  pal-buf 2 + LDA
  2007        STA
  2007        STA \ skip extra entry

  pal-buf 3 + LDA
  2007        STA
  pal-buf 4 + LDA
  2007        STA
  pal-buf 5 + LDA
  2007        STA
  2007        STA \ skip extra entry

  pal-buf 6 + LDA
  2007        STA
  pal-buf 7 + LDA
  2007        STA
  pal-buf 8 + LDA
  2007        STA
  2007        STA \ skip extra entry

  pal-buf 9 +  LDA
  2007         STA
  pal-buf 0A + LDA
  2007         STA
  pal-buf 0B + LDA
  2007         STA
  2007         STA \ skip extra entry
] ;

: nmi [

  PHA
  TXA
  PHA
  TYA
  PHA

  val-addr vppu-mask LDA
  18 AND.#
  IFNE
    sprites >byte LDA.#
    4014          STA

    \ pal-update LDA
    IFNE
      0 LDX.#
      \ pal-update STX

      3F LDA.#
      2006 STA
      2006 STX

    THEN
  THEN

] ;

0 val frame-started

: main
  begin frame-started until
  0 to frame-started
 
  recurse-tail 
;

: set-ppu
  dup >byte 2006 !
  <byte 2006 !
;

: reset
  \ TODO DO STUFF HERE
  main
;

['] reset set-reset!

: frame
  save-for-interrupt

  \ sprites >byte OAM-DMA !

  0 set-ppu
  
  16 0 do
    i 2007 !
  loop

  1 to frame-started

  restore-for-interrupt
;int

['] frame set-nmi!

: ram-test save-ram rest-ram ;
: t ." compressed:" ;

ram-test
freeze

