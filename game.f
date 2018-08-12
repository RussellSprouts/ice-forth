
hex

0700 constant sprites
4014 constant OAM-DMA

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
  asm-reset
  main
;

['] reset set-reset!

: frame
  save-for-interrupt

  sprites >byte OAM-DMA !

  0 set-ppu
  
  0 16 do
    i 2007 !
  loop

  1 to frame-started

  restore-for-interrupt
;int

['] frame set-nmi!
