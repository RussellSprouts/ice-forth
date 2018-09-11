hex

0700 constant sprites

0 val frame-started

val colors

: main
  begin
    ppu-wait-nmi
    colors 1+ 7 and to colors

    colors rol rol rol to vppu-mask
  0 until
;

['] nmi set-nmi!

: 2000! [
  stack LDA.ZX
  2000 STA
] ;

: done
  freeze
  wait-for-ppu
  80 2000!
  main ;
