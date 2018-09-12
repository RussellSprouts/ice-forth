hex

0 val colors
0 val addr

: main
    ppu-wait-nmi
    colors 1+ 7 and to colors

    addr 1+ and 1FFF to addr

    colors asl asl asl asl asl to vppu-mask
;

['] nmi set-nmi!

: done
  freeze
  wait-for-ppu
  [
    80 LDA.#
    2000 STA
  ]
  begin main 0 until
;
