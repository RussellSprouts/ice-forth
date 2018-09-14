hex

0 val colors
0 val addr

: main
    ppu-wait-nmi
    colors 1+ 7 and to colors

    addr 1+ and 1FFF to addr
    [
      val-addr addr 1+ LDA
      2006 STA
      val-addr addr LDA
      2006 STA
      val-addr colors LDA
      2007 STA
    ]


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
