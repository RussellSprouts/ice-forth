hex

0 val colors
0 val addr

: frame
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

: init
  [ 80 LDA.#
    2000 STA ]
;

: done
  freeze
  wait-for-ppu
  init
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

['] print9 co-start .
['] print8 co-start .
500 ['] printany co-start >co
