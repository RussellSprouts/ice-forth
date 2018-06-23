
IO_PORT := $401C
DICT_IMMEDIATE = $80

.macro push val
  dex
  sta Stack, x
  lda val
.endmacro

.macro drop
  lda Stack, x
  inx
.endmacro

.macro push2 val
  push #>(val)
  push #<(val)
.endmacro

.macro DictEntry name, flags, label, previous
  .ident(label):
  .byte .strlen(name) | flags, name
  .word previous
  .ident(.concat(label, "_IMPL")):
.endmacro

.segment "ZEROPAGE"
.res 4
Stack: .res 64
DictionaryLastEntry: .res 2
TMP: .res 8

.segment "RAM"

ReturnStack: .res $100
WordPad: .res 33

.segment "CODE"

ldx #63
ldy #0
: lda Message, y
  beq :+
  iny
  sta IO_PORT
  jmp :-
: 
  ldy #0
@loop:
  jsr BL_IMPL
  jsr WORD_IMPL
  jsr FIND_IMPL
  jsr CDOT_IMPL
  jsr DOT_IMPL
  jmp @loop

Message:
.byte "Hello world!", 0

DictEntry "FIND", 0, "FIND", 0
  ldy #>LOAD
  sty DictionaryLastEntry+1
  ldy #<LOAD
  sty DictionaryLastEntry

  push DictionaryLastEntry+1
  push DictionaryLastEntry
@loop:
  jsr DUP2_IMPL
  jsr STRCMP_IMPL
  bne @found
  drop ; drop flag from strcmp
  jsr DUP_IMPL ; dup dict entry address
  jsr CLOAD_IMPL ; C@
  tay
  iny
  tya
  jsr EXTEND_IMPL ; EXTEND
  jsr PLUS_IMPL ; +
  jsr LOAD_IMPL ; @
  jmp @loop
@found:
  drop ; drop flag from strcmp
  jsr DUP_IMPL ; dup dict entry address
  jsr CLOAD_IMPL ; get length of string header
  push #3 ; skip string length, link to previous
  jsr CPLUS_IMPL
  jsr EXTEND_IMPL ; extend to 2 byte number
  jsr PLUS_IMPL
  push #$FF ; push true
  rts

DictEntry "EMIT", 0, "EMIT", FIND
  sta IO_PORT
  inx
  inx 
  rts

DictEntry ".", 0, "DOT", EMIT
  sta TMP+1
  lda Stack, x
  jsr one_digit
  lda TMP+1
  jsr one_digit
  lda #' '
  sta IO_PORT
  inx
  inx
  lda Stack-1, x
  rts
one_digit:
  sta TMP
  lsr
  lsr
  lsr
  lsr
  tay
  lda digits, y
  sta IO_PORT
  lda TMP
  and #$F
  tay
  lda digits, y
  sta IO_PORT
  rts
digits:
.repeat 16
.byte "0123456789ABCDEF"
.endrepeat

DictEntry .concat(".", .string('"')), DICT_IMMEDIATE, "DOT_QUOTE", DOT
  rts ; todo

DictEntry "(", DICT_IMMEDIATE, "PAREN", DOT_QUOTE
  lda IO_PORT
  cmp #')'
  bne PAREN_IMPL
  rts

DictEntry "BL", 0, "BL", PAREN
  push #' '
  rts

; Gets characters from io-port until
; the end of the t.o.s. character is
; reached. Pushes the address of the
; string.
DictEntry "WORD", 0, "WORD", BL
  sta TMP
  ldy #0
: lda IO_PORT
  cmp TMP
  beq @done
  cmp #$0A ; \n
  beq @done
  sta WordPad+1, y
  iny
  cpy #33
  bne :-
@done:
  sty WordPad
  dex
  lda #>WordPad
  sta Stack, x
  lda #<WordPad
  rts

DictEntry "2DUP", 0, "DUP2", WORD
  ;                    (a, +0, +1, +2, +3, +4, +5, +6)
  ; (a, +0, +1, +2) -> (a, +0, +1, +2,  a, +0, +1, +2)
  dex
  dex
  dex
  dex
  sta Stack+3, x
  ldy Stack+6, x
  sty Stack+2, x
  ldy Stack+5, x
  sty Stack+1, x
  ldy Stack+4, x
  sty Stack+0, x
  rts

DictEntry "DUP", 0, "DUP", DUP2
  ldy Stack, x
  dex
  sta Stack, x
  dex
  sty Stack, x
  rts

DictEntry "CDUP", 0, "CDUP", DUP
  dex
  sta Stack, x
  rts

DictEntry "STRCMP", 0, "STRCMP", CDUP
  @A := TMP
  @B := TMP+2
  sta @A
  lda Stack, x
  sta @A+1
  lda Stack+1, x
  sta @B
  lda Stack+2, x
  sta @B+1
  ldy #0
  lda (@A), y
  cmp (@B), y
  bne @ne
  tay ; initialize to last character in string
  beq @eq ; zero length strings are equal
@loop:
  lda (@A), y ; check current characters
  cmp (@B), y ; 
  bne @ne
  dey
  bne @loop
@eq:
  inx
  inx
  inx
  lda #$FF
  rts
@ne:
  inx
  inx
  inx
  lda #0
  rts

DictEntry "CLOAD", 0, "CLOAD", STRCMP
  sta Stack-1, x
  lda (Stack-1, x)
  inx
  rts

DictEntry "+", 0, "PLUS", CLOAD
  clc
  adc Stack+1, x
  tay
  lda Stack, x
  adc Stack+2, x
  sta Stack+2, x
  tya
  inx
  inx
  rts

DictEntry "C+", 0, "CPLUS", PLUS
  clc
  adc Stack, x
  inx
  rts

DictEntry "EXTEND", 0, "EXTEND", PLUS
  ldy #0
  dex
  sty Stack, x
  rts

DictEntry "@", 0, "LOAD", EXTEND
  sta TMP
  lda Stack, x
  sta TMP+1
  ldy #1
  lda (TMP), y
  sta Stack, x
  dey
  lda (TMP), y
  rts
