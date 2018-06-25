
IO_PORT := $401C
F_IMMED = $80
F_HIDDEN = $20

.segment "ZEROPAGE": zeropage
TMP1: .res 1
TMP2: .res 1
TMP3: .res 1
TMP4: .res 1
TMP5: .res 1
TMP6: .res 1
TMP7: .res 1
TMP8: .res 1

Stack: .res 40
Stack_End: .res 8 ; buffer zone

.segment "VINIT"
VINIT_START:

.segment "RAM"
RStack: .res $100

.macro push val
  dex
  dex
  lda #<val
  sta Stack, x
  lda #>val
  sta Stack+1, x
.endmacro

.macro pop
  inx
  inx
.endmacro

.struct DictEntry
  PreviousPtr .word
  CodePtr .word
  Len .byte
  Name .byte
.endstruct

.macro DEBUG_START
  .byte $FF
.endmacro

.macro DEBUG_END
  .byte $FE
.endmacro

.macro DEBUG_TRACE str
   .byte $FD
   .asciiz str
.endmacro

.macro defword name, flags, label, previous
  .segment "DICT"
    .ident(.concat(.string(label), "_DICT_ENTRY")):
    ; previous pointer
    .word .ident(.concat(.string(previous), "_DICT_ENTRY"))

    ; code pointer
    .word .ident(.string(label))
    ; length and name
    .byte .strlen(name) | flags, name
  .segment "DICT_CODE"
    .word .ident(.concat(.string(label), "_DICT_ENTRY"))
    .ident(.string(label)):
.endmacro

.macro defvar name, init, label, previous
  defword name, 0, label, previous
    dex
    dex
    lda #<.ident(.concat(.string(label), "_VALUE"))
    sta Stack, x
    lda #>.ident(.concat(.string(label), "_VALUE"))
    sta Stack+1, x
    rts
  .segment "VARIABLES"
  .ident(.concat(.string(label), "_VALUE")):
  .res 2
  .segment "VINIT"
    .word .ident(.concat(.string(label), "_VALUE"))
    .word init
.endmacro

.macro defvarzp name, init, label, previous
  defword name, 0, label, previous
    dex
    dex
    lda #<.ident(.concat(.string(label), "_VALUE"))
    sta Stack, x
    lda #>.ident(.concat(.string(label), "_VALUE"))
    sta Stack+1, x
    rts
  .segment "ZEROPAGE": zeropage
  .ident(.concat(.string(label), "_VALUE")):
  .res 2
  .segment "VINIT"
    .word .ident(.concat(.string(label), "_VALUE"))
    .word init
.endmacro

.macro defconst name, value, label, previous
  defword name, 0, label, previous
    dex
    dex
    lda #<value
    sta Stack, x
    lda #>value
    sta Stack+1, x
    rts
.endmacro

NULL_DICT_ENTRY := 0

; ( a -- )
defword "DROP", 0, DROP, NULL
  pop
  rts

; ( a b -- b a )
defword "SWAP", 0, SWAP, DROP
  lda Stack, x
  ldy Stack+2, x
  sty Stack, x
  sta Stack+2, x
  lda Stack+1, x
  ldy Stack+3, x
  sty Stack+1, x
  sta Stack+3, x
  rts

; ( a -- a a )
defword "DUP", 0, DUP, SWAP
  dex
  dex
  lda Stack+2, x
  sta Stack, x
  lda Stack+3, x
  sta Stack+1, x
  rts

; ( a b -- a b a )
defword "OVER", 0, OVER, DUP
  dex
  dex
  lda Stack+4, x
  sta Stack, x
  lda Stack+5, x
  sta Stack+1, x

; ( a b c -- b c a )
defword "ROT", 0, ROT, OVER
  lda Stack, x
  pha
  ldy Stack+2, x
  lda Stack+4, x
  sta Stack, x
  sty Stack+4, x
  pla
  sta Stack+2, x
  lda Stack+1, x
  pha
  ldy Stack+3, x
  lda Stack+5, x
  sta Stack+1, x
  sty Stack+5, x
  pla
  sta Stack+3, x
  rts

; ( a -- a a | 0 )
defword "?DUP", 0, QDUP, ROT
  lda Stack, x
  ora Stack+1, x
  beq @done
  jmp DUP
@done:
  rts

; ( a -- a + 1 )
defword "1+", 0, INCR, QDUP
  inc Stack, x
  beq @hi
  rts
@hi:
  inc Stack+1, x
  rts

defword "1-", 0, DECR, INCR
  sec
  lda Stack, x
  sbc #1
  sta Stack, x
  lda Stack+1, x
  sbc #0
  sta Stack+1, x
  rts

defword "+", 0, ADD, DECR
  clc
  lda Stack, x
  adc Stack+2, x
  sta Stack+2, x
  lda Stack+1, x
  adc Stack+3, x
  sta Stack+3, x
  pop
  rts

defword "-", 0, SUB, ADD
  sec
  lda Stack+2, x
  sbc Stack, x
  sta Stack+2, x
  lda Stack+3, x
  sbc Stack+1, x
  sta Stack+3, x
  pop
  rts

defword "=", 0, EQU, SUB
  pop
  lda Stack-2, x
  cmp Stack, x
  beq @hi
  lda #0
  sta Stack, x
  sta Stack+1, x
  rts
@hi:
  ldy #0
  lda Stack-1, x
  cmp Stack+1, x
  bne @diff
  dey
@diff:
  sty Stack, x
  sty Stack+1, x
  rts

defword "0=", 0, ZEQU, EQU
  ldy #0
  lda Stack, x
  ora Stack+1, x
  bne @nonzero
  dey
@nonzero:
  sty Stack, x
  sty Stack+1, x
  rts

defword "0>", 0, ZGT, ZEQU
  lda Stack+1, x
  ldy #0
  bmi @minus
  dey
@minus:
  sty Stack, x
  sty Stack+1, x
  rts

defword "AND", 0, AND_, ZGT
  lda Stack, x
  and Stack+2, x
  sta Stack+2, x
  lda Stack+1, x
  and Stack+3, x
  sta Stack+3, x
  pop
  rts

defword "OR", 0, OR, AND_
  lda Stack, x
  ora Stack+2, x
  sta Stack+2, x
  lda Stack+1, x
  ora Stack+3, x
  sta Stack+3, x
  pop
  rts

defword "XOR", 0, XOR, OR
  lda Stack, x
  eor Stack+2, x
  sta Stack+2, x
  lda Stack+1, x
  eor Stack+3, x
  sta Stack+3, x
  pop
  rts

defword "INVERT", 0, INVERT, XOR
  lda Stack, x
  eor #$FF
  sta Stack, x
  lda Stack+1, x
  eor #$FF
  sta Stack+1, x
  rts

DEX_OP = $CA
LDA_IMM_OP = $A9
STA_ZP_X_OP = $95

defword "!", 0, STORE, INVERT
  lda Stack, x
  sta TMP1
  lda Stack+1, x
  sta TMP2
  ldy #0
  lda Stack+2, x
  sta (TMP1), y
  lda Stack+3, x
  iny
  sta (TMP1), y
  pop
  pop
  rts

defword "+!", 0, ADDSTORE, STORE
  lda Stack, x
  sta TMP1
  lda Stack+1, x
  sta TMP2
  
  ldy #0
  clc
  lda (TMP1), y
  adc Stack+2, x
  sta (TMP1), y
  
  iny
  lda (TMP1), y
  adc Stack+3, x
  sta (TMP1), y
  
  pop
  pop
  rts

defword "-!", 0, SUBSTORE, ADDSTORE
  lda Stack, x
  sta TMP1
  lda Stack+1, x
  sta TMP2
  
  ldy #0
  sec
  lda (TMP1), y
  sbc Stack+2, x
  sta (TMP1), y
  
  iny
  lda (TMP1), y
  sbc Stack+3, x
  sta (TMP1), y
  
  pop
  pop
  rts

defword "C!", 0, CSTORE, SUBSTORE
  lda Stack+2, x
  sta (Stack, x)
  pop
  pop
  rts

defword "@", 0, FETCH, CSTORE
  lda Stack, x
  sta TMP1
  lda Stack+1, x
  sta TMP2
  ldy #0
  lda (TMP1), y
  sta Stack, x
  iny
  lda (TMP1), y
  sta Stack+1, x
  rts

defword "C@", 0, CFETCH, FETCH
  lda (Stack, x)
  sta Stack, x
  lda #0
  sta Stack+1, x
  rts

defconst "VERSION", 1, VERSION, CFETCH

defword ">R", 0, TOR, VERSION
  lda Stack+1, x
  pha
  lda Stack, x
  pha
  pop
  rts

defword "R>", 0, FROMR, TOR
  dex
  dex
  pla
  sta Stack, x
  pla
  sta Stack+1, x
  rts

defword "RDROP", 0, RDROP, FROMR
  pla
  pla
  rts

defword "DSP@", 0, DSPFETCH, RDROP
  dex
  dex
  txa
  sta Stack, x
  ldy #0
  sty Stack+1, x
  rts

defword "DSP!", 0, DSPSTORE, DSPFETCH
  lda Stack, x
  tax
  rts

defword "EMIT", 0, EMIT, DSPSTORE
  lda Stack, x
  sta IO_PORT
  pop
  rts

defword "KEY", 0, KEY, EMIT
  dex
  dex
  lda IO_PORT
  ldy #0
  sta Stack, x
  sty Stack+1, x
  rts

; ( -- str-ptr len )
defword "WORD", 0, WORD, KEY
  lda #0
  sta TMP1
  jsr KEY
  lda Stack, x
  pop
  cmp #'\'
  beq @skipComment
  cmp #' ' + 1
  bcc WORD

@loop:
  ldy TMP1
  sta @word_buffer, y
  inc TMP1
  jsr KEY
  lda Stack, x
  pop
  cmp #' ' + 1
  bcs @loop

  push @word_buffer
  dex
  dex
  lda TMP1
  sta Stack, x
  lda #0
  sta Stack+1, x
  rts

@skipComment:
  jsr KEY
  lda Stack, x
  pop
  cmp #$A ; \n
  bne @skipComment
  beq WORD ; bra

.segment "VARIABLES"
  @word_buffer: .res 32

defvar "BASE", 10, BASE, WORD

defword "1", 0, ONE, BASE
  dex
  dex
  ldy #1
  sty Stack, x
  dey
  sty Stack+1, x
  rts

; ( str len -- parsed-number )
; or ( str len -- str len ) on error
; Also, carry is set if there's an error.
defword "NUMBER", 0, NUMBER, ONE

  Str := TMP1
  Result := TMP3
  Len := TMP5
  ResultAcc := TMP6
  
  lda Stack, x
  sta Len ; save string length

  lda Stack+2, x
  sta Str
  lda Stack+3, x
  sta Str+1 ; save str pointer

  lda #0
  sta Result
  sta Result+1

  txa
  pha ; save the stack pointer
  
  ldy #0
@eachDigit:
  lda (Str), y
  cmp #'A'
  bcc @digit ; if ascii < 'A', treat is as a decimal digit
  and #<~$20 ; convert to uppercase letter.
  sbc #'A' - '9' - 1 ; move it to the range $3A+, right after '9'.
@digit:
  sec
  sbc #'0' ; now it should be in a range of 0-BASE-1
  cmp BASE_VALUE
  bcs @invalid ; if digit >= BASE
  pha ; save the digit

    ldx BASE_VALUE ; loop BASE times, to multiply Result by BASE using repeated addition
    lda #0
    sta ResultAcc
    sta ResultAcc+1 ; reset ResultAcc
  @multLoop:
    clc
    lda ResultAcc
    adc Result
    sta ResultAcc
    lda ResultAcc+1
    adc Result+1
    sta ResultAcc+1 ; add Result to ResultAcc
    dex
    bne @multLoop

  pla ; get the digit
  clc
  adc ResultAcc
  sta Result
  lda ResultAcc+1
  adc #0
  sta Result+1 ; Result = ResultAcc + Digit

  iny
  cpy Len
  bne @eachDigit ; loop again for the next digit until we've exhausted the string.

  pla
  tax
  pop ; drop the string length
  lda Result
  sta Stack, x
  lda Result+1
  sta Stack+1, x ; put parsed number of the stack.
  clc ; signal OK
  rts

@invalid:
  pla
  tax ; restore stack pointer
  sec ; signal the error
  rts

defvarzp "LATEST", EXECUTE_DICT_ENTRY, LATEST, NUMBER

; ( str-ptr len -- dictionary-pointer )
; or ( str-ptr len -- str-ptr len 0 ) if it wasn't found
; Searches the dictionary for a definition of the given word.
defword "FIND", 0, FIND, LATEST

  LPointer := TMP1
  MyStr := TMP3

  lda LATEST_VALUE
  sta LPointer
  lda LATEST_VALUE+1
  sta LPointer+1 ; load LATEST into LPointer

  sec
  lda Stack+2, x
  sbc #<(DictEntry::Name)
  sta MyStr
  lda Stack+3, x
  sbc #>(DictEntry::Name)
  sta MyStr+1 ; put the string pointer - name offset in MyStr
              ; that way we can use lda (MyStr), y to read
              ; MyStr using the same offset that we do from LPointer

; check if LPointer matches
@loop:
  ldy #(DictEntry::Len)
  lda (LPointer), y ; get length from dictionary entry
  and #$1F | F_HIDDEN ; mask to get length, allowing hidden to change length.
  cmp Stack, x ; compare to the length on the stack
  bne @next ; if lengths don't match, follow the next pointer.

  ; now compare the strings.
  lda Stack, x
  clc
  adc #(DictEntry::Name) - 1
  tay ; Init y to check the end of the string

@strcmpLoop:
  lda (LPointer), y
  cmp (MyStr), y
  bne @next
  dey
  cpy #(DictEntry::Len)
  beq @found
  bne @strcmpLoop ; bra

@next:
  ldy #(DictEntry::PreviousPtr+1)
  lda (LPointer), y
  pha
  dey
  lda (LPointer), y
  sta LPointer
  pla
  sta LPointer+1 ; follow the link and store in Current.

  ; Check if current is now the null pointer.
  lda LPointer
  ora LPointer+1
  beq @notFound
  bne @loop

@notFound:
  dex
  dex
  lda #0
  sta Stack, x
  sta Stack+1, x
  rts
@found:
  pop
  lda LPointer
  sta Stack, x
  lda LPointer+1
  sta Stack+1, x
  rts

; ( dict-ptr -- code-ptr )
defword ">CFA", 0, TOCFA, FIND
  lda Stack, x
  sta TMP1
  lda Stack+1, x
  sta TMP2

  ldy #(DictEntry::CodePtr)
  lda (TMP1), y
  sta Stack, x
  iny
  lda (TMP1), y
  sta Stack+1, x
  rts

; Code Here pointer
; Points to the next free byte in the code area.
defvarzp "CHERE", CHERE_INIT, CHERE, TOCFA

; Dictionary Here pointer
; Points to the next free byte in the dictionary area.
defvarzp "DHERE", DHERE_INIT, DHERE, CHERE

; ( str-ptr len -- )
; Creates a new dictionary entry
defword "CREATE", 0, CREATE, DHERE
  ; Set previous pointer
  ldy #(DictEntry::PreviousPtr)
  lda LATEST_VALUE
  sta (DHERE_VALUE), y
  lda LATEST_VALUE+1
  iny
  sta (DHERE_VALUE), y ; store the previous pointer

  ; LATEST = DHERE
  lda DHERE_VALUE
  sta LATEST_VALUE
  lda DHERE_VALUE+1
  sta LATEST_VALUE+1

  ; Set code pointer to CHERE+2
  ldy #(DictEntry::CodePtr)
  clc
  lda CHERE_VALUE
  adc #<2
  sta (DHERE_VALUE), y
  lda CHERE_VALUE+1
  adc #>2
  iny
  sta (DHERE_VALUE), y
  
  lda #0
  lda DHERE_VALUE
  sta (CHERE_VALUE), y
  lda DHERE_VALUE+1
  iny
  sta (CHERE_VALUE), y ; store the dictionary pointer in the code space

  lda Stack, x ; length byte
  ldy #(DictEntry::Len)
  sta (DHERE_VALUE), y ; store the length byte

  clc
  lda #<2
  adc CHERE_VALUE
  sta CHERE_VALUE
  lda #>2
  adc CHERE_VALUE+1
  sta CHERE_VALUE+1 ; add 2 to CHERE

  clc
  lda #<(DictEntry::Len+1)
  adc DHERE_VALUE
  sta DHERE_VALUE
  lda #>(DictEntry::Len+1)
  adc DHERE_VALUE+1
  sta DHERE_VALUE+1 ; move DHERE to point after the Len byte.

  ; now we need to copy the name string.
  lda Stack, x ; get length
  tay
  dey

  lda Stack+2, x
  sta TMP1
  lda Stack+3, x
  sta TMP2 ; copy str pointer into TMP1

@loop:
  lda (TMP1), y
  sta (DHERE_VALUE), y
  dey
  bpl @loop

  clc
  lda Stack, x ; get length
  adc DHERE_VALUE
  sta DHERE_VALUE
  lda #0
  adc DHERE_VALUE+1
  sta DHERE_VALUE+1 ; add string length to DHERE.

  pop
  pop
  rts

defword ",", 0, COMMA, CREATE
  ldy #0
  lda Stack, x
  sta (CHERE_VALUE), y
  lda Stack+1, x
  iny
  sta (CHERE_VALUE), y

  clc
  lda CHERE_VALUE
  adc #<2
  sta CHERE_VALUE
  lda CHERE_VALUE+1
  adc #>2
  sta CHERE_VALUE+1
  pop
  rts

defword "C,", 0, CCOMMA, COMMA
  lda Stack, x
  ldy #0
  sta (CHERE_VALUE), y
  
  inc CHERE_VALUE
  bne @done
  inc CHERE_VALUE+1
@done:
  pop
  rts

defvar "STATE", 0, STATE, CCOMMA

defword "[", F_IMMED, LSQUARE, STATE
  lda #0
  sta STATE_VALUE
  sta STATE_VALUE+1
  rts

defword "]", 0, RSQUARE, LSQUARE
  lda #1
  sta STATE_VALUE
  lda #0
  sta STATE_VALUE+1
  rts

defword ":", 0, COLON, RSQUARE
  jsr WORD
  jsr CREATE ; create the dictionary entry
  jsr LATEST
  jsr FETCH
  jsr HIDDEN ; toggle hidden in the entry
  jmp RSQUARE ; enter compile mode

RTS_OP = $60

defword ";", F_IMMED, SEMICOLON, COLON
  push RTS_OP
  jsr CCOMMA ; append rts to code

  jsr LATEST
  jsr FETCH
  jsr HIDDEN ; toggle hidden flag
  jmp LSQUARE ; go back to immediate mode

defword "IMMEDIATE", F_IMMED, IMMEDIATE, SEMICOLON
  ldy #(DictEntry::Len)
  lda #F_IMMED
  eor (LATEST_VALUE), y
  sta (LATEST_VALUE), y
  rts

; ( dict-ptr -- )
; Marks the dictionary entry as hidden
defword "HIDDEN", 0, HIDDEN, IMMEDIATE
  lda Stack, x
  sta TMP1
  lda Stack+1, x
  sta TMP2

  ldy #(DictEntry::Len)
  lda #F_HIDDEN
  eor (TMP1), y
  sta (TMP1), y

  pop
  rts

defword "HIDE", 0, HIDE, HIDDEN
  jsr WORD
  jsr FIND
  jmp HIDDEN

defword "[']", 0, TICK, HIDE
  jsr WORD
  jsr FIND
  jmp TOCFA

CLV_OP = $B8
BVC_OP = $50

defword "BRANCH", F_IMMED, BRANCH, TICK
  lda STATE_VALUE
  bne @compiling
  rts
@compiling:
  push (CLV_OP | (BVC_OP << 8))
  jsr COMMA ; append clv; bvc
  push 0
  jmp CCOMMA ; append 0, will be filled in later

BEQ_OP = $F0
JSR_OP = $20

defword "0BRANCH", F_IMMED, ZBRANCH, BRANCH
  lda STATE_VALUE
  bne @compiling
  rts
@compiling:
  push JSR_OP
  jsr CCOMMA
  push ZBRANCH_TEST
  jsr COMMA ; append jsr ZBRANCH_TEST 
  push BEQ_OP
  jsr COMMA ; append BEQ 0
ZBRANCH_TEST:
  pop
  lda Stack-2, x
  ora Stack-1, x
  rts

defword "QUIT", 0, QUIT, ZBRANCH
  stx TMP1
    ldx #$FF
    txs
  ldx TMP1
  jsr INTERPRET
  jmp QUIT

defword "INTERPRET", 0, INTERPRET, QUIT
  jsr WORD
  jsr FIND
  lda Stack, x
  ora Stack+1, x
  beq @notFound
  
  lda Stack, x ; now that we have the dictionary entry, check if it's immediate.
  sta TMP1
  lda Stack+1, x
  sta TMP2
  ldy #(DictEntry::Len)
  lda (TMP1), y
  and #F_IMMED
  bne @execute

  lda STATE_VALUE
  beq @execute
@compiling:
  push JSR_OP
  jsr CCOMMA
  
  ldy #(DictEntry::CodePtr)
  lda (TMP1), y
  sta Stack, x
  iny
  lda (TMP1), y
  sta Stack+1, x
  jsr COMMA
  rts

@execute:
  pop ; drop dictionary pointer
  ldy #(DictEntry::CodePtr)
  lda (TMP1), y
  sta TMP3
  iny
  lda (TMP1), y
  sta TMP4
  jmp (TMP3) ; tailcall to the word, which will return to QUIT

@notFound:
  pop ; drop 0 error
  ; Didn't find the word in the dictionary, maybe it's a literal.
  jsr NUMBER ; parse as number 
  bcs @nan ; if error code set
  ; Now we have a number on the top of stack.
  lda STATE_VALUE
  beq @executeLiteral
@compileLiteral:
  push (DEX_OP | (DEX_OP << 8))
  jsr COMMA ; compile dex; dex
  jsr DUP
  lda Stack, x
  sta Stack+1, x
  lda #LDA_IMM_OP
  sta Stack, x
  jsr COMMA ; compile LDA #lo
  push ((STA_ZP_X_OP) | (Stack << 8))
  jsr COMMA ; compile sta Stack, x
  lda #LDA_IMM_OP
  sta Stack, x
  jsr COMMA ; compile LDA #hi
  push ((STA_ZP_X_OP) | ((Stack+1) << 8))
  jsr COMMA ; compile sta Stack+1, x
  rts
@executeLiteral:
  rts

@nan:
  ; pop ; drop -1 code
  lda Stack+2, x
  sta TMP1
  lda Stack+3, x
  sta TMP2 ; TMP12 = str-pointer

  ldy #0
@messageLoop:
  lda @errorMessage, y
  beq @printWord
  sta IO_PORT
  iny 
  bne @messageLoop ; bra
@printWord:
  ldy #0
@printWordLoop:
  lda (TMP1), y
  sta IO_PORT
  iny
  tya
  cmp Stack, x
  bne @printWordLoop; if y != string length
  rts ; return to QUIT

@errorMessage:
  .byte $A, "ERROR: Couldn't find word: ", 0

defword "CHAR", 0, CHAR, INTERPRET
  jsr WORD
  pop
  lda (Stack, x)
  sta Stack, x
  lda #0
  sta Stack+1, x
  rts

defword ".", 0, DOT, CHAR
  ; Set the V flag. While v is set, we will skip
  ; leading 0s. Once we see a digit which is non-zero, clv.
  bit @setV
  pop

  lda Stack-1, x
  ora Stack-2, x
  bne @nonZero
  jsr @zero
@nonZero:

  lda Stack-1, x
  jsr @digit
  lda Stack-2, x
  jsr @digit
@ending:
  lda #' '
  sta IO_PORT
  rts

@digit:
  sta TMP1
  lsr
  lsr
  lsr
  lsr
  beq :+
  clv
: bvs :+
  tay
  lda HexDigits, y
  sta IO_PORT
: lda TMP1
  and #$F
  beq :+
  clv
: bvs @setV
@zero:
  tay
  lda HexDigits, y
  sta IO_PORT
@setV:
  rts

HexDigits: .byte "0123456789ABCDEF"

defword ".s", 0, DOT_S, DOT
  txa
  cmp #Stack_End - 1
  beq @done
  jsr DOT
  jmp DOT_S
@done:
  rts

defword "CR", 0, CR, DOT_S
  lda #10
  sta IO_PORT
  rts

defword "HI", 0, HI, CR
  lda Stack+1, x
  sta Stack, x
  lda #0
  sta Stack+1, x
  rts

defword "LO", 0, LO, HI
  lda #0
  sta Stack+1, x
  rts

; Variable which points to the next free variable RAM space.
defvar "VHERE", VHERE_VALUE+2, VHERE, LO

; Executes the word on the stack.
defword "EXECUTE", 0, EXECUTE, VHERE
  lda Stack, x
  sta TMP1
  lda Stack+1, x
  sta TMP2
  jmp (TMP1)

.segment "DICT"
DHERE_INIT:

.segment "DICT_CODE"
CHERE_INIT:

.segment "VINIT"
VINIT_END:
.assert VINIT_END - VINIT_START < 100, error

.segment "CODE"
reset:
  ; Initialize the variables.
  ; VINIT is organized as | ADDR | VALUE | ADDR | VALUE | ...
  ldx #0
@vinitLoop:
  lda VINIT_START, x
  sta TMP1
  lda VINIT_START+1, x
  sta TMP1+1
  lda VINIT_START+2, x
  ldy #0
  sta (TMP1), y
  iny
  lda VINIT_START+3, x
  sta (TMP1), y
  inx
  inx
  inx
  inx
  cpx #(VINIT_END - VINIT_START)
  bne @vinitLoop

  lda #'>'
  sta IO_PORT
  lda #' '
  sta IO_PORT
  lda 10
  sta IO_PORT
  ldx #Stack_End - 1

  jmp QUIT

.segment "VECTORS"
.word 0
.word reset
.word 0
