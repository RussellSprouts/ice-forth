; This file bootstraps enough code to start a STC Forth
; for the 6502 system. It will allow programming NES programs
; using Forth. It's modelled using a NES-like memory
; map:
;   00-FF: zero page
;   100-1FF: hardware stack
;   200-7FF: User RAM
;   6000-8000: More RAM - used for the dictionary
;   8000-FFFF: Cartridge space
;
; Unlike a NES cartridge, initially the cartridge space is
; RAM. The code will run in the simulator, and the user can
; interact with it, test functions, etc. Once they execute
; The word FREEZE, however, the contents of RAM from 8000-FFFF
; will be extracted and put into a .nes file with an appropriate
; header.
;
; The implementation loosely follows Jones Forth.
;
; Specifics:
;
; - The data stack is stored in the zero page with the x register
; as the stack pointer.
; - Words are compiled to a series of jsr instructions, or to inline
; code.
; - The words are relocatable. The entry point for each word is the same
; as the dictionary entry.

; The simulator uses this address as the IO port.
; A write to this address will output that character
; to stdout, and a read will read the next character from
; stdin, possibly blocking, since the simulator reads line-by-
; line.
IO_PORT := $401C

; Flags for the dictionary entries.
; The word should execute immediately even in compile mode
F_IMMED = $80
; The word should not be found in a dictionary search.
F_HIDDEN = $20

.segment "DICT"
; Reserve space to push the dictionary to the end of the memory
; space, since it now grows down.
.res $D61

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

; A segment for initializing variables.
.segment "VINIT"
VINIT_START:

.segment "RAM"
; Reserve the hardware stack.
RStack: .res $100

; Pushes the given value onto the stack.
.macro push val
  dex
  dex
  lda #<val
  sta Stack, x
  lda #>val
  sta Stack+1, x
.endmacro

; Removes the top of stack.
.macro pop
  inx
  inx
.endmacro

.macro toTMP1
  lda Stack, x
  sta TMP1
  lda Stack+1, x
  sta TMP2
.endmacro

; The structure of a dictionary entry.
.struct DictEntry
  Jmp .byte
  CodePtr .word
  ; The pointer to the previous dictionary entry.
  PreviousPtr .word
  ; The length of the name. Also includes the flags
  Len .byte
  ; The entry name
  Name .byte
.endstruct

; The simulator treats $FF as a command to start
; logging each instruction for debugging.
.macro DEBUG_START
  .byte $FF
.endmacro

; The simulator treats $EF as a command to stop
; logging each instruction.
.macro DEBUG_END
  .byte $EF
.endmacro

; The simulator treats $DF as a command to print
; out the next bytes until a zero character.
.macro DEBUG_TRACE str
   .byte $DF
   .asciiz str
.endmacro

; New dictionary format, unifying xt and dictionary entry.
; jmp CODE_IMPL
; .word LINK
; .word IMMEDIATE | HIDDEN | LEN
; .byte "DUP"
; 
; The dictionary entry length does not change - once it is created
; with a name, it is fixed. However, we can update the jmp CODE_IMPL
; instruction to move the code around.
.macro defword name, flags, label, previous
  .segment "DICT"
    label:
    jmp .ident(.concat(.string(label), "_IMPL"))
    ; previous pointer
    .word previous
    ; length and name
    .byte .strlen(name) | flags
    .byte name
  .segment "DICT_CODE"
    .ident(.concat(.string(label), "_IMPL")):
.endmacro

; Variables are a special kind of word which reserve
; two bytes of space in the VARIABLES segment, and when
; executed, push that address on the stack.
; We want them to keep their values even after freezing,
; so we also put an entry in VINIT, which consists of the
; address of the variable and the initial value. On reset,
; the code reads those entries and initializes all of them.
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

; The same as a variable, except it is allocated in the zero
; page.
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

; A constant is a word which pushes its value onto the stack
; when it is executed.
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

defconst "dict::impl", DictEntry::CodePtr, DictEntryCodePtr, DictEntryPreviousPtr
defconst "dict::prev", DictEntry::PreviousPtr, DictEntryPreviousPtr, DictEntryLen
defconst "dict::len", DictEntry::Len, DictEntryLen, DictEntryName
defconst "dict::name", DictEntry::Name, DictEntryName, DROP

; ( a -- )
defword "drop", 0, DROP, SWAP
  pop
  rts

; ( a b -- b a )
defword "swap", 0, SWAP, DUP
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
defword "dup", 0, DUP, OVER
  dex
  dex
  lda Stack+2, x
  sta Stack, x
  lda Stack+3, x
  sta Stack+1, x
  rts

; ( a b -- a b a )
defword "over", 0, OVER, ROT
  dex
  dex
  lda Stack+4, x
  sta Stack, x
  lda Stack+5, x
  sta Stack+1, x

; ( a b c -- b c a )
defword "rot", 0, ROT, QDUP
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
defword "?dup", 0, QDUP, INCR
  lda Stack, x
  ora Stack+1, x
  beq @done
  jmp DUP
@done:
  rts

; ( a -- a + 1 )
defword "1+", 0, INCR, DECR
  inc Stack, x
  beq @hi
  rts
@hi:
  inc Stack+1, x
  rts

defword "1-", 0, DECR, ADD
  sec
  lda Stack, x
  sbc #1
  sta Stack, x
  lda Stack+1, x
  sbc #0
  sta Stack+1, x
  rts

defword "+", 0, ADD, SUB
  clc
  lda Stack, x
  adc Stack+2, x
  sta Stack+2, x
  lda Stack+1, x
  adc Stack+3, x
  sta Stack+3, x
  pop
  rts

defword "-", 0, SUB, EQU
  sec
  lda Stack+2, x
  sbc Stack, x
  sta Stack+2, x
  lda Stack+3, x
  sbc Stack+1, x
  sta Stack+3, x
  pop
  rts

defword "=", 0, EQU, NEQU
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

defword "<>", 0, NEQU, ZEQU
  pop
  lda Stack-2, x
  cmp Stack, x
  beq @hi
  lda #$FF
  sta Stack, x
  sta Stack+1, x
  rts
@hi:
  ldy #$0
  lda Stack-1, x
  cmp Stack+1, x
  beq @same
  dey
@same:
  sty Stack, x
  sty Stack+1, x
  rts 

defword "0=", 0, ZEQU, ZGT
  ldy #0
  lda Stack, x
  ora Stack+1, x
  bne @nonzero
  dey
@nonzero:
  sty Stack, x
  sty Stack+1, x
  rts

defword "0>", 0, ZGT, AND_
  lda Stack+1, x
  ldy #0
  bmi @minus
  dey
@minus:
  sty Stack, x
  sty Stack+1, x
  rts

defword "and", 0, AND_, OR
  lda Stack, x
  and Stack+2, x
  sta Stack+2, x
  lda Stack+1, x
  and Stack+3, x
  sta Stack+3, x
  pop
  rts

defword "or", 0, OR, XOR
  lda Stack, x
  ora Stack+2, x
  sta Stack+2, x
  lda Stack+1, x
  ora Stack+3, x
  sta Stack+3, x
  pop
  rts

defword "xor", 0, XOR, INVERT
  lda Stack, x
  eor Stack+2, x
  sta Stack+2, x
  lda Stack+1, x
  eor Stack+3, x
  sta Stack+3, x
  pop
  rts

defword "invert", 0, INVERT, STORE
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

defword "!", 0, STORE, ADDSTORE
  toTMP1
  ldy #0
  lda Stack+2, x
  sta (TMP1), y
  lda Stack+3, x
  iny
  sta (TMP1), y
  pop
  pop
  rts

defword "+!", 0, ADDSTORE, SUBSTORE
  toTMP1
  
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

defword "-!", 0, SUBSTORE, CSTORE
  toTMP1
  
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

defword "c!", 0, CSTORE, FETCH
  lda Stack+2, x
  sta (Stack, x)
  pop
  pop
  rts

defword "@", 0, FETCH, CFETCH
  toTMP1

  ldy #0
  lda (TMP1), y
  sta Stack, x
  iny
  lda (TMP1), y
  sta Stack+1, x
  rts

defword "c@", 0, CFETCH, CMOVE
  lda (Stack, x)
  sta Stack, x
  lda #0
  sta Stack+1, x
  rts

; ( ptr1 ptr2 n -- )
defword "cmove", 0, CMOVE, VERSION

defconst "version", 1, VERSION, TOR

defword ">r", 0, TOR, FROMR
  ; Store return pointer temporarily.
  pla
  sta TMP1
  pla
  sta TMP2

  lda Stack+1, x
  pha
  lda Stack, x
  pha

  ; Restore return pointer
  lda TMP2
  pha
  lda TMP1
  pha

  pop
  rts

defword "r>", 0, FROMR, RDROP
  ; Store return pointer temporarily
  pla
  sta TMP1
  pla
  sta TMP2

  dex
  dex
  pla
  sta Stack, x
  pla
  sta Stack+1, x

  ; Restore return pointer
  lda TMP2
  pha
  lda TMP1
  pha
  
  rts

defword "rdrop", 0, RDROP, DSPFETCH
  pla
  pla
  rts

defword "dsp@", 0, DSPFETCH, DSPSTORE
  dex
  dex
  txa
  sta Stack, x
  ldy #0
  sty Stack+1, x
  rts

defword "dsp!", 0, DSPSTORE, EMIT
  lda Stack, x
  tax
  rts

defword "emit", 0, EMIT, KEY
  lda Stack, x
  sta IO_PORT
  pop
  rts

defword "key", 0, KEY, WORD
  dex
  dex
  lda IO_PORT
  ldy #0
  sta Stack, x
  sty Stack+1, x
  rts

; ( -- str-ptr len )
defword "word", 0, WORD, BASE
  lda #0
  sta TMP1
  jsr KEY
  lda Stack, x
  pop
  cmp #'\'
  beq @skipComment
  cmp #' ' + 1
  bcc WORD_IMPL

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
  beq WORD_IMPL ; bra

.segment "VARIABLES"
  @word_buffer: .res 32

defvar "base", 10, BASE, NUMBER

; ( str len -- parsed-number )
; or ( str len -- str len ) on error
; Also, carry is set if there's an error.
defword "number", 0, NUMBER, LATEST

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

defvarzp "latest", DictEntryCodePtr, LATEST, FIND

; ( str-ptr len -- dictionary-pointer )
; or ( str-ptr len -- str-ptr len 0 ) if it wasn't found
; Searches the dictionary for a definition of the given word.
defword "find", 0, FIND, CHERE

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

; Code Here pointer
; Points to the next free byte in the code area.
defvarzp "chere", CHERE_INIT, CHERE, DHERE

; Dictionary Here pointer
; The dictionary grows downward, so this points to the
; first used byte in dictionary memory.
; This must be initialized to the first dict entry.
defvarzp "dhere", DictEntryCodePtr, DHERE, CREATE

JMP_OP = $4C

; ( str-ptr len -- )
; Creates a new dictionary entry
defword "create", 0, CREATE, COMMA
  ; The dict entry needs str-len + DictEntry::Name bytes of space.
  ; Subtract from DHERE to allocate the space.
  jsr DUP ; str length
  push DictEntry::Name
  jsr ADD
  jsr DHERE
  jsr FETCH
  jsr SWAP
  jsr SUB
  jsr DHERE
  jsr STORE

  ; Set the new entry's previous pointer
  ; LATEST @ DHERE @ PreviousPtr + !
  jsr LATEST
  jsr FETCH
  jsr DHERE
  jsr FETCH
  push DictEntry::PreviousPtr
  jsr ADD
  jsr STORE

  ; Update LATEST to the new entry
  ; DHERE @ LATEST !
  jsr DHERE
  jsr FETCH
  jsr LATEST
  jsr STORE

  ; ( stack is now string-ptr len )
  ; Store length in new entry
  ; DUP DHERE Len + C!
  jsr DUP
  jsr DHERE
  jsr FETCH
  push DictEntry::Len
  jsr ADD
  jsr CSTORE

  ; Write jmp CHERE to the dictionary entry.
  ; JMP_OP dhere @ Jmp + c!
  push JMP_OP
  jsr DHERE
  jsr FETCH
  push DictEntry::Jmp
  jsr ADD
  jsr CSTORE
  ; chere @ dhere @ codeptr + !
  jsr CHERE
  jsr FETCH
  jsr DHERE
  jsr FETCH
  push DictEntry::CodePtr
  jsr ADD
  jsr STORE  
  
  ; Move DHERE past the Len byte and pointers
  clc
  lda #<(DictEntry::Name)
  adc DHERE_VALUE
  sta TMP3
  lda #>(DictEntry::Name)
  adc DHERE_VALUE+1
  sta TMP4

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
  sta (TMP3), y
  dey
  bpl @loop

  pop
  pop
  rts

defword ",", 0, COMMA, CCOMMA
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

defword "c,", 0, CCOMMA, STATE
  lda Stack, x
  ldy #0
  sta (CHERE_VALUE), y
  
  inc CHERE_VALUE
  bne @done
  inc CHERE_VALUE+1
@done:
  pop
  rts

defvar "state", 0, STATE, LSQUARE

defword "[", F_IMMED, LSQUARE, RSQUARE
  lda #0
  sta STATE_VALUE
  sta STATE_VALUE+1
  rts

defword "]", 0, RSQUARE, COLON
  lda #1
  sta STATE_VALUE
  lda #0
  sta STATE_VALUE+1
  rts

defword ":", 0, COLON, SEMICOLON
  jsr WORD
  jsr CREATE ; create the dictionary entry
  jsr LATEST
  jsr FETCH
  jsr HIDDEN ; toggle hidden in the entry
  jmp RSQUARE ; enter compile mode

RTS_OP = $60

defword ";", F_IMMED, SEMICOLON, IMMEDIATE
  push RTS_OP
  jsr CCOMMA ; append rts to code

  jsr LATEST
  jsr FETCH
  jsr HIDDEN ; toggle hidden flag
  jmp LSQUARE ; go back to immediate mode

defword "immediate", F_IMMED, IMMEDIATE, HIDDEN
  ldy #(DictEntry::Len)
  lda #F_IMMED
  eor (LATEST_VALUE), y
  sta (LATEST_VALUE), y
  rts

; ( dict-ptr -- )
; Marks the dictionary entry as hidden
defword "hidden", 0, HIDDEN, HIDE
  toTMP1

  ldy #(DictEntry::Len)
  lda #F_HIDDEN
  eor (TMP1), y
  sta (TMP1), y

  pop
  rts

defword "hide", 0, HIDE, TICK
  jsr WORD
  jsr FIND
  jmp HIDDEN

defword "[']", 0, TICK, QUIT
  jsr WORD
  jmp FIND

CLV_OP = $B8
BVC_OP = $50

BEQ_OP = $F0
JSR_OP = $20

defword "quit", 0, QUIT, INTERPRET
  stx TMP1
    cpx #Stack_End
    bcs @underflow
    ldx #$FF
    txs
  ldx TMP1
  jsr INTERPRET
  jmp QUIT
@underflow:
  jsr DODOTQUOTE
  .asciiz "Stack underflow detected!"
  ldx #Stack_End-1
  jmp QUIT

defword "interpret", 0, INTERPRET, CHAR
  jsr WORD
  jsr FIND
  lda Stack, x
  ora Stack+1, x
  beq @notFound
  
  ; now that we have the dictionary entry, check if it's immediate.
  toTMP1
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
  pop
  pop ; discard string length and pointer.
  rts ; return to QUIT

@errorMessage:
  .byte $A, "ERROR: Couldn't find word: ", 0

defword "char", 0, CHAR, DOT
  jsr WORD
  pop
  lda (Stack, x)
  sta Stack, x
  lda #0
  sta Stack+1, x
  rts

defword ".", 0, DOT, DOT_S
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

defword ".s", 0, DOT_S, CR
  txa
  cmp #Stack_End - 1
  beq @done
  jsr DOT
  jmp DOT_S
@done:
  rts

defword "cr", 0, CR, HI
  lda #10
  sta IO_PORT
  rts

defword ">byte", 0, HI, LO
  lda Stack+1, x
  sta Stack, x
  lda #0
  sta Stack+1, x
  rts

defword "<byte", 0, LO, VHERE
  lda #0
  sta Stack+1, x
  rts

; Variable which points to the next free variable RAM space.
defvar "vhere", VHERE_VALUE+2, VHERE, DODOTQUOTE

; Prints out the following bytes as a zero-terminated string.
; Use like:
;   jsr DODOTQUOTE
;   .asciiz "Some string"
defword "(.')", 0, DODOTQUOTE, EXECUTE
  pla
  sta TMP1
  pla
  sta TMP2

  ldy #1 ; add 1 because the jsr pushes the last byte of the jsr
@loop:
  lda (TMP1), y
  beq @done
  sta IO_PORT
  iny
  bne @loop  
@done:
  clc
  tya
  adc TMP1
  sta TMP1
  lda TMP2
  adc #0
  pha
  lda TMP1
  pha
  rts

; Executes the word on the stack.
defword "execute", 0, EXECUTE, 0
  toTMP1
  jmp (TMP1)

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

  ldx #Stack_End - 1

  jmp QUIT

.segment "VECTORS"
.word 0
.word reset
.word 0
