.byte "Trying a ROM hack? Source code available! https://github.com/RussellSprouts/ice-forth", 0


; Pushes the given value onto the stack.
; You can use immediate mode with #, or load
; from memory without.
.macro push arg
  dex
  dex
  .if (.match (.left (1, {arg}), #))
    ; immediate mode
    lda #<(.right (.tcount ({arg})-1, {arg}))
    sta Stack, x
    lda #>(.right (.tcount ({arg})-1, {arg}))
    sta Stack+1, x
  .else
    ; assume absolute or zero page
    lda arg
    sta Stack, x
    lda 1+(arg)
    sta Stack+1, x
  .endif
.endmacro

; Pushes a single byte value onto the stack.
; You can use memory, or `pushb a` to push the
; value of the a register.
.macro pushb arg
  dex
  dex
  .if (.not .match ({arg}, a))
    lda arg
  .endif
  sta Stack, x
  lda #0
  sta Stack+1, x
.endmacro

; Removes the top of stack.
.macro pop
  inx
  inx
.endmacro

; Copy the top of stack to the address.
.macro copyTo addr
  lda Stack, x
  sta addr
  lda Stack+1, x
  sta addr+1
.endmacro

.macro cmpTopZero
  lda Stack, x
  ora Stack+1, x
.endmacro

.struct DictEntry
  Jmp .byte
  CodePtr .word
  Flags2 .byte
  Len .byte
  Name .byte
.endstruct

; Defines a dictionary entry
; dict - the dictionary segment to place the entry in
; dict_code - the code segment to place the implementation in
; name - the forth name of the word, as a string
; flags - the dictionary flags
; label - the label of the word to use in assembly code
.macro defword_dict dict, dict_code, name, flags, label
  .segment dict
    label:
    .export label
    jmp .ident(.concat(.string(label), "_IMPL"))
    ; length and name
    .byte >flags
    .byte .strlen(name) | <flags
    .byte name
  .segment dict_code
    .ident(.concat(.string(label), "_IMPL")):
.endmacro

; Defines a dictionary entry in the permanent dictionary
.macro defword name, flags, label
  defword_dict "DICT", "DICT_CODE", name, flags, label
.endmacro

; Defines a dictionary entry in the temporary dictionary
.macro defwordtmp name, flags, label
  defword_dict "TMP_DICT", "TMP_DICT_CODE", name, flags, label
.endmacro

; Variables are a special kind of word which reserve
; two bytes of space in the VARIABLES segment, and when
; executed, push that address on the stack.
.macro defvar_dict vars, name, init, label
  defword name, 0, label
    push #.ident(.concat(.string(label), "_VALUE"))
    rts
  .segment vars
  .ident(.concat(.string(label), "_VALUE")):
  .word init
.endmacro

.macro defvar name, init, label
  defvar_dict "VARIABLES", name, init, label
.endmacro

.macro defvartmp name, init, label
  defvar_dict "TMP_VARIABLES", name, init, label
.endmacro

; A constant is a word which pushes its value onto the stack
; when it is executed.
.macro defconst name, value, label
  defword name, 0, label
    push #value
    rts
.endmacro

.macro defconsttmp name, value, label
  defwordtmp name, 0, label
    push #value
    rts
.endmacro

; A val is a word which pushes its value onto the
; stack when executed, like constants, but stores the
; value in RAM, allowing it to be changed.
.macro defval name, init, label
  defword name, 0, label
    push .ident(.concat(.string(label), "_VALUE"))
    rts
  .segment "VARIABLES"
    .ident(.concat(.string(label), "_VALUE")):
    .word init
.endmacro

; A c-val is a `val` that only has values between 0 and 255.
.macro defcval name, init, label
  defword name, F_SINGLE_BYTE, label
    pushb .ident(.concat(.string(label), "_VALUE"))
    rts
  .segment "VARIABLES"
  .ident(.concat(.string(label), "_VALUE")):
    .byte init
.endmacro

; Gives some convenient instruction macros, like `bze` as an alias for `beq`
.macpack generic
; Gives long branches which use `jmp` if necessary to branch further than allowed normally.
.macpack longbranch

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
; Always inline the word
F_INLINE = $40
; This is the last entry in the dictionary
F_END = $8000
; This is a single-byte val
F_SINGLE_BYTE = $4000

.segment "DICT"
; Reserve space to push the dictionary to the end of the memory
; space, since it now grows down.
.res $CE7
DHERE_PERM_INIT:

.segment "TMP_DICT"
.res $649
DHERE_TMP_INIT:

.segment "ZEROPAGE": zeropage
TMP1: .res 1
TMP2: .res 1
TMP3: .res 1
TMP4: .res 1
TMP5: .res 1
TMP6: .res 1
TMP7: .res 1
TMP8: .res 1

Stack: .res 80
Stack_End: .res 8 ; buffer zone

ControlFlowSP: .res 1
ControlFlowStack: .res 32
ControlFlowStackEnd:

.segment "STACK"
; Reserve the hardware stack.
RStack: .res $100

defwordtmp "first-test", 0, FIRST_TEST
  push #1234
  rts

defconsttmp "dict::impl", DictEntry::CodePtr, DictEntryCodePtr
defconsttmp "dict::len", DictEntry::Len, DictEntryLen
defconsttmp "dict::flags", DictEntry::Flags2, DictEntryFlags
defconsttmp "dict::name", DictEntry::Name, DictEntryName
defconst "c-sp", ControlFlowSP, C_SP
defconst "cstack", ControlFlowStack, CSTACK

; ( a -- )
defword "drop", F_INLINE, DROP
  pop
  rts

; ( a b -- b a )
defword "swap", 0, SWAP
  lda Stack+0, x
  ldy Stack+2, x
  sty Stack+0, x
  sta Stack+2, x
  lda Stack+1, x
  ldy Stack+3, x
  sty Stack+1, x
  sta Stack+3, x
  rts

; ( a -- a a )
defword "dup", 0, DUP
  dex
  dex
  lda Stack+2, x
  sta Stack+0, x
  lda Stack+3, x
  sta Stack+1, x
  rts

; ( a -- a + 1 )
defword "1+", 0, INCR
  inc Stack, x
  beq @hi
  rts
@hi:
  inc Stack+1, x
  rts

defword "1-", 0, DECR
  lda Stack, x
  sub #1
  sta Stack, x
  lda Stack+1, x
  sbc #0
  sta Stack+1, x
  rts

defword "+", 0, ADD
  lda Stack, x
  add Stack+2, x
  sta Stack+2, x
  lda Stack+1, x
  adc Stack+3, x
  sta Stack+3, x
  pop
  rts

; Divides a single byte by 3, truncating.
; Just counts up by 3s until reaching or surpassing
; the number.
defword "3/", 0, DIV3
  ldy #$FF
  lda #$FD
: add #3
  iny
  cpy #86
  bge @done
  cmp Stack, x
  beq @exact
  blt :-
@done:
  dey
@exact:
  sty Stack, x
  lda #0
  sta Stack+1, x
  rts

defword "-", 0, SUB
  lda Stack+2, x
  sub Stack, x
  sta Stack+2, x
  lda Stack+3, x
  sbc Stack+1, x
  sta Stack+3, x
  pop
  rts

defword "!", 0, STORE
  copyTo TMP1
  ldy #0
  lda Stack+2, x
  sta (TMP1), y
  lda Stack+3, x
  iny
  sta (TMP1), y
  pop
  pop
  rts

defword "c!", 0, CSTORE
  lda Stack+2, x
  sta (Stack, x)
  pop
  pop
  rts

defword "@", 0, FETCH
  copyTo TMP1
  
  ldy #0
  lda (TMP1), y
  sta Stack, x
  iny
  lda (TMP1), y
  sta Stack+1, x
  rts

defword "c@", 0, CFETCH
  lda (Stack, x)
  sta Stack, x
  lda #0
  sta Stack+1, x
  rts

; ( ptr1 ptr2 n -- )
defword "cmove", 0, CMOVE
; TODO

defwordtmp "key", 0, KEY
  pushb IO_PORT
  rts

; ( -- str-ptr len )
defwordtmp "word", 0, WORD
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
  sta word_buffer, y
  inc TMP1
  jsr KEY
  lda Stack, x
  pop
  cmp #' ' + 1
  bge @loop

  ldy TMP1
  ; iny
  lda #0
  sta word_buffer, y ; zero terminate

  push #word_buffer
  pushb TMP1
  rts

@skipComment:
  jsr KEY
  lda Stack, x
  pop
  cmp #$A ; \n
  bne @skipComment
  beq WORD_IMPL ; bra

.segment "TMP_VARIABLES"
  word_buffer: .res 32

defvartmp "base", 10, BASE

; ( str len -- parsed-number non-zero )
; or ( str len -- str len 0 ) on error
defwordtmp "number", 0, NUMBER

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
  sub #'0' ; now it should be in a range of 0-BASE-1
  cmp BASE_VALUE
  bcs @invalid ; if digit >= BASE
  pha ; save the digit

    ldx BASE_VALUE ; loop BASE times, to multiply Result by BASE using repeated addition
    lda #0
    sta ResultAcc
    sta ResultAcc+1 ; reset ResultAcc
  @multLoop:
    lda ResultAcc
    add Result
    sta ResultAcc
    lda ResultAcc+1
    adc Result+1
    sta ResultAcc+1 ; add Result to ResultAcc
    dex
    bne @multLoop

  pla ; get the digit
  add ResultAcc
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

  push #HANDLE_NUMBER
  rts

@invalid:
  pla
  tax ; restore stack pointer
  push #0
  rts


defwordtmp "d:perm", 0, DICT_PERM
  push DHERE_PERM
  jmp DFIND

defwordtmp "d:tmp", 0, DICT_TMP
  push DHERE_TMP
  jmp DFIND

defwordtmp "hDict", 0, HANDLE_DICT
  ; now that we have the dictionary entry, check if it's immediate.
  copyTo TMP1
  ldy #(DictEntry::Len)
  lda (TMP1), y
  and #F_IMMED
  bne @execute

  lda STATE_VALUE
  beq @execute
@compiling:
  lda (TMP1), y
  and #F_INLINE
  bne @inline
  push #JSR_OP
  jsr CCOMMA
  jmp COMMA

@inline: ; simple inlining -- just copy until you see an rts.
  jmp INLINE

@execute:
  pop ; drop dictionary pointer
  jmp (TMP1) ; tailcall to the word, which will return to QUIT

; ( str-ptr len dict-start -- dictionary-pointer handler )
; or ( str-ptr len dict-start -- str-ptr len 0 ) if it wasn't found
; Searches the dictionary at the given start for the given word.
defwordtmp "dfind", 0, DFIND
  LPointer := TMP1
  MyStr := TMP3

  lda Stack, x
  sta LPointer
  lda Stack+1, x
  sta LPointer+1
  inx
  inx
  
  lda Stack+2, x
  sub #<(DictEntry::Name)
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
  add #(DictEntry::Name) - 1
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

  ; Check if this is the last dictionary entry
  ldy #(DictEntry::Flags2)
  lda (LPointer), y
  and #>F_END
  bne @notFound

  ldy #(DictEntry::Len)
  lda (LPointer), y
  and #$1F ; TODO - magic constant
  add LPointer
  sta LPointer
  lda #0
  adc LPointer + 1
  sta LPointer + 1 ; add string length to LPointer

  lda #DictEntry::Name
  add LPointer
  sta LPointer
  lda #0
  adc LPointer + 1
  sta LPointer + 1 ; add the other bytes of the header.

  clv
  bvc @loop ; bra

@notFound:
  push #0
  rts
@found:
  pop
  lda LPointer
  sta Stack, x
  lda LPointer+1
  sta Stack+1, x
  push #HANDLE_DICT
  rts

defwordtmp "hNum", 0, HANDLE_NUMBER
  lda STATE_VALUE
  beq @executeLiteral
@compileLiteral:
  jsr ASM
  .byte 1, DEX_OP ; DEX
  jsr ASM
  .byte 1, DEX_OP ; DEX
  jsr DUP         ; dup
  jsr ASM
  .byte 2, LDA_IMM_OP ; LDA.#
  push #Stack
  jsr ASM
  .byte 2, STA_ZP_X_OP ; stack STA.ZX
  lda Stack+1, x
  sta Stack, x ; >byte
  jsr ASM
  .byte 2, LDA_IMM_OP ; LDA.#
  push #Stack+1
  jsr ASM
  .byte 2, STA_ZP_X_OP ; stack 1+ STA.ZX
  rts
@executeLiteral:
  rts

defwordtmp "d:asm", 0, DICT_ASM
  lda Stack+2, x
  sta TMP1
  lda Stack+3, x
  sta TMP2
  jsr ParseInstruction
  lda Stack, x ; check the length of the instruction parsed.
  beq :+
  ; Remove the string
  lda Stack, x
  sta Stack+4, x
  lda Stack+1, x
  sta Stack+5, x
  lda Stack+2, x
  sta Stack+6, x
  lda Stack+3, x
  sta Stack+7, x
  pop
  pop
  jsr SWAP
  jsr DUP
  jsr DIV3
  jsr ADD
  jsr SWAP
  push #ASM_COMP
: rts

; ( str-ptr len -- dictionary-pointer )
; or ( str-ptr len -- str-ptr len 0 ) if it wasn't found
; Searches the dictionary for a definition of the given word.
defwordtmp "find", 0, FIND
  
  jsr DICT_PERM
  cmpTopZero
  beq :+
  pop
  rts
: pop
  jsr DICT_TMP
  cmpTopZero
  beq :+
  pop
: rts

; Given a pointer, gets the name of the dictionary entry,
; or an empty string if it's not in the dictionary.
; ( addr -- addr len )
defwordtmp "rfind", 0, RFIND
  @LPointer := TMP1

  jsr DHERE
  lda Stack, x
  sta @LPointer
  lda Stack+1, x
  sta @LPointer+1
  pop

@loop:
  lda @LPointer
  cmp Stack, x
  bne @ne
  lda @LPointer+1
  cmp Stack+1, x
  bne @ne
  ; We've found the dictionary entry, now return the string.
  ldy #DictEntry::Len
  lda (@LPointer), y
  and #$1F ; mask just the length, no flags.
  pushb a
  lda @LPointer
  add #DictEntry::Name
  sta Stack+2, x
  lda @LPointer+1
  adc #0
  sta Stack+3, x
  rts
@ne:
  ldy #DictEntry::Len
  lda (@LPointer), y
  and #31 ; TODO - magic number
  add @LPointer
  sta @LPointer
  lda #0
  adc @LPointer+1
  sta @LPointer+1

  lda #DictEntry::Name
  add @LPointer
  sta @LPointer
  lda #0
  adc @LPointer+1
  sta @LPointer+1

  lda @LPointer+1
  cmp #>DICT_END ; assumes DICT_END is the last entry in the dictionary
  blt @loop
  lda @LPointer
  cmp #<DICT_END
  beq @loop
  blt @loop
@notFound:
  ; Didn't find anything, so return 0 len string and the original address.
  push #0
  rts

defconst "<perm>", HERE_PERM, PERM_LATEST
defconst "<tmp>", HERE_TMP, TMP_LATEST

defwordtmp "definitions:", 0, DEFINITIONS
  jsr HERE
  jsr STORE
  rts

defword "dicts", 0, DICTS
  push #0
  push DHERE_TMP
  push DHERE_PERM
  rts

; Points to a list of 3 RAM addresses, holding
; the current HERE pointers, representing where
; to place the next code, dictionary, or variable bytes.
defvartmp "here", HERE_PERM, HERE

; Gives the address of the next free byte of code
defwordtmp "chere", 0, CHERE
  jsr HERE
  jmp FETCH

; Gives the address of the next byte to add to the dictionary
defwordtmp "dhere", 0, DHERE
  jsr HERE
  jsr FETCH
  push #2
  jmp ADD

; Gives the address of the next free byte of RAM
defwordtmp "vhere", 0, VHERE
  jsr HERE
  jsr FETCH
  push #4
  jmp ADD

.segment "TMP_VARIABLES"
HERE_TMP:
  CHERE_TMP: .word CHERE_TMP_INIT
  DHERE_TMP: .word DHERE_TMP_INIT
  VHERE_TMP: .word VHERE_TMP_INIT

HERE_PERM:
  CHERE_PERM: .word CHERE_PERM_INIT
  DHERE_PERM: .word DHERE_PERM_INIT
  VHERE_PERM: .word VHERE_PERM_INIT

JMP_OP = $4C

; ( str-ptr len -- )
; Creates a new dictionary entry
defwordtmp "create", 0, CREATE
  ; The dict entry needs str-len + DictEntry::Name bytes of space.
  ; Subtract from DHERE to allocate the space.
  jsr DUP ; str length
  push #DictEntry::Name
  jsr ADD
  jsr DHERE
  jsr FETCH
  jsr SWAP
  jsr SUB
  jsr DHERE
  jsr STORE

  ; ( stack is now string-ptr len )
  ; Store length in new entry
  ; dup dhere @ dict:len + c!
  jsr DUP
  jsr DHERE
  jsr FETCH
  push #DictEntry::Len
  jsr ADD
  jsr CSTORE

  ; Write jmp CHERE to the dictionary entry.
  ; JMP_OP dhere @ dict:jmp + c!
  push #JMP_OP
  jsr DHERE
  jsr FETCH
  push #DictEntry::Jmp
  jsr ADD
  jsr CSTORE
  ; chere @ dhere @ codeptr + !
  jsr CHERE
  jsr FETCH
  jsr DHERE
  jsr FETCH
  push #DictEntry::CodePtr
  jsr ADD
  jsr STORE  
  
  ; Get DHERE value
  jsr DHERE
  jsr FETCH
  ; Add the offset to the name part
  lda #<(DictEntry::Name)
  add Stack, x
  sta TMP3
  lda #>(DictEntry::Name)
  adc Stack+1, x
  sta TMP4
  pop ; Drop DHERE value

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

defwordtmp ",", 0, COMMA
  jsr CHERE
  jsr FETCH
  jsr STORE

  jsr CHERE
  jsr FETCH
  jsr INCR
  jsr INCR
  jsr CHERE
  jmp STORE

defwordtmp "c,", 0, CCOMMA
  jsr CHERE
  jsr FETCH
  jsr CSTORE

  jsr CHERE
  jsr FETCH
  jsr INCR
  jsr CHERE
  jmp STORE

defvartmp "state", 0, STATE

defwordtmp "[", F_IMMED, LSQUARE
  lda #0
  sta STATE_VALUE
  sta STATE_VALUE+1
  rts

defwordtmp "]", 0, RSQUARE
  lda #1
  sta STATE_VALUE
  lda #0
  sta STATE_VALUE+1
  rts

defwordtmp ":", 0, COLON
  jsr WORD
  jsr CREATE ; create the dictionary entry
  jsr DHERE
  jsr FETCH
  jsr HIDDEN ; toggle hidden in the entry
  jmp RSQUARE ; enter compile mode

RTS_OP = $60

defwordtmp ";", F_IMMED, SEMICOLON
  ; append rts to code
  jsr ASM
  .byte 1, RTS_OP

  jsr DHERE
  jsr FETCH
  jsr HIDDEN ; toggle hidden flag
  jmp LSQUARE ; go back to immediate mode

; ( dict-ptr -- )
; Marks the dictionary entry as hidden
defwordtmp "hidden", 0, HIDDEN
  copyTo TMP1

  ldy #(DictEntry::Len)
  lda #F_HIDDEN
  eor (TMP1), y
  sta (TMP1), y

  pop
  rts

defwordtmp "c-val-tog", 0, C_VAL_TOGGLE
  copyTo TMP1
  ldy #(DictEntry::Flags2)
  lda #>F_SINGLE_BYTE
  eor (TMP1), y
  sta (TMP1), y

  pop
  rts

defwordtmp "hide", 0, HIDE
  jsr WORD
  jsr FIND
  jmp HIDDEN

JSR_OP = $20

defwordtmp "[asm]", 0, ASM
  pla
  sta TMP1
  pla
  sta TMP2
  
  lda TMP1
  add #2
  tay

  lda TMP2
  adc #0
  pha ; add 2 to the return address because there are 2 bytes of parameters.
  tya
  pha

  ; Move the two parmeters to the stack.
  ldy #2
  lda (TMP1), y
  pushb a

  dey
  lda (TMP1), y
  pushb a

  jmp AsmCompExec
  

; ( arg? instruction n-bytes -- )
; Given an optional argument, the number
; of bytes in the instruction, and the instruction
; number, compiles the instruction and arg to
; the code space.
defwordtmp "asm-comp", 0, ASM_COMP
  lda STATE_VALUE
  beq AsmCompExec
@compile:
  push #JSR_OP
  jsr CCOMMA
  push #ASM
  jsr COMMA
  jsr CCOMMA
  jsr CCOMMA
  rts
AsmCompExec:
  lda Stack, x
  pha
    pop
    jsr CCOMMA ; write instruction byte
  pla
  cmp #2
  beq @two
  blt @one
@three:
  jmp COMMA
@two:
  jmp CCOMMA
@one:
  rts
  
defwordtmp "quit", 0, QUIT
  stx TMP1
    cpx #Stack_End - Stack
    bge @underflow
    ldx #$FF
    txs
  ldx TMP1
  jsr INTERPRET
  jmp QUIT
@underflow:
  jsr DODOTQUOTE
  .asciiz "Stack underflow detected!"
  ldx #Stack_End - Stack - 1
  jmp QUIT

; ( xt -- )
; Inlines the code of the execution token,
; stopping when it reaches an RTS instruction.
INLINE:
  push #DictEntry::CodePtr
  jsr ADD
  jsr FETCH
@loop2:
  jsr DUP
  jsr CFETCH
  lda Stack, x
  cmp #$60
  beq @doneInlining2
  jsr CCOMMA
  jsr INCR
  clv
  bvc @loop2
@doneInlining2:
  pop ; drop the two stack values
  pop
  rts

DEX_OP = $CA
LDA_IMM_OP = $A9
STA_ZP_X_OP = $95

defword "execute", 0, EXECUTE
  copyTo TMP1
  pop
  jmp (TMP1)

defwordtmp "interpret", 0, INTERPRET
  .macro tryDict dict
    jsr dict
    cmpTopZero
    bne @found
    pop
  .endmacro
  jsr WORD
  tryDict DICT_PERM
  tryDict DICT_TMP
  tryDict NUMBER
  tryDict DICT_ASM
@notFound:
  jsr DODOTQUOTE
  .byte "ERROR: Couldn't find word ", '"', 0
  jsr TYPE
  lda #'"'
  sta IO_PORT
  rts
@found:
  jmp EXECUTE

defwordtmp ".", 0, DOT
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

; Prints out the following bytes as a zero-terminated string.
; Use like:
;   jsr DODOTQUOTE
;   .asciiz "Some string"
defwordtmp "(.')", 0, DODOTQUOTE
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
  tya ; fix up the return address to the address after the string
  add TMP1
  sta TMP1
  lda TMP2
  adc #0
  pha
  lda TMP1
  pha
  rts

; ( str-addr len -- )
; Prints a string
defwordtmp "type", 0, TYPE
  lda Stack, x
  add Stack+2, x
  sta Stack, x
  lda Stack+1, x
  adc Stack+3, x
  sta Stack+1, x

@loop:
  lda Stack, x
  cmp Stack+2, x
  bne :+
  lda Stack+1, x
  cmp Stack+3, x
  beq @done
: lda (Stack+2, x)
  sta IO_PORT

  inc Stack+2, x
  bne :+
  inc Stack+3, x
: clv
  bvc @loop ; bra
@done:
  pop
  pop
  rts

; ( addr -- addr+1 c )
defword "c@1+", 0, FETCH_INC
  jsr DUP
  jsr INCR
  jsr SWAP
  jmp CFETCH 

defword "thaw", 0, THAW
  Source := TMP1
  Target := TMP3

  ; Basic setup
  sei
  cld

  ; Set source pointer
  ; (Self-modifying code set by freeze)
SourcePtrLo:
  lda #<0000
  sta Source
SourcePtrHi:
  lda #>0000
  sta Source+1

  ; Set target pointer (just after TMP1-8)
  lda #<Stack
  sta Target
  lda #>Stack
  sta Target+1

SrcAgain:
  ldy #0
  lda (Source), y ; read byte
  iny
  cmp (Source), y ; peek a comparison with next byte
  beq @run
@single:
  ; it's a single byte, not a run
  ldy #0
  sta (Target), y
  ldx #1 ; signal to increment source by 1
  bne @then ; bra
@run:
  ; It's a run of the same byte
  sta TMP6
    iny
    lda (Source), y ; read run length
    sta TMP5
    tay
  lda TMP6
  iny
  iny ; add 2 to run length
@loop:
  dey
  sta (Target), y
  bne @loop

  ldy TMP5
  iny
  ldx #3 ; signal to increment source by 3
@then:
  ; Now we need to increment source and target
  ; y+1 tells us how much to increment target
  ; x tells us how much to increment source
  sty TMP5
  lda Target
  sec ; adjust +1
  adc TMP5
  sta Target

  lda Target+1
  adc #0
  sta Target+1 ; adjusted target

  stx TMP5
  lda Source
  add TMP5
  sta Source

  lda Source+1
  adc #0
  sta Source+1 ; adjusted source

  lda EndPtrLo+1
  lda Source
EndPtrLo:
  cmp #<0000
  bne SrcAgain

  lda Source+1
EndPtrHi:
  cmp #>0000
  bne SrcAgain

  ; Source has now reached the end, so we've restored all of RAM.
@restoreRegs:
  ; Restore the register states (self-modifying code -- modified in freeze)
SPSave:
  ldx #0
  txs

  ; We're going to finish with an rti,
  ; but rts pushes return-1, while an interrupt
  ; pushes the return address directly. So,
  ; we need to increment our return address by 1.
  inc $101, x
  bne PSave
  inc $102, x

PSave:
  lda #0
  pha ; store processor flag on stack
ASave:
  lda #0
XSave:
  ldx #0
YSave:
  ldy #0
  rti

defwordtmp "freeze", 0, FREEZE
  @source := TMP1
  @target := TMP3

  ; Save register states.
  sta ASave+1

  php
  pla
  sta PSave+1
  stx XSave+1
  sty YSave+1
  tsx
  stx SPSave+1

  ; Set source pointer
  lda #<Stack
  sta @source
  lda #>Stack
  sta @source+1

  ; Set target pointer, and source pointer of decompress
  lda CHERE_PERM
  sta SourcePtrLo+1
  sta @target
  lda CHERE_PERM+1
  sta SourcePtrHi+1
  sta @target+1

@again:
  ; Encode a single run
  lda @source+1
  cmp #>$800
  beq @done
  ldy #0
  lda (@source), y
@loop:
  iny
  beq @loopEnd
  cmp (@source), y
  beq @loop
@loopEnd:
  ; Now we've found the end of the run, or found 256 in a row.
  sta TMP6
    dey ; adjust y to 0-FF range
    ; Increment source to the byte after the run.
    sty TMP5
    lda @source
    sec ; add a plus 1 adjustment
    adc TMP5
    sta @source
    lda @source+1
    adc #0
    sta @source+1
  lda TMP6

  cpy #0
  bne @multiple
@single:
  ; A run of a single byte is encoded as itself.
  ldy #0
  sta (@target), y
  beq @incrementTarget ; bra
@multiple:
  ; A run of multiple is encoded as 2 repeated bytes, followed by a
  ; byte indicating the remaining repeats. So 5 0 bytes is encoded as 0 0 3
  ldy #0
  sta (@target), y
  iny
  sta (@target), y
  lda TMP5 ; load repeat count - 1
  sub #1 ; subtract 1 from it 
  iny
  sta (@target), y

@incrementTarget:
  ; y now tells us how far to increment target
  sty TMP5
  lda @target
  sec ; adjust +1, since y is either 0 or 2.
  adc TMP5
  sta @target
  lda @target+1
  adc #0
  sta @target+1

  clv
  bvc @again ; bra

@done:
  lda @source
  beq @noOvershoot

  ; The last run has a length that takes it past
  ; the end of RAM.
  lda @target
  sub #1
  sta @target
  lda @target+1
  sbc #0
  sta @target+1
  
  ldy #0
  lda (@target), y ; get the length of the last one
  sub @source ; subtract by the overshoot amount
  sta (@target), y

  inc @target
  bne @noOvershoot
  inc @target+1

@noOvershoot:
  ; Save end pointer
  lda @target
  sta EndPtrLo+1
  lda @target+1
  sta EndPtrHi+1

  jmp 0 ; signal emulator to stop

.segment "CODE"

reset:
  sei
  cld
  ldx #$FF
  txs

  ldx #Stack_End - Stack - 1
  lda #ControlFlowStackEnd - ControlFlowStack - 1
  sta ControlFlowSP

  jmp QUIT

.segment "VECTORS"
.word 0
.word reset
.word 0

;(C) 2015 Alex Semenov (Shiru)
;(C) 2016 Lauri Kasanen
;
; This software is provided 'as-is', without any express or implied
; warranty.  In no event will the authors be held liable for any damages
; arising from the use of this software.
;
; Permission is granted to anyone to use this software for any purpose,
; including commercial applications, and to alter it and redistribute it
; freely, subject to the following restrictions:
;
; 1. The origin of this software must not be misrepresented; you must not
;    claim that you wrote the original software. If you use this software
;    in a product, an acknowledgment in the product documentation would be
;    appreciated but is not required.
; 2. Altered source versions must be plainly marked as such, and must not be
;    misrepresented as being the original software.
; 3. This notice may not be removed or altered from any source distribution. 
;
; ---------------------------------------------------------------------------
; This has been ported from the original version (meant to be called from
; CC65) to have a Forth interface.
; ---------------------------------------------------------------------------


.export PPU_MASK_VAR
defcval "vppu-mask", 0, PPU_MASK_VAR
defcval "vppu-ctrl", 0, PPU_CTRL_VAR
defcval "vppu-stat", 0, PPU_STATUS_VAR
defcval "scroll-x", 0, SCROLL_X_VAR
defcval "scroll-y", 0, SCROLL_Y_VAR

defcval "joy1", 0, JOY1_VAR
defcval "joy2", 0, JOY2_VAR
defcval "oldjoy1", 0, OLD_JOY1
defcval "oldjoy2", 0, OLD_JOY2

defconst "btnA", $80, BTN_A
defconst "btnB", $40, BTN_B
defconst "btnSel", $20, BTN_SELECT
defconst "btnSt", $10, BUTTON_START
defconst "btnU", $8, BUTTON_UP
defconst "btnD", $4, BUTTON_DOWN
defconst "btnL", $2, BUTTON_LEFT
defconst "btnR", $1, BUTTON_RIGHT

.segment "ZEROPAGE"
; Initialize color brightness to normal.
PAL_SPR_PTR: .word palBrightTable4
PAL_BG_PTR: .word palBrightTable4

.segment "RAM"
.align 256
; Initialize OAM buffer with $FF to hide all sprites.
OAM_BUF: .res 256, $FF
PAL_BUF: .res 32, $20
FRAME_CNT1: .res 1
PAL_UPDATE: .res 1
NAME_UPD_ENABLE: .res 1
VRAM_UPDATE: .res 1

.segment "CODE"

; NMI handler
defword "nmi", 0, NMI
  pha
  txa
  pha
  tya
  pha

  lda PPU_MASK_VAR_VALUE ;if rendering is disabled, do not access the VRAM at all
  and #%00011000
  jeq @skipAll

@doUpdate:

  lda #0
  sta $4013

  lda #>OAM_BUF   ;update OAM
  sta $4014

  lda PAL_UPDATE   ;update palette if needed
  bne @updPal
  jmp @updVRAM

@updPal:

  ldx #0
  stx PAL_UPDATE

  lda #$3f
  sta $2006
  stx $2006

  ldy PAL_BUF       ;background color, remember it in X
  lda (PAL_BG_PTR),y
  sta $2007
  tax

  .repeat 3,I
  ldy PAL_BUF+1+I
  lda (PAL_BG_PTR),y
  sta $2007
  .endrepeat

  .repeat 3,J
  stx $2007      ;background color
  .repeat 3,I
  ldy PAL_BUF+5+(J*4)+I
  lda (PAL_BG_PTR),y
  sta $2007
  .endrepeat
  .endrepeat

  .repeat 4,J
  stx $2007      ;background color
  .repeat 3,I
  ldy PAL_BUF+17+(J*4)+I
  lda (PAL_SPR_PTR),y
  sta $2007
  .endrepeat
  .endrepeat

@updVRAM:

  lda VRAM_UPDATE
  beq @skipUpd
  lda #0
  sta VRAM_UPDATE

  lda NAME_UPD_ENABLE
  beq @skipUpd

  jsr _flush_vram_update_nmi

@skipUpd:

  lda #0
  sta $2006
  sta $2006

  lda SCROLL_X_VAR_VALUE
  sta $2005
  lda SCROLL_Y_VAR_VALUE
  sta $2005

  lda PPU_CTRL_VAR_VALUE
  sta $2000

@skipAll:

  lda PPU_MASK_VAR_VALUE
  sta $2001

  inc FRAME_CNT1

  jsr FamiToneUpdate

  ; Read controllers
  lda JOY1_VAR_VALUE
  sta OLD_JOY1_VALUE
  lda JOY2_VAR_VALUE
  sta OLD_JOY2_VALUE

  lda #$01
  sta $4016
  sta JOY2_VAR_VALUE
  lsr
  sta $4016
@controllerLoop:
  lda $4016
  and #$03
  cmp #$01
  rol JOY1_VAR_VALUE
  lda $4017
  and #$03
  cmp #$01
  rol JOY2_VAR_VALUE
  bcc @controllerLoop

  pla
  tay
  pla
  tax
  pla

irq:

  rti

FamiToneUpdate:
  rts

; Set bg and spr palettes, data is 32 bytes array
; ( palette-buffer -- )
defword "pal!", 0, pal_all
  copyTo TMP1
  lda #$20

; Given a length in A, sets the palette buffer
; given the buffer address in TMP1-2
pal_copy:
  sta TMP3

  ldy #0

@0:
  lda (TMP1), y
  sta PAL_BUF, y
  iny
  dec TMP3
  bne @0

  inc PAL_UPDATE

  pop
  rts

; Set bg palette only, data is 16 bytes array
; ( palette-buffer -- )
defword "pal-bg!", 0, pal_bg
  copyTo TMP1
  lda #$10
  bne pal_copy ; bra

; Set spr palette only, data is 16 bytes array
; ( palette-buffer -- )
defword "pal-spr!", 0, pal_spr
  copyTo TMP1
  lda #$10  
  sta TMP3
  ldy #0

@0:
  lda (TMP1), y
  sta PAL_BUF + $10, y
  iny
  dec TMP3
  bne @0

  inc <PAL_UPDATE
  pop
  rts

; Set a palette entry. index is 0..31
; ( index color -- )
defword "pal-col!", 0, pal_col
  ldy Stack+2, x
  lda Stack, x
  and #$1f
  sta PAL_BUF, y
  inc PAL_UPDATE
  pop
  pop
  rts

; reset palette to $0F
; ( -- )
defword "pal-clear", 0, pal_clear
  lda #$0F
  ldy #0

@1:
  sta PAL_BUF, y
  iny
  cpy #$20
  bne @1
  sty PAL_UPDATE
  rts

; Set virtual bright for sprites only
; ( brightness -- )
defword "pal-spr-bright", 0, pal_spr_bright
  ldy Stack, x
  lda palBrightTableL, y
  sta PAL_SPR_PTR
  lda palBrightTableH, y
  sta PAL_SPR_PTR+1
  sta PAL_UPDATE
  pop
  rts

; Set virtual bright for background only
; ( brightness -- )
defword "pal-bg-bright", 0, pal_bg_bright
  ldy Stack, x
  lda palBrightTableL, y
  sta <PAL_BG_PTR
  lda palBrightTableH, y
  sta <PAL_BG_PTR+1
  sta <PAL_UPDATE
  pop
  rts

; Set virtual bright both for sprites and background.
; 0 is black, 4 is normal, 8 is white.
defword "pal-bright", 0, pal_bright
  jsr DUP
  jsr pal_spr_bright
  jmp pal_bg_bright

; Turn off rendering, nmi still enabled when rendering is disabled.
; ( -- )
defword "ppu-off", 0, ppu_off
  lda PPU_MASK_VAR
  and #%11100111
  sta PPU_MASK_VAR
  jmp ppu_wait_nmi

; Turn on bg, spr
; ( -- )
defword "ppu-on-all", 0, ppu_on_all
  lda PPU_MASK_VAR
  ora #%00011000

ppu_onoff:
  sta PPU_MASK_VAR
  jmp ppu_wait_nmi

; Turn on bg only
; ( -- )
defword "ppu-on-bg", 0, ppu_on_bg
  lda PPU_MASK_VAR
  ora #%00001000
  bne ppu_onoff ; bra

; Turn on spr only
; ( -- )
defword "ppu-on-spr", 0, ppu_on_spr
  lda PPU_MASK_VAR
  ora #%00010000
  bne ppu_onoff ; bra

; Clear OAM buffer -- all sprites are hidden
; ( -- )
defword "oam-clear", 0, oam_clear
  ldy #0
  lda #$FF
@1:
  sta OAM_BUF, y
  iny
  iny
  iny
  iny
  bne @1
  rts

; Set sprite display mode. False for 8x8, true for 8x16
; ( flag -- )
defword "oam-size", 0, oam_size
  ldy #0
  lda Stack, x
  ora Stack+1, x
  beq :+
  ldy #%00100000
: sty TMP1
  lda PPU_CTRL_VAR
  and #%11011111
  ora TMP1
  sta PPU_CTRL_VAR

  rts

; Set a sprite in the OAM buffer. chrnum is tile, attr is
; attribute, sprid is offset in OAM in bytes
; returns sprid+4 which is offset for the next sprite
; ( x y chrnum attr sprid -- new-sprite-id )
defword "oam-spr", 0, oam_spr
  ldy Stack, x ; get sprite ID

  lda Stack+2, x ; attribute
  sta OAM_BUF+2, y

  lda Stack+4, x ; chrnum
  sta OAM_BUF+1, y

  lda Stack+6, x ; y
  sta OAM_BUF, y

  lda Stack+8, x
  sta OAM_BUF+3, y

  pop
  pop
  pop
  pop

  tya
  add #4
  sta Stack, x
  lda #0
  sta Stack+1, x
  rts

; Set metasprite in OAM buffer
; meta sprites is a const unsigned char array, with
; four bytes per sprite in order x, y, tile, attribute
; x=128 is the end of a metasprite
; returns sprid of next sprite
; TODO 
oam_meta_spr:

; hide all remaining sprites from given offset
; ( sprid -- )
defword "oam-hide-rest", 0, oam_hide_rest
  ldy Stack, x
  lda #240
@1:
  sta OAM_BUF, y
  iny
  iny
  iny
  iny
  bne @1

  pop
  rts

; Wait actual TV frame, 50hz for PAL, 60hz for NTSC
; ( -- )
defword "ppu-wait-nmi", 0, ppu_wait_nmi
  lda #1
  sta VRAM_UPDATE
  lda FRAME_CNT1

@1:
  cmp FRAME_CNT1
  beq @1
  rts

; Set scroll after screen split invoked by the sprite 0 hit.
; warning: all CPU time between the function call and the actual split
; point will be wasted!
; warning: only X scroll can be changed with this version.
; ( x y -- )
split: ; TODO

; Select current bank for sprites 0..1
; ( n -- )
defword "bank-spr", 0, bank_spr
  lda Stack, x
  and #1
  asl
  asl
  asl
  sta TMP1
  lda PPU_CTRL_VAR
  and #%11110111
  ora TMP1
  sta PPU_CTRL_VAR
  pop

  rts

; Select current bank for background, 0..1
; ( n -- )
defword "bank-bg", 0, bank_bg
  lda Stack, x
  and #1
  asl
  asl
  asl
  asl
  sta TMP1
  lda PPU_CTRL_VAR
  and #%11101111
  ora TMP1
  sta PPU_CTRL_VAR

  rts

; read a block from current address of vram, only works
; when rendering is turned off
; ( dst size -- )
vram_read: ; TODO

; write a block to current address of vram, only works
; when rendering is turned off
; ( src size -- )
vram_write: ; TODO

.export delay
; delay for N frames
; ( frames -- )
defword "delay", 0, delay
  ldy Stack, x

@1:
  jsr ppu_wait_nmi
  dey
  bne @1  

  pop 
  rts

defword "oam", 0, OAM_BUFFER_LOC
  push #OAM_BUF
  rts

; Does a bulk copy of data to the PPU
; ( src len target -- )
defword "mv>ppu", 0, MV_TO_PPU

  PpuAddr := Stack
  ; Write to PPUADDR
  bit $2002
  lda PpuAddr+1, x
  sta $2006
  lda PpuAddr, x
  sta $2006
  pop

  End     := Stack
  Data    := Stack+2

  lda Data, x
  add End, x
  sta End, x
  lda Data+1, x
  adc End+1, x
  sta End+1, x

  lda #0
  sta TMP1
  lda Data+1, x
  sta TMP2

  lda End, x
  sta TMP3

  ldy Data, x

@loop:
  lda (TMP1), y
  sta $2007
  iny
  bne :+
  inc TMP2
: cpy TMP3
  bne @loop
  lda TMP2
  cmp End+1, x
  bne @loop

  pop
  pop
  rts

_flush_vram_update_nmi: rts

palBrightTableL:

  .byte <palBrightTable0,<palBrightTable1,<palBrightTable2
  .byte <palBrightTable3,<palBrightTable4,<palBrightTable5
  .byte <palBrightTable6,<palBrightTable7,<palBrightTable8

palBrightTableH:

  .byte >palBrightTable0,>palBrightTable1,>palBrightTable2
  .byte >palBrightTable3,>palBrightTable4,>palBrightTable5
  .byte >palBrightTable6,>palBrightTable7,>palBrightTable8

palBrightTable0:
  .byte $0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f ;black
palBrightTable1:
  .byte $0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f
palBrightTable2:
  .byte $0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f
palBrightTable3:
  .byte $0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f
palBrightTable4:
  .byte $00,$01,$02,$03,$04,$05,$06,$07,$08,$09,$0a,$0b,$0c,$0f,$0f,$0f ;normal colors
palBrightTable5:
  .byte $10,$11,$12,$13,$14,$15,$16,$17,$18,$19,$1a,$1b,$1c,$00,$00,$00
palBrightTable6:
  .byte $10,$21,$22,$23,$24,$25,$26,$27,$28,$29,$2a,$2b,$2c,$10,$10,$10 ;$10 because $20 is the same as $30
palBrightTable7:
  .byte $30,$31,$32,$33,$34,$35,$36,$37,$38,$39,$3a,$3b,$3c,$20,$20,$20
palBrightTable8:
  .byte $30,$30,$30,$30,$30,$30,$30,$30,$30,$30,$30,$30,$30,$30,$30,$30 ;white
  .byte $30,$30,$30,$30,$30,$30,$30,$30,$30,$30,$30,$30,$30,$30,$30,$30
  .byte $30,$30,$30,$30,$30,$30,$30,$30,$30,$30,$30,$30,$30,$30,$30,$30
  .byte $30,$30,$30,$30,$30,$30,$30,$30,$30,$30,$30,$30,$30,$30,$30,$30


.segment "TMP_CODE"

ASSEMBLER_START:

; To save space, the 3 letters and addressing mode of the
; instruction are stored in 4-bits each.
; Turns out we can almost fit all of the letters with
; just 16 possiblitiies for each one. (See below for the
; exceptions).

; See http://www.obelisk.me.uk/6502/addressing.html for an explanation
; of the different addressing modes.
.enum mode
  impl = 0 ; no args
  acc = $10 ; accumulator is arg, used for modifying instructions which can take A.
  
  ; 1 byte arg
  imm = $20  ; immediate mode
  rel = $30  ; relative mode, used by branch instructions
  indx = $40 ; (indirect, x) addressing mode
  indy = $50 ; (indirect), y addressing mode
  zpg = $60  ; zero page
  zpgx = $70 ; zero page, x 
  zpgy = $80 ; zero page, y

  ; 2 byte arg
  ind = $90  ; (indirect), only used by jmp (indirect)
  abs = $A0  ; absolute
  absx = $B0 ; absolute, x
  absy = $C0 ; absolute, y
.endenum

; Letters used in the instruction names.
; Checking all of the instruction names, these
; are the letters that can appear in each position
; of the name (with some exceptions).
;
; FirstLetter:  .byte "ABCDEIJLNOPRSTxx"
; SecondLetter: .byte "ABCDEHILMNOPRSTV"
; ThirdLetter:  .byte "ACDEIKLPQRSTVXYx"

; By re-arranging the letters, we can overlap the lists and save space:
SecondLetter:
  .byte "HMV" ; only second
FirstLetter:
  ; Overlap of first and second
  .byte "BNO"
ThirdLetter:
  ; Overlap of all 3
  .byte "ACDEILPRST"
  ; Overlap of first and third
  .byte "JKQ"
  ; Only third
  .byte "VXY"

; The resulting lists look like this:
; FirstLetter:  .byte "BNOACDEILPRSTJKQ"
; SecondLetter: .byte "HMVBNOACDEILPRST"
; ThirdLetter:  .byte "ACDEILPRSTJKQVXY"

; Enum for first letter in an instruction
.enum l1
  B
  N
  O
  A_
  C
  D
  E
  I
  L
  P
  R
  S
  T
  J
  K_unused
  Q_unused
.endenum

; Enum for second letter in an instruction
; (Store in high nybble)
.enum l2
  H = $00
  M = $10
  V = $20
  B = $30
  N = $40
  O = $50
  A_= $60
  C = $70
  D = $80
  E = $90
  I = $A0
  L = $B0
  P = $C0
  R = $D0
  S = $E0
  T = $F0
.endenum

; Enum for third letter in an instruction
.enum l3
  A_
  C
  D
  E
  I
  L
  P
  R
  S
  T
  J_unused
  K
  Q
  V
  X_
  Y_
.endenum

message: .byte "(.')", $0A, ".byte ", $22, $0

; Handles disassembly of DODOTQUOTE, which takes
; its argument inline after the jsr DOTQUOTE
PrintString:
  inx
  inx
  ldy #0
@loop:
  lda message, y
  bze @done
  sta IO_PORT
  iny
  bne @loop
@done:
  lda Stack, x
  sta TMP1
  lda Stack+1, x
  sta TMP2
  ldy #0
@loop2:
  lda (TMP1), y
  bze @done2
  sta IO_PORT
  iny
  bne @loop2
@done2:
  lda #'"'
  sta IO_PORT
  sty TMP1
  lda Stack, x
  sec
  adc TMP1
  sta Stack, x
  lda Stack+1, x
  adc #0
  sta Stack+1, x
  rts

; Prints the argument with the IP at the top of the stack,
; and a as the addressing mode, and returns the new IP.
PrintArg:
  cmp #$1
  ble @return ; no arg implied or accumulator instruction
  pha
    jsr DUP
  pla
  cmp #$9
  bge @two ; modes >= $9 are two byte modes. 
@one:
  jsr INCR
  jsr SWAP
  jsr CFETCH
  jmp DOT ; print one byte
@two:
  jsr INCR
  jsr INCR
  jsr SWAP
  jsr FETCH
  ; If the address is DOODOTQUOTE, then this is probably
  ; a JSR DODOTQUOTE call, so we should check the string
  ; that comes after it.
  lda Stack, x
  cmp #<DODOTQUOTE
  bne :+
  lda Stack+1, x
  cmp #>DODOTQUOTE
  bne :+
  jmp PrintString
:
  jsr RFIND
  lda Stack, x
  ora Stack+1, x
  beq @notInDict
  lda Stack+2, x
  sta TMP1
  lda Stack+3, x
  sta TMP2
  lda Stack, x
  sta TMP3 ; save length in TMP3
  ldy #0
@nameLoop:
  lda (TMP1), y
  sta IO_PORT
  iny
  cpy TMP3
  bne @nameLoop
  inx
  inx
  inx
  inx
  rts
@notInDict:
  inx ; drop the zero length 
  inx
  jmp DOT ; print just the bytes.
  
@return:
  rts

defwordtmp "ins", 0, INS
  jsr WORD
  pop ; drop string length
  copyTo TMP1
  pop
  jsr ParseInstruction
  beq :+
  jsr SWAP
  jsr DUP
  jsr DIV3
  jsr ADD
  jsr SWAP
: rts

defwordtmp "see", 0, SEE
  jmp Instruction

; A lookup table for the ascii values of the
; l1, l2, and l3 enums.
LetterIndicesLo:
  .byte <FirstLetter, <SecondLetter, <ThirdLetter
LetterIndicesHi:
  .byte >FirstLetter, >SecondLetter, >ThirdLetter

; Given a zero-terminated string address in TMP1-2, parses the string as
; an instruction, returning the number of bytes of it takes up and the
; instruction number on the stack. Returns 0 on the stack if the instruction
; was not found.
ParseInstruction:
  ldy #0
@instructionLoop:
  lda LetterIndicesLo, y
  sta TMP3
  lda LetterIndicesHi, y
  sta TMP4
  tya
  pha
    lda (TMP1), y
    bze @badLetter
    jsr ParseLetter
    bmi @badLetter
    dex
    sty Stack, x ; temporarily save on stack.
  pla
  tay
  iny
  cpy #3
  bne @instructionLoop

  ; now the three bytes on the stack are the three letter indices.
  lda (TMP1), y
  beq @noTail
  cmp #'.'
  bne @notDot
  iny
  lda (TMP1), y ; get first letter of tail
  dex
  sta Stack, x ; store on stack
  iny
  lda (TMP1), y ; get second letter of tail
  dex
  sta Stack, x ; store on stack

@findMode:
  ldy #0
@findModeLoop:
  lda ModeBeg2, y
  cmp Stack, x
  bne :+
    lda ModeBeg, y
    cmp Stack+1, x
    bne :+
    sty Stack+1, x
    inx ; put mode number on the stack.
    bne @foundAll ; bra
  :
  iny
  cpy #13
  bne @findModeLoop ; bra
@badMode:
  lda #5
  pha
  ; fall-through
@badLetter:
  pla ; get saved y value.
  tay
  ; discard stack values.
  cpy #0
  beq @discardDone
@discardLoop:
  inx
  dey
  bne @discardLoop
@discardDone:
@notDot:
  push #0
  rts
@noTail:
  ; The three bytes bytes on the stack represent the letter indices,
  ; and the mode is either implied, relative, or absolute (these modes
  ; have no tail).
  dex
  lda #0
  sta Stack, x
  jsr CompactStack

  ldy #0
@noTailLoop:
  lda Instructions, y
  cmp Stack+1, x
  bne :+
    lda Instructions_end, y
    and #$F0
    cmp #mode::impl
    beq @ok
    cmp #mode::rel
    beq @ok
    cmp #mode::abs
    beq @ok
    bne :+
  @ok:
    lda Instructions_end, y
    and #$F
    cmp Stack, x
    bne :+
    ; Found the instruction!
    beq @success
  :
  iny
  cpy #192
  bne @noTailLoop
@pushError:
  lda #0
  sta Stack, x
  sta Stack+1, x
  rts

@foundAll:
  ; The four bytes on the stack represent the letter indices
  ; and the mode.
  jsr CompactStack
  ldy #0
@foundAllLoop:
  lda Instructions, y
  cmp Stack+1, x
  bne :+
    lda Instructions_end, y
    cmp Stack, x
    bne :+
    ; Found the instruction!
    beq @success
  :
  iny
  cpy #192
  bne @foundAllLoop
  beq @pushError

@success:
  ; y is the instruction.
  sty Stack, x
  lda #0
  sta Stack+1, x

  dex
  dex
  lda #0
  sta Stack+1, x
  lda Instructions_end, y
  lsr
  lsr
  lsr
  lsr
  cmp #1
  ble @one
  cmp #9
  bge @three
@two:
  lda #2
  sta Stack, x
  rts
@one:
  lda #1
  sta Stack, x
  rts
@three:
  lda #3
  sta Stack, x
  rts

CompactStack:
  ; Given four bytes on the stack are l1 l2 l3 m.
  ; Compact it to two bytes as 21 m3, one nibble
  ; for each.
  lda Stack+2, x
  asl
  asl
  asl
  asl
  ora Stack+3, x
  sta Stack+3, x

  lda Stack, x
  asl
  asl
  asl
  asl
  ora Stack+1, x
  sta Stack+2, x
  inx
  inx
  rts

Illegal:
  lda #'*'
  sta IO_PORT ; print *
  jsr DOT
  lda #$0A ; '\n'
  sta IO_PORT
  rts

; Given a pointer to a list of up to 16 letters in TMP3-4,
; and a letter in A, returns the index of the letter in y.
; If not found, returns -1
ParseLetter:
  ldy #0
@loop:
  cmp (TMP3), y
  beq @found
  iny
  cpy #16
  bne @loop
  ldy #$FF
@found:
  rts

; After printing the instruction,
; print these characters, based on the mode.
; A zero means to print nothing.
; If the first character is non-zero, print a '.' to
; separate the mode from the instruction.
ModeBeg:
  .byte 0, 'A', '#', 0, 'X', 'I', 'Z', 'Z', 'Z', 'I', 0, 'X', 'Y' 

ModeBeg2:
  .byte 0, 0,    0,  0, 'I', 'Y', 0,   'X', 'Y', 0,   0, 0,   0 


; Given an address, prints that instruction and then
; returns the address of the next instruction.
.export Instruction
Instruction:
  jsr DUP
  jsr INCR
  jsr SWAP
  jsr CFETCH ; get the instruction byte
  lda Stack, x
  and #%11
  cmp #%11
  ; All instructions which end in %11 are illegal
  beq Illegal

  ; We can only have 16 different second letters, so
  ; add special cases for txa:$8A txs:$9A tya:$98
  ; so we eliminate X and Y as possible second letters.
  lda Stack, x
  ldy #0
  cmp #$8A
  beq @special_txa
  cmp #$9A
  beq @special_txs
  cmp #$98
  beq @special_tya
  lsr
  lsr
  eor #$FF
  sec
  adc Stack, x ; Calculate ins - ins/4, because we ignore all %11 instructions
  tay
  lda Instructions_end, y ; get the last letter and addressing mode
  pha ; save them
    lda Instructions, y ; get the first two letters
    pha ; save them
      and #$F ; mask to get the first letter
      tay
      lda FirstLetter, y
      sta IO_PORT
    pla ; get the first two letters again
    lsr
    lsr
    lsr
    lsr
    tay
    lda SecondLetter, y
    sta IO_PORT
  pla
  pha ; retrieve last letter and addressing mode
    and #$F ; mask to get the last letter
    tay
    lda ThirdLetter, y
    sta IO_PORT
  pla
  lsr
  lsr
  lsr
  lsr
  tay ; a is now the mode number
  lda ModeBeg, y
  beq :+
    pha
    lda #'.'
    sta IO_PORT
    pla
    sta IO_PORT
  :
  lda ModeBeg2, y
  beq :+
    sta IO_PORT
  :
  lda #' '
  sta IO_PORT
  tya
  pha
    inx
    inx ; drop the instruction, leaving the address of the next byte.
    jsr PrintArg
  pla
  @newlineAndReturn:
  lda #$0A ; '\n'
  sta IO_PORT
  rts

@special_txa:
  iny
@special_tya:
  iny
@special_txs:
  ; now y is 0 for txs, 1 for tya, and 2 for txa.
  lda #'T'
  sta IO_PORT
  lda @special2, y
  sta IO_PORT
  lda @special3, y
  sta IO_PORT
  bne @newlineAndReturn ; bra

@special2:
  .byte "XYX"
@special3:
  .byte "SAA"

; The first two letters of each instruction,
; stored as a nibble each.
; Illegal instructions are represented as
; EEE because E was already present as a possible
; letter in all three positions.
; Each instruction with opcode b is stored at
; index b - floor(b/4). This is because we
; skip all instructions that end with the bits
; 11, which are all illegal.
Instructions:
.byte l1::B|l2::R,  l1::O|l2::R,  l1::E|l2::E
.byte l1::E|l2::E,  l1::O|l2::R,  l1::A_|l2::S
.byte l1::P|l2::H,  l1::O|l2::R,  l1::A_|l2::S
.byte l1::E|l2::E,  l1::O|l2::R,  l1::A_|l2::S
.byte l1::B|l2::P,  l1::O|l2::R,  l1::E|l2::E
.byte l1::E|l2::E,  l1::O|l2::R,  l1::A_|l2::S
.byte l1::C|l2::L,  l1::O|l2::R,  l1::E|l2::E
.byte l1::E|l2::E,  l1::O|l2::R,  l1::A_|l2::S
.byte l1::J|l2::S,  l1::A_|l2::N, l1::E|l2::E
.byte l1::B|l2::I,  l1::A_|l2::N, l1::R|l2::O
.byte l1::P|l2::L,  l1::A_|l2::N, l1::R|l2::O
.byte l1::B|l2::I,  l1::A_|l2::N, l1::R|l2::O
.byte l1::B|l2::M,  l1::A_|l2::N, l1::E|l2::E
.byte l1::E|l2::E,  l1::A_|l2::N, l1::R|l2::O
.byte l1::S|l2::E,  l1::A_|l2::N, l1::E|l2::E
.byte l1::E|l2::E,  l1::A_|l2::N, l1::R|l2::O
.byte l1::R|l2::T,  l1::E|l2::O,  l1::E|l2::E
.byte l1::E|l2::E,  l1::E|l2::O,  l1::L|l2::S
.byte l1::P|l2::H,  l1::E|l2::O,  l1::L|l2::S
.byte l1::J|l2::M,  l1::E|l2::O,  l1::L|l2::S
.byte l1::B|l2::V,  l1::E|l2::O,  l1::E|l2::E
.byte l1::E|l2::E,  l1::E|l2::O,  l1::L|l2::S
.byte l1::C|l2::L,  l1::E|l2::O,  l1::E|l2::E
.byte l1::E|l2::E,  l1::E|l2::O,  l1::L|l2::S
.byte l1::R|l2::T,  l1::A_|l2::D, l1::E|l2::E
.byte l1::E|l2::E,  l1::A_|l2::D, l1::R|l2::O
.byte l1::P|l2::L,  l1::A_|l2::D, l1::R|l2::O
.byte l1::J|l2::M,  l1::A_|l2::D, l1::R|l2::O
.byte l1::B|l2::V,  l1::A_|l2::D, l1::E|l2::E
.byte l1::E|l2::E,  l1::A_|l2::D, l1::R|l2::O
.byte l1::S|l2::E,  l1::A_|l2::D, l1::E|l2::E
.byte l1::E|l2::E,  l1::A_|l2::D, l1::R|l2::O
.byte l1::E|l2::E,  l1::S|l2::T,  l1::E|l2::E
.byte l1::S|l2::T,  l1::S|l2::T,  l1::S|l2::T
.byte l1::D|l2::E,  l1::E|l2::E,  l1::E|l2::E
.byte l1::S|l2::T,  l1::S|l2::T,  l1::S|l2::T
.byte l1::B|l2::C,  l1::S|l2::T,  l1::E|l2::E
.byte l1::S|l2::T,  l1::S|l2::T,  l1::S|l2::T
.byte l1::E|l2::E,  l1::S|l2::T,  l1::E|l2::E
.byte l1::E|l2::E,  l1::S|l2::T,  l1::E|l2::E
.byte l1::L|l2::D,  l1::L|l2::D,  l1::L|l2::D
.byte l1::L|l2::D,  l1::L|l2::D,  l1::L|l2::D
.byte l1::T|l2::A_, l1::L|l2::D,  l1::T|l2::A_
.byte l1::L|l2::D,  l1::L|l2::D,  l1::L|l2::D
.byte l1::B|l2::C,  l1::L|l2::D,  l1::E|l2::E
.byte l1::L|l2::D,  l1::L|l2::D,  l1::L|l2::D
.byte l1::C|l2::L,  l1::L|l2::D,  l1::T|l2::S
.byte l1::L|l2::D,  l1::L|l2::D,  l1::L|l2::D
.byte l1::C|l2::P,  l1::C|l2::M,  l1::E|l2::E
.byte l1::C|l2::P,  l1::C|l2::M,  l1::D|l2::E
.byte l1::I|l2::N,  l1::C|l2::M,  l1::D|l2::E
.byte l1::C|l2::P,  l1::C|l2::M,  l1::D|l2::E
.byte l1::B|l2::N,  l1::C|l2::M,  l1::E|l2::E
.byte l1::E|l2::E,  l1::C|l2::M,  l1::D|l2::E
.byte l1::C|l2::L,  l1::C|l2::M,  l1::E|l2::E
.byte l1::E|l2::E,  l1::C|l2::M,  l1::D|l2::E
.byte l1::C|l2::P,  l1::S|l2::B,  l1::E|l2::E
.byte l1::C|l2::P,  l1::S|l2::B,  l1::I|l2::N
.byte l1::I|l2::N,  l1::S|l2::B,  l1::N|l2::O
.byte l1::C|l2::P,  l1::S|l2::B,  l1::I|l2::N
.byte l1::B|l2::E,  l1::S|l2::B,  l1::E|l2::E
.byte l1::E|l2::E,  l1::S|l2::B,  l1::I|l2::N
.byte l1::S|l2::E,  l1::S|l2::B,  l1::E|l2::E
.byte l1::E|l2::E,  l1::S|l2::B,  l1::I|l2::N

; The last letter and addressing mode of each instruction.
; Each instruction with byte b is stored at index
; b - floor(b/4)
Instructions_end:    
.byte l3::K|mode::imm,  l3::A_|mode::indx, l3::E|mode::impl
.byte l3::E|mode::impl,  l3::A_|mode::zpg,  l3::L|mode::zpg
.byte l3::P|mode::impl,  l3::A_|mode::imm,  l3::L|mode::impl
.byte l3::E|mode::impl,  l3::A_|mode::abs,  l3::L|mode::abs
.byte l3::L|mode::rel,   l3::A_|mode::indy, l3::E|mode::impl
.byte l3::E|mode::impl,  l3::A_|mode::zpgx, l3::L|mode::zpgx
.byte l3::C|mode::impl,  l3::A_|mode::absy, l3::E|mode::impl
.byte l3::E|mode::impl,  l3::A_|mode::absx, l3::L|mode::absx
.byte l3::R|mode::abs,   l3::D|mode::indx,  l3::E|mode::impl
.byte l3::T|mode::zpg,   l3::D|mode::zpg,   l3::L|mode::zpg
.byte l3::P|mode::impl,  l3::D|mode::imm,   l3::L|mode::impl
.byte l3::T|mode::abs,   l3::D|mode::abs,   l3::L|mode::abs
.byte l3::I|mode::rel,   l3::D|mode::indy,  l3::E|mode::impl
.byte l3::E|mode::impl,  l3::D|mode::zpgx,  l3::L|mode::zpgx
.byte l3::C|mode::impl,  l3::D|mode::absy,  l3::E|mode::impl
.byte l3::E|mode::impl,  l3::D|mode::absx,  l3::L|mode::absx
.byte l3::I|mode::impl,  l3::R|mode::indx,  l3::E|mode::impl
.byte l3::E|mode::impl,  l3::R|mode::zpg,   l3::R|mode::zpg
.byte l3::A_|mode::impl, l3::R|mode::imm,   l3::R|mode::acc
.byte l3::P|mode::abs,   l3::R|mode::abs,   l3::R|mode::abs
.byte l3::C|mode::rel,   l3::R|mode::indy,  l3::E|mode::impl
.byte l3::E|mode::impl,  l3::R|mode::zpgx,  l3::R|mode::zpgx
.byte l3::I|mode::impl,  l3::R|mode::absy,  l3::E|mode::impl
.byte l3::E|mode::impl,  l3::R|mode::absx,  l3::R|mode::absx
.byte l3::S|mode::impl,  l3::C|mode::indx,  l3::E|mode::impl
.byte l3::E|mode::impl,  l3::C|mode::zpg,   l3::R|mode::zpg
.byte l3::A_|mode::impl, l3::C|mode::imm,   l3::R|mode::impl
.byte l3::P|mode::ind,   l3::C|mode::abs,   l3::R|mode::abs
.byte l3::S|mode::rel,   l3::C|mode::indy,  l3::E|mode::impl
.byte l3::E|mode::impl,  l3::C|mode::zpgx,  l3::R|mode::zpgx
.byte l3::I|mode::impl,  l3::C|mode::absy,  l3::E|mode::impl
.byte l3::E|mode::impl,  l3::C|mode::absx,  l3::R|mode::absx
.byte l3::E|mode::impl,  l3::A_|mode::indx, l3::E|mode::impl
.byte l3::Y_|mode::zpg,  l3::A_|mode::zpg,  l3::X_|mode::zpg
.byte l3::Y_|mode::impl, l3::E|mode::impl,  l3::A_|mode::impl
.byte l3::Y_|mode::abs,  l3::A_|mode::abs,  l3::X_|mode::abs
.byte l3::C|mode::rel,   l3::A_|mode::indy, l3::E|mode::impl
.byte l3::Y_|mode::zpgx, l3::A_|mode::zpgx, l3::X_|mode::zpgy
.byte l3::A_|mode::impl, l3::A_|mode::absy, l3::S|mode::impl
.byte l3::E|mode::impl,  l3::A_|mode::absx, l3::E|mode::impl
.byte l3::Y_|mode::imm,  l3::A_|mode::indx, l3::X_|mode::imm
.byte l3::Y_|mode::zpg,  l3::A_|mode::zpg,  l3::X_|mode::zpg
.byte l3::Y_|mode::impl, l3::A_|mode::imm,  l3::X_|mode::impl
.byte l3::Y_|mode::abs,  l3::A_|mode::abs,  l3::X_|mode::abs
.byte l3::S|mode::rel,   l3::A_|mode::indy, l3::E|mode::impl
.byte l3::Y_|mode::zpgx, l3::A_|mode::zpgx, l3::X_|mode::zpgy
.byte l3::V|mode::impl,  l3::A_|mode::absy, l3::X_|mode::impl
.byte l3::Y_|mode::absx, l3::A_|mode::absx, l3::X_|mode::absy
.byte l3::Y_|mode::imm,  l3::P|mode::indx,  l3::E|mode::impl
.byte l3::Y_|mode::zpg,  l3::P|mode::zpg,   l3::C|mode::zpg
.byte l3::Y_|mode::impl, l3::P|mode::imm,   l3::X_|mode::impl
.byte l3::Y_|mode::abs,  l3::P|mode::abs,   l3::C|mode::abs
.byte l3::E|mode::rel,   l3::P|mode::indy,  l3::E|mode::impl
.byte l3::E|mode::impl,  l3::P|mode::zpgx,  l3::C|mode::zpgx
.byte l3::D|mode::impl,  l3::P|mode::absy,  l3::E|mode::impl
.byte l3::E|mode::impl,  l3::P|mode::absx,  l3::C|mode::absx
.byte l3::X_|mode::imm,  l3::C|mode::indx,  l3::E|mode::impl
.byte l3::X_|mode::zpg,  l3::C|mode::zpg,   l3::C|mode::zpg
.byte l3::X_|mode::impl, l3::C|mode::imm,   l3::P|mode::impl
.byte l3::X_|mode::abs,  l3::C|mode::abs,   l3::C|mode::abs
.byte l3::Q|mode::rel,   l3::C|mode::indy,  l3::E|mode::impl
.byte l3::E|mode::impl,  l3::C|mode::zpgx,  l3::C|mode::zpgx
.byte l3::D|mode::impl,  l3::C|mode::absy,  l3::E|mode::impl
.byte l3::E|mode::impl,  l3::C|mode::absx,  l3::C|mode::absx

ASSEMBLER_END:


; How many coroutines to support
N_COS = 4
; How much data stack RAM for each one
N_RAM_EACH = (Stack_End-Stack) / N_COS
; How much return stack RAM for each one
N_RSTACK_EACH = $20
; How many bytes of control flow stack per coroutine
N_CONTROL_FLOW_STACK = (ControlFlowStackEnd-ControlFlowStack) / N_COS

.segment "ZEROPAGE"

CoXSave: .res N_COS
CoSPSave: .res N_COS
CoCFSSave: .res N_COS

.segment "RAM"

CurrentCo: .res 1
ActiveCos: .res 1, $1 ; initially, only the first coroutine is active.

.segment "CODE"

Bits:
.repeat N_COS, I
  .byte 1 << I
.endrepeat

XInit:
.repeat N_COS, I
  .byte Stack_End - Stack - 1 - (N_RAM_EACH * I)
.endrepeat

SPInit:
.repeat N_COS, I
  ; Reserve 4 bytes for the initial stack contents.
  .byte $FF - 4 - (N_RSTACK_EACH * I)
.endrepeat

CFSInit:
.repeat N_COS, I
  .byte ControlFlowStackEnd - ControlFlowStack - 1 - (N_CONTROL_FLOW_STACK * I)
.endrepeat

; Yields the current coroutine execution
; and resumes the next one.
; If there are no other routines to resume, it's a no-op.
defword "yield", 0, YIELD

  ; Save current register values
  ldy CurrentCo
  stx CoXSave, y
  tsx
  stx CoSPSave, y
  lda ControlFlowSP
  sta CoCFSSave, y

  ; If there are no active coroutines, then do nothing.
  lda ActiveCos
  beq @none

  ; Check the coroutines until we find the next active one.
: iny
  tya
  and #N_COS-1
  tay

  lda Bits, y
  and ActiveCos
  beq :-

  sty CurrentCo

  lda CoCFSSave, y
  sta ControlFlowSP
  ldx CoSPSave, y
  txs
  ldx CoXSave, y

@none:
  rts

; Given an execution token on the stack,
; puts the coroutine in the next empty slot.
; It will be start when it reaches its turn.
; ( xt -- coroutine-id )
defword "co-start", 0, CO_START

  ldy CurrentCo

  ; Find the next inactive coroutine slot.
: iny
  tya
  and #N_COS-1
  tay
  
  lda Bits, y 
  and ActiveCos
  bne :-

  ; Mark this coroutine as active
  lda ActiveCos
  ora Bits, y
  sta ActiveCos

  ; Set the values to restore
  lda XInit, y
  sta CoXSave, y

  lda CFSInit, y
  sta CoCFSSave, y

  lda SPInit, y
  sta CoSPSave, y

  sty TMP1

  tay ; initialize the return
      ; stack with co-exit and the execution token,
      ; so that the routine will start right away.
  lda #>(CO_EXIT - 1)
  sta $104, y
  lda #<(CO_EXIT - 1)
  sta $103, y

  ; Decrement xt on stack, since rts returns to addr+1
  lda Stack, x
  bne :+
  dec Stack+1, x
: dec Stack, x

  ; Put xt on return stack
  lda Stack+1, x
  sta $102, y
  lda Stack, x
  sta $101, y

  ; Put the coroutine number of the stack.
  lda TMP1
  sta Stack, x
  lda #0
  sta Stack+1, x

  rts

; Exits the current coroutine, and marks it
; as inactive.
defword "co-exit", 0, CO_EXIT
  ldy CurrentCo
  lda Bits, y
  eor #$FF
  and ActiveCos
  sta ActiveCos
  jmp YIELD

; Transfers val to the top of stack of the coroutine.
; ( val coroutine-id -- )
defword ">co", 0, TO_CO
  ; Get val from stack
  lda Stack+2, x
  sta TMP1
  lda Stack+3, x
  sta TMP2

  ; Get coroutine ID from the stack, save original stack pointer
  lda Stack, x
  tay
  lda CoXSave, y
  stx TMP3
  tax

    ; Now x is the other routine's data stack pointer.
    ; Push the value there.
    push TMP1

    stx CoXSave, y

    ; Restore our stack pointer
    lda TMP3
    add #4 ; deallocate the 2 values
  tax
  rts


.segment "VARIABLES"
VHERE_PERM_INIT:

.segment "TMP_VARIABLES"
VHERE_TMP_INIT:

; A marker for the end of the dictionary. This must be linked last.
defword "E", F_END, DICT_END
  rts

.segment "DICT_CODE"
CHERE_PERM_INIT:

defwordtmp "E", F_END, TMP_DICT_END
  rts

.segment "TMP_DICT_CODE"
CHERE_TMP_INIT:


