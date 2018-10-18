
.ifndef __FORTH_INC__
.define __FORTH_INC__

; The address of the IO port in the 6502 emulator
.global IO_PORT

; Flags for the dictionary
.globalzp F_IMMED
.globalzp F_INLINE
.globalzp F_HIDDEN
.global F_END
.global F_SINGLE_BYTE

; Temporary storage
.globalzp TMP1, TMP2, TMP3, TMP4, TMP5, TMP6, TMP7, TMP8

; The data stack
.globalzp Stack, Stack_End

; The control flow stack
.globalzp ControlFlowSP, ControlFlowStack, ControlFlowStackEnd

; The return stack
.global RStack

.global DUP

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

.macro toTMP3
  lda Stack, x
  sta TMP3
  lda Stack+1, x
  sta TMP4
.endmacro

.macro toTMP5
  lda Stack, x
  sta TMP5
  lda Stack+1, x
  sta TMP6
.endmacro

.macro fromTMP1
  lda TMP1
  sta Stack, x
  lda TMP2
  sta Stack+1, x
.endmacro

.macro cmpTopZero
  lda Stack, x
  ora Stack+1, x
.endmacro

; The structure of a dictionary entry.
;
;  0                   1                   2
;  0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3
; +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
; |  $4C (jmp)    |          code pointer         |
; +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
; |E|S| reserved  |I|H|N| length  | name...       |
; +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
;
; code ptr - the start of the implementation code
; length - the length of the name, in bytes
; E - this is the last entry in the dictionary
; S - this is a single byte `val`
; I - this is an immediate word
; H - this word is hidden
; N - always inline this word
; name - the name, in ASCII
;
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
    push .ident(.concat(.string(label), "_VALUE"))
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
    push value
    rts
.endmacro

.macro defconsttmp name, value, label
  defwordtmp name, 0, label
    push value
    rts
.endmacro

; A val is a word which pushes its value onto the
; stack when executed, like constants, but stores the
; value in RAM, allowing it to be changed.
.macro defval name, init, label
  defword name, 0, label
    dex
    dex
    lda .ident(.concat(.string(label), "_VALUE"))
    sta Stack, x
    lda .ident(.concat(.string(label), "_VALUE")) + 1
    sta Stack + 1, x
    rts
  .segment "VARIABLES"
    .ident(.concat(.string(label), "_VALUE")):
    .word init
.endmacro

; A c-val is a `val` that only has values between 0 and 255.
.macro defcval name, init, label
  defword name, F_SINGLE_BYTE, label
    dex
    dex
    lda .ident(.concat(.string(label), "_VALUE"))
    sta Stack, x
    lda #0
    sta Stack + 1, x
    rts
  .segment "VARIABLES"
  .ident(.concat(.string(label), "_VALUE")):
    .byte init
.endmacro

.endif
