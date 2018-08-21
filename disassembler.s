; -----------------------------------------------
; Implements a self-hosting 6502 disassembler
; It's a very basic one-pass disassembler.
; -----------------------------------------------

.segment "CODE"

.macpack generic
.import IO_PORT, DUP, FETCH, CFETCH, INCR, SWAP, DOT, DODOTQUOTE, RFIND
.importzp Stack, TMP1, TMP2, TMP3, TMP4

; To save space, the 3 letters and addressing mode of the
; instruction are stored in 4-bits each for every instruction.
; Turns out we can almost fit all of the instructions with
; just 16 possiblitiies for each one. (See below for the
; exceptions).
.enum mode
  impl = 0 ; no args
  acc = $10 ; accumulator is arg
  
  ; 1 byte arg
  imm = $20
  rel = $30
  indx = $40
  indy = $50
  zpg = $60
  zpgx = $70
  zpgy = $80

  ; 2 byte arg
  ind = $90
  abs = $A0
  absx = $B0
  absy = $C0
.endenum

; Enum for first letter in an instruction
.enum l1
  A_
  B
  C
  D
  E
  I
  J
  L
  N
  O
  P
  R
  S
  T
.endenum

; Enum for second letter in an instruction
.enum l2
  A_ = $0
  B = $10
  C = $20
  D = $30
  E = $40
  H = $50
  I = $60
  L = $70
  M = $80
  N = $90
  O = $A0
  P = $B0
  R = $C0
  S = $D0
  T = $E0
  V = $F0
.endenum

; Enum for third letter in an instruction
.enum l3
  A_
  C
  D
  E
  I
  K
  L
  P
  Q
  R
  S
  T
  V
  X_
  Y_
.endenum

; Letters used in the instruction names.
; We can only support up to 16 of each,
; so that they can be referenced with
; 4 bits.
FirstLetter:  .byte "ABCDEIJLNOPRST"
SecondLetter: .byte "ABCDEHILMNOPRSTV"
ThirdLetter:  .byte "ACDEIKLPQRSTVXYA" ; pad end to 16 characters with a.

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
  sec
  lda Stack, x
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
.export ParseInstruction
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
  lda #0
  dex
  dex
  sta Stack, x
  sta Stack+1, x
  rts
@noTail:
  ; The three bytes bytes on the stack represent the letter indices,
  ; and the mode is either implied, relative, or absolute.
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
  lda #'t'
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
; Each instruction with byte b is stored at
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


