; -----------------------------------------------
; Implements a self-hosting 6502 disassembler
; It's a very basic one-pass disassembler.
; It will
; -----------------------------------------------

.segment "CODE"

.macpack generic
.import IO_PORT, DUP, FETCH, CFETCH, INCR, SWAP, DOT, DODOTQUOTE
.importzp Stack, TMP1, TMP2, TMP3

.enum mode
  impl = 0 ; no args

  ; 1 byte arg
  imm = $10
  rel = $20
  indx = $30
  indy = $40
  zpg = $50
  zpgx = $60
  zpgy = $70

  ; 2 byte arg
  ind = $80
  abs = $90
  absx = $A0
  absy = $B0
.endenum
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

message: .byte ".byte ", $22, $0

; Handles disassembly of DODOTQUOTE, which takes
; it's argument in the following code bytes.
PrintString:
  jsr DOT ; print the address of DODOTQUOTE
  lda #$0A ; '\n'
  sta IO_PORT
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
  cmp #0
  beq @return
  pha
  jsr DUP
  pla
  cmp #$8
  bge @two ; modes >= $80 are two byte modes.
  
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
  lda Stack, x
  cmp #<DODOTQUOTE
  bne :+
  lda Stack+1, x
  cmp #>DODOTQUOTE
  bne :+
  jmp PrintString
:
  jmp DOT ; print two bytes
  
@return:
  rts

Illegal:
  lda #'*'
  sta IO_PORT ; print *
  jsr DOT
  lda #$0A ; '\n'
  sta IO_PORT
  rts

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
  lda @instructions_end, y ; get the last letter and addressing mode
  pha ; save them
    lda @instructions, y ; get the first two letters
    pha ; save them
      and #$F ; mask to get the first letter
      tay
      lda @firstLetter, y
      sta IO_PORT
    pla ; get the first two letters again
    lsr
    lsr
    lsr
    lsr
    tay
    lda @secondLetter, y
    sta IO_PORT
  pla
  pha ; retrieve last letter and addressing mode
    and #$F ; mask to get the last letter
    tay
    lda @thirdLetter, y
    sta IO_PORT
    lda #' '
    sta IO_PORT
  pla
  lsr
  lsr
  lsr
  lsr
  tay ; a is now the mode number
  lda @mode_beg, y
  beq :+
    sta IO_PORT
  :
  tya
  pha
    inx
    inx ; drop the instruction, leaving the address of the next byte.
    jsr PrintArg
  pla
  tay
  lda @mode_end1, y
  beq :+
    sta IO_PORT
  :
  lda @mode_end2, y
  beq :+
    sta IO_PORT
  :
  lda @mode_end3, y
  beq @newlineAndReturn
    sta IO_PORT
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

; Letters used in the instruction names.
; We can only support up to 16 of each,
; so that they can be referenced with
; 4 bits.
@firstLetter:  .byte  "ABCDEIJLNOPRST"
@secondLetter: .byte "ABCDEHILMNOPRSTV"
@thirdLetter:  .byte  "ACDEIKLPQRSTVXY"

; After printing the instruction and a space,
; print these characters, based on the mode.
; A zero means to print nothing.
@mode_beg:
  .byte 0, '#', 0, '(', '(', 0, 0,   0,   '(', 0, 0,   0 

; After printing the instruction and arg, end the
; line with these characters, based on the mode.
; A zero means to print nothing.
@mode_end1:
  .byte 0, 0,   0, ',', ')', 0, ',', ',', ')', 0, ',', ','
@mode_end2:
  .byte 0, 0,   0, 'x', ',', 0, 'x', 'y', 0,   0, 'x', 'y'
@mode_end3:
  .byte 0, 0,   0, ')', 'y', 0, 0,   0,   0,   0, 0,   0

; The first two letters of each instruction,
; stored as a nibble each.
; Illegal instructions are represented as
; EEE because E was already present as a possible
; letter in all three positions.
; Each instruction with byte b is stored at
; index b - floor(b/4).
@instructions:
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
@instructions_end:    
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
.byte l3::A_|mode::impl, l3::R|mode::imm,   l3::R|mode::impl
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
