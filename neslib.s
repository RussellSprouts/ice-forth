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


.include "forth.inc"

.macpack generic
.macpack longbranch

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
  toTMP1
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
  toTMP1
  lda #$10
  bne pal_copy ; bra

; Set spr palette only, data is 16 bytes array
; ( palette-buffer -- )
defword "pal-spr!", 0, pal_spr
  toTMP1
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
  clc
  adc #4
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
  push OAM_BUF
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
  clc
  adc End, x
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
