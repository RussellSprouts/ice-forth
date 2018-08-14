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

;NMI handler

nmi:
  pha
  txa
  pha
  tya
  pha

  lda PPU_MASK_VAR ;if rendering is disabled, do not access the VRAM at all
  and #%00011000
  bne @doUpdate
  jmp @skipAll

@doUpdate:

  lda #>OAM_BUF   ;update OAM
  sta PPU_OAM_DMA

  lda PAL_UPDATE   ;update palette if needed
  bne @updPal
  jmp @updVRAM

@updPal:

  ldx #0
  stx PAL_UPDATE

  lda #$3f
  sta PPU_ADDR
  stx PPU_ADDR

  ldy PAL_BUF       ;background color, remember it in X
  lda (PAL_BG_PTR),y
  sta PPU_DATA
  tax

  .repeat 3,I
  ldy PAL_BUF+1+I
  lda (PAL_BG_PTR),y
  sta PPU_DATA
  .endrepeat

  .repeat 3,J
  stx PPU_DATA      ;background color
  .repeat 3,I
  ldy PAL_BUF+5+(J*4)+I
  lda (PAL_BG_PTR),y
  sta PPU_DATA
  .endrepeat
  .endrepeat

  .repeat 4,J
  stx PPU_DATA      ;background color
  .repeat 3,I
  ldy PAL_BUF+17+(J*4)+I
  lda (PAL_SPR_PTR),y
  sta PPU_DATA
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
  sta PPU_ADDR
  sta PPU_ADDR

  lda SCROLL_X
  sta PPU_SCROLL
  lda SCROLL_Y
  sta PPU_SCROLL

  lda PPU_CTRL_VAR
  sta PPU_CTRL

@skipAll:

  lda PPU_MASK_VAR
  sta PPU_MASK

  inc FRAME_CNT1
  inc FRAME_CNT2
  lda FRAME_CNT2
  cmp #6
  bne @skipNtsc
  lda #0
  sta <FRAME_CNT2

@skipNtsc:

  jsr FamiToneUpdate

  pla
  tay
  pla
  tax
  pla

irq:

    rti

; Set bg and spr palettes, data is 32 bytes array
; ( palette-buffer -- )
pal_all:
  toTMP1
  lda #$20

; Given a length in A, sets the palette buffer
; given the buffer address in TMP1-2
pal_copy:
  sta <LEN

  ldy #0

@0:
  lda (TMP1), y
  sta PAL_BUF, y
  iny
  dec LEN
  bne @0

  inc <PAL_UPDATE

  pop
  rts

; Set bg palette only, data is 16 bytes array
; ( palette-buffer -- )
pal_bg:
  toTMP1
  lda #$10
  bne pal_copy ; bra

; Set spr palette only, data is 16 bytes array
; ( palette-buffer -- )
pal_spr:
  toTMP1
  lda #$10  
  sta <LEN
  ldy #0

@0:
  lda (TMP1), y
  sta PAL_BUF + $10, y
  iny
  dec LEN
  bne @0

  inc <PAL_UPDATE
  pop
  rts

; Set a palette entry. index is 0..31
; ( index color -- )
pal_col:
  ldy Stack+2, x
  lda Stack, x
  and #$1f
  sta PAL_BUF, y
  inc <PAL_UPDATE
  pop
  pop
  rts

; reset palette to $0F
; ( -- )
pal_clear:
  lda #$0F
  ldy #0

@1:
  sta PAL_BUF, y
  iny
  cpy #$20
  bne @1
  sty PAL_UPDATE
  rts

; Set virtual bright for background only
; ( brightness -- )
pal_spr_bright:
  ldy Stack, x
  lda palBrightTableL, y
  sta <PAL_SPR_PTR
  lda palBrightTableH, y
  sta <PAL_SPR_PTR+1
  sta <PAL_UPDATE
  pop
  rts

; Set virtual bright for sprites only
; ( brightness -- )
pal_bg_bright:
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
pal_bright:
  jsr DUP
  jsr pal_spr_bright
  jmp pal_bg_bright

; Turn off rendering, nmi still enabled when rendering is disabled.
; ( -- )
ppu_off:
  lda PPU_MASK_VAR
  and #%11100111
  sta PPU_MASK_VAR
  jmp ppu_wait_nmi

; Turn on bg, spr
; ( -- )
ppu_on_all:
  lda PPU_MASK_VAR
  ora #%00011000

ppu_onoff:
  sta PPU_MASK_VAR
  jmp ppu_wait_nmi

; Turn on bg only
; ( -- )
ppu_on_bg:
  lda PPU_MASK_VAR
  ora #%00001000
  bne ppu_onoff ; bra

; Turn on spr only
; ( -- )
ppu_on_spr:
  lda PPU_MASK_VAR
  ora #%00010000
  bne ppu_onoff ; bra

; Clear OAM buffer -- all sprites are hidden
; ( -- )
oam_clear:
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
oam_size:
  ldy #0
  lda Stack, x
  ora Stack+1, x
  beq :+
  ldy #%00100000
: sty TMP1
  lda <PPU_CTRL_VAR
  and #%11011111
  ora TMP1
  sta <PPU_CTRL_VAR

  rts

; Set a sprite in the OAM buffer. chrnum is tile, attr is
; attribute, sprid is offset in OAM in bytes
; returns sprid+4 which is offset for the next sprite
; ( x y chrnum attr sprid -- new-sprite-id )
oam_spr:
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
oam_hide_rest:
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
ppu_wait_nmi:
  lda #1
  sta VRAM_UPDATE
  lda FRAME_CNT1

@1:
  cmp FRAME_CNT1
  beq @1
  rts

; unpack RLE data to current address of VRAM, works only when rendering is
; off.
; ( data -- )
vram_unrle:
  ldy Stack, x
  lda Stack+1, x
  sta RLE_HIGH
  lda #0
  sta RLE_LOW

  lda (RLE_LOW), y
  sta RLE_TAG
  iny
  bne @1
  inc RLE_HIGH

@1:
  lda (RLE_LOW), y
  iny
  bne @11
  inc RLE_HIGH

@11:
  cmp RLE_TAG
  beq @2
  sta PPU_DATA
  sta RLE_BYTE
  bne @1

@2:
  lda (RLE_LO), y
  beq @4
  iny
  bne @21
  inc RLE_HIGH

@21:
  ; TODO - finish

; Set scroll, including rhe top bits
; It is always applied at beginning of TV frame, not at function call
; ( x y -- )
scroll: ; TODO

; Set scroll after screen split invoken by the sprite 0 hit.
; warning: all CPU time between the function call and the actual split
; point will be wasted!
; warning: only X scroll can be changed with this version.
; ( x y -- )
split: ; TODO

; Select current bank for sprites 0..1
; ( n -- )
bank_spr:
  lda Stack, x
  and #1
  asl
  asl
  asl
  sta <TMP1
  lda PPU_CTRL_VAR
  and #%11110111
  ora TEMP
  sta PPU_CTRL_VAR
  pop

  rts

; Select current bank for background, 0..1
; ( n -- )
bank_bg:
  lda Stack, x
  and #1
  asl
  asl
  asl
  asl
  sta TEMP
  lda PPU_CTRL_VAR
  and #%11101111
  ora TEMP
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

; delay for N frames
; ( frames -- )
delay:
  ldy Stack, x

@1:
  jsr ppu_wait_nmi
  dey
  bne @1  

  pop 
  rts

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
