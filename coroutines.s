
.include "forth.inc"

; How many coroutines to support
N_COS = 4
; How much data stack RAM for each one
N_RAM_EACH = (Stack_End-Stack) / N_COS
; How much return stack RAM for each one
N_RSTACK_EACH = $20
; How many bytes of control flow stack per coroutine
N_CONTROL_FLOW_STACK = (ControlFlowStackEnd-ControlFlowStack) / N_COS

.segment "ZEROPAGE"

XSave: .res N_COS
SPSave: .res N_COS
CFSSave: .res N_COS

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
  stx XSave, y
  tsx
  stx SPSave, y
  lda ControlFlowSP
  sta CFSSave, y

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

  lda CFSSave, y
  sta ControlFlowSP
  ldx SPSave, y
  txs
  ldx XSave, y

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
  sta XSave, y

  lda CFSInit, y
  sta CFSSave, y

  lda SPInit, y
  sta SPSave, y

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
  lda XSave, y
  stx TMP3
  tax

    ; Now x is the other routine's data stack pointer.
    ; Push the value there.
    dex
    dex
    lda TMP1
    sta Stack, x
    lda TMP2
    sta Stack+1, x

    stx XSave, y

    ; Restore our stack pointer
    lda TMP3
    adc #4 ; deallocate the 2 values
  tax
  rts
