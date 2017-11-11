
.segment "ZEROPAGE"

N: .res 8
STACK: .res 128

.segment "CODE"

STORE:
    sta STACK-1, x
    lda STACK+1, x
    sta (STACK-1, x)
    inx
    inx
    inx
    rts

LIT: ; run time
    dex
    sta STACK, x
    lda #0
    done

LIT: ; compile time
    sta 
