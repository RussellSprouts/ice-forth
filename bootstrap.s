
IO_PORT := $401C

.segment "CODE"

ldy #0
: lda Message, y
  beq :+
  iny
  sta IO_PORT
  jmp :-
: brk

Message:
.byte "Hello world!", 0

