
.include "forth.inc"

.export VHERE, VHERE_VALUE
; Variable which points to the next free variable RAM space.
defvar "vhere", VHERE_VALUE+2, VHERE

; A marker for the end of the dictionary. This must be linked last.
.export DICT_END
defword "E", F_END, DICT_END
  rts

.segment "DICT_CODE"
.export CHERE_INIT
CHERE_INIT:


