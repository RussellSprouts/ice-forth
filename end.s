
.include "forth.inc"

.segment "VARIABLES"
.export VHERE_PERM_INIT
VHERE_PERM_INIT:

.segment "TMP_VARIABLES"
.export VHERE_TMP_INIT
VHERE_TMP_INIT:

; A marker for the end of the dictionary. This must be linked last.
defword "E", F_END, DICT_END
  rts

.segment "DICT_CODE"
.export CHERE_PERM_INIT
CHERE_PERM_INIT:

defwordtmp "E", F_END, TMP_DICT_END
  rts

.segment "TMP_DICT_CODE"
.export CHERE_TMP_INIT
CHERE_TMP_INIT:
