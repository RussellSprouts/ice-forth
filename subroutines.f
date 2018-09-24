
: { immediate
  CLV IFVS
;

: } immediate
  RTS
  dup THEN
  [compile] literal
;

: test 0 { 1 } execute ;
