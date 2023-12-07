1 { x ; s/#x:[^#]*/#x:0/ ; tjmp0 ; s/$/#x:0/ ; :jmp0 ; x ; x ; p ; x ; x ; tjmp3 ; :jmp3 ; s/(#x:0?($|#))/YES|\1/ ; x ; tjmp1 ; i\
TRUE
 ; bjmp2 ; :jmp1 ; i\
FALSE
 ; :jmp2 ; }

