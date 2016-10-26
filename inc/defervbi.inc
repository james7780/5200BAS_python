;-----------------------------------------------------------
; Deferred VBI handler          
;-----------------------------------------------------------
.ORG    $~O2~                   
        LDA     KEYDB           ;check key debounce counter
        BMI     VBIDONE         ;if zero, skip
        INC     KEYDB           ;otherwise increase by one
VBIDONE PLA                     
        TAY                     ;POP Y
        PLA     
        TAX                     ;POP X
        PLA                     ;POP A
        RTI     
