;-----------------------------------------------------------
; KEYPAD -- Keypad buffer handler
; On entry: A=key just pressed  
;-----------------------------------------------------------
.ORG    $~O5~                   
        LDX     KEYDB
        LDY     #$00
        STY     KEYDB           ;reset debounce timer to 0
        CPX     #$05            ;check previous debounce for 5
        BCC     KEYDONE        
        STA     KEY             ;reached, store keypress
KEYDONE PLA     
        TAY                     ;POP Y
        PLA     
        TAX                     ;POP X
        PLA                     ;POP A
        RTI     
