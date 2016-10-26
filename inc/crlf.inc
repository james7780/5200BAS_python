;---------------------------------------------------------------------
; CRLF -- moves the cursor down one row, scrolls the screen if needed
;---------------------------------------------------------------------
CRLF    JSR     POS             
        INX     
        TXA     
        TAY     
        LDA     #$00
        STA     TEMPL
        LDX     #$~O14<~
        LDA     #$28
        JSR     MULADD          ; MULADD: (X:A)=A*Y+(X:TEMPL) if Y>0
        STX     LOCATEH
        STA     LOCATEL
        JSR     CHKROW
        RTS     
