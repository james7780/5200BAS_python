LOCATEH .EQU    $1A     
LOCATEL .EQU    $19
TEMPH   .EQU    $1C     
TEMPL   .EQU    $1B     
JOYTRIG .EQU    $1D
SCREENH .EQU    $1F
SCREENL .EQU    $1E
FROMH   .EQU    $21
FROML   .EQU    $20
TOH     .EQU    $23
TOL     .EQU    $22
COUNTH  .EQU    $25
COUNTL  .EQU    $24
COPYLEN .EQU    $26
SKIP    .EQU    $27
SPRITEH .EQU    $29
SPRITEL .EQU    $28
KEY     .EQU    $2A
KEYLAST .EQU    $2B
KEYDB   .EQU    $2C
RND     .EQU    RANDOM
;---------------------------------------------------------------------
; MAIN
;---------------------------------------------------------------------
MAIN    SEI
        LDA     #$00
        TAX          
CL1     STA     $00,X           ;Clear zero page
        STA     ANTIC,X         ;Clear ANTIC
        STA     GTIA,X          ;Clear GTIA
        STA     POKEY,X         ;Clear POKEY
        DEX
        BNE     CL1
;----------------------------- Clear RAM -----------------------------
        LDY     #$00            ;Clear RAM from $0300-$3FFF
        STY     $80
        LDA     #$03            
        STA     $81             
        JSR     CLRRAM
#IF TITLE=1
        JSR     Y2K
#ELSE
        NOP
        NOP
        NOP
#ENDIF
;------------------------- Set up IRQ vectors ------------------------
        LDX     #$1A            ;14 IRQ's 
SETVECT LDA     OPTIONS,X
        STA     $0200,X
        DEX
        BPL     SETVECT
;---------------------------------------------------------------------
        LDA     #$02            ;\ Set normal characters
        STA     CHACTL          ;/
        LDA     #$02            ;\ Enable keypad scanning
        STA     SKCTL           ;/  
        LDA     #$04            ;\ Enable joystick POTs, set to keypad 0
        STA     CONSOL          ;/ 
        LDA     #$FF
        STA     KEY
        STA     KEYLAST
