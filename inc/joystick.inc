;---------------------------------------------------------------------
; JOYSTICK -- VTRIGR hook.  Sets JOYTRIG=1 when joystick fire pressed
; Changes: JOYTRIG
;---------------------------------------------------------------------
.ORG    $~O6~
        LDA     #$01
        STA     JOYTRIG
        PLA
        RTI
