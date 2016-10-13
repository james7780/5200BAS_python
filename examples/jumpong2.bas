' JumPong 2 - 5200Basic Version
' (c) James Higgs 2002-2004

' TODO:
' 1. Select paddle/joystick control scheme
' 2. ball speed-up after every 10 bounces
' 3. Player can speed up ball by holding fire

' define variables
' NOTE: variables cannot be more than 7 chars long!
DEFINE P1Y, $A0
DEFINE P2Y, $A1
DEFINE P1DY, $A2
DEFINE P2DY, $A3
DEFINE BALLX, $A4
DEFINE BALLYLO, $A6
DEFINE BALLYHI, $A7
DEFINE BALLDX, $AA
DEFINE BALDYLO, $AB
DEFINE BALDYHI, $AC
' is sound playing?
DEFINE PLAYING, $AD
' envelope index
DEFINE SINDEX, $AE
' sound freqency
DEFINE SFREQ, $AF
' PL1 and PL2 scores
DEFINE P1SCORE, $B0
DEFINE P2SCORE, $B1
DEFINE SERVER, $B2
DEFINE T1, $B3
DEFINE CNTRLR, $B4
DEFINE BALLSPD, $B5
DEFINE BOUNCES, $B6
' ADD16 vars
DEFINE IN1LO, $F0
DEFINE IN1HI, $F1
DEFINE IN2LO, $F2
DEFINE IN2HI, $F3
DEFINE OUTLO, $F4
DEFINE OUTHI, $F5

TITLE "jumpong V2"
AUTHOR "BY JAMES HIGGS 2004"


' --------------------------------------------------------------
' Init graphics
' --------------------------------------------------------------

' set up gfx
SET DLIST=$BA20 MYDLIST
CHARSET $AC00

' initialise sprites
SPRITES ON
A=$03 : POKE GRACTL,A
'A=1 : POKE PRIOR,A
PALETTE 5, $1D
PALETTE 6, $FE
PALETTE 7, $CE
PALETTE 8, $BE

' set up background
PALETTE 0, $02
' copy BG data to RAM
MEMCOPY $B800,$1000,540
'PUT 0, 0, JUMDAT
A=49: POKE HPOSP0,A
A=201 : POKE HPOSP1,A
' sprite Y bounds: top 38 bottom: 184
P1Y=100
P1DY=1
P2Y=100
P2DY=$FF
' initial controller = joystick
CNTRLR=0

' --------------------------------------------------------------
' Title/Attract screen
' --------------------------------------------------------------
TITLESCREEN:
VOLUME 0,0,0
KEYPAD 1
DO
' show sprite 0
GOSUB CLEARSPRITE0
GOSUB PUTS0
' show sprite 1
GOSUB CLEARSPRITE1
GOSUB PUTS1
GOSUB WAITVSYNC
' show which controller
LOCATE 12,6
A = CNTRLR
IF A=0 THEN
PRINT "joystick"
ELSE
PRINT "@paddle@"
END IF
' move PL1
A = P1Y
A = A + P1DY
P1Y = A
IF A=38 THEN GOTO PL1DOWN
IF A=184 THEN GOTO PL1UP
GOTO PL1OK
PL1DOWN:
P1DY=1
GOTO PL1OK
PL1UP:
P1DY=$FF
PL1OK:
' move PL2
A = P2Y
A = A + P2DY
P2Y = A
IF A=38 THEN GOTO PL2DOWN
IF A=184 THEN GOTO PL2UP
GOTO PL2OK
PL2DOWN:
P2DY=1
GOTO PL2OK
PL2UP:
P2DY=$FF
PL2OK:
' *** emu bug: KBCODE doesn't update unless a key is pressed !!! ***
KEYPAD 0
A=INKEY
IF A=$0A THEN:CNTRLR=0:END IF
IF A=$0B THEN:CNTRLR=1:END IF
IF A=$0C THEN EXIT DO
IN1LO = $98
IN1HI = $99
IN2LO = $08
IN2HI = $00
GOSUB ADD16
LOOP

' --------------------------------------------------------------
' Main GAME LOOP
' --------------------------------------------------------------
SERVER = 0
P1SCORE = 0
P2SCORE = 0

' reset sound envelope, sound off
SINDEX=0
PLAYING=0

'clear middle of screen of text
MEMCOPY $B828,$1078,80
MEMCOPY $B828,$10C8,80
MEMCOPY $B828,$1118,80
MEMCOPY $B828,$1168,80
MEMCOPY $B828,$11A8,80

MAINGAME:
' reset ball to centre
BALLX = 128
BALLYHI = 124

' draw scores
GOSUB DRAWSCORE
A = P1SCORE
IF A = 9 THEN
LOCATE 4,3
PRINT "player{0}{17}{0}wins{1}"
GOTO TITLESCREEN
END IF
A = P2SCORE
IF A = 9 THEN
LOCATE 4,3
PRINT "player{0}{18}{0}wins{1}"
GOTO TITLESCREEN
END IF

' wait for player to press trigger to release ball
DO
A = SERVER
IF A=1 THEN GOTO PL2_START
A=JOYTRIG(0)
GOTO CHECKSTART
PL2_START:
A=JOYTRIG(1)
CHECKSTART:
IF A <> $01 THEN EXIT DO
GOSUB WAITVSYNC2
GOSUB CLEARALL
GOSUB PUTS0
GOSUB PUTS1
GOSUB MOVEPLAYERS
GOSUB UPDATESND
LOOP

' start ball acoording to server
BALLSPD=1 : BOUNCES = 0
A=SERVER : IF A=1 THEN GOTO PL2_SERVE
BALLDX=1
GOTO SERVE
PL2_SERVE:
BALLDX=$FE
SERVE:
BALDYHI=0
BALDYLO=$01

' ball in play
GOSUB STARTNEWBALL

DO
' clear collision regs
POKE HITCLR,A
GOSUB WAITVSYNC2
GOSUB CLEARALL
GOSUB PUTS0
GOSUB PUTS1
GOSUB PUTBALL
GOSUB MOVEPLAYERS

' move ball horizontally
A=BALLX
A=A+BALLDX
BALLX=A
' move ball vertically
IN1LO = BALLYLO
IN1HI = BALLYHI
IN2LO = BALDYLO
IN2HI = BALDYHI
GOSUB ADD16
BALLYLO = OUTLO
BALLYHI = OUTHI

' check for bounce against top and bottom walls
A = BALLYHI
IF A<38 THEN GOTO BOUNCE
IF A>228 THEN GOTO BOUNCE
GOTO ENDBOUNCE
BOUNCE:
A = $FF
A = A - BALDYLO
BALDYLO = A
A = $FF
A = A - BALDYHI
BALDYHI = A

GOSUB STARTPING
ENDBOUNCE:

' hit bat of PL1 or PL2?
A=PEEK(M2PL)
IF A=$01 THEN GOTO PL1_HIT
IF A=$02 THEN GOTO PL2_HIT 
GOTO ENDHIT
PL1_HIT:
BALLDX=BALLSPD
' *** calc vertical deflection change based on where ball hits bat
T1 = P1Y
GOSUB DEFLECT

' ***
GOSUB STARTPING
GOTO ENDHIT
PL2_HIT:
A=BALLSPD : A=A XOR $FF : A=A+1
BALLDX=A
' calc angle
T1 = P2Y
GOSUB DEFLECT

GOSUB STARTPING
ENDHIT:

' update sound
GOSUB UPDATESND

' check if we need to speed the ball up
A=BALLSPD
IF A<5 THEN
    A=BOUNCES
    IF A=50 THEN
        BOUNCES=0
        BALLSPD=BALLSPD+1
    END IF
END IF

' check if ball off screen, and exit loop if so
A=BALLX
IF A<30 THEN EXIT DO
IF A>220 THEN EXIT DO

LOOP

' ball off screen
IF A<30 THEN GOTO P1_MISS
P1SCORE = P1SCORE + 1
SERVER = 0
GOTO ENDMISS
P1_MISS:
P2SCORE = P2SCORE + 1
SERVER = 1
ENDMISS:

GOSUB STARTMISS
GOTO MAINGAME

' --------------------------------------------------------------
' PLAYER CONTROL
' --------------------------------------------------------------
MOVEPLAYERS:
A=CNTRLR:IF A=0 THEN GOTO USEJOYSTICK

' --- paddle controller
USEPADDLE:
' pl 1
A = JOYX(0)
IF A > 185 THEN:A=185:END IF
IF A < 38 THEN:A=38:END IF
P1Y = A
' pl 2
A = JOYX(1)
IF A > 185 THEN:A=185:END IF
IF A < 38 THEN:A=38:END IF
P2Y = A
RETURN

' --- joystick controller
USEJOYSTICK:
' move PL1
P1DY=0
A = JOYY(0)
IF A > 192 THEN GOTO PLR1DOWN
IF A < 64 THEN GOTO PLR1UP
GOTO PLR1DONE
PLR1DOWN:
P1DY=3
A = P1Y
A = A + P1DY
IF A > 185 THEN GOTO PLR1BOTTOM
P1Y = A
GOTO PLR1DONE
PLR1UP:
P1DY=$FD
A = P1Y
A = A + P1DY
IF A < 38 THEN GOTO PLR1TOP
P1Y = A
GOTO PLR1DONE
' latch to top or bounds
PLR1TOP:
P1Y = 38
GOTO PLR1DONE
' latch to bottom of bounds
PLR1BOTTOM:
P1Y = 185

PLR1DONE:

' move PL2
P2DY=0
A = JOYY(1)
IF A > 192 THEN GOTO PLR2DOWN
IF A < 64 THEN GOTO PLR2UP
GOTO PLR2DONE
'
PLR2DOWN:
P2DY=3
A = P2Y
A = A + P2DY
IF A > 185 THEN GOTO PLR2BOTTOM
P2Y = A
GOTO PLR2DONE
' 
PLR2UP:
P2DY=$FD
A = P2Y
A = A + P2DY
IF A < 38 THEN GOTO PLR2TOP
P2Y = A
GOTO PLR2DONE
' latch to top or bounds
PLR2TOP:
P2Y = 38
GOTO PLR2DONE
' latch to bottom of bounds
PLR2BOTTOM:
P2Y = 185

PLR2DONE:

RETURN




' --------------------------------------------------------------
' CALCULATE VERTICAL DEFLECTION OFF PADDLE
' --------------------------------------------------------------
' *** calc vertical deflection change here, based on
' *** where ball hits bat
' input: T1 = PADDLE Y
DEFLECT:
A=BALLYHI
A=A-T1
A=A-24
IN1LO = A
IF NFLAG=0 THEN
IN1HI = 0
ELSE
IN1HI = $FF
END IF
IN2LO = BALDYLO
IN2HI = BALDYHI
GOSUB ADD16
BALDYLO = OUTLO
BALDYHI = OUTHI
BOUNCES = BOUNCES + 1
RETURN

' --------------------------------------------------------------
' SPRITE ROUTINES
' --------------------------------------------------------------
' Putsprite 0: Y-posn in X reg
PUTS0:
Y=0
X=P1Y
DO
A=PEEK(SPRBAT + Y)
IF A=0 THEN EXIT DO
POKE $3400+X,A
X=X+1 : Y=Y+1
LOOP
RETURN

' Putsprite 1: Y-posn in X reg
PUTS1:
Y=0
X=P2Y
DO
A=PEEK(SPRBAT + Y)
IF A=0 THEN EXIT DO
POKE $3500+X,A
X=X+1 : Y=Y+1
LOOP
RETURN

' draw ball (missile 0)
PUTBALL:
X=BALLYHI
A=$F0
POKE $3300+X,A
INX
POKE $3300+X,A
INX
POKE $3300+X,A
INX
POKE $3300+X,A
INX
A=BALLX
POKE HPOSM2,A
RETURN


' clearsprite0
CLEARSPRITE0:
        LDY     #35
        LDA     #$00
DJ:
        STA     $3400,Y
        INY
        BNE     DJ
RETURN

' clearsprite1
CLEARSPRITE1:
        LDY     #35
        LDA     #$00
DJ1:
        STA     $3500,Y
        INY
        BNE     DJ1
RETURN

' clearmissiles
CLEARMISSILES:
        LDY     #35
        LDA     #$00
DJ2:
        STA     $3300,Y
        INY
        BNE     DJ2
RETURN

' clearall
CLEARALL:
        LDY     #35
        LDA     #$00
DJ3:
        STA     $3300,Y
        STA     $3400,Y
        STA     $3500,Y
        INY
        BNE     DJ3
RETURN


' wait for vsync (with colourbars)
WAITVSYNC:
DO
A=PEEK(VCOUNT)
IF A=118 THEN EXIT DO
        CLC
        ADC        RTCLOK+1
' we need to have RAW here so 5200bas doesn't get confused
'RAW        AND        #$0F
'        ORA        #$70
'A=A AND $F6
'A=A OR $70
        STA        COLBK
        STA        WSYNC
LOOP
        LDA        #$00
        STA        COLBK
RETURN

' wait for vsync (no colourbars)
WAITVSYNC2:
DO
A=PEEK(VCOUNT)
IF A=100 THEN EXIT DO
        STA        WSYNC
LOOP
RETURN

'---------------------------------------------------------------------
' 16-bit ADD
'---------------------------------------------------------------------
' input:        $F0 - low byte of number 1
'               $F1 - high byte of number 1
'               $F2 - low byte of number 2
'               $F3 - high byte of number 3
' output:       $F4 - low byte of result
'               $F5 - high byte of result
ADD16:
        CLC
        LDA        IN1LO
        ADC        IN2LO
        STA        OUTLO
        LDA        IN1HI
        ADC        IN2HI
        STA        OUTHI
RETURN


'---------------------------------------------------------------------
' SCORE ROUTINES
'---------------------------------------------------------------------
DRAWSCORE:
        CLC
        LDA        P1SCORE
        ADC        #16
        STA        $1001
        LDA        P2SCORE
        ADC        #16
        STA        $1012
RETURN        

'---------------------------------------------------------------------
' SOUND ROUTINES
'---------------------------------------------------------------------
' start ping sound
STARTPING:
PLAYING=1
SINDEX=0
GOSUB UPDATESND
RETURN

' start new ball sound
STARTNEWBALL:
PLAYING=1
SINDEX=14
GOSUB UPDATESND
RETURN

' start miss sound
STARTMISS:
PLAYING=1
SINDEX=26
GOSUB UPDATESND
RETURN

' update sound
UPDATESND:
A=PLAYING
IF A=0 THEN GOTO EXITUPDATESND
X=SINDEX
A=PEEK(ENVSTART+X)
POKE AUDC1,A
PLAYING=A
X=X+1
A=PEEK(ENVSTART+X)
POKE AUDF1,A
X=X+1
SINDEX = X
EXITUPDATESND:
RETURN

' charset
.ORG    $AC00
#INCLUDE CHARSET1.INC
' NOTE: If CHARSET1.INC is missing, use ASCIISET.INC instead
'       (Characters will be wrong, but it least it will build)

'---------------------------------------------------------------------
' DATA
'---------------------------------------------------------------------
.ORG    $B400                   
SPRBAT:                                ; graphics for bat
.BYTE   $FC,$FC,$FC,$FC,$CC,$CC,$CC,$CC
.BYTE   $CC,$CC,$CC,$CC,$CC,$CC,$CC,$CC
.BYTE   $CC,$CC,$CC,$CC,$CC,$CC,$CC,$CC
.BYTE   $CC,$CC,$CC,$CC,$CC,$CC,$CC,$CC
.BYTE   $CC,$CC,$CC,$CC,$CC,$CC,$CC,$CC
.BYTE   $CC,$CC,$CC,$CC,$FC,$FC,$FC,$FC
.BYTE   0
SPRBALL:                        ; graphics for ball
.BYTE   $38,$7C,$FE,$FE,$FE,$7C,$38
.BYTE   0
BTABLE:                                ; bounce DY table
.BYTE        $00,$FE,$FC,$FA,$F8,$F6,$F4,$F2
.BYTE        $10,$0E,$0C,$0A,$08,$06,$04,$02
.BYTE        0,0,0,0

.ORG    $B600
ENVSTART:
PINGENV:                        ; ping sound envelope
.BYTE   $AA,$AF
.BYTE   $AF,$AF
.BYTE   $A8,$AF
.BYTE   $A4,$AF
.BYTE   $A2,$AF
.BYTE   $A1,$AF
.BYTE   $00,$00
NEWBALLENV:                     ; new ball sound
.BYTE   $AA,$23
.BYTE   $A0,$33
.BYTE   $AA,$23
.BYTE   $A0,$33
.BYTE   $AA,$23
.BYTE   $00,$00
MISSENV:                        ; ball miss vol envelope
.BYTE   $AA,$44
.BYTE   $AB,$33
.BYTE   $AC,$22
.BYTE   $AD,$11
.BYTE   $AE,$66
.BYTE   $AD,$55
.BYTE   $AC,$44
.BYTE   $AB,$33
.BYTE   $AA,$88
.BYTE   $A9,$77
.BYTE   $A8,$66
.BYTE   $A8,$55
.BYTE   $A7,$AA
.BYTE   $A7,$99
.BYTE   $A6,$88
.BYTE   $A6,$77
.BYTE   $A5,$CC
.BYTE   $A5,$BB
.BYTE   $A4,$AA
.BYTE   $A4,$99
.BYTE   $A3,$EE
.BYTE   $A3,$DD
.BYTE   $A2,$CC
.BYTE   $00,$00

'---------------------------------------------------------------------
' JUMPONG MODE 6 BG DATA (25 lines, 460 bytes, to be copied to $1000)
'---------------------------------------------------------------------
' Note: char 33 = 'A', char 16 = '0' (zero)
' Trans table:
' 0                         16        0
' 1        !                17        1
' 2        "                18        2
' 3        #                19        3
' 4        $                20        4
' 5        %                21        5
' 6        &                22        6
' 7        '                23        7
' 8        (                24        8
' 9        )                25        9
' 10        *                26        :
' 11        +                27        ;
' 12        ,                28        <
' 13        -                29        =
' 14        .                30        >
' 15        /                31        ?

' 32        @                48        P
' 33        A                49        Q
' 34        B                50        R
' 35        C                51        S
' 36        D                52        T
' 37        E                53        U
' 38        F                54        V
' 39        G                55        W
' 40        H                56        X
' 41        I                57        Y
' 42        J                58        Z
' 43        K
' 44        L
' 45        M
' 46        N
' 47        0
.ORG $B800                        ; BG data
BGDATA:
        .BYTE 0,0,0,0,0,42,53,45,48,47,46,39,0,54,18,0,0,0,0,0
        .BYTE 13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13
        .BYTE 0,0,0,0,0, 0,0,0,0,0, 0,0,0,0,0, 0,0,0,0,0
        .BYTE 0,0,0,0,0, 0,0,0,0,0, 0,0,0,0,0, 0,0,0,0,0
        .BYTE 0,0,0,0,0, 0,0,0,0,0, 0,0,0,0,0, 0,0,0,0,0
        .BYTE 0,0,0,0,0, 0,0,0,0,0, 0,0,0,0,0, 0,0,0,0,0
        .BYTE 0,0,0,0,0, 0,0,0,0,0, 0,0,0,0,0, 0,0,0,0,0
        .BYTE 0,0,0,34,57, 0,42,53,45,0, 40,41,39,0,18, 16,16,18,0,0
        .BYTE 0,0,0,0,0, 0,0,0,0,0, 0,0,0,0,0, 0,0,0,0,0
        .BYTE 0,0,0,0,0, 0,0,0,0,0, 0,0,0,0,0, 0,0,0,0,0
        .BYTE 0,0,0,0,0, 0,0,0,0,0, 0,0,0,0,0, 0,0,0,0,0
        .BYTE 0,0,0,0,0, 0,0,0,0,0, 0,0,0,0,0, 0,0,0,0,0
        .BYTE 0,0,0,0,0, 0,0,0,0,0, 0,0,0,0,0, 0,0,0,0,0
        .BYTE 0,0,0,0,55, 50,41,52,52,37, 46,0,53,51,41, 46,39,0,0,0
        .BYTE 0,0,0,0,0, 0,0,0,0,0, 0,0,0,0,0, 0,0,0,0,0
        .BYTE 0,0,0,0,0, 21,18,16,16,0, 34,33,51,41,35, 0,0,0,0,0
        .BYTE 0,0,0,0,0, 0,0,0,0,0, 0,0,0,0,0, 0,0,0,0,0
        .BYTE 0,0,0,0,0, 0,0,0,0,34, 57,0,0,0,0, 0,0,0,0,0
        .BYTE 0,0,0,0,0, 0,0,0,0,0, 0,0,0,0,0, 0,0,0,0,0
        .BYTE 0,0,0,0,0, 0,35,33,44,33, 45,33,50,41,0, 0,0,0,0,0
        .BYTE 0,0,0,0,0, 0,0,0,0,0, 0,0,0,0,0, 0,0,0,0,0
        .BYTE 0,0,0,0,0, 0,0,0,0,0, 0,0,0,0,0, 0,0,0,0,0
        .BYTE 0,0,0,0,0, 0,0,0,0,0, 0,0,0,0,0, 0,0,0,0,0
        .BYTE 0,0,0,0,0, 0,0,0,0,0, 0,0,0,0,0, 0,0,0,0,0
        .BYTE 0,0,0,0,0, 0,0,0,0,0, 0,0,0,0,0, 0,0,0,0,0
        .BYTE 0,0,0,0,0, 0,0,0,0,0, 0,0,0,0,0, 0,0,0,0,0
        .BYTE 13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13

'---------------------------------------------------------------------
' MY DISPLAY LIST (26 MODE 6 lines)
'---------------------------------------------------------------------
MYDLIST:
.ORG        $BA20                        ; My display list
        .BYTE   $70,$70                ;skip 16 scan lines
        .BYTE   $46             ;set up gr.mode 6 screen
        .WORD   $1000           ;address of screen memory
        .BYTE   $06,$06,$06,$06,$06,$06,$06,$06,$06,$06
        .BYTE   $06,$06,$06,$06,$06,$06,$06,$06,$06,$06
        .BYTE   $06,$06,$06,$06,$06,$06
        .BYTE   $41
        .WORD   $BF00           ;jump back to top of list
