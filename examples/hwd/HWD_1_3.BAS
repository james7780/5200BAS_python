'          Use this at your own risk.  I am learning to program the 5200 and sharing
'          this source code for whatever use you may make of it.  Just keep in mind
'          I am no expert and the comments here may not be accurate.
'
'   Hunting with Dick
'            
'          an Atari 5200 game by (SIO2), 2014
'          based on Shooting Gallery by Sebastian Mihai, 2012
'
'
' Press FIRE to shoot!
' 
' Shoot the target not your hunting buddies.
' This example game demonstrates sprites, missiles, joystick input, hardware collision detection,
' sounds, turn based player movement, reading the pause button, copying a title screen to screen memory
' displaying multiple character sets via use of display list interrupts
'
' Quirk: Do _NOT_ use tabs in your source code. Only use spaces, or the compilation will fail
'
' Notes: Title screen is composed of multiple character sets.  A display list interrupt (DLI #1) is used
'        to load the title screen.  Once we leave the title screen, the multiple char sets are not used
'        so, in opts: we monitor vcount to determine when a vertical blank occurs 
'        then switch to DLI #2 and set charbase to point to our normal char set.
'        DLI #1 uses a loop counter to be able to switch character sets every mode line
'        I was advised to put the loop counter in a vertical blank interrupt. 
'        For some reason having the VBI enabled caused strange results when reading the controller keys.
'        My work around was to store the original VBI vector then restore when done with VBI and before reading more keys.
'   
'        Use of semicolon ; character.  ; is another way of saying REM
'        If you comment an assembly language line or assembler complains about your comment use ; instead of '
'
'variable definitions (variable names must be 8 or fewer chars)
DEFINE RANDTEN,$FE    'VARIABLE HOLDING A RANDOM NUMBER (0-9) TENS DIGIT CREATED BY THE RAND: SUBROUTINE
DEFINE RANDONE,$FD    'VARIABLE HOLDING A RANDOM NUMBER (0-9) ONES CREATED BY THE RAND: SUBROUTINE
DEFINE TURN,$FC       'TURN COUNTER FOR TURN BASED MOVEMENT
DEFINE D1C,$FB        'COUNTS TIMES WE ACCESSED DISPLAY LIST INTERRUPT #1 
DEFINE IML,$FA        'MISSILE 0 IN FLIGHT FLAG 0=NO 1=YES
DEFINE LEVEL,$F9      'SKILL LEVEL (NARROWS MARGINS)
DEFINE MARGR,$F8      'RIGHT SCREEN MARGIN
DEFINE MARGL,$F7      'LEFT SCREEN MARGIN

'sound counters - count down after the volume is turned on for channel 
'once counter reaches 0, the volume is turned off, so the sound is only heard briefly
DEFINE SNDCN3,$F6  'Joy sound = free life sound timer
DEFINE SNDCN2,$F5  'Fire gun sound timer
DEFINE SNDCN1,$F4  'Hit Friend sound timer
DEFINE SNDCN0,$F3  'Hit Target sound timer

DEFINE LIVES,$F2  'Variable to hold number of player lives remaining

DEFINE SCF,$F1    'SCx INDIVIDUAL DIGITS OF SCORE
DEFINE SCE,$F0
DEFINE SCD,$EF   
DEFINE SCC,$EE
DEFINE SCB,$ED
DEFINE SCA,$EC  

DEFINE Z,$EB      'Z is used in some for next loops
DEFINE PAUSE,$EA  'Pause flag  0=NOT PAUSE  1=PAUSE
'                  Pause is not useful at this stage of development because there is 
'                  no time limit and nothing spurring the player to act.

DEFINE PX,$E9     'PX = Player 0 x coordinate

DEFINE BX,$E8     'Missile 0 coordinates (bullet coordinates)
DEFINE BY,$E7

DEFINE TX1,$E6    'Target 1,2,3 x coordinates
DEFINE TX2,$E5
DEFINE TX3,$E4

DEFINE TD1,$E3    'targets direction (0=left, 1=right)
DEFINE TD2,$E2
DEFINE TD3,$E1
DEFINE VLOW,$E0
DEFINE VHIG,$DF
'Above are zero page addresses.  I think we can use from $D6 TO $FF on 5200
'Some of these addresses would be overwritten on 8bit computers during floating point ops from Basic

'Enable these if you want to use the standard ATARI title screen
'TITLE "HUNTING WITH DICK"
'AUTHOR "    BY SIO2 2014    "
TITLE OFF

SCREEN 4'SUBSEQUENT SCREEN COMMANDS DON'T WORK SO DO ONLY ONCE
'5200bas documentation says use only SCREEN 2 or SCREEN 4
'Mihai used screen 7 which works also but was easier for me to do char set in screen 4
'It might be possible to switch screen modes though if poll and do it during virtical blank
'like the way I had to change chbase (below keep reading comments)

'CUSTOM TITLE SCREEN IN BLACK AND WHITE MADE WITH G2F THEN CONVERTED USING BIN2INC 
MEMCOPY $8400,$1000,960 'COPY THE ATARI TITLE SCREEN DATA SCREEN RAM

              ;DLI CAN BE TURNED OFF BY SETTING NMIEN TO $40 WHICH ALLOWS VBI TO CONTINUE RUNNING
A=#$C0        ;ENABLE INTERRUPTS $C0 IS NORMAL VALUE USED AND ENABLES BOTH VBI AND DLI 
POKE $D40E,A  ;SET BIT 7 OF NMIEN (NON MASKABLE INTERRUPT ENABLE)

'Method of seeting VBI vector on 8 bit computers - is supposed to prevent possible problem 
'where one byte of the vector gets set but not the other before the VBI is called.
'Appears to not be useful for 5200 though. 
'Y=#$00       ;IN 8BIT COMPUTERS SETVBV SET VERTICAL BLANK VECTOR 
'X=#$7C       ;LOAD Y WITH LOW BYTE, X WITH HI BYTE A WITH 7 FOR DEFFERRED OR 6 FOR IMMEDIATE
'A=#$07       ;THEN JSR TO SETVBV  (DOESN'T SEEM TO WORK ON 5200 PROBABLY DOESN'T EXIST IN 5200'S MUCH SMALLER OS)
'JSR $E462    ;SET VERTICAL BLANK VECTOR
'
'STORE THE ORIGINAL VBI VECTOR
A=PEEK($0204)  ;READ LOW BYTE OF ORIGINAL VBI VECTOR AND STORE VALUE IN VLOW
VLOW=A
A=PEEK($0205) ;READ HI BYTE OF ORIGINAL VBI VECTOR AND STORE VALUE IN VHIG
VHIG=A

'SET THE VBI VECTOR TO POINT TO OUR VBI ROUTINE
A=#$00        ;LOW BYTE OF ADDRESS $7C00
POKE $0204,A  ;SET LOW BYTE OF USER VIRTICAL BLANK INTERRUPT VECTOR(VVBLKD DEFERRED VBI)
A=#$7C        ;HI BYTE OF ADDRESS $7C00
POKE $0205,A  ;SET HI BYTE OF USER VIRTICAL BLANK INTERRUPT VECTOR(VVBLKD DEFERRED VBI)

'SET THE DLI VECTOR TO POINT TO OUR DLI ROUTINE
A=#$00        ;LOW BYTE OF ADDRESS $7E00
POKE $0206,A  ;SET LOW BYTE OF DISPLAY LIST INTERRUPT VECTOR
A=#$7E        ;HI BYTE OF ADDRESS $7E00
POKE $0207,A  ;SET HI BYTE OF DISPLAY LIST INTERRUPT VECTOR
DLIST $7400   ;REDIRECT SYSTEM TO MY DISPLAY LIST

'Print on Title Screen "Press Start" then wait for start key press
LOCATE 4,1
PRINT "{0}{0}{0}{0}{0}{0}{0}{0}{0}{0}{0}{0}{0}{16}{18}{5}{19}{19}{0}{19}{20}{1}{18}{20}"
DO UNTIL X=#$0C  ;wait for start key press
ATTRACT 
INPUT X 
LOOP

BEGIN:
GOSUB OPTS       'Options screen 
GOSUB INITIALIZE 'Set initial variable values, etc.
GOSUB CLEARHITS  'CLEAR THE COLLISION REGISTERS (IN CASE SET FROM LAST GAME)

mainloop:        'Where all the magic happens
DO
'EVERYTURN ROUTINES
GOSUB PAWS        'PAUSE / UNPAUSE
GOSUB NOSOUND     'STOP SOUNDS AT PROPER TIMES
GOSUB COLLISIONS  'CHECK FOR COLLISIONS   
GOSUB CLEARHITS   'CLEAR THE COLLISION REGISTERS 

'INDIVIDUAL TURN ROUTINES
X=TURN
SELECT CASE X
CASE 0 'PLAYER 0 ROUTINES
GOSUB MOVEMIS0    'MOVE MISSILE 0
GOSUB READSTICK   'READ FIRST JOYSTICK  
GOSUB drawplayer0 'MOVE PLAYER0
GOSUB FIRETEST    'CHECK TO SEE IF PLAYER0 PRESSED FIRE
CASE 1            'PLAYER 1 ROUTINES
GOSUB MOVE1       'MOVE PLAYER1 (TARGET) 
CASE 2            'PLAYER 2 ROUTINES
GOSUB MOVE2       'MOVE PLAYER2 (BLOCKER)
CASE 3            'PLAYER 3 ROUTINES
GOSUB MOVE3       'MOVE PLAYER3 (BLOCKER)
END SELECT

'INCRIMENT THE TURN (NEXT PLAYER)
X=TURN
X=X+1
IF X>3 THEN
TURN=0
ELSE
TURN=X
END IF
LOOP

CLEARHITS:
    A=1
    POKE $C01E,A   'CLEARS ALL PM HIT REGISTERS
    'Real hardware requires clear before can detect hits
    'Jum52 emulator doesn't catch this and will detect hits without this statement.
RETURN    

MOVEMIS0:    
    'move MISSILE 0 up
X=IML
SELECT CASE X
    CASE 0  'MISSILE NOT IN FLIGHT SO DON'T MOVE IT    
            'CAN PUT DELAY LOOP HERE TO BALANCE TIMING
            GOSUB DELAY
    CASE 1  'MISSILE IN FLIGHT SO MOVE IT
    A = BY: A = A - 5: BY = A: PUT (BX,BY),$B5,4,8
    'CHECK TO SEE IF MISSILE 0 HIT TOP OF SCREEN
    A=BY 
    IF A<6 THEN
    GOSUB HITTOP
    END IF
END SELECT
RETURN   

MOVE1:
    'ATTEMPT TO MOVE TARGET 1 LEFT OR RIGHT AND TEST IF TARGET 1 HIT MARGIN
    A = TD1
    SELECT CASE A
    CASE 0  'LEFT
    A = TX1: A = A - 1: TX1 = A:IF A < MARGL THEN: TD1 = 1: END IF
    CASE 1  'RIGHT
    A = TX1: A = A + 1: TX1 = A:IF A > MARGR THEN: TD1 = 0: END IF 
    END SELECT
    PUT (TX1,18),$B2,1,8
RETURN
    
MOVE2:            
    'ATTEMPT TO MOVE TARGET 2 LEFT OR RIGHT AND TEST IF TARGET 2 HIT MARGIN      
    A = TD2
    SELECT CASE A
    CASE 0  'LEFT
    A = TX2: A = A - 1: TX2 = A:IF A < MARGL THEN: TD2 = 1: END IF 
    CASE 1  'RIGHT
    A = TX2: A = A + 1: TX2 = A:IF A > MARGR THEN: TD2 = 0: END IF
    END SELECT
    PUT (TX2,30),$B3,2,8
RETURN

MOVE3:    
    'ATTEMPT TO MOVE TARGET 3 LEFT OR RIGHT AND TEST IF TARGET 3 HIT MARGIN         
    A = TD3
    SELECT CASE A
    CASE 0  'LEFT
    A = TX3: A = A - 1: TX3 = A:IF A < MARGL THEN: TD3 = 1: END IF
    CASE 1  'RIGHT
    A = TX3: A = A + 1: TX3 = A:IF A > MARGR THEN: TD3 = 0: END IF
    END SELECT
    PUT (TX3,42),$B4,3,8
RETURN


HITTOP:  'If missile 0 hit top of screen
IML=0    'IML is the flag for Is Missile Launched? 0=no
BX=215   'Mihai referred to the missile as a bullet hence BX and BY variable
BY=0
PUT (BX,BY),$B5,4,8  'PUT STATEMENTS DRAW P/M GRAPHICS ON SCREEN THIS ONE DRAWS MISSILE 0 
RETURN

FIRETEST:
    A = JOYTRIG2
    X = JOYTRIG($00)
    'was either trigger pressed? (shoot)
    IF A =#$00 or X=#$00 THEN:
    ATTRACT
    Y=IML
    SELECT CASE Y
    CASE 0  'No missile in the air, let us launch one
    GOSUB SOUND2
    IML=1:A = PX:A = A + 3:BX = A:BY = 180
    CASE 1  'Missile already in the air = nothing to do
    END SELECT
    END IF
RETURN

hittarget1:  'Hit main target
IML=0
BX=215
BY=0
PUT (BX,BY),$B5,4,8    ;Missile 0 hit target 1 so put missile 0 off screen     
GOSUB SOUND0
GOSUB KEEPSCORE
TX1 = 200
RETURN

hittarget2:   'NOT SUPPOSED TO HIT THIS TARGET
IML=0
BX=215
BY=0
PUT (BX,BY),$B5,4,8    ;Missile 0 hit target 2 so put missile 0 off screen
    GOSUB SOUND1
    TX2 = 200
    gosub TRACKLIVES  'LOST LIFE
RETURN

hittarget3:
IML=0
BX=215
BY=0
PUT (BX,BY),$B5,4,8    ;Missile 0 hit target 3 so put missile 0 off screen
    GOSUB SOUND1
    TX3 = 200
    gosub TRACKLIVES
RETURN

COLLISIONS:    
    A = PEEK($C008)  'RETURNS 1 IF HITTING NOTHING
    SELECT CASE A
    CASE 2
         GOSUB hittarget1  'PINK TARGET
    CASE 4       
         GOSUB hittarget2  'BLUE TARGET
    CASE 8        
         GOSUB hittarget3  'GREEN TARGET
    END SELECT
RETURN

READSTICK:    
    A = JOYX(0)         'READ JOYSTICK
    IF A > MARGR THEN   'CHECK TO SEE IF HIT RIGHT BOUNDRY
    A = MARGR           'IF SO, LIMIT MOTION
    END IF	
    IF A < MARGL THEN   'IF HIT LEFT BOUNDRY 
    A = MARGL           'LIMIT MOTION
    END IF
    PX = A              'SET PLAYER 0 X COORDINATE
RETURN

drawplayer0:
    PUT (PX,190),$B1,0,8       
RETURN

PUTPMOFF:   'put all P/M graphics off screen margin out of way while we go to options screen, etc.
    PUT (200,190),$B1,0,8  'PLAYER 0    PX
    PUT (200,18),$B2,1,8   'PLAYER 1    TX1
    PUT (200,30),$B3,2,8   'PLAYER 2    TX2
    PUT (200,42),$B4,3,8   'PLAYER 3    TX3
    PUT (200,100),$B5,4,8  'MISSILE 0   BX
RETURN


'SOUND ROUTINES 
'Atari has 4 sound channels that can play independantly or at the same time
'SOUND channel,frequency  The valid frequency range is 1-255
'VOLUME channel, noise, volume  - total volume not to excede 32
NOSOUND:
    'decrement sound counter 0
    A = SNDCN0
    A = A - 1
    SNDCN0 = A
    'once sound counter reaches 0, we stop the sound, so it doesn't last indefinitely
    IF A = 0 THEN: VOLUME 0, 2, 0: END IF

    'decrement sound counter 1
    A = SNDCN1
    A = A - 1
    SNDCN1 = A
    'once sound counter reaches 0, we stop the sound, so it doesn't last indefinitely
    IF A = 0 THEN: VOLUME 1, 2, 0: END IF

    'decrement sound counter 2
    A = SNDCN2
    A = A - 1
    SNDCN2 = A
    'once sound counter reaches 0, we stop the sound, so it doesn't last indefinitely
    IF A = 0 THEN: VOLUME 2, 2, 0: END IF

    'decrement sound counter 3
    A = SNDCN3
    A = A - 1
    SNDCN3 = A
    'once sound counter reaches 0, we stop the sound, so it doesn't last indefinitely
    IF A = 0 THEN: VOLUME 3, 2, 0: END IF
RETURN


SOUND0:  'Hit Target
    'play sound and move target off screen, so it reappears again soon
    SNDCN0 = 30
    VOLUME 0, 14, 8
    SOUND 0, 255
RETURN

SOUND1: 'Hit Friend
    'play sound and move target off screen, so it reappears again soon
    SNDCN1 = 80
    VOLUME 1, 12, 8
    SOUND 1, 7
RETURN

SOUND2: 'Fire gun
    'play sound and move target off screen, so it reappears again soon
    SNDCN2 = 20
    VOLUME 2, 6, 8
    SOUND 2, 65
    '0 13  OR 2,65 OR 6,65 IS GOOD, 8,65 OK  
RETURN

SOUND3: 'Joy sound = free life
    'play sound and move target off screen, so it reappears again soon
    SNDCN3 = 80
    VOLUME 3, 10, 8
    SOUND 3, 65
RETURN

PAWS:
'TEST FOR PAUSE KEY OR START KEY PRESS DURING GAME
X = PAUSE
SELECT CASE X
CASE 0
A=INKEY
IF A=#$0D THEN   ;PAUSE KEY PRESS
PAUSE=1
END IF
IF A=#$0C THEN   ;START KEY PRESS
GOSUB PUTPMOFF 
GOTO BEGIN
END IF
CASE 1
A=#$FF
DO WHILE A<>#$0D ;LOCKED IN THIS LOOP UNTIL PAUSE KEY PRESSED AGAIN TO RELEASE
A=INKEY
LOOP
PAUSE=0
END SELECT
RETURN


KEEPSCORE:
    SCA=SCA+1
    A=SCA
   IF A > 36 THEN   'Zero ones digit and incriment 10s digit
    SCA=27          'In custom char set, character 27 is 0 (zero)
    SCB=SCB+1
    LIVES=LIVES+1   'Free life but remember to test later if exceeded max of 5
    'I didn't test lives here but probably could be done her if you want.  
    GOSUB SOUND3
        A=SCB
    IF A > 36 THEN  'Zero tens digit and incriment 100s digit
    SCB=27
    SCC=SCC+1
    'Looks like you can only incriment or decriment (add / subtract one)
    'One solution is iterations (incriment or decriment multiple times) with for/next
     X=LEVEL
    IF X < 6 THEN    
    Z=1:FOR Z TO 10:MARGL=MARGL+1:MARGR=MARGR-1:NEXT Z  'LEVEL UP BY NARROWING MARGINS
    END IF
    END IF
    A=SCC
    IF A > 36 THEN  'Zero hundreds digit and incriment 1000s digit
    SCC=27
    SCD=SCD+1
    END IF
    A=SCD
    IF A > 36 THEN  'Zero thousands digit and incriment 10000s digit
    SCD=27
    SCE=SCE+1
    END IF
    A=SCE
    IF A > 36 THEN  'Zero ten thousands digit and incriment 100000s digit
    SCE=27
    SCF=SCF+1
    END IF
    A=SCF
    IF A > 36 THEN  'ROLL OVER at 1,000,000 and zero all six digits
    SCA=27:SCB=27:SCC=27:SCD=27:SCE=27:SCF=27  '27 is the offset for the zero character in the custom character set
    END IF
   END IF
     LOCATE 0,15
    PRINT CHR$(SCF):RIGHT  'print the scoreboard digits to the screen
    PRINT CHR$(SCE):RIGHT
    PRINT CHR$(SCD):RIGHT
    PRINT CHR$(SCC):RIGHT
    PRINT CHR$(SCB):RIGHT
    PRINT CHR$(SCA):RIGHT
A=LIVES
IF A>5 THEN 
LIVES=5
END IF
GOSUB SHOWLIVES   
RETURN

TRACKLIVES:       'Subtracts lives 
LIVES=LIVES-1
GOSUB SHOWLIVES:
RETURN

SHOWLIVES:  'The print "{0}" structure prints characters from the custom char set.  {0} char zero is a space on screen
LOCATE 0,30:PRINT "{0}{0}{0}{0}{0}{0}{0}" '"      "
LOCATE 0,30
Z=1
FOR Z TO LIVES
PRINT "{38}"
RIGHT
NEXT Z
A=LIVES
IF A < 1 THEN
GOTO GAMEOVER:
END IF
RETURN

GAMEOVER:
VOLUME 0, 2, 0   'Stop Sound 0
VOLUME 1, 2, 0   'Stop Sound 1
VOLUME 2, 2, 0   'Stop Sound 2
VOLUME 3, 2, 0   'Stop Sound 3

LOCATE 10,14:PRINT "{7}{1}{13}{5}{0}{15}{22}{5}{18}" '"GAME OVER"
X=#$FF
INPUT X
LOCATE 10,14:PRINT "{0}{0}{0}{0}{0}{0}{0}{0}{0}" '"BLANKS"
GOSUB PUTPMOFF
GOTO BEGIN

INITIALIZE:
A=#$00
POKE $C008,A     ;SET PLAYER 0 TO NORMAL SIZE 0=NORMAL 1=DOUBLE 3=QUADRUPLE 
POKE $C009,A     ;SET PLAYER 1 TO NORMAL SIZE
POKE $C00A,A     ;SET PLAYER 2 TO NORMAL SIZE
POKE $C00B,A     ;SET PLAYER 3 TO NORMAL SIZE
BX=215  'MISSILE 0 HIDES OFF RIGHT MARGIN OF SCREEN WHEN NOT IN FLIGHT 
BY=0    'HAVING MISSILE OFF RIGHT SIDE RATHER THAN ON TOP OF PLAYER 0 NEARLY ELIMINATES FLICKER IN SCREEN 4

PAUSE=0 'PAUSE FLAG OFF
TURN=0  'TURN BASED PLAY

'CHANGING LEVLES RESULTS IN NARROWER MARGINS (MARGL = LEFT MARGIN)  (MARGR = RIGHT MARGIN)
A=LEVEL
SELECT CASE A
CASE 0
MARGR=149
MARGL=1
CASE 1
MARGR=139
MARGL=11
CASE 2
MARGR=129
MARGL=21
CASE 3
MARGR=119
MARGL=31
CASE 4
MARGR=109
MARGL=41
CASE 5
MARGR=99
MARGL=51
END SELECT

LIVES = 3  'Number of lives to start with
SCA = 27   
SCB = 27   'Set all SCx (score board variables) to 0
SCC = 27   'BASE CHR$(27) = 0 (ZERO) WHEN PRINTED ON SCREEN 
SCD = 27   'WITH OUR CUSTOM CHAR SET
SCE = 27
SCF = 27

SPRITES ON  'TURN ON PLAYER/MISSILE GRAPHICS (ALSO KNOW AS SPRITES)

' Colour information is stored in one byte
'[7 6 5 4  3 2 1 0]
'[  hue   | brightness ]
'
' hue     : 0=grey, 1=gold, 2=gold-orange, 3=red-orange, 4=orange, 5=magenta, 6=purple-blue
'           7=blue, 8=blue, 9=cyan, 10=blue-green, 11=blue-green, 12=green, 13=yellow-green
'           14=yellow, 15=yellow-red
'
' brightness: 0, 2, 4, 6, 8, 10, 12, 14 (only even values permitted)

'Sprite/missile palettes (5 is for first, 6 for second, etc.)
PALETTE 5,#$1E:PALETTE 6,#$6A:PALETTE 7,#$96:PALETTE 8,#$B6
'Background palette
PALETTE 0,#$C0   

'the accumulator, A,  is often used for simple arithmetic operations, and to read in values
'from various ports

GOSUB SHOWLIVES    ' Start showing information
LOCATE 0,15
PRINT "{27}{27}{27}{27}{27}{27}" '"000000"

TX1 = 10    ;TARGET X COORDINATES
TX2 = 50
TX3 = 90

TD1 = 1     ;TARGET DIRECTION OF MOVEMENT
TD2 = 1
TD3 = 1

A=0          'Zero the register variables as best practice
X=0          
Y=0
RETURN
	
OPTS:
Y=1         ;THIS GUARANTEES DO LOOP EXECUTES AT LEAST ONCE
DO UNTIL Y=0
DLIST $7600 ;HAD TROUBLE WITH SYSTEM SEEING THIS.  JUST KEEP SETTING UNTIL A VERTICAL BLANK TAKES PLACE
CHARSET $AC00 ;CHANGE TO THE OPTIONS/GAME CHAR SET

'RESTORE ORIGINAL VBI VECTOR
A=VLOW        ;LOW BYTE OF ADDRESS OF ORIGINAL DEFERRED VBI STORED IN VLOW
POKE $0204,A  ;SET LOW BYTE OF USER VIRTICAL BLANK INTERRUPT VECTOR
A=VHIG        ;HI BYTE OF ADDRESS OF ORIGINAL DEFERRED VBI STORED IN VHIG
POKE $0205,A  ;SET HI BYTE OF USER VIRTICAL BLANK INTERRUPT VECTOR

Y=VCOUNT    ;MONITOR VCOUNT TO DETERMINE WHEN A VERTICAL BLANK HAS TAKEN PLACE
LOOP

MEMCOPY $8800,$1000,960 'COPY THE DATA TO DRAW THE OPTIONS SCREEN TO SCREEN RAM

'SET COLORS FOR OPTIONS SCREEN
PALETTE 0,#$0    'COLBK  BLACK  0
PALETTE 1,#$E    'COLPF0 WHITE  $E
PALETTE 2,#$76   'COLPF1 BLUE $76
PALETTE 3,#$36   'COLPF2 RED  $36
PALETTE 4,#$0    'COLINV

LOCATE 18,1
PRINT "{0}{0}{0}{0}{0}{0}{8}{21}{14}{20}{9}{14}{7}{0}{23}{9}{20}{8}{0}{4}{9}{3}{11}{0}{29}{27}{28}{31}" '"      HUNTING WITH DICK 2014"
DOWN
DOWN
PRINT "{12}{5}{22}{5}{12}{0}{27}" '"LEVEL 0"
X=0
LEVEL=0  'INITIAL LEVEL
DOWN
DOWN
PRINT "{19}{5}{12}{5}{3}{20}{0}{12}{5}{22}{5}{12}{0}{27}{37}{32}{0}{20}{8}{5}{14}{0}{16}{18}{5}{19}{19}{0}{19}{20}{1}{18}{20}" '"SELECT LEVEL 0-5 THEN PRESS START"
DO
INPUT X     'STOP AND WAIT FOR KEY
SELECT CASE X
CASE 0
LOCATE 20,1:PRINT "{12}{5}{22}{5}{12}{0}{27}" '"LEVEL 0"
LEVEL=0
CASE 1
LOCATE 20,1:PRINT "{12}{5}{22}{5}{12}{0}{28}" '"LEVEL 1"
LEVEL=1
CASE 2
LOCATE 20,1:PRINT "{12}{5}{22}{5}{12}{0}{29}" '"LEVEL 2"
LEVEL=2
CASE 3
LOCATE 20,1:PRINT "{12}{5}{22}{5}{12}{0}{30}" '"LEVEL 3"
LEVEL=3
CASE 4
LOCATE 20,1:PRINT "{12}{5}{22}{5}{12}{0}{31}" '"LEVEL 4"
LEVEL=4
CASE 5
LOCATE 20,1:PRINT "{12}{5}{22}{5}{12}{0}{32}" '"LEVEL 5"
LEVEL=5
CASE #$0C
LOCATE 18,1
PRINT "{0}{0}{0}{0}{0}{0}{0}{0}{0}{0}{0}{0}{0}{0}{0}{0}{0}{0}{0}{0}{0}{0}{0}{0}{0}{0}{0}{0}{0}" '"BLANKS"
DOWN
DOWN
PRINT "{0}{0}{0}{0}{0}{0}{0}{0}{0}{0}{0}"
DOWN
DOWN
PRINT "{0}{0}{0}{0}{0}{0}{0}{0}{0}{0}{0}{0}{0}{0}{0}{0}{0}{0}{0}{0}{0}{0}{0}{0}{0}{0}{0}{0}{0}{0}{0}{0}{0}{0}{0}" '"BLANKS"
CLS
RETURN
END SELECT
LOOP
RETURN

'Random number generator (not in use yet but handy in a game)
RAND:  
DO 'return a random number from 0 to 9.
  A=RND
  A=A AND 15
LOOP UNTIL A<10
RANDONE=A
DO 'return a random number from 0 to 9.
  A=RND
  A=A AND 15
LOOP UNTIL A<10
RANDTEN=A
RETURN

DELAY:       ;5200bas IS AN INTERESTING MIX OF ASSEMBLY LANGUAGE AND BASIC AND IT ACCEPTS INLINE ASSEMBLY INSTRUCTIONS
  ldx #25   ;Assebmly language for load x register with number 255 same as x=255
DO WHILE X>1
  nop        ;NOP is assembly language for No Opertion (null) do nothing
  nop 
  nop        ;Use semicolon to add remarks (REM) assembly lines instead of apostrophy 
  nop 
  nop 
  dex        
LOOP         
rts          ;Assembly language for Return from Sub Routine same as 5200bas Return

.ORG $9800  ;TABLE OF CHARACTER SET BASES AND PALETTES SO CAN CHANGE CHAR SET AND
'            COLORS EVERY MODE LINE IF WE WISH. FORMAT: CHBASE,COLOR,COLOR,COLOR,COLOR,COLOR
.BYTE $9C,$00,$0E,$0E,$0E,$0E,$9C,$00,$1C,$0E,$0E,$0E,$AC,$00,$0E,$0E,$0E,$0E
.BYTE $AC,$00,$BB,$0E,$0E,$0E,$AC,$00,$0E,$0E,$0E,$0E,$9C,$00,$1C,$0E,$0E,$0E
.BYTE $9C,$00,$0E,$0E,$0E,$0E,$9C,$8F,$0E,$0E,$0E,$0E,$9C,$8D,$0E,$0E,$0E,$0E
.BYTE $9C,$8B,$0E,$0E,$0E,$0E,$9C,$89,$0E,$0E,$0E,$0E,$9C,$87,$0E,$0E,$0E,$0E
.BYTE $9C,$85,$0E,$0E,$0E,$0E,$9C,$83,$0E,$0E,$0E,$0E,$9C,$81,$0E,$0E,$0E,$0E
.BYTE $9C,$90,$0E,$0E,$0E,$0E,$9C,$00,$0E,$0E,$0E,$0E,$9C,$00,$0E,$0E,$0E,$0E
.BYTE $9C,$00,$0E,$0E,$0E,$0E,$9C,$00,$0E,$0E,$0E,$0E,$9C,$00,$0E,$0E,$0E,$0E
.BYTE $9C,$00,$0E,$0E,$0E,$0E,$9C,$00,$0E,$0E,$0E,$0E,$9C,$00,$0E,$0E,$0E,$0E

;---------------------------------------------------------------------
; DISPLAY LIST 1   (EVERY MODE LINE CALLS DLI PROCESS)
;---------------------------------------------------------------------
.ORG $7400
.BYTE $70,$70,$70                        ;LEAVES 3 BLANK LINES AT TOP TO ALLOW FOR TV OVERSCAN
.BYTE $C4                                ;$C4 = 196 DECIMAL OR 128 TO CALL DLI PLUS 64 LMS PLUS 4 MODE BYTE
.WORD $1000                              ;ADDRESS OF SCREEN MEMORY
.BYTE $84,$84,$84,$84,$84,$84,$84,$84    ;84 = DECIMAL 128 PLUS 4  $84 CALLS THE DLI ROUTINE
.BYTE $84,$84,$84,$84,$84,$84,$84,$84
.BYTE $84,$84,$84,$84,$84,$84,$84
.BYTE $41                                ;JUMP BACK TO START OF DISPLAY LIST ADDRESS FOLLOWS     
.WORD $7400                              ;ADDRESS OF START OF DISPLAY LIST

;---------------------------------------------------------------------
; DISPLAY LIST 2    DOES NOT CALL ANY INTERRUPTS 
;---------------------------------------------------------------------
.ORG $7600
.BYTE $70,$70,$70                        ;LEAVES 3 BLANK LINES AT TOP TO ALLOW FOR TV OVERSCAN
.BYTE $44                                ;$40 LMS + $04 ANTIC SCREEN MODE 4 (NO INTERRUPT)
.WORD $1000                              ;ADDRESS OF SCREEN MEMORY
.BYTE $04,$04,$04,$04,$04,$04,$04,$04    ;MODE LINES IN SCREEN 4 (NO INTERRUPT CALLS)
.BYTE $04,$04,$04,$04,$04,$04,$04,$04
.BYTE $04,$04,$04,$04,$04,$04,$04
.BYTE $41                                ;JMP to address that follows
.WORD $7600                              ;ADDRESS OF THE START OF THIS DISPLAY LIST

';---------------------------------------------------------------------
'; VERTICAL BLANK INTERRUPT ROUTINE
';---------------------------------------------------------------------
.ORG $7C00
D1C=0                  ;ZERO DLI LOOP COUNTER (WAS ADVISED IS BETTER DO TO HERE THAN RELY ON DLI TO DO IT)
POP ALL                ;CLEAR THE STACK
RTI                    ;RETURN FROM INTERRUPT


';---------------------------------------------------------------------
'; DISPLAY LIST INTERRUPT ROUTINE
';---------------------------------------------------------------------
.ORG $7E00
PUSH ALL               ;STORE THE REGISTERS ON STACK
LDX D1C                ;COUNTER X IS INDEX VALUE FOR TABLE SINCE CHANGE CHAR SETS EACH LINE
LDA $9800,X            ;LOAD THE CORRESPONDING CHAR BASE INDEXED BY X
'MUST STORE CHBASE AHEAD OF SYNC OR ELSE SWAPPING FONTS LEAVES SCANLINE IN OLD FONT 
STA CHBASE             ;STORE THE CHAR BASE ADDRESS IN THE VECTOR
STA $D40A              ;WSYNC WAIT FOR HORIZONTAL SYNC
INX                    ;USING X TO INDEX COLORS FOR CHARACTER SET PALETTE
LDA $9800,X            ;LOAD COLOR FROM TABLE
POKE $C01A,A           ;PUT FIRST COLOR INTO HARDWARE REGISTER
INX                    ;INCRIMENT X (X=X+1) 
LDA $9800,X            ;LOAD COLOR FROM TABLE  
POKE $C016,A           ;PUT SECOND COLOR INTO HARDWARE REGISTER
INX                    ;INCRIMENT X AGAIN (PROCESS REPEATS FOR ALL 5 COLORS)
LDA $9800,X
POKE $C017,A
INX
LDA $9800,X
POKE $C018,A
INX
LDA $9800,X
POKE $C019,A
INX                    ;INCRIMENT TO INDEX NEXT CHAR BASE FOR START OF NEXT LOOP
D1C=X                  ;SET THE LOOP COUNTER AT A MULTIPLE OF 6 (CHBASE, PLUS 5 COLORS)
POP ALL                ;RESTORE THE REGISTERS
RTI                    ;RETURN FROM INTERRUPT


'TILE INCLUDES GO HERE
'TILES ARE THE GRAPHICAL INFORMATION FOR THE SHAPE OF THE
'PLAYERS AND/OR MISSILES
'NOTICE IN THE PUT COMMAND $B1 CORRESPONDS TO $B100 WHERE 
'PLAYER 1 DATA IS STORED.
'THE 5200bas assembler doesn't seem to allow binary includes 
'Make sure includes are in the .Byte $00,$01 format
'Use semicolon to add remarks on include lines.  Do not use apostrophy there.
.ORG    $9C00
#INCLUDE Atari1.inc      ;character set to draw Atari Logo 

.ORG    $8400
#INCLUDE Atari1.scr      ;screen data for Atari Logo

.ORG    $AC00             
#INCLUDE BB_char4.inc    ;character set to draw words, score board, hearts

.ORG    $8800
#INCLUDE BB_opts.scr     ;screen data for the options screen

.ORG    $B100             
#INCLUDE BB_player0.inc  ;player 0 data to draw sprite

.ORG    $B200             
#INCLUDE BB_player1.inc  ;player 1 data to draw sprite

.ORG    $B300
#INCLUDE BB_player2.inc  ;player 2 data to draw sprite

.ORG    $B400
#INCLUDE BB_player3.inc  ;player 3 data to draw sprite

.ORG    $B500             
#INCLUDE BB_missile0.inc ;Missile 0 data to draw sprite

.ORG    $B600             
#INCLUDE BB_missile1.inc ;Missile 1 data to draw sprite 

.ORG    $B700             
#INCLUDE BB_missile2.inc ;Missile 2 data to draw sprite  

.ORG    $B800             
#INCLUDE BB_missile3.inc ;Missile 3 data to draw sprite 

'these are commented out and left here just as example calls for putting sprites on screen
'num - 0-3 are sprites, 4-7 are missiles
'label is the segment where tiles begin ($B100 in this case, so segment is $B1)
'PUT ( <x-coord>,<y-coord> ), <label>, <num>, [<height> OR -<height> for upside down]
'PUT (42,42),$B1,0,5         'player  0
'PUT (42,52),$B2,1,5         'player  1
'PUT (42,62),$B3,2,12        'player  2
'PUT (42,72),$B4,3,5         'player  3
'PUT (22,22),$B5,4,5         'missile 0
'PUT (18,18),$B6,5,5         'missile 1
'PUT (14,14),$B7,6,5         'missile 2
'PUT (10,10),$B8,7,5         'missile 3

'In the 5200 the GTIA chip occupies addresses $C000-$C01F
'THIS CHART IS HANDY WHEN USING PLAYER MISSILE GRAPHICS
'SOME ADDRESSES IN THIS CHART HAVE DIFFERENT FUNCTIONS DEPENDING ON IF THEY ARE BEING
'READ FROM OR WRITTEN TO.  EX: READ $C008 FOR COLLISION DETECTION MISSILE 0 TO PLAYER
'WRITE TO (POKE) $C008 TO CHANGE PLAYER 0 ZIZE.
'---------------------------------------------------------------------------------
'             |  PLAYER #0      PLAYER #1      PLAYER #2      PLAYER #3      
'---------------------------------------------------------------------------------
'PLAYER COLOR |  COLPM0 $C012   COLPM1 $C013   COLPM2 $C014   COLPM3 $C015
'             |  PCOLR0 $0008   PCOLR1 $0009   PCOLR2 $000A   PCOLR3 $000B
'---------------------------------------------------------------------------------
'PLAYER SIZE  |  SIZEP0 $C008   SIZEP1 $C009   SIZEP2 $C00A   SIZEP3 $C00B
'             |         (WRITE)        (WRITE)        (WRITE)        (WRITE)
'  0=NORMAL   |
'  1=DOUBLE   |
'  3=QUADRUPLE|
'---------------------------------------------------------------------------------
'PLAYER HORIZ.|  HPOS0  $C000   HPOS1  $C001   HPOS2  $C003   HPOS3  $C003
'POSITION     |         (WRITE)        (WRITE)        (WRITE)        (WRITE)
'---------------------------------------------------------------------------------
'Collision    |  P0PL   $C00C   P1PL   $C00D   P2PL   $C00E   P3PL   $C00F
'Playr2Player |         (READ)         (READ)         (READ)         (READ)
'---------------------------------------------------------------------------------
'COLLISION    |  P0PF   $C004   P1PF   $C005   P2PF   $C006   P3PF   $C007
'PLAYR2PLAYFLD|         (READ)         (READ)         (READ)         (READ)
'PF0 = 1      |
'PF1 = 2      |
'PF2 = 4      |
'PF3 = 8      |
'-------------------------------------------------------------------------------------
'             |  MISSILE #0      MISSILE #1      MISSILE #2      MISSILE #3      
'-------------------------------------------------------------------------------------
'MISSILE COLOR|  COLPM0 $C012    COLPM1 $C013    COLPM2 $C014    COLPM3 $C015
'(SAME AS PLYR|  PCOLR0 $0008    PCOLR1 $0009    PCOLR2 $000A    PCOLR3 $000B
'---------------------------------------------------------------------------------
'MISSILE HORZ.|  HPOSM0  $C004   HPOSM1 $C005    HPOSM2 $C006    HPOSM3 $C007
'POSITION     |          (WRITE)        (WRITE)         (WRITE)         (WRITE)
'---------------------------------------------------------------------------------
'COLLISION    |  M0PL   $C008    M1PL   $C009    M2PL   $C00A    M3PL   $C00B
'MISSIL2PLAYER|         (READ)          (READ)          (READ)          (READ)
'PLAYER 0 = 1 |
'PLAYER 1 = 2 |
'PLAYER 2 = 4 |
'PLAYER 3 = 8 |
'---------------------------------------------------------------------------------
'COLLISION    |  M0PF   $C000    M1PF   $C001    M2PF   $C002    M3PF   $C003
'MISSIL2PFIELD|         (READ)          (READ)          (READ)          (READ)
'PF0 = 1      |
'PF1 = 2      |
'PF2 = 4      |
'PF3 = 8      |
'---------------------------------------------------------------------------------
'PFIELD COLOR |  MUST USE HARDWARE REG FROM WITHIN DLI 
'HARDWARE REG |  COLPF0 $C016   COLPF1 $C017   COLPF2 $C018   COLPF3 $C019   COLBK  $C01A
'SHADOW       |  COLOR0 $000C   COLOR1 $000D   COLOR2 $000E   COLOR3 $000F   COLOR4 $0010
'---------------------------------------------------------------------------------
  

'NTSC TIA PALLETTE SUMMARY
'HUE RANGES 0-F
'LUMINENCE RANGES 0-F IN STEPS OF TWO
'COLORS EXPRESSED IN FORMAT $(DIGIT)(DIGIT) ORDER OF $(HUE)(LUM)
'$00 = BLACK
'$02 TO $0D = SHADES OF GREY
'$0E OR $0F = WHITE
'HUE 1 = YELLOW, HUE 2 = LIGHT ORANGE, HUE 3 = MED ORANGE, HUE 4 =DARK ORANGE
'HUE 5 = PINK, HUE 6 = PURPLE, HUE 7,8,9,A = BLUES, HUE ABCDE = GREENS, HUE F = BROWNS

'MEMORY
'5200 System RAM ranges from $0000 - $3FFF
'5200 HAS 16K AND 32K CARTS.  I AM BUILDING FOR 32K CARTS AND THOSE WORK WITH ATARIMAX 128-IN-1 USB FLASH MULTI CART
'32k CART MEMORY RANGES FROM $4000-$BFFF   
'16K CART MEMORY RANGES FROM $8000-$BFFF

'MEMORY PLAN
'$0000 - $3FFF    SYSTEM RAM  $0000 through $0018 and $0200 - $021B and Page $01 are reserved by system
'$4000 - $73FF    PROGRAM 
'$7400 - $7BFF    DISPLAY LISTS
'$7C00 - $83FF    INTERRUPT ROUTINES (VBI AND DLI)
'$8400 - $97FF    SCREEN DATA
'$9800 - $9BFF    TABLE OF CHARACTER SET BASES AND COLOR PALETTES
'$9C00 - $AFFF    CHARACTER SETS DATA
'$B000 - $B0FF    LEAVE EMPTY - TRIED SPRITE DATA HERE, WOULD NOT DRAW PLAYER ?
'$B100 - $B8FF    SPRITE DATA
'$B900 - $BFFF    LEAVE EMPTY. Y2K FIX AND POSSIBLY OTHER STUFF GOING ON HERE
