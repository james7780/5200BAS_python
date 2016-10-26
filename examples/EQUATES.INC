;	'400/800/5200 Equate Tables'
;	By J. Leiterman
;	8/29/82

;	Last Modification
;	8/29/82
;	Modified for TASM by Alan Davis 8/31/00
;*******************************************************
;*** (C) 1982 Atari Inc.  All Rights Reserved.  No   ***
;*** part of this program of publication may be      ***
;*** Reproduced, Transmitted, Transcribed, stored    ***
;*** in a Retrieval System or Translated into any    ***
;*** Language or Computer Language, in any form or   ***
;*** by any means, Electronic, Mechanical, Magnetic, ***
;*** Optical, Chemical, Manual or otherwise, without ***
;*** the prior written permission of Atari Inc. 1196 ***
;*** Borregas Ave.  Sunnyvale  CA 94086              ***
;*******************************************************
;
;	[ATARI]
;	0=400/800	(Candy/Colleen)
;	1=CX5200	(Pam)           

#IF	ATARI=0

; 400/800 Candy/Colleen Equates

GTIA	.EQU	$D000	;Gtia Base
POKEY	.EQU	$D200	;Pokey Base
PIA	.EQU	$D300	;Port Base
ANTIC	.EQU	$D400	;Antic Base
CHRORG	.EQU	$E000	;Character Generator Base

CASINI	.EQU	$0002	;Vec. for cassette program init.
DOSVEC	.EQU	$000A	;Software start vector
DOSINI	.EQU	$000C	;Software Init address
POKMSK	.EQU	$0010	;Mask for Pokey IRQ enable
BRKKEY	.EQU	$0011	;BREAK flag
RTCLOK	.EQU	$0012	;60 hz. clock
CRITIC	.EQU	$0042	;Critical section
ATRACT	.EQU	$004D	;Attract Mode


;Interrupt Vectors
                 
VDSLST	.EQU	$0200	;Display List Interrupt NMI 
VPRCED	.EQU	$0202	;Proceed Line IRQ 
VINTER	.EQU	$0204	;Interrupt Line IRQ
VBREAK	.EQU	$0206	;Software Break IRQ
VKEYBD	.EQU	$0208	;Keyboard IRQ
VSERIN	.EQU	$020A	;Serial Input Rdy. IRQ
VSEROR	.EQU	$020C	;Serial Output Rdy. IRQ
VSEROC	.EQU	$020E	;Serial Output Complete IRQ
VTIMR1	.EQU	$0210	;Pokey Timer 1 IRQ
VTIMR2	.EQU	$0212	;Pokey Timer 2 IRQ
VTIMR4	.EQU	$0214	;Pokey Timer 4 IRQ
VIMIRQ	.EQU	$0216	;Vector to IRQ Handler
VVBLKI	.EQU	$0222	;Immediate Vertical Blank NMI
VVBLKD	.EQU	$0224	;Deferred Vertical Blank
CDTMA1	.EQU	$0226	;#1 System Timer JSR Address
CDTMA2	.EQU	$0228	;#2	"
SDMCTL	.EQU	$022F	;DMACTL Shadow
SDLSTL	.EQU	$0230	;DLISTL	"
SDLSTH	.EQU	$0231	;DLISTH 	"
SSKCTL	.EQU	$0232	;Serial Port control
LPENH	.EQU	$0234	;PENH Shadow
LPENV	.EQU	$0235	;PENV Shadow
BRKKY	.EQU	$0236	;BREAK Key Vector (Only Rev. B)
GPRIOR	.EQU	$026F	;PRIOR Shadow

PADDL0	.EQU	$0270	;POT0 Shadow
PADDL1	.EQU	$0271	;POT1  "
PADDL2	.EQU	$0272	;POT2  "
PADDL3	.EQU	$0273	;POT3  "
PADDL4	.EQU	$0274	;POT4  "
PADDL5	.EQU	$0275	;POT5  "
PADDL6	.EQU	$0276	;POT6  "
PADDL7	.EQU	$0277	;POT7  "

STICK0	.EQU	$0278	;0 Joystick
STICK1	.EQU	$0279	;1  "
STICK2	.EQU	$027A	;2  "
STICK3	.EQU	$027B	;3  "

STRIG0	.EQU	$0284	;TRIG0 Shadow
STRIG1	.EQU	$0285	;TRIG1  "
STRIG2	.EQU	$0286	;TRIG2  "
STRIG3	.EQU	$0287	;TRIG3  "
                                
SHFLK	.EQU	$02BE	;Shift-Lock flag
           
PCOLR0	.EQU	$02C0	;COLPM0 Shadow
PCOLR1	.EQU	$02C1	;COLPM1  "
PCOLR2	.EQU	$02C2	;COLPM2  "
PCOLR3	.EQU	$02C3	;COLPM3  "

COLOR0	.EQU	$02C4	;COLPF0 Shadow
COLOR1	.EQU	$02C5	;COLPF1  "
COLOR2	.EQU	$02C6	;COLPF2  "
COLOR3	.EQU	$02C7	;COLPF3  "
COLOR4	.EQU	$02C8	;COLBK   "
                                   
MEMLO	.EQU	$02E7	;Start of user memory
CHBAS	.EQU	$02F4	;CHBASE Shadow
CH	.EQU	$02FC	;KBCODE  "

                         
;	Device Control Block

DDEVIC	.EQU	$0300	;Device Bus I.D.
DUNIT	.EQU	$0301	;Device unit number
DCOMND	.EQU	$0302	;Device command
DSTATS	.EQU	$0303	;Device status
DBUFLO	.EQU	$0304	;Lo Handler Buffer Address 
DBUFHI	.EQU	$0305	;Hi
DTIMLO	.EQU	$0306	;Device Timeout
DBYTLO	.EQU	$0308	;Lo Buffer Length
DBYTHI	.EQU	$0309	;Hi
DAUX1	.EQU	$030A	;Aux 1
DAUX2	.EQU	$030B	;Aux 2


;	I/O Control Block

IOCB	.EQU	$0340
ICHID	.EQU	IOCB	;Handler I.D.
ICDNO	.EQU	IOCB+1	;Device number
ICCOM	.EQU	IOCB+2	;Command Byte
ICSTA	.EQU	IOCB+3	;Status
ICBAL	.EQU	IOCB+4	;Lo Buffer Address
ICBAH	.EQU	IOCB+5	;Hi 
ICPTL	.EQU	IOCB+6	;Lo Put Byte Vector
ICPTH	.EQU	IOCB+7	;Hi
ICBLL	.EQU	IOCB+8	;Lo Buffer Length
ICBLH	.EQU	IOCB+9	;Hi
ICAX1	.EQU	IOCB+$A	;Aux. #1
ICAX2	.EQU	IOCB+$B	;Aux. #2
ICSPR	.EQU	IOCB+$C	;Spare bytes for handler use
                  

;	Pia I/O chip

PORTA	.EQU	PIA	;Port A Data
PORTB	.EQU	PIA+1	;Port B Data
PACTL	.EQU	PIA+2	;Port A Control
PBCTL	.EQU	PIA+3	;Port B Control


;	Rom Vectors                           

DISKIV	.EQU	$E450	;Disk handler Init.
DSKINV	.EQU	$E453	;Disk handler
CIOV	.EQU	$E456	;Central I/O 
SIOV	.EQU	$E459	;Serial I/O 
SETVBV	.EQU	$E45C	;Set system timers
SYSVBV	.EQU	$E45F	;System Vertical Blank
XITVBV	.EQU	$E462	;Exit Vertical Blank
SIOINV	.EQU	$E465	;Serial I/O Init.
SENDEV	.EQU	$E468	;Serial bus send enable
INTINV	.EQU	$E46B	;Interrupt handler
CIOINV	.EQU	$E46E	;Central I/O Init.
BLKBDV	.EQU	$E471	;Blackboard mode (Memopad)
WARMSV	.EQU	$E474	;Warm start entry (System Reset)
COLDSV	.EQU	$E477	;Cold start entry (Power-up)
RBLOKV	.EQU	$E47A	;Cassette read block
CSOPIV	.EQU	$E47D	;Cassette Open for Input
    

; Cartridge Parameters

CARTCS	.EQU	$BFFA	;Cartridge Start Address
CART	.EQU	$BFFC	;0=Cart Exists
CARTFG	.EQU	$BFFD	;Option Byte
			;D7  0=Not a Diagnostic Cart
			;    1=Is a Diagnostic cart and control is 
			;      given to cart before any OS is init.
			;D2  0=Init but Do not Start Cart
			;    1=Init and Start Cart
			;D0  0=Do not boot disk
			;    1=Boot Disk
CARTAD	.EQU	$BFFE	;Cartridge Init. Address

 
#ELSE
                    

; CX5200 Pam Equates

GTIA	.EQU	$C000	;Gtia Base
POKEY	.EQU	$E800	;Pokey Base
ANTIC	.EQU	$D400	;Antic Base
CHRORG	.EQU	$F800	;Character Generator Base

POKMSK	.EQU	$0000	;Mask for Pokey IRQ enable
RTCLOK	.EQU	$0001	;60 hz. clock
JUMP	.EQU	$0001	
CRITIC	.EQU	$0003	;Critical section
ATRACT	.EQU	$0004	;Attract Mode 

SDLSTL	.EQU	$0005	;DLISTL Shadow
SDLSTH	.EQU	$0006	;DLISTH  "
SDMCTL	.EQU	$0007	;DMACTL  "

PCOLR0	.EQU	$0008	;COLPM0 Shadow
PCOLR1	.EQU	$0009	;COLPM1  "
PCOLR2	.EQU	$000A	;COLPM2  "
PCOLR3	.EQU	$000B	;COLPM3  "

COLOR0	.EQU	$000C	;COLPF0 Shadow
COLOR1	.EQU	$000D	;COLPF1  "
COLOR2	.EQU	$000E	;COLPF2  "
COLOR3	.EQU	$000F	;COLPF3  "
COLOR4	.EQU	$0010	;COLBK   "

PADDL0	.EQU	$0011	;POT0 Shadow
PADDL1	.EQU	$0012	;POT1  "
PADDL2	.EQU	$0013	;POT2  "
PADDL3	.EQU	$0014	;POT3  "
PADDL4	.EQU	$0015	;POT4  "
PADDL5	.EQU	$0016	;POT5  "
PADDL6	.EQU	$0017	;POT6  "
PADDL7	.EQU	$0018	;POT7  "


;Interrupt Vectors
                 
VIMIRQ	.EQU	$0200	;Immediate IRQ    
			;Preset $FC03 (SYSIRQ)
VVBLKI  .EQU    $0202   ;Vblank immediate
			;Preset $FCB8 (SYSVBL)
VVBLKD	.EQU	$0204	;Vblank deferred
			;Preset $FCB2 (XITVBL)
VDSLST	.EQU	$0206	;Display List 
      			;Preset $FEA1 (OSDLI)
VKYBDI	.EQU	$0208	;Keyboard immediate
			;Preset $FD02 (SYSKBD)
VKYBDF	.EQU	$020A	;Deferred Keyboard
			;Preset $FCB2 (XITVBL)
VTRIGR	.EQU	$020C	;Soft Trigger   
VBRKOP	.EQU	$020E	;BRK Opcode
VSERIN	.EQU	$0210	;Serial in Ready
VSEROR	.EQU	$0212	;Serial Out Ready
VSEROC	.EQU	$0214	;Serial Output complete
VTIMR1	.EQU	$0216	;Pokey Timer 1
VTIMR2	.EQU	$0218	;Pokey Timer 2
VTIMR4	.EQU	$021A	;Pokey Timer 4


;Cartridge Parameters

CARTNM	.EQU	$BFE8	;Cartridge Name Area
COPYD	.EQU	$BFFC	;Copyright Decade in Cart
COPYR	.EQU	$BFFD	;Copyright Year in Cart
			; $FF=Diagnostic Cart
GOCART	.EQU	$BFFE	;Cartridge Start Vector


#ENDIF


;**************************
;***   GTIA Registers   ***
;**************************

CTIA	.EQU	GTIA 
HPOSP0	.EQU	GTIA+$00 ;0 Player Horz. position
HPOSP1	.EQU	GTIA+$01 ;1	"
HPOSP2	.EQU	GTIA+$02 ;2	"
HPOSP3	.EQU	GTIA+$03 ;3	"

HPOSM0	.EQU	GTIA+$04 ;0 Missile Horz. position
HPOSM1	.EQU	GTIA+$05 ;1	"
HPOSM2	.EQU	GTIA+$06 ;2	"
HPOSM3	.EQU	GTIA+$07 ;3	"

SIZEP0	.EQU	GTIA+$08 ;0 Player Size
SIZEP1	.EQU	GTIA+$09 ;1	"
SIZEP2	.EQU	GTIA+$0A ;2	"
SIZEP3	.EQU	GTIA+$0B ;3	"

SIZEM	.EQU	GTIA+$0C ;Missiles size

M0PF	.EQU	GTIA+$00 ;0 Missile to playfield collision
M1PF	.EQU	GTIA+$01 ;1	"
M2PF	.EQU	GTIA+$02 ;2	"
M3PF	.EQU	GTIA+$03 ;3	"

P0PF	.EQU	GTIA+$04 ;0 Player to playfield collision
P1PF	.EQU	GTIA+$05 ;1	"
P2PF	.EQU	GTIA+$06 ;2	"
P3PF	.EQU	GTIA+$07 ;3	"

M0PL	.EQU	GTIA+$08 ;0 Missile to player collision
M1PL	.EQU	GTIA+$09 ;1	"
M2PL	.EQU	GTIA+$0A ;2	"
M3PL	.EQU	GTIA+$0B ;3	"

P0PL	.EQU	GTIA+$0C ;0 Player to Player collision
P1PL	.EQU	GTIA+$0D ;1	"
P2PL	.EQU	GTIA+$0E ;2	"
P3PL	.EQU	GTIA+$0F ;3	"

GRAFP0	.EQU	GTIA+$0D ;0 Player Graphics
GRAFP1	.EQU	GTIA+$0E ;1	"
GRAFP2	.EQU	GTIA+$0F ;2	"
GRAFP3	.EQU	GTIA+$10 ;3	"

GRAFM	.EQU	GTIA+$11 ;Missile Graphics

TRIG0	.EQU	GTIA+$10 ;0 Joystick Trigger
TRIG1	.EQU	GTIA+$11 ;1	" 
TRIG2	.EQU	GTIA+$12 ;2	"
TRIG3	.EQU	GTIA+$13 ;3	"

COLPM0	.EQU	GTIA+$12 ;0 Player/Missile Color
COLPM1	.EQU	GTIA+$13 ;1	"
COLPM2	.EQU	GTIA+$14 ;2	"
COLPM3	.EQU	GTIA+$15 ;3	"

COLPF0	.EQU	GTIA+$16 ;0 Playfield Color
COLPF1	.EQU	GTIA+$17 ;1	"
COLPF2	.EQU	GTIA+$18 ;2	"
COLPF3	.EQU	GTIA+$19 ;3	"
COLBK	.EQU	GTIA+$1A ;Background/Border Color

PRIOR	.EQU	GTIA+$1B ;Display Depth Priorities
VDELAY	.EQU	GTIA+$1C ;Vertical Delay
GRACTL	.EQU	GTIA+$1D ;Graphics Control
HITCLR	.EQU	GTIA+$1E ;Clear All P/M collision Reg.
CONSOL	.EQU	GTIA+$1F ;Console Key register


;**************************
;***   POKEY Registers  ***
;**************************

AUDF1   .EQU    POKEY+$00 ;1 Audio Frequency
AUDF2	.EQU	POKEY+$02 ;2	"
AUDF3	.EQU	POKEY+$04 ;3	"
AUDF4	.EQU	POKEY+$06 ;4	"

AUDC1	.EQU	POKEY+$01 ;1 Audio Control
AUDC2	.EQU	POKEY+$03 ;2	"
AUDC3	.EQU	POKEY+$05 ;3	"
AUDC4	.EQU	POKEY+$07 ;4	"

AUDCTL	.EQU	POKEY+$08 ;Audio Master Control

POT0	.EQU	POKEY+$00 ;0 Analog in port
POT1	.EQU	POKEY+$01 ;1	"
POT2	.EQU	POKEY+$02 ;2	"
POT3	.EQU	POKEY+$03 ;3	"
POT4	.EQU	POKEY+$04 ;4	"
POT5	.EQU	POKEY+$05 ;5	"
POT6	.EQU	POKEY+$06 ;6	"
POT7	.EQU	POKEY+$07 ;7	"

ALLPOT	.EQU	POKEY+$08 ;Read 8 Line Pot Port status

STIMER	.EQU	POKEY+$09 ;Start Timer
KBCODE	.EQU	POKEY+$09 ;Keyboard Code
SKRES	.EQU	POKEY+$0A ;Reset Serial Port Status
RANDOM	.EQU	POKEY+$0A ;Random Number Generator
POTGO   .EQU    POKEY+$0B ;Start Pot Scan sequence
SEROUT	.EQU	POKEY+$0D ;Serial Port Output
SERIN	.EQU	POKEY+$0D ;Serial Port Input
IRQEN	.EQU	POKEY+$0E ;IRQ enable mask
IRQST	.EQU	POKEY+$0E ;IRQ status
SKCTL	.EQU	POKEY+$0F ;serial port control
SKSTAT	.EQU	POKEY+$0F ;Serial port status


;**************************
;***   ANTIC Registers  ***
;**************************

DMACTL	.EQU	ANTIC+$00 ;DMA Control 
CHACTL	.EQU	ANTIC+$01 ;Character Set Control
DLISTL	.EQU	ANTIC+$02 ;Display List Low
DLISTH	.EQU	ANTIC+$03 ;             High
HSCROL	.EQU	ANTIC+$04 ;Horz. Fine Scroll 
VSCROL	.EQU	ANTIC+$05 ;Vert. Fine Scroll
PMBASE	.EQU	ANTIC+$07 ;Player/Missile Base
CHBASE	.EQU	ANTIC+$09 ;Character Set Base
WSYNC	.EQU	ANTIC+$0A ;Wait for Horz. Sync
VCOUNT	.EQU	ANTIC+$0B ;Scan Line Counter
PENH	.EQU	ANTIC+$0C ;Horz. Light Pen
PENV	.EQU	ANTIC+$0D ;Vert. Light Pen
NMIEN	.EQU	ANTIC+$0E ;Non-maskable Interrupt enable
NMIRES	.EQU	ANTIC+$0F ;NMI reset
NMIST	.EQU	ANTIC+$0F ;MNI status
                                   

;****************************
;*** Display List Equates ***
;****************************

;   Graphic Modes vary from 2-F

BLANK1	.EQU	$00	;1 Blank Scan Line
BLANK2	.EQU	$10	;2 Blank Scan Lines
BLANK3	.EQU	$20	;3 	"
BLANK4	.EQU	$30	;4	"
BLANK5	.EQU	$40	;5 	"
BLANK6	.EQU	$50	;6 	"
BLANK7	.EQU	$60	;7 	"
BLANK8	.EQU	$70	;8	"

RLDMSC	.EQU	$40	;This + Graphic Mode=pointer
				; to memory for scan line
JMPWT	.EQU	$41	;Point to top to Display List
	
HSC	.EQU	$10	;Fine Horz. Scroll Enable
VSC	.EQU	$20	;Fine Vert. Scroll Enable
INT	.EQU	$80	;Display List Interrupt Set
