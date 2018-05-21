
	processor 6502
	include "vcs.h"
	include "macro.h"

  SEG.U vars
  ORG $80

SpriteHeight = 7

SpritePosition ds 1
PlayerX ds 2
PS_temp ds 1
Sprite ds 2
Sprite1 ds 2
SpriteEnd ds 1
Sprite1End ds 1
XIncrement ds 1
X1Increment ds 1
YIncrement ds 1
Y1Increment ds 1
YVelocity ds 1
Y1Velocity ds 1
YCounter ds 1
Y1Counter ds 1
XVelocity ds 1
X1Velocity ds 1
XCounter ds 1
X1Counter ds 1
XTemp ds 1
YTemp ds 1
YMaxLimit ds 1
YMinLimit ds 1
Y1MaxLimit ds 1
Y1MinLimit ds 1
XMaxLimit ds 1
XMinLimit ds 1
X1MaxLimit ds 1
X1MinLimit ds 1
InAir ds 1
VelTemp ds 1
PlayedCrashSound ds 1
PlayedP1CrashSound ds 1
Volume ds 1
CCDetached ds 1
CCParachuteDelay ds 1
ChuteDeployed ds 1
P0Crashed ds 1
P1Crashed ds 1
P0Success ds 1
P1Success ds 1

	SEG code
	ORG $F000

fineAdjustBegin

  DC.B %01110000; Left 7 
  DC.B %01100000; Left 6
  DC.B %01010000; Left 5
  DC.B %01000000; Left 4
  DC.B %00110000; Left 3
  DC.B %00100000; Left 2
  DC.B %00010000; Left 1
  DC.B %00000000; No movement.
  DC.B %11110000; Right 1
  DC.B %11100000; Right 2
  DC.B %11010000; Right 3
  DC.B %11000000; Right 4
  DC.B %10110000; Right 5
  DC.B %10100000; Right 6
  DC.B %10010000; Right 7

fineAdjustTable EQU fineAdjustBegin - %11110001; NOTE: %11110001 = -15

Reset
	; Clear RAM and all TIA registers
	ldx #0
	lda #0
Clear	sta 0,x
	inx
	bne Clear
;---------------------
	ldx #$0F
	stx COLUBK ; set the background color

StartOfIntroFrame
	lda #2
	sta VBLANK
	sta VSYNC

	sta WSYNC
	sta WSYNC
	sta WSYNC

  lda #43
  sta TIM64T

	lda #0
	sta VSYNC

	;-------------------
	ldx #0
  lda #$82 ; blue color
	sta COLUPF ; set the playfield color


  bit INPT4
  bpl StartOfGame
  bit INPT5
  bpl StartOfGame

WaitForIntroVblankEnd
  lda INTIM
  bne WaitForIntroVblankEnd

	;-------------------

	ldx #0
  lda #0
  sta WSYNC
  sta VBLANK

  lda #83
  sta TIM64T

WaitForCenter
  lda INTIM
  bne WaitForCenter

  sta WSYNC

DrawBLUE
	lda LEFTPF0,x
	sta PF0
	lda LEFTPF1,x
	sta PF1
	lda LEFTPF2,x
	sta PF2

	SLEEP 6

	lda RIGHTPF0,x
	sta PF0
	lda RIGHTPF1,x
	sta PF1
	lda RIGHTPF2,x
	sta PF2

	nop

	sta WSYNC

	inx
  cpx #15
  beq BlackPF
	cpx #60
	bne DrawBLUE

	lda #0
	sta PF0
	sta PF1
	sta PF2

BlankLines
	sta WSYNC
	inx
	cpx #118
	bne BlankLines

	;------------------
EndOfIntroFrame
	; 30 scanlines of overscan
	ldx #0
  lda #2
OverscanIntro sta WSYNC
	sta VBLANK

	inx
	cpx #30
	bne OverscanIntro

  ldx #0
  sta WSYNC
	jmp StartOfIntroFrame

BlackPF
  lda #$00 ; black color
  sta COLUPF ; set the playfield color
  jmp DrawBLUE

StartOfGame
;---------------------
	lda #$20 ; brown color
	sta COLUPF ; set the playfield color

	ldx #0
	stx COLUBK ; set the background color

  lda #$0F
  sta COLUP0
  sta COLUP1

  ;; Set the ball size to be 4 pixels wide
  lda #$20
  sta CTRLPF

  lda #80

  sta PlayerX+1

  lda #<Sprite0Data
  sta Sprite
  lda #>Sprite0Data
  sta Sprite+1

  lda #<Sprite1Data
  sta Sprite1
  lda #>Sprite1Data
  sta Sprite1+1

  lda #191
  sta YIncrement

  lda #186
  sta Y1Increment

  lda #140
  sta XIncrement

  lda #140
  sta X1Increment

  lda #192
  sta YMaxLimit
  sta Y1MaxLimit

  lda #6
  sta YMinLimit
  sta Y1MinLimit

  lda #161
  sta XMaxLimit
  sta X1MaxLimit

  lda #2
  sta XMinLimit
  sta X1MinLimit

  lda #$20
  sta NUSIZ0

;---------------------

StartOfFrame
	lda #2
	sta VBLANK
	sta VSYNC

	sta WSYNC
	sta WSYNC
	sta WSYNC

  lda #43
  sta TIM64T

	lda #0
	sta VSYNC
;-------------------
CheckForReset
  lda SWCHB
  and #1
  bne CheckParachuteDelay
  jmp Reset
CheckParachuteDelay
  bit CCDetached
  bvc CheckCCCommand
  lda CCParachuteDelay
  cmp #50
  bcs CheckCCCommand
  inc CCParachuteDelay
  jmp CheckForDetachCommand
CheckCCCommand
  bit CCDetached
  bvc CheckForDetachCommand

  bit INPT5
  bpl DeployChute
  jmp DecrementY

DeployChute
  lda #<Sprite1DataPara
  sta Sprite1
  lda #>Sprite1DataPara
  sta Sprite1+1
  lda #$FF
  sta ChuteDeployed
  jmp DecrementY

CheckForDetachCommand
  bit INPT5
  bpl DetachCC
  jmp DecrementY
DetachCC
  lda #$FF
  sta CCDetached
  lda #<Sprite0DataNoCC
  sta Sprite
  lda #>Sprite0DataNoCC
  sta Sprite+1
  dec Y1Velocity                ; gotta keep it separated

DecrementY                      ; UP JOYSTICK
  ;; If up button pressed
  lda #$10
  bit SWCHA
  bne MuteRocketSound                ; else not pressed, goto X
  lda #1
  sta InAir                     ; we are now in the air

  ;; Do not increase velocity if we are at max
  lda #-126
  cmp YVelocity
  beq IncrementX                ; Do not increase velocity
  lda #-127                     ; check both since we can change by two
  cmp YVelocity
  beq IncrementX                ; Do not increase velocity

  lda #150
  cmp YIncrement
  bcc loudest

  lda #120
  cmp YIncrement
  bcc louder
  
  lda #90
  cmp YIncrement
  bcc loud

  lda #70
  cmp YIncrement
  bcc normal

  lda #50
  cmp YIncrement
  bcc quiet

  lda #30
  cmp YIncrement
  bcc quieter

  lda #10
  cmp YIncrement
  bcc quietest
  jmp quietest

loudest
  lda #15
  jmp setVolume
louder
  lda #12
  jmp setVolume
loud
  lda #10
  jmp setVolume
normal
	  lda #8
	  jmp setVolume
quiet
	  lda #5
	  jmp setVolume
quieter
	  lda #3
	  jmp setVolume
quietest
  lda #1
  jmp setVolume

setVolume  sta AUDV0

  lda #9
  sta AUDF0

  lda #14
  sta AUDC0

  dec YVelocity
  dec YVelocity

  jmp IncrementX

MuteRocketSound
  lda #0
  sta AUDV0

IncrementX                      ; RIGHT JOYSTICK
  lda #$80
  bit SWCHA
  bne DecrementX  

  ;; Dont go past the wall
  ldx XIncrement
  cpx XMaxLimit
  beq DecrementX

  ;; However, do not increase velocity if we are at max
  lda #127
  cmp XVelocity
  beq DecrementX  ; Do not increase velocity
  inc XVelocity

DecrementX                      ; LEFT JOYSTICK
  lda #$40
  bit SWCHA
  bne MovePlayer

  ;; Dont go past the wall
  ldx XIncrement
  cpx XMinLimit
  beq MovePlayer

  ;; Do not increase velocity if we are at max
  lda #-126
  cmp XVelocity
  beq MovePlayer  ; Do not increase velocity

  dec XVelocity

MovePlayer
  ;; If we are not in air then do not move
  lda InAir
  beq SetupSpriteEnd

  ;; Add gravity
  ;; However, do not increase velocity if we are at max
  lda #127
  cmp YVelocity
  beq ChangeYCounter                ; Do not increase velocity
  ;; check if we are at our largest YIncrement (lowest point on the screen)
  ldx YIncrement
  cpx YMaxLimit
  beq ChangeYCounter
  inc YVelocity

ChangeYCounter
  lda YVelocity
  clc
  adc YCounter                  ; attempt to overflow
  sta YCounter
  bvc SetupSpriteEnd            ; did not overflow so do not move

  ;; decrement y (move player up) if YVelocity is negative
  lda YVelocity
  bpl MoveDown

  ;; check if we are at out smallest YIncrement (highest point on the screen)
  ldx YIncrement
  cpx YMinLimit
  beq SetupSpriteEnd            ; don't move if we are at our limit

  ;; overflowed and had negative velocity so move up (decrement x)
  dec YIncrement
  jmp SetupSpriteEnd

MoveDown
  ;; check if we are at out largest YIncrement (lowest point on the screen)
  ldx YIncrement
  cpx YMaxLimit
  beq SetupSpriteEnd            ; don't move if we are at our limit

  ;; overflowed and had positive velocity so move down (increment x)
  inc YIncrement

SetupSpriteEnd
  ldx #0
  lda #0
  clc
  adc YIncrement
  sta SpriteEnd

  bit CCDetached
  bvs DetachedY                 ; CC is detached

  lda YIncrement                ; CC is not detached
  sta Y1Increment
  lda #-5
  clc
  adc Y1Increment
  sta Y1Increment
  lda YVelocity
  sta Y1Velocity
  jmp StoreY1

DetachedY
  lda #0
  clc
  adc Y1Increment
  
  ;; Do not increase velocity past chute max if chute deployed
  bit ChuteDeployed
  bvc AddGravityOnCC            ; Add gravity as normal if chute not deployed
  lda #60
  cmp Y1Velocity
  beq AddCCVelocityToPos        ; We are at max velocity
  bvc AddGravityOnCC
  lda #60
  sta Y1Velocity

AddGravityOnCC
  ;; Add gravity
  ;; However, do not increase velocity if we are at max
  lda #127
  cmp Y1Velocity
  beq AddCCVelocityToPos      ; Do not increase velocity
  ;; check if we are at our largest YIncrement (lowest point on the screen)
  ldx Y1Increment
  cpx Y1MaxLimit
  beq AddCCVelocityToPos
  inc Y1Velocity

AddCCVelocityToPos
  lda Y1Velocity
  clc
  adc Y1Counter                  ; attempt to overflow
  sta Y1Counter
  bvc StoreY1             ; did not overflow so do not move

  ;; decrement y (move player up) if YVelocity is negative
  lda Y1Velocity
  bpl MoveY1Down

  ;; check if we are at out smallest YIncrement (highest point on the screen)
  ldx Y1Increment
  cpx Y1MinLimit
  beq StoreY1            ; don't move if we are at our limit

  ;; overflowed and had negative velocity so move up (decrement x)
  dec Y1Increment
  jmp StoreY1

MoveY1Down
  ;; check if we are at out largest Y1Increment (lowest point on the screen)
  ldx Y1Increment
  cpx Y1MaxLimit
  beq StoreY1            ; don't move if we are at our limit

  ;; overflowed and had positive velocity so move down (increment x)
  inc Y1Increment

StoreY1
  lda #0
  clc
  adc Y1Increment
  sta Sprite1End

CheckXVelocity
  lda XVelocity
  clc
  adc XCounter                  ; attempt to overflow
  sta XCounter
  bvc MoveX            ; did not overflow so do not move

  ;; decrement x (move player left) if XVelocity is negative
  lda XVelocity
  bpl MoveRight

  ;; check if we are at out smallest XIncrement 
  ldx XIncrement
  cpx XMinLimit
  beq MoveX            ; don't move if we are at our limit

  ;; overflowed and had negative velocity so move left (decrement x)
  dec XIncrement
  jmp MoveX

MoveRight
  ;; check if we are at our largest XIncrement (furthest right point on the screen)
  ldx XIncrement
  cpx XMaxLimit
  beq MoveX            ; don't move if we are at our limit

  ;; overflowed and had positive velocity so move right (increment x)
  inc XIncrement

MoveX
  lda #0
  clc
  adc XIncrement
  sta PlayerX

  ;; Move PM4 in X
  stx XTemp
  ldx #0
  jsr PosObject

  bit CCDetached
  bvs MoveCCinX
  lda XIncrement
  sta X1Increment
  lda XVelocity
  sta X1Velocity
  jmp StoreX1

MoveCCinX
  lda X1Velocity
  clc
  adc X1Counter                  ; attempt to overflow
  sta X1Counter
  bvc StoreX1            ; did not overflow so do not move

  ;; decrement x (move player left) if XVelocity is negative
  lda X1Velocity
  bpl MoveX1Right

  ;; check if we are at out smallest XIncrement 
  ldx X1Increment
  cpx X1MinLimit
  beq StoreX1            ; don't move if we are at our limit

  ;; overflowed and had negative velocity so move left (decrement x)
  dec X1Increment
  jmp StoreX1

MoveX1Right
  ;; check if we are at our largest XIncrement (furthest right point on the screen)
  ldx X1Increment
  cpx X1MaxLimit
  beq StoreX1            ; don't move if we are at our limit

  ;; overflowed and had positive velocity so move right (increment x)
  inc X1Increment

StoreX1
  lda X1Increment
  ;; Move Crew Capsule in X
  ldx #1
  jsr PosObject
  ldx XTemp

MoveMissle
  lda #40

  ;; Start of missle X positioning 
  sta WSYNC                ; 00     Sync to start of scanline.
  sec                      ; 02     Set the carry flag so no borrow will be applied during the division.
.divideby15missle sbc #15                  ; 04     Waste the necessary amount of time dividing X-pos by 15!
  bcs .divideby15missle          ; 06/07  11/16/21/26/31/36/41/46/51/56/61/66
  tay
  lda fineAdjustTable,y    ; 13 -> Consume 5 cycles by guaranteeing we cross a page boundary

  sta RESM0                ; 21/ 26/31/36/41/46/51/56/61/66/71 - Set the rough position.

  sta WSYNC
  sta HMOVE

  ;; End of missle positioning

ExtraGameSetup
  ldx #0
	stx COLUBK ; set the background color

  lda #0
  sta PF0
  sta PF1
  sta PF2

  ;; disable missle 0
  lda #0
  sta ENAM0

CheckCCLanding
  lda P1Crashed
  bne CrashedP1 ; P1 has already crashed
  bit CXP1FB
  bpl CheckPMLanding

CheckP1Landing
  lda #61
  cmp Y1Velocity
  bcs P1SuccessLand                  ; Successful CC landing!
  ;; crash!
CrashedP1
  lda #0

  lda #<Sprite0CrashData
  sta Sprite1
  lda #>Sprite0CrashData
  sta Sprite1+1

  lda #1
  sta P1Crashed

  lda PlayedP1CrashSound
  bne LowerCrashVolume1
  lda #15
  sta Volume

LowerCrashVolume1
  lda Volume
  beq DoNotLowerAnymore1
  dec Volume

DoNotLowerAnymore1
  ;; Don't play the crash sound more than once
  lda #35
  cmp PlayedP1CrashSound
  beq TurnOffCrashSound1
  clc
  lda #1
  adc PlayedP1CrashSound
  sta PlayedP1CrashSound
  ;; crash sound
  lda Volume
  sta AUDV0
  sta AUDV1
  lda #31
  sta AUDF0
  lda #15
  sta AUDC0
  lda #8
  sta AUDC1
  lda #31
  sta AUDF1
  jmp FreezeP1

TurnOffCrashSound1
  lda #0
  sta AUDV0
  jmp FreezeP1

P1SuccessLand
  lda #1
  sta P1Success

FreezeP1
  lda X1Increment
  sta X1MaxLimit
  sta X1MinLimit

  lda Y1Increment
  sta Y1MaxLimit
  sta Y1MinLimit
  lda #0
  sta Y1Velocity
  jmp CheckPMLanding

CheckPMLanding
  lda P0Crashed
  bne CheckCrash
  bit CXM0P
  bvc CheckCrash
  ;; Landed successfully!
  lda #1
  sta P0Success
  jmp FreezeP0

CheckCrash
  bit CXP0FB
  bpl WaitForVblankEnd
  ;; Crash!

  lda #0

  lda #<Sprite0CrashData
  sta Sprite
  lda #>Sprite0CrashData
  sta Sprite+1

  lda #1
  sta P0Crashed

  lda PlayedCrashSound
  bne LowerCrashVolume
  lda #15
  sta Volume

LowerCrashVolume
  lda Volume
  beq DoNotLowerAnymore
  dec Volume

DoNotLowerAnymore
  ;; Don't play the crash sound more than once
  lda #35
  cmp PlayedCrashSound
  beq TurnOffCrashSound
  clc
  lda #1
  adc PlayedCrashSound
  sta PlayedCrashSound
  ;; crash sound
  lda Volume
  sta AUDV0
  sta AUDV1
  lda #31
  sta AUDF0
  lda #15
  sta AUDC0
  lda #8
  sta AUDC1
  lda #31
  sta AUDF1
  jmp FreezeP0

TurnOffCrashSound
  lda #0
  sta AUDV0

  jmp FreezeP0

WaitForVblankEnd
  ;; Check For Success
  lda P0Success
  beq WaitForVblankEndReal
  lda P1Success
  beq WaitForVblankEndReal

  jmp SuccessWaitForVblank

WaitForVblankEndReal
  lda INTIM
  bne WaitForVblankEndReal
ContinueAfterWait
;-------------------
  ldx #192                      ; 2

  lda #0
  sta WSYNC
  sta VBLANK

ScanLoop
  sta WSYNC
  lda #SpriteHeight; 2
  dcp SpriteEnd                 ; 5
  bcc SkipDrawRight                 ; 2 = 9
  ldy SpriteEnd
  lda (Sprite),y
continueRight
  sta GRP0
  jmp DrawRight

SkipDrawRight                 ; 3 from BCC
  lda #0
  sta GRP0
DrawRight                 ; 3 from BCC
  lda #SpriteHeight; 2
  dcp Sprite1End                 ; 5
  bcc SkipDrawing                 ; 2 = 9
  ldy Sprite1End
  lda (Sprite1),y
  sta GRP1
  jmp EndDrawing 
SkipDrawing
  lda #0                        ; 2
  sta GRP1

EndDrawing

  ;; End of y positioning of P0

  cpx #3
  bne ContinueGround

  lda #3
  ;; enable missle 0 
  sta ENAM0

HandlePlayfield
  SLEEP 72

  lda #$FF
  ldy #$FC
  sta PF0
  sty PF1

ContinueGround
  cpx #2
  bne NoBackground

  lda #$FF
  sta PF1
  sta PF2
DrawGround
  ldy #$20

  ;; disable missle 0 
  lda #0
  sta ENAM0 

  SLEEP 10

	sty COLUBK ; set the background color

NoBackground
  dex                           ; 2
	bne ScanLoop 

;------------------
EndOfFrame
; 30 scanlines of overscan
	ldx #0
  lda #2
Overscan sta WSYNC
	sta VBLANK

	inx
	cpx #30
	bne Overscan

  ldx #0
  sta WSYNC
	jmp StartOfFrame

PosObject SUBROUTINE
	  sta WSYNC                ; 00     Sync to start of scanline.
	  sec                      ; 02     Set the carry flag so no borrow will be applied during the division.
.divideby15 sbc #15        ; 04     Waste the necessary amount of time dividing X-pos by 15!
	  bcs .divideby15          ; 06/07  11/16/21/26/31/36/41/46/51/56/61/66
	  tay                      ; 2
	  lda fineAdjustTable,y    ; 13 -> Consume 5 cycles by guaranteeing we cross a page boundary

	  sta HMP0,x                      ; 3
	  sta RESP0,x              ; 21/ 26/31/36/41/46/51/56/61/66/71 - Set the rough position.

	  ;; End of X positioning 
	  rts


FreezeP0
  lda XIncrement
  sta XMaxLimit
  sta XMinLimit

  lda YIncrement
  sta YMaxLimit
  sta YMinLimit
  lda #0
  sta YVelocity
  jmp WaitForVblankEnd


SuccessWaitForVblank
  lda INTIM
  bne SuccessWaitForVblank
ContinueAfterSuccessWait
;-------------------
  ldx #192                      ; 2
  ldy #0
  lda #0
  sta WSYNC
  sta VBLANK

SuccessLoop
  sta WSYNC

  lda #$C6 ; green color
	sta COLUPF ; set the playfield color

DrawSuccess
	lda WINLEFTPF0,y
	sta PF0
	lda WINLEFTPF1,y
	sta PF1
	lda WINLEFTPF2,y
	sta PF2

  lda #SpriteHeight; 2
  dcp SpriteEnd                 ; 5
  lda #SpriteHeight; 2
  dcp Sprite1End                 ; 5

	lda WINRIGHTPF0,y
	sta PF0
	lda WINRIGHTPF1,y
	sta PF1
	lda WINRIGHTPF2,y
	sta PF2

	nop

  dex                           ; 2
  iny
  cpy #26
	bne SuccessLoop

	lda #0
	sta PF0
	sta PF1
	sta PF2

  lda #$20 ; brown color
	sta COLUPF ; set the playfield color

	jmp ScanLoop

LEFTPF0
	.byte $30,$30,$30,$50,$50,$30,$30,$50,$50,$50,$30,$30,$30,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0
LEFTPF1
	.byte $89,$89,$89,$89,$89,$89,$89,$89,$89,$89,$E6,$E6,$E6,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$1,$2,$4,$8,$10,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0
LEFTPF2
	.byte $8E,$8E,$8E,$42,$42,$46,$46,$46,$42,$42,$8E,$8E,$8E,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$80,$80,$80,$80,$D0,$D0,$F0,$F0,$F0,$F8,$78,$BC,$DE,$EE,$F6,$FA,$FC,$FF,$7E,$3C,$3C,$14,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0

RIGHTPF0
	.byte $90,$90,$90,$A0,$A0,$A0,$A0,$A0,$A0,$A0,$90,$90,$90,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$A0,$A0,$E0,$F0,$F0,$F0,$F0,$F0,$F0,$70,$F0,$B0,$F0,$D0,$F0,$E0,$70,$70,$70,$70,$70,$70,$10,$10,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0
RIGHTPF1
	.byte $93,$93,$93,$54,$54,$54,$95,$95,$95,$54,$57,$53,$53,$0,$0,$2,$6,$6,$E,$1E,$3E,$3C,$3C,$34,$7C,$FC,$EC,$FC,$F8,$D8,$F8,$F8,$B0,$F0,$F0,$70,$F0,$E0,$E0,$E0,$C0,$80,$80,$80,$80,$80,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0
RIGHTPF2
	.byte $94,$94,$B4,$B5,$B5,$B4,$B5,$D5,$D5,$D5,$D5,$94,$94,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0

WINLEFTPF0
	.byte $0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0
WINLEFTPF1
	.byte $0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$1,$1,$1,$1,$1,$1,$1,$1
WINLEFTPF2
	.byte $0,$0,$0,$0,$0,$C0,$20,$50,$10,$10,$50,$50,$90,$20,$C0,$0,$0,$0,$E8,$48,$48,$48,$4A,$4F,$4D,$E8

WINRIGHTPF0
	.byte $0,$0,$0,$0,$0,$30,$40,$A0,$80,$80,$A0,$A0,$90,$40,$30,$0,$0,$0,$20,$60,$60,$60,$A0,$A0,$A0,$20
WINRIGHTPF1
	.byte $0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$A8,$A8,$A8,$A8,$80,$80,$A8,$A8
WINRIGHTPF2
	.byte $0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0

Sprite0Data
	.byte #%00000000
	.byte #%00111100
	.byte #%00011000
	.byte #%00011000
	.byte #%00011000
	.byte #%00111100
	.byte #%00011000
	.byte #%00000000

Sprite0DataNoCC
	.byte #%00000000
	.byte #%00111100
	.byte #%00011000
	.byte #%00011000
	.byte #%00011000
	.byte #%00111100
	.byte #%00000000
	.byte #%00000000

Sprite1DataPara
	.byte #%00000000
	.byte #%00011100
	.byte #%00001000
	.byte #%00001000
	.byte #%00001000
	.byte #%01001001
	.byte #%00111110
	.byte #%00000000

Sprite1Data
	.byte #%00000000
	.byte #%00011000
	.byte #%00000000
	.byte #%00000000
  .byte #%00000000
  .byte #%00000000
  .byte #%00000000
	.byte #%00000000

Sprite0CrashData
	.byte #%00000000
	.byte #%01010101
	.byte #%10101010
	.byte #%01010101
	.byte #%10101010
	.byte #%01010101
	.byte #%10101010
	.byte #%00000000

	ORG $FFFA

InterruptVectors
	.word Reset ; NMI
	.word Reset ; RESET
	.word Reset ; IRQ

END
