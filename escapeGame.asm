	processor 6502

	include "vcs.h"
	include "macro.h"

	
	seg.u Variables
	org $80
GraczWys	byte
GraczY		byte
GraczX		byte
WrogWys		byte
WrogY		byte
WrogX		byte

Zmienna		byte
Random		byte
RandomWPrawo	byte
RandomWDol		byte
RandomWLewo		byte
RandomWGore		byte
	
LimitRuchu	byte	; ile razy przesunąć pocisk	
KtoryCykl	byte
Punkty		byte
Temp		byte
PunktyPrzesuniecie	word
OnesDigitOffset word         ; lookup table offset for the score Ones digit
TensDigitOffset word  
ScoreSprite	byte
JakaSzybkosc	byte
FlagaGameOver	byte


DIGITS_HEIGHT = 5   
	
	seg code
	org $F000

Start:
	CLEAN_START
	
	lda #100
	sta GraczY
	
	lda #50
	sta GraczX
	
	
	lda #4
	sta GraczWys
	sta	WrogWys
	
	lda #50
	sta WrogY
	
	lda #1
	sta WrogX
	
	lda #50
	sta LimitRuchu
	
	lda #67
	sta Zmienna
	
	lda #%11010100
    sta Random 
	
	lda #%11110110
    sta RandomWPrawo 
	
	lda #%01011100
    sta RandomWDol
	
	lda #%11010100
    sta RandomWLewo 

	lda #%10010111
    sta RandomWGore 
	
	lda #0
	sta KtoryCykl
	
	; lda #88 		
	; sta COLUBK
	
	lda #0
	sta Punkty
	
	
NowaRamka:
	
	lda KtoryCykl
	cmp #4
	beq ResetujCykl
	jmp NieResetujCyklu
ResetujCykl:
	lda #0
	sta KtoryCykl
	
	
NieResetujCyklu:
	
	lda #%00000010
	sta VBLANK
	sta VSYNC
	
	
	sta WSYNC
	sta WSYNC
	sta WSYNC
	
	lda #0
	sta VSYNC

	lda GraczX
	ldy #0
	jsr UstawPozycjeX
	
	lda WrogX
	ldy #1
	jsr UstawPozycjeX
	
	
	
	ldx #35
LoopVBlank:
	sta WSYNC
	dex
	bne LoopVBlank
	
	lda #0
	sta VBLANK
	
	jsr CalculateDigitOffset
	
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Display the scoreboard lines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; czy jestesmy w trakcie game over. Jesli tak zostaw aktualny kolor tla
	
	
	lda #0                   ; reset TIA registers before displaying the score
    sta COLUBK
  
NieZmieniajKoloruTla:
	sta PF0
    sta PF1
    sta PF2
    sta GRP0
    sta GRP1
    sta CTRLPF

    lda #$1E
    sta COLUPF               ; set the scoreboard playfield color with yellow

    ldx #DIGITS_HEIGHT       ; start X counter with 5 (height of digits)

.ScoreDigitLoop:
    ldy TensDigitOffset      ; get the tens digit offset for the Score
    lda Digits,Y             ; load the bit pattern from lookup table
    and #$F0                 ; mask/remove the graphics for the ones digit
    sta ScoreSprite          ; save the score tens digit pattern in a variable

    ldy OnesDigitOffset      ; get the ones digit offset for the Score
    lda Digits,Y             ; load the digit bit pattern from lookup table
    and #$0F                 ; mask/remove the graphics for the tens digit
    ora ScoreSprite          ; merge it with the saved tens digit sprite
    sta ScoreSprite          ; and save it
    sta WSYNC                ; wait for the end of scanline
    sta PF1                  ; update the playfield to display the Score sprite

    ldy ScoreSprite          ; preload for the next scanline
    sta WSYNC                ; wait for next scanline

    sty PF1                  ; update playfield for the score display
    inc TensDigitOffset

    inc OnesDigitOffset


  

    dex                      ; X--
    sta PF1                  ; update the playfield for the Timer display
    bne .ScoreDigitLoop      ; if dex != 0, then branch to ScoreDigitLoop

    sta WSYNC

    lda #0
    sta PF0
    sta PF1
    sta PF2
    sta WSYNC
    sta WSYNC
    sta WSYNC

	lda FlagaGameOver
	cmp #1
	beq TloGameOver
	lda #0
	sta COLUBK
	jmp TloNormalne
TloGameOver:
	lda #66
	sta COLUBK
TloNormalne:
	
	
	ldx #192


RysujLinie:

;rysuj gracza
	txa
	sec
	sbc GraczY
	cmp GraczWys
	bcc RysujGracz
	lda #0
	
RysujGracz:
	tay
	
	lda GraczWzor,Y
	;sta WSYNC
	sta GRP0
	lda GraczKolor,Y
	sta COLUP0

;rysuj wroga

	txa
	sec
	sbc WrogY
	cmp WrogWys
	bcc RysujWrog
	lda #0
	
RysujWrog:
	tay
	lda WrogWzor,Y
	;sta WSYNC
	sta GRP1
	lda WrogKolor,Y
	sta COLUP1	
	
	
	sta WSYNC
	dex
	bne RysujLinie
	
	
	
;Overscan
	ldx #30
LoopOverscan:
	sta WSYNC
	dex
	bne LoopOverscan
	
	; dec GraczX
	; ;inc GraczY
	
	; lda GraczX
	; cmp #30
	; bne Dalej
	; lda #120
	; sta GraczX
	
Dalej:
	

	
	
;joystick
lewo:
	lda #%01000000
	bit SWCHA
	bne prawo
	
	lda GraczX
	cmp #20
	bcs zmienlewo  ;czy 33 > 30 
	jmp prawo
zmienlewo:	
	dec GraczX
	
prawo:
	lda #%10000000
	bit SWCHA
	bne dol
	lda GraczX
	cmp #100
	bcc zmnienprawo ;czy 110 < 120
	jmp dol
zmnienprawo:	
	inc GraczX
	
	
	
dol:
	lda #%00100000
	bit SWCHA
	bne gora
	lda GraczY
	cmp #30
	bcs zmniendol ; czy 35 > 30
	jmp gora
zmniendol:	
	dec GraczY
	
	
gora:
	lda #%00010000
	bit SWCHA
	bne brakruchu
	
	lda GraczY
	cmp #160
	bcc zmiengora  ; czy 150 < 160
	jmp brakruchu
zmiengora:		
	inc GraczY
	
brakruchu:
	
	

	lda FlagaGameOver
	cmp #01
	beq NieRuszajWroga1

	lda KtoryCykl
	cmp #0
	beq WrogPrawo
	cmp #1
	beq WrogDol
	cmp #2
	beq WrogLewo
	cmp #3
	beq WrogGora

WrogPrawo:
	
	lda WrogX
	cmp #110
	bcs ZmienKierunekSkok
	
	inc WrogX
	lda JakaSzybkosc
	cmp #05
	bcc PrawoPierwsza
	inc WrogX
PrawoPierwsza:
	;lda JakaSzybkosc
	cmp #20
	bcc PrawoDruga
	inc WrogX
PrawoDruga:
	;lda JakaSzybkosc
	cmp #31
	bcc PrawoTrzecia
	inc WrogX
PrawoTrzecia:
	jmp Koniec

WrogDol:
	lda WrogY
	cmp #30
	bcc ZmienKierunekSkok
	dec WrogY
	
	lda JakaSzybkosc
	cmp #05
	bcc DolPierwsza
	dec WrogY
DolPierwsza:
	;lda JakaSzybkosc
	cmp #20
	bcc DolDruga
	dec WrogY
DolDruga:
	;lda JakaSzybkosc
	cmp #31
	bcc DolTrzecia
	dec WrogY
DolTrzecia:
	
	jmp Koniec

WrogLewo:
	lda WrogX
	cmp #20
	bcc ZmienKierunekSkok
	dec WrogX
	
	lda JakaSzybkosc
	cmp #05
	bcc LewoPierwsza
	dec WrogX
LewoPierwsza:
	;lda JakaSzybkosc
	cmp #20
	bcc LewoDruga
	dec WrogX
LewoDruga:
	;lda JakaSzybkosc
	cmp #31
	bcc LewoTrzecia
	dec WrogX
LewoTrzecia:
	
	jmp Koniec

NieRuszajWroga1:
	lda FlagaGameOver
	cmp #01
	beq NieRuszajWroga2

	
WrogGora:
	lda WrogY
	cmp #170
	bcs ZmienKierunekSkok
	inc WrogY
	
	
	lda JakaSzybkosc
	cmp #05
	bcc GoraPierwsza
	inc WrogY
GoraPierwsza:
	;lda JakaSzybkosc
	cmp #20
	bcc GoraDruga
	inc WrogY
GoraDruga:
	lda JakaSzybkosc
	cmp #31
	bcc GoraTrzecia
	inc WrogY
GoraTrzecia:
	
	jmp Koniec
	
ZmienKierunekSkok:
	inc Punkty
	jsr ZmienKierunek
	
	
	lda JakaSzybkosc
	cmp #30
	bcs	NieZmieniajSzybkosci
	inc JakaSzybkosc
NieZmieniajSzybkosci:
	
NieRuszajWroga2:
Koniec:
	
	lda #%10000000
	bit CXPPMM
	bne Kolizja
		
	; lda #66 		
	; sta COLUBK
	jmp NieMaKolizji
	
Kolizja:
	lda #$66	
	sta COLUBK
	lda #01
	sta FlagaGameOver
	

	; lda #%10000000
	; bit INPT4
	; bne NieNacisnietoPrzycisku
	; lda #0
	; sta Punkty
	; sta JakaSzybkosc
	; jsr ZmienKierunek

; NieNacisnietoPrzycisku:
	
NieMaKolizji:
	sta CXCLR

;sprawdz czy przycisk nacisniety jesli game over
	lda FlagaGameOver
	cmp #0
	beq NieNacisnietoPrzycisku2

	lda #%10000000
	bit INPT4
	bne NieNacisnietoPrzycisku2
	lda #0
	sta Punkty
	sta JakaSzybkosc
	sta FlagaGameOver
	jsr ZmienKierunek
	
NieNacisnietoPrzycisku2:
	
	jmp NowaRamka
	
	
UstawPozycjeX subroutine

	sta WSYNC
	sta HMCLR
	sec
	
DivideLoop:
	sbc #15
	bcs DivideLoop
	eor #7
	asl
	asl
	asl
	asl
	sta HMP0,Y
	sta RESP0,Y
	sta WSYNC
	sta HMOVE
	
	sta WSYNC
	sta WSYNC
	rts
	
CalculateDigitOffset subroutine


    lda Punkty             ; load A with Timer (X=1) or Score (X=0)
    and #$0F                 ; remove the tens digit by masking 4 bits 00001111
    sta Temp                 ; save the value of A into Temp
    asl                      ; shift left (it is now N*2)
    asl                      ; shift left (it is now N*4)
    adc Temp                 ; add the value saved in Temp (+N)
    sta OnesDigitOffset   ; save A in OnesDigitOffset+1 or OnesDigitOffset

    lda Punkty              ; load A with Timer (X=1) or Score (X=0)
    and #$F0                 ; remove the ones digit by masking 4 bits 11110000
    lsr                      ; shift right (it is now N/2)
    lsr                      ; shift right (it is now N/4)
    sta Temp                 ; save the value of A into Temp
    lsr                      ; shift right (it is now N/8)
    lsr                      ; shift right (it is now N/16)
    adc Temp                 ; add the value saved in Temp (N/16+N/4)
    sta TensDigitOffset   ; store A in TensDigitOffset+1 or TensDigitOffset

   

    rts


losuj subroutine
;generuj liczby losowe	
	lda Random
    asl
    eor Random
    asl
    eor Random
    asl
    asl
    eor Random
    asl
    rol Random            
 
	cmp #33    		; 
	bcs wieksze1 	;skacz jesli A >= 33
	
	lda #0 		; seledynowy bo pierwsza czesc
	sta KtoryCykl
	jmp koniecporownania

wieksze1:
	cmp #66
	bcs wieksze2	;skacz jesli A >=66
	
	lda #1
	sta KtoryCykl	;kolor bo druga czesc
	jmp koniecporownania
	
wieksze2:			

	cmp #150
	bcs wieksze3
	lda #2		; kolor bo trzecia czesc
	sta KtoryCykl
	jmp koniecporownania

wieksze3:
	lda #3
	sta KtoryCykl

koniecporownania:
	
	rts


losujY subroutine

jeszczerazY:
	lda RandomWPrawo
    asl
    eor RandomWPrawo
    asl
    eor RandomWPrawo
    asl
    asl
    eor RandomWPrawo
    asl
    rol RandomWPrawo            
	asl
	
	cmp #30
	bcs Yjestok1
	jmp jeszczerazY
Yjestok1:
	cmp #160
	bcc Yjestok2
	jmp jeszczerazY
Yjestok2:

	
	
	rts

losujX subroutine
jeszczerazX:
	lda RandomWDol
    asl
    eor RandomWDol
    asl
    eor RandomWDol
    asl
    asl
    eor RandomWDol
    asl
    rol RandomWDol            
	asl
	
	cmp #20
	bcs Xjestok1
	jmp jeszczerazX
Xjestok1:
	cmp #100
	bcc Xjestok2
	jmp jeszczerazX
Xjestok2:

	
	
	rts
	
ZmienKierunek subroutine
	jsr losuj
	lda KtoryCykl
	cmp #0
	beq ZmienKierunekNaPrawo
	cmp #1
	beq ZmienKierunekNaDol
	cmp #2
	beq ZmienKierunekNaLewo
	cmp #3
	beq ZmienKierunekNaGora

ZmienKierunekNaPrawo:
	
	jsr losujY
	;lda #80
	sta WrogY
	lda #0
	sta WrogX
	jmp KoniecZmianyKierunku

ZmienKierunekNaDol:
	
	lda #190
	sta WrogY
	;lda #50
	jsr losujX
	sta WrogX
	jmp KoniecZmianyKierunku

ZmienKierunekNaLewo:
	;lda #50
	jsr losujY
	sta WrogY
	lda #130
	sta WrogX
	jmp KoniecZmianyKierunku

ZmienKierunekNaGora:
	lda #3
	sta WrogY
	;lda #70
	jsr losujX
	sta WrogX
	jmp KoniecZmianyKierunku

KoniecZmianyKierunku:
	rts
	

Digits:
    .byte %01110111          ; ### ###
    .byte %01010101          ; # # # #
    .byte %01010101          ; # # # #
    .byte %01010101          ; # # # #
    .byte %01110111          ; ### ###

    .byte %00010001          ;   #   #
    .byte %00010001          ;   #   #
    .byte %00010001          ;   #   #
    .byte %00010001          ;   #   #
    .byte %00010001          ;   #   #

    .byte %01110111          ; ### ###
    .byte %00010001          ;   #   #
    .byte %01110111          ; ### ###
    .byte %01000100          ; #   #
    .byte %01110111          ; ### ###

    .byte %01110111          ; ### ###
    .byte %00010001          ;   #   #
    .byte %00110011          ;  ##  ##
    .byte %00010001          ;   #   #
    .byte %01110111          ; ### ###

    .byte %01010101          ; # # # #
    .byte %01010101          ; # # # #
    .byte %01110111          ; ### ###
    .byte %00010001          ;   #   #
    .byte %00010001          ;   #   #

    .byte %01110111          ; ### ###
    .byte %01000100          ; #   #
    .byte %01110111          ; ### ###
    .byte %00010001          ;   #   #
    .byte %01110111          ; ### ###

    .byte %01110111          ; ### ###
    .byte %01000100          ; #   #
    .byte %01110111          ; ### ###
    .byte %01010101          ; # # # #
    .byte %01110111          ; ### ###

    .byte %01110111          ; ### ###
    .byte %00010001          ;   #   #
    .byte %00010001          ;   #   #
    .byte %00010001          ;   #   #
    .byte %00010001          ;   #   #

    .byte %01110111          ; ### ###
    .byte %01010101          ; # # # #
    .byte %01110111          ; ### ###
    .byte %01010101          ; # # # #
    .byte %01110111          ; ### ###

    .byte %01110111          ; ### ###
    .byte %01010101          ; # # # #
    .byte %01110111          ; ### ###
    .byte %00010001          ;   #   #
    .byte %01110111          ; ### ###

    .byte %00100010          ;  #   #
    .byte %01010101          ; # # # #
    .byte %01110111          ; ### ###
    .byte %01010101          ; # # # #
    .byte %01010101          ; # # # #

    .byte %01110111          ; ### ###
    .byte %01010101          ; # # # #
    .byte %01100110          ; ##  ##
    .byte %01010101          ; # # # #
    .byte %01110111          ; ### ###

    .byte %01110111          ; ### ###
    .byte %01000100          ; #   #
    .byte %01000100          ; #   #
    .byte %01000100          ; #   #
    .byte %01110111          ; ### ###

    .byte %01100110          ; ##  ##
    .byte %01010101          ; # # # #
    .byte %01010101          ; # # # #
    .byte %01010101          ; # # # #
    .byte %01100110          ; ##  ##

    .byte %01110111          ; ### ###
    .byte %01000100          ; #   #
    .byte %01110111          ; ### ###
    .byte %01000100          ; #   #
    .byte %01110111          ; ### ###

    .byte %01110111          ; ### ###
    .byte %01000100          ; #   #
    .byte %01100110          ; ##  ##
    .byte %01000100          ; #   #
    .byte %01000100          ; #   #
	
GraczWzor:
	byte #%00000000
	byte #%00111100
	byte #%00111100
	byte #%00111100

GraczKolor:
	byte #$00
	byte #$AA
	byte #$AB
	byte #$AC
	
	
WrogWzor:
	byte #%00000000
	byte #%00111110
	byte #%00010100
	byte #%00111110

WrogKolor:
	byte #$00
	byte #$33
	byte #$44
	byte #$55
	
	org $FFFC
	.word Start
	.word Start
	
	