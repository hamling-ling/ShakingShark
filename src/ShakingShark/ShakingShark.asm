;=============================================================
; title			: ShakingShark
; author		: Nobuhiro Kuroiwa
; started on	: 12/12/2012
; clock			: clk=8MHz
;=============================================================
; debug directive
#define	DEBUG

; chip select
#define			ATMEGA168	; Atmega168
;#define			ATTINY45	; AtTiny45

.include "musicnote.inc"	; definition of music stuff

;=============================================================
; ports and pins
;=============================================================
#ifdef ATMEGA168
.include "m168def.inc"	;
.equ PRT_LV		= portb	; port for level indicator
.equ DDR_LV		= ddrb	; ddr  for PRT_LV
.equ PRT_SND	= portc	; port for sound pwm
.equ DDR_SND	= ddrc	; ddr for PRT_SND
.equ PIN_SND	= 5		; pin for above
.equ PRT_ACCX	= portc	; port for x-axis accelerometer read
.equ DDR_ACCX	= ddrc	; ddr for PRT_ACCX
.equ PIN_ACCX	= 0		; pin for above
.equ PRT_ACCY	= portc	; pin for y-axis accelerometer read
.equ DDR_ACCY	= ddrc	; ddr for PRT_ACCY
.equ PIN_ACCY	= 1		; pin for above
.equ TIMERMASK	= TIMSK0
.equ ADCSRAVAL	= (1<<ADEN)|(1<<ADSC)|(1<<ADIF)|(1<<ADIE)|(1<<ADPS2)|(1<<ADPS1)|(1<<ADPS0)
.equ ADMUXVAL	= (1<<REFS0)|(1<<ADLAR)	; adc ref voltage=AVcc, 8bit precision
#endif

#ifdef ATTINY45
.include "tn45def.inc"
.equ PRT_LV		= portb	; port for level indicator
.equ DDR_LV		= ddrb	; ddr  for PRT_LV
.equ PIN_LV		= 3
.equ PRT_SND	= portb	; port for sound pwm
.equ DDR_SND	= ddrb	; ddr for PRT_SND
.equ PIN_SND	= 0		; pin for above
.equ PRT_ACCX	= portb	; port for x-axis accelerometer read
.equ DDR_ACCX 	= ddrb	; ddr for PRT_ACCX
.equ PIN_ACCX	= 2		; pin for above
.equ PRT_ACCY	= portb	; pin for y-axis accelerometer read
.equ DDR_ACCY  	= ddrb	; ddr for PRT_ACCY
.equ PIN_ACCY	= 4		; pin for above
.equ TIMERMASK	= TIMSK
.equ ADCSRAVAL	= (1<<ADEN)|(1<<ADSC)|(1<<ADIF)|(1<<ADIE)|(1<<ADPS2)|(1<<ADPS1)|(1<<ADPS0)
.equ ADMUXVAL	= (1<<ADLAR)|(1<<MUX0)	; adc ref vref=internal, 8bit precision
#endif

;=============================================================
; constants
;=============================================================
#ifdef DEBUG
.equ SENSITIVITY = 16	; sensitivity
#else
.equ SENSITIVITY = 0	; sensitivity
#endif
.equ T10USEC	= 248	; Pre Scale=1/8, 100KHz
.equ PRE_SCALE	= 0x2	; 1/8
.equ VCNTMAX	= 20	; max valu of vcnt
.equ ZEROGREADX	= 123	; 0g value of accerelometer read along x-axis
.equ ZEROGREADY	= 129	; same for y-axis
.equ INPUT_LV0	= 0		; input level 0
.equ INPUT_LV1	= 1		; input level 1
.equ SNDDATA_ID_LV0 = 0
.equ SNDDATA_ID_LV1 = 1
.equ SNDDATA_ID_LV2 = 2
.equ SNDDATA_ID_LV3 = 3
.equ SNDDATA_ID_LV4 = 4
.equ SNDDATA_ID_LV5 = 5
.equ SNDDATA_ID_LV6 = 6
.equ SNDDATA_ID_LV7 = 7
.equ SNDDATA_ID_LV8 = 8
.equ SNDDATA_ID_MAX = SNDDATA_ID_LV8

;=============================================================
; variables
;=============================================================
.def sreg_save	= r0	; to store sreg
.def one		= r1	; constant 1
.def ten		= r2	; constant 10
.def t10us		= r3	; count for 10us
.def t100us		= r4	; count for 100us
.def t1ms		= r5	; count for 1ms
.def t10ms		= r6	; count for 10ms
.def t100ms		= r7	; count for 100ms
.def t1s		= r8	; count for 1s
.def sctop		= r9	; sound interval count
.def mcnt		= r10	; t100ms counter
.def mtop		= r11	; tcnt top value
.def scnt		= r12	; compare to scnt for rythm
.def vval			= r16	; last applied voltage displacement
.def vread			= r17	; voltage displacement read in process
.def cur_data_id	= r18
.def nxt_data_id	= r19
.def cur_data_edh	= r20	; current phrase data end high address
.def cur_data_edl	= r21	; current phrase data end low address
.def acc		= r22	; accumulator
.def acc2		= r23	; accumulator2

;=============================================================
; macro
;=============================================================
; preserve registers
.macro StoreRegs
	in		sreg_save, SREG	; preserve status
	push	sreg_save
	push	acc				; preserve acc
	push	acc2			; preserve acc2
.endmacro

.macro RestoreRegs
	pop		acc2			; restore acc2
	pop		acc				; restore acc
	pop		sreg_save
	out		SREG, sreg_save	; restore sreg
.endmacro

; ten count
.macro TimeCount10		; TIME_COUNT @0 @1
	inc		@0			; increment register given by @0
	cp		@0, ten		; compare the register
	brne	@1			; if the register != 10 jump to @1
	clr		@0			; clear the register
.endmacro

; flip port output
.macro FlipOut			; FLIP_PORT portx, port_bit
	in		acc, @0
	ldi		acc2, @1	; bit mask
	eor		acc, acc2
	out		@0, acc		; output
.endmacro

; usage: InReg reg, addr 
.macro InReg 
	.if @1 < 0x40 
		in @0, @1 
	.elif ((@1 >= 0x60) && (@1 < SRAM_START)) 
		lds @0,@1 
	.else 
		.error "InReg: Invalid I/O register address" 
	.endif 
.endmacro 

; usage: OutReg addr, reg 
.macro OutReg 
	.if @0 < 0x40 
		out @0, @1 
	.elif ((@0 >= 0x60) && (@0 < SRAM_START)) 
		sts @0,@1 
	.else 
		.error "OutReg: Invalid I/O register address" 
	.endif 
.endmacro 

; usage: SetData addr_start, addr_end 
.macro SetData 
	ldi		zl, low(@0<<1)
	ldi		zh, high(@0<<1)
	ldi		cur_data_edl, low(@1<<1)
	ldi		cur_data_edh, high(@1<<1)
	lpm		mtop, z+		; initialize tcnt compare value
	lpm		sctop, z+		; count untill scnt becomes this value
.endmacro 

;=============================================================
; program
;=============================================================
.cseg					   ; Code segment

;=============================================================
; vectors
;=============================================================
#ifdef ATMEGA168
.org 	0x0000		rjmp main	 	; reset handler
.org	0x0020		rjmp intr_time0	; timer0 overflow handler
.org	0x002A		rjmp adc_comp	; adc complete
#endif
#ifdef ATTINY45
.org 	0x0000		rjmp main	 	; reset handler
.org	0x0005		rjmp intr_time0	; timer0 overflow handler
.org	0x0008		rjmp adc_comp	; adc complete
#endif

;=============================================================
; main
;=============================================================
main:
	cli

	; initialize stack pointer
	ldi		acc, low(ramend)	; get lower byte of end of ram address
	out		spl, acc			; init stack lower pointer
	ldi		acc, high(ramend)	; get higher byte of end of ram address
	out		sph, acc			; init stack higher pointer

	; initialize constant register
	ldi		acc, 1			; put constant 1 in register
	mov		one, acc
	ldi		acc, 10			; put constant 10 in register
	mov		ten, acc

	; initialize ports
#ifdef ATMEGA168
	ldi	 	acc, 0xFF		; P_LV all bits are output
	out	 	DDR_LV, acc		; set direction
	ldi	 	acc, 0x00		; set output data
	out	 	PRT_LV, acc		; set all to low
#endif
#ifdef ATTINY45
	sbi		DDR_LV, PIN_LV
#endif
	sbi		DDR_SND, PIN_SND
	cbi		DDR_ACCX, PIN_ACCX
	cbi		DDR_ACCY, PIN_ACCY

	; initialize our counters
	clr	 	t10us			; init counter for 10us
	clr	 	t100us			; init counter for 100usr
	clr	 	t1ms			; init counter for 1ms
	clr		t10ms			; init counter for 10ms
	clr		t100ms
	clr		t1s

	; initialize sound interval counter
	clr		sctop			; sound interval count
	clr		scnt

	; initialize melody counter
	clr		mtop
	clr		mcnt			; initialize melody counter

	; load data
	clr		zl				; init zl
	clr		zh				; init zh

	clr		cur_data_id
	clr		nxt_data_id
	clr		cur_data_edl
	clr		cur_data_edh

	ldi		cur_data_id, SNDDATA_ID_LV0
	ldi		nxt_data_id, SNDDATA_ID_LV0

	; Timer/Counter 0 initialize
	; tccr0a=0, standard mode
	ldi		acc, 0
	sbr	 	acc,(1<<TOIE0)	; set overflow interruption bit
	OutReg	TIMERMASK, acc	; allow timer0 overflow interruption
	ldi	 	acc, T10USEC	; 10us count
	out	 	TCNT0, acc		; set timer0 counter
	ldi	 	acc, PRE_SCALE	; set prescale
	out	 	TCCR0B, acc		; start timer0

	; initialize adc
	clr		vread
	clr		vval
	ldi		acc, ADMUXVAL
	OutReg	ADMUX, acc
	ldi		acc, ADCSRAVAL
	OutReg	ADCSRA, acc
	ldi		acc, 0x00
	OutReg	ADCSRB, acc

	sei						; allow all interruption

main_loop:

	rjmp	main_loop		; loop

;=============================================================
; timer0 interruption
;=============================================================
intr_time0:
	StoreRegs

	; reset timer
	clr		acc				; stop counter
	out		tccr0b, acc
	ldi		acc, T10USEC	; 10usec count
	out		TCNT0, acc		; set timer0

	ldi		acc, PRE_SCALE
	out		tccr0b, acc

	TimeCount10	t10us,	intr_time0_sndpwm	; count wrap around for 10us
	TimeCount10	t100us,	intr_time0_sndpwm	; count wrap around for 100us
	TimeCount10	t1ms,	intr_time0_sndpwm	; count wrap around for 1ms
	TimeCount10	t10ms,	intr_time0_sndpwm	; count wrap around for 10ms
	TimeCount10	t100ms,	intr_time0_setsnd	; count wrap around for 100ms
	TimeCount10	t1s,	intr_time0_setsnd
intr_time0_setsnd:
	; generate sound
	rcall	set_freq						; called every 100ms

	; request adc interruption
	InReg	acc, ADMUX
#ifdef ATMEGA168
	ldi		acc2, (1<<MUX0)
#endif
#ifdef ATTINY45
	ldi		acc2, (1<<MUX1)|(1<<MUX0)
#endif
	eor		acc, acc2
	OutReg	ADMUX, acc

	ldi		acc, ADCSRAVAL
	OutReg	ADCSRA, acc

intr_time0_sndpwm:
	rcall	snd_pwm

intr_time0_end:

	RestoreRegs
	reti

;=============================================================
; set sound frequency
; supporsed to be called every 100ms
;=============================================================
set_freq:
	cpi		cur_data_id, SNDDATA_ID_LV0	; if cur_data_id != null, go to increment
	brne	set_freq_inc
	; if it reached here, cur_data_id is nothing
	cpi		nxt_data_id, SNDDATA_ID_LV0
	breq	set_freq_ext
	; if it reached here, play nxt_data_id
	rjmp	set_freq_nxt

set_freq_inc:
	mov		acc, mcnt
	inc		mcnt
	cp		acc, mtop
	brlt	set_freq_ext	; if mcnt<mtop, do nothing

	; check more data left
	cp		zl, cur_data_edl
	brne	set_freq_asgn
	cp		zh, cur_data_edh
	brne	set_freq_asgn
	clr		mcnt

set_freq_nxt:
	rcall	load_snd_for_id

	cpi		cur_data_id, SNDDATA_ID_LV0
	breq	set_freq_ext
	clr		scnt
	clr		mcnt
	rjmp	set_freq_ext

set_freq_asgn:
	lpm		mtop, z+		; initialize tcnt compare value
	lpm		sctop, z+		; count untill scnt becomes this value
	clr		scnt
	mov		mcnt, one
set_freq_ext:

	;debug
	;out		PRT_LV, sctop

	ret

;=============================================================
; sound frequency pwm
; supporsed to be called every 10us
;=============================================================
snd_pwm:
	cpi		cur_data_id, SNDDATA_ID_LV0
	breq	snd_pwm_mute
	rjmp	snd_pwm_out
snd_pwm_mute:
	cbi		PRT_SND, PIN_SND
	ret
snd_pwm_out:
	inc		scnt
	cp		scnt, sctop
	brlo	snd_pwm_ext
	FlipOut	PRT_SND, 1<<PIN_SND
	clr		scnt
snd_pwm_ext:
	ret

;=============================================================
; adc_complete interruption
;=============================================================
adc_comp:
	StoreRegs
	rcall	readv				; read voltage
	RestoreRegs
	reti

;=============================================================
; readv
;=============================================================
readv:
	ldi		acc2, ZEROGREADX	; use x-axis neutral value
	InReg	acc, ADMUX
	sbrc	acc, 0
	ldi		acc2, ZEROGREADY	; use y-axis neutral value

	clc
	InReg	acc, ADCH			; read D/A converted value
	sbc		acc, acc2			; acc-netral value
	brpl	readv_positive		; if acc-acc2 > 0, goto readv_positive
	neg		acc					; else take absolute value
readv_positive:

	InReg	acc2, ADMUX

	sbrc	acc2, 0
	rjmp	readv_y
readv_x:
	mov		vread, acc
	ret
readv_y:
	add		vread, acc
	add		vval, vread
	lsr		vval

readv_compare:
	cpi		vval, 26-SENSITIVITY
	brlo	readv_level0
	cpi		vval, 27-SENSITIVITY
	brlo	readv_level1
	cpi		vval, 28-SENSITIVITY
	brlo	readv_level2
	cpi		vval, 29-SENSITIVITY
	brlo	readv_level3
	cpi		vval, 30-SENSITIVITY
	brlo	readv_level4
	cpi		vval, 31-SENSITIVITY
	rjmp	readv_level5
readv_level0:
	ldi		acc, INPUT_LV0
	ldi		acc2, 0b0000_0000
	rjmp readv_ext
readv_level1:
	ldi		acc, INPUT_LV0
	ldi		acc2, 0b0000_0001
	rjmp readv_ext
readv_level2:
	ldi		acc, INPUT_LV1
	ldi		acc2, 0b0000_0011
	rjmp readv_ext
readv_level3:
	ldi		acc, INPUT_LV1
	ldi		acc2, 0b0000_0111
	rjmp readv_ext
readv_level4:
	ldi		acc, INPUT_LV1
	ldi		acc2, 0b0000_1111
	rjmp readv_ext
readv_level5:
	ldi		acc, INPUT_LV1
	ldi		acc2, 0b0001_1111
	rjmp readv_ext
readv_ext:
	rcall	sel_nxt_snd
#ifdef ATMEGA168
	out		PRT_LV, acc2
#endif
#ifdef ATTINY45
	sbi		PRT_LV, PIN_LV
#endif
	ret

;=============================================================
; select next sound
; set sound id to nxtdata_id register
;=============================================================
sel_nxt_snd:
	cpi		acc, INPUT_LV0
	breq	sel_nxt_snd_lv0
	rjmp	sel_nxt_snd_lv1
sel_nxt_snd_lv0:
	ldi		acc, SNDDATA_ID_LV0
	rjmp	sel_nxt_snd_ext
sel_nxt_snd_lv1:
	mov		acc, cur_data_id
	inc		acc
	cpi		acc, SNDDATA_ID_MAX+1
	brlo	sel_nxt_snd_ext
	ldi		acc, SNDDATA_ID_LV4
sel_nxt_snd_ext:
	mov nxt_data_id, acc
	ret

;=============================================================
; select next sound
; set sound id to nxt_data_id register
;=============================================================
load_snd_for_id:
	cpi		nxt_data_id, SNDDATA_ID_LV0
	breq	load_snd_for_id_lv0
	cpi		nxt_data_id, SNDDATA_ID_LV1
	breq	load_snd_for_id_lv1
	cpi		nxt_data_id, SNDDATA_ID_LV2
	breq	load_snd_for_id_lv2
	cpi		nxt_data_id, SNDDATA_ID_LV3
	breq	load_snd_for_id_lv3
	cpi		nxt_data_id, SNDDATA_ID_LV4
	breq	load_snd_for_id_lv4
	cpi		nxt_data_id, SNDDATA_ID_LV5
	breq	load_snd_for_id_lv5
	cpi		nxt_data_id, SNDDATA_ID_LV6
	breq	load_snd_for_id_lv6
	cpi		nxt_data_id, SNDDATA_ID_LV7
	breq	load_snd_for_id_lv7
	cpi		nxt_data_id, SNDDATA_ID_LV8
	breq	load_snd_for_id_lv8
	rjmp	load_snd_for_id_lv0
load_snd_for_id_lv0:
	SetData	0, 0
	rjmp	load_snd_for_id_ext
load_snd_for_id_lv1:
	SetData	PRS1, PRS1_END
	rjmp	load_snd_for_id_ext
load_snd_for_id_lv2:
	SetData	PRS2, PRS2_END
	rjmp	load_snd_for_id_ext
load_snd_for_id_lv3:
	SetData	PRS3, PRS3_END
	rjmp	load_snd_for_id_ext
load_snd_for_id_lv4:
	SetData	PRS4, PRS4_END
	rjmp	load_snd_for_id_ext
load_snd_for_id_lv5:
	SetData	PRS5, PRS5_END
	rjmp	load_snd_for_id_ext
load_snd_for_id_lv6:
	SetData	PRS6, PRS6_END
	rjmp	load_snd_for_id_ext
load_snd_for_id_lv7:
	SetData	PRS7, PRS7_END
	rjmp	load_snd_for_id_ext
load_snd_for_id_lv8:
	SetData	PRS8, PRS8_END
	rjmp	load_snd_for_id_ext
load_snd_for_id_ext:
	clr		mcnt
	clr		scnt
	mov		cur_data_id, nxt_data_id
	clr		nxt_data_id

	;debug
	;out		PRT_LV, cur_data_id

	ret

;=============================================================
; data
;=============================================================

PRS1:
	.db NOTE_8, TONE_1E		;ta-da
	.db NOTE_32, TONE_1F
	.db NOTE_32, TONE_NONE
	.db NOTE_16, TONE_NONE
	.db NOTE_8, TONE_NONE
	.db NOTE_8, TONE_NONE
PRS1_END:

PRS2:
	.db NOTE_8, TONE_1E		;ta-da,ta-da
	.db NOTE_32, TONE_1F
	.db NOTE_32, TONE_NONE
	.db NOTE_16, TONE_NONE
	.db NOTE_8, TONE_1E
	.db NOTE_32, TONE_1F
	.db NOTE_32, TONE_NONE
	.db NOTE_16, TONE_NONE
PRS2_END:

PRS3:
	.db NOTE_32, TONE_1E
	.db NOTE_32, TONE_NONE
	.db NOTE_32, TONE_1F
	.db NOTE_32, TONE_NONE
	.db NOTE_32, TONE_1E
	.db NOTE_32, TONE_NONE
	.db NOTE_32, TONE_1F
	.db NOTE_32, TONE_NONE
	.db NOTE_32, TONE_1E
	.db NOTE_32, TONE_NONE
	.db NOTE_32, TONE_1F
	.db NOTE_32, TONE_NONE
	.db NOTE_32, TONE_1E
	.db NOTE_32, TONE_NONE
	.db NOTE_32, TONE_1F
	.db NOTE_32, TONE_NONE
PRS3_END:

PRS4:
	.db NOTE_32, TONE_2E
	.db NOTE_32, TONE_2G
	.db NOTE_WL, TONE_2AS
PRS4_END:

PRS5:
	.db NOTE_32, TONE_2E
	.db NOTE_32, TONE_2G
	.db NOTE_WL, TONE_3C
PRS5_END:

PRS6:
	.db NOTE_8, TONE_3E
	.db NOTE_8, TONE_2B
	.db NOTE_8, TONE_3FS
	.db NOTE_8, TONE_2B
	.db NOTE_16, TONE_3GS
	.db NOTE_16, TONE_3A
	.db NOTE_16, TONE_3B
	.db NOTE_16, TONE_3GS
	.db NOTE_8, TONE_3FS
	.db NOTE_8, TONE_2B

	.db NOTE_8, TONE_3E
	.db NOTE_8, TONE_2B
	.db NOTE_16, TONE_3FS
	.db NOTE_16, TONE_2A
	.db NOTE_16, TONE_3GS
	.db NOTE_16, TONE_2FS
	.db NOTE_8, TONE_3CS
	.db NOTE_4, TONE_2B
	.db NOTE_8, TONE_2B
PRS6_END:

PRS7:
	.db NOTE_32, TONE_1E
	.db NOTE_32, TONE_NONE
	.db NOTE_32, TONE_1F
	.db NOTE_32, TONE_NONE
	.db NOTE_32, TONE_1E
	.db NOTE_32, TONE_NONE
	.db NOTE_32, TONE_1F
	.db NOTE_32, TONE_NONE
	.db NOTE_32, TONE_1E
	.db NOTE_32, TONE_NONE
	.db NOTE_32, TONE_1F
	.db NOTE_32, TONE_NONE
	.db NOTE_32, TONE_1E
	.db NOTE_32, TONE_NONE
	.db NOTE_32, TONE_1F
	.db NOTE_32, TONE_NONE
PRS7_END:

PRS8:
	.db NOTE_32, TONE_2E
	.db NOTE_32, TONE_NONE
	.db NOTE_32, TONE_2F
	.db NOTE_32, TONE_NONE
	.db NOTE_32, TONE_2E
	.db NOTE_32, TONE_NONE
	.db NOTE_32, TONE_2F
	.db NOTE_32, TONE_NONE
	.db NOTE_32, TONE_2E
	.db NOTE_32, TONE_NONE
	.db NOTE_32, TONE_2F
	.db NOTE_32, TONE_NONE
	.db NOTE_32, TONE_2E
	.db NOTE_32, TONE_NONE
	.db NOTE_32, TONE_2F
	.db NOTE_32, TONE_NONE

	.db NOTE_4, TONE_NONE
PRS8_END:

;=============================================================
;=============================================================
;		END
;=============================================================
