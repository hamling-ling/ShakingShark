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
.equ PRT_VOL	= portd	; port for volume pwm
.equ DDR_VOL	= ddrd	; ddr for PRT_VOL
.equ PIN_VOL	= 1		; pin for above
.equ PRT_SND	= portd	; port for sound pwm
.equ DDR_SND	= ddrd	; ddr for PRT_SND
.equ PIN_SND	= 0		; pin for above
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
.equ PRT_VOL	= portb	; port for volume pwm
.equ DDR_VOL	= ddrb	; ddr for PRT_VOL
.equ PIN_VOL	= 1		; pin for above
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
.equ SENSITIVITY = 26	; sensitivity
#else
.equ SENSITIVITY = 0	; sensitivity
#endif
.equ T10USEC	= 248	; Pre Scale=1/8, 100KHz
.equ PRE_SCALE	= 0x2	; 1/8
.equ VCNTMAX	= 20	; max valu of vcnt
.equ ZEROGREADX	= 128	; 0g value of accerelometer read along x-axis
.equ ZEROGREADY	= 128	; same for y-axis
.equ INPUT_LV0	= 0		; input level 0
.equ INPUT_LV1	= 1		; input level 1
.equ INPUT_LV2	= 2		; input level 2
.equ INPUT_LV3	= 3		; input level 3
.equ INPUT_LV4	= 4		; input level 4
.equ SNDDATA_ID_LV0		= 0
.equ SNDDATA_ID_LV1		= 1
.equ SNDDATA_ID_LV2		= 2
.equ SNDDATA_ID_LV3		= 3
.equ SNDDATA_ID_LV4_1	= 4
.equ SNDDATA_ID_LV4_2	= 5
.equ SNDDATA_ID_LV4_3	= 6
.equ SNDDATA_ID_LV4_4	= 7

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
.def vval		= r16	; last applied voltage displacement
.def vread		= r17	; voltage displacement read in process
.def curdata_id	= r18
.def nxtdata_id	= r19
.def curdata_sth= r20	; current phrase data start high address
.def curdata_stl= r21	; current phrase data start low address
.def curdata_edh= r22	; current phrase data end high address
.def curdata_edl= r23	; current phrase data end low address
.def acc		= r24	; accumulator
.def acc2		= r25	; accumulator2

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

; time count
.macro TimeCount		; TIME_COUNT @0 @1
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
	ldi		curdata_stl, low(@0<<1)
	ldi		curdata_sth, high(@0<<1)
	ldi		curdata_edl, low(@1<<1)
	ldi		curdata_edh, high(@1<<1)
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
	sbi		DDR_SND, PIN_SND
	sbi		DDR_VOL, PIN_VOL
	sbi		DDR_LV, PIN_LV
	cbi		DDR_ACCX, PIN_ACCX
	cbi		DDR_ACCY, PIN_ACCY
#endif

	; Timer/Counter 0 initialize
	; tccr0a=0, standard mode
	ldi		acc, 0
	sbr	 	acc,(1<<TOIE0)	; set overflow interruption bit
	OutReg	TIMERMASK, acc	; allow timer0 overflow interruption
	ldi	 	acc, T10USEC	; 10us count
	out	 	TCNT0, acc		; set timer0 counter
	ldi	 	acc, PRE_SCALE	; set prescale
	out	 	TCCR0B, acc		; start timer0

	; initialize our counters
	clr	 	t10us			; init counter for 10us
	clr	 	t100us			; init counter for 100usr
	clr	 	t1ms			; init counter for 1ms
	clr		t10ms			; init counter for 10ms
	clr		t100ms
	clr		t1s

	; initialize sound interval counter
	clr		sctop			; sound interval count

	; initialize melody counter
	clr		mcnt			; initialize melody counter

	; load data
	clr		zl				; init zl
	clr		zh				; init zh

	clr		curdata_id
	clr		nxtdata_id
	SetData	0, 0

	; initialize adc
	clr		vread
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

	TimeCount	t10us,	intr_time0_sndpwm	; count wrap around for 10us
	TimeCount	t100us,	intr_time0_sndpwm	; count wrap around for 100us
	TimeCount	t1ms,	intr_time0_sndpwm	; count wrap around for 1ms
	TimeCount	t10ms,	intr_time0_sndpwm	; count wrap around for 10ms
	TimeCount	t100ms,	initr_time0_setsnd	; count wrap around for 100ms
	TimeCount	t1s,	initr_time0_setsnd	; count wrap around for 1s

initr_time0_setsnd:
	; generate sound
	rcall	set_freq						; called every 100ms

	; request adc interruption
	InReg	acc, ADMUX
	ldi		acc2, (1<<MUX1)|(1<<MUX0)
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
;=============================================================
set_freq:
	cp		mcnt, mtop
	inc		mcnt
	brlt	set_freq_exit	; if mcnt<mtop, do nothing
	clr		mcnt

	; check more data left
	cp		zl, curdata_edl
	brne	set_freq_asgn
	cp		zh, curdata_edh
	brne	set_freq_asgn

	mov		acc, nxtdata_id
	rcall	load_snd_for_id

set_freq_asgn:
	lpm		mtop, z+		; initialize tcnt compare value
	lpm		sctop, z+		; count untill scnt becomes this value
	clr		scnt
set_freq_exit:
	ret

;=============================================================
; sound frequency pwm
;=============================================================
snd_pwm:
	cpi		curdata_id, SNDDATA_ID_LV0
	breq	snd_pwm_mute
	rjmp	snd_pwm_out
snd_pwm_mute:
	cbi		PRT_SND, 1<<PIN_SND
	ret
snd_pwm_out:
	cp		scnt, sctop
	inc		scnt
	brlt	snd_pwm_ext
	FlipOut	PRT_SND, 1<<PIN_SND
	clr		scnt
snd_pwm_ext:
	inc		mcnt
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
	sub		acc, acc2			; acc-netral value
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
	brlt	readv_level0
	cpi		vval, 28-SENSITIVITY
	brlt	readv_level1
	cpi		vval, 30-SENSITIVITY
	brlt	readv_level2
	cpi		vval, 32-SENSITIVITY
	brlt	readv_level3
	cpi		vval, 34-SENSITIVITY
	rjmp	readv_level4
readv_level0:
	ldi		acc, INPUT_LV0
	ldi		acc2, 0b0000_0001
	rjmp readv_ext
readv_level1:
	ldi		acc, INPUT_LV1
	ldi		acc2, 0b0000_0011
	rjmp readv_ext
readv_level2:
	ldi		acc, INPUT_LV2
	ldi		acc2, 0b0000_0111
	rjmp readv_ext
readv_level3:
	ldi		acc, INPUT_LV3
	ldi		acc2, 0b0000_1111
	rjmp readv_ext
readv_level4:
	ldi		acc, INPUT_LV4
	ldi		acc2, 0b0001_1111
	rjmp readv_ext
readv_ext:
	rcall	select_snd
#ifdef ATMEGA168
	out		PRT_LV, acc2
#endif
#ifdef ATTINY45
	sbi		PRT_LV, PIN_LV
#endif
	ret

;=============================================================
; set input level
;=============================================================
select_snd:
	cpi		acc, INPUT_LV0
	breq	select_snd_lv0
	cpi		acc, INPUT_LV1
	breq	select_snd_lv1
	cpi		acc, INPUT_LV2
	breq	select_snd_lv2
	cpi		acc, INPUT_LV3
	breq	select_snd_lv3
	cpi		acc, INPUT_LV4
	breq	select_snd_lv4
select_snd_lv0:
	ldi		acc, SNDDATA_ID_LV0
	rjmp	select_snd_ext
select_snd_lv1:
	ldi		acc, SNDDATA_ID_LV1
	rjmp	select_snd_ext
select_snd_lv2:
	ldi		acc, SNDDATA_ID_LV2
	rjmp	select_snd_ext
select_snd_lv3:
	ldi		acc, SNDDATA_ID_LV3
	rjmp	select_snd_ext
select_snd_lv4:
	ldi		acc, SNDDATA_ID_LV4_1
	rjmp	select_snd_ext
select_snd_ext:
	mov nxtdata_id, acc
	ret

load_snd_for_id:
	cpi		acc, 0
	breq	load_snd_for_id_lv0
	cpi		acc, SNDDATA_ID_LV0
	breq	load_snd_for_id_lv1
	cpi		acc, SNDDATA_ID_LV1
	breq	load_snd_for_id_lv2
	cpi		acc, SNDDATA_ID_LV2
	breq	load_snd_for_id_lv3
	cpi		acc, SNDDATA_ID_LV3
	breq	load_snd_for_id_lv4_1
	cpi		acc, SNDDATA_ID_LV4_1
	breq	load_snd_for_id_lv4_2
	cpi		acc, SNDDATA_ID_LV4_2
	breq	load_snd_for_id_lv4_3
	cpi		acc, SNDDATA_ID_LV4_3
	breq	load_snd_for_id_lv4_4
	cpi		acc, SNDDATA_ID_LV4_4
load_snd_for_id_lv0:
	SetData	0, 0
	rjmp	select_snd_ext
load_snd_for_id_lv1:
	SetData	SND_LV1, SND_LV1_END
	rjmp	select_snd_ext
load_snd_for_id_lv2:
	SetData	SND_LV2, SND_LV2_END
	rjmp	select_snd_ext
load_snd_for_id_lv3:
	SetData	SND_LV3, SND_LV3_END
	rjmp	select_snd_ext
load_snd_for_id_lv4_1:
	SetData	SND_LV4_1, SND_LV4_1_END
	rjmp	select_snd_ext
load_snd_for_id_lv4_2:
	SetData	SND_LV4_2, SND_LV4_2_END
	rjmp	select_snd_ext
load_snd_for_id_lv4_3:
	SetData	SND_LV4_3, SND_LV4_3_END
	rjmp	select_snd_ext
load_snd_for_id_lv4_4:
	SetData	SND_LV4_4, SND_LV4_4_END
	rjmp	select_snd_ext
load_snd_for_id_ext:
	mov		nxtdata_id, acc
	ret

;=============================================================
; data
;=============================================================

SND_LV1:
	.db NOTE_8, TONE_1E		;ta-da
	.db NOTE_32, TONE_1F
	.db NOTE_32, TONE_NONE
	.db NOTE_16, TONE_NONE
	.db NOTE_8, TONE_NONE
	.db NOTE_8, TONE_NONE
SND_LV1_END:

SND_LV2:
	.db NOTE_8, TONE_1E		;ta-da,ta-da
	.db NOTE_32, TONE_1F
	.db NOTE_32, TONE_NONE
	.db NOTE_16, TONE_NONE
	.db NOTE_8, TONE_1E
	.db NOTE_32, TONE_1F
	.db NOTE_32, TONE_NONE
	.db NOTE_16, TONE_NONE
SND_LV2_END:

SND_LV3:
	.db NOTE_32, TONE_1E
	.db NOTE_32, TONE_NONE
	.db NOTE_32, TONE_1F
	.db NOTE_32, TONE_NONE
SND_LV3_END:

SND_LV4_1:
	.db NOTE_32, TONE_2E
	.db NOTE_32, TONE_2G
	.db NOTE_WL, TONE_2AS
SND_LV4_1_END:

SND_LV4_2:
	.db NOTE_32, TONE_2E
	.db NOTE_32, TONE_2G
	.db NOTE_WL, TONE_3C
SND_LV4_2_END:

SND_LV4_3:
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
SND_LV4_3_END:

SND_LV4_4:
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
SND_LV4_4_END:

SND_LV4_5:
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
SND_LV4_5_END:

;=============================================================
;=============================================================
;		   END
;=============================================================
