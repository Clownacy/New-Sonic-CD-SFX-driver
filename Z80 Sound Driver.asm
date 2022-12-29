; ---------------------------------------------------------------------------
; ===========================================================================
; ║                                                                         ║
; ║                             SONIC&K SOUND DRIVER                        ║
; ║                         Modified SMPS Z80 Type 2 DAC                    ║
; ║                                                                         ║
; ===========================================================================
; Disassembled by MarkeyJester
; Routines, pointers and stuff by Linncaki
; Thoroughly commented and improved (including optional bugfixes) by Flamewing
; ===========================================================================
; Constants
; ===========================================================================

	CPU 68000
	padding off
	listing purecode	; Want listing file, but only the final code in expanded macros
	supmode on	; we don't need warnings about privileged instructions
	page	0	; Don't want form feeds

SonicDriverVer = 4
use_s1_samples = 0
use_s2_samples = 0
use_s3_samples = 0
use_sk_samples = 0
use_s3d_samples = 0
	include "_smps2asm_inc.asm"

; ---------------------------------------------------------------------------
zTrack STRUCT DOTS
	; Playback control bits:
	; 	0 (01h)		Noise channel (PSG) or FM3 special mode (FM)
	; 	1 (02h)		Do not attack next note
	; 	2 (04h)		SFX is overriding this track
	; 	3 (08h)		'Alternate frequency mode' flag
	; 	4 (10h)		'Track is resting' flag
	; 	5 (20h)		'Pitch slide' flag
	; 	6 (40h)		'Sustain frequency' flag -- prevents frequency from changing again for the lifetime of the track
	; 	7 (80h)		Track is playing
	PlaybackControl:	ds.b 1	; S&K: 0
	; Voice control bits:
	; 	0-1    		FM channel assignment bits (00 = FM1 or FM4, 01 = FM2 or FM5, 10 = FM3 or FM6/DAC, 11 = invalid)
	; 	2 (04h)		For FM/DAC channels, selects if reg/data writes are bound for part II (set) or part I (unset)
	; 	3 (08h)		Unknown/unused
	; 	4 (10h)		Unknown/unused
	; 	5-6    		PSG Channel assignment bits (00 = PSG1, 01 = PSG2, 10 = PSG3, 11 = Noise)
	; 	7 (80h)		PSG track if set, FM or DAC track otherwise
	VoiceControl:		ds.b 1	; S&K: 1
	TempoDivider:		ds.b 1	; S&K: 2
	DataPointerLow:		ds.b 1	; S&K: 3
	DataPointerHigh:	ds.b 1	; S&K: 4
	Transpose:			ds.b 1	; S&K: 5
	Volume:				ds.b 1	; S&K: 6
	ModulationCtrl:		ds.b 1	; S&K: 7		; Modulation is on if nonzero. If only bit 7 is set, then it is normal modulation; otherwise, this-1 is index on modulation envelope pointer table
	VoiceIndex:			ds.b 1	; S&K: 8		; FM instrument/PSG voice
	StackPointer:		ds.b 1	; S&K: 9		; For call subroutine coordination flag
	AMSFMSPan:			ds.b 1	; S&K: 0Ah
	DurationTimeout:	ds.b 1	; S&K: 0Bh
	SavedDuration:		ds.b 1	; S&K: 0Ch		; Already multiplied by timing divisor
	; ---------------------------------
	; Alternate names for same offset:
	FreqLow:			ds.b 1	; S&K: 0Dh		; For FM/PSG channels
	; ---------------------------------
	FreqHigh:			ds.b 1	; S&K: 0Eh		; For FM/PSG channels
	Detune:			ds.b 1	; S&K: 10h	; In S&K, some places used 11h instead of 10h
	VolEnv:				ds.b 1	; S&K: 17h		; Used for dynamic volume adjustments
	; ---------------------------------
	; Alternate names for same offsets:
	PSGNoise:					; S&K: 1Ah
	; ---------------------------------
	TLPtrLow:			ds.b 1	; S&K: 1Ch
	TLPtrHigh:			ds.b 1	; S&K: 1Dh
	ModulationPtrLow:	ds.b 1	; S&K: 20h
	ModulationPtrHigh:	ds.b 1	; S&K: 21h
	; ---------------------------------
	; Alternate names for same offset:
	ModulationValLow:		ds.b 1	; S&K: 22h
	; ---------------------------------
	ModulationValHigh:	ds.b 1	; S&K: 23h
	ModulationWait:		ds.b 1	; S&K: 24h
	; ---------------------------------
	; Alternate names for same offset:
	ModulationSpeed:	ds.b 1	; S&K: 25h
	; ---------------------------------
	ModulationDelta:	ds.b 1	; S&K: 26h
	ModulationSteps:	ds.b 1	; S&K: 27h
	LoopCounters:		ds.b 2	; S&K: 28h		; Might overflow into the following data
	VoicesLow:			ds.b 1	; S&K: 2Ah		; Low byte of pointer to track's voices, used only if zUpdatingSFX is set
	VoicesHigh:			ds.b 1	; S&K: 2Bh		; High byte of pointer to track's voices, used only if zUpdatingSFX is set
	Stack_top:			ds.b 4	; S&K: 2Ch-2Fh	; Track stack; can be used by LoopCounters
zTrack ENDSTRUCT
; ---------------------------------------------------------------------------
z80_stack				=	$2000
z80_stack_end				=	z80_stack-$60
; equates: standard (for Genesis games) addresses in the memory map
zYM2612_A0				=	$4000
zYM2612_D0				=	$4001
zYM2612_A1				=	$4002
zYM2612_D1				=	$4003
zBankRegister			=	$6000
zPSG					=	$7F11
zROMWindow				=	$8000
; ---------------------------------------------------------------------------
; z80 RAM:

zVariables STRUCT NOEXTNAMES
	!org $1C09

zDataStart:

zNextSound:			ds.b 1
; The following 3 variables are used for M68K input
zMusicNumber:		ds.b 1	; Play_Sound
zSFXNumber0:		ds.b 1	; Play_Sound_2
zSFXNumber1:		ds.b 1	; Play_Sound_2

	!org $1C16
zTempVariablesStart:

zCommunication:	ds.b 1
zRingSpeaker:		ds.b 1
zSFXVoiceTblPtr:	ds.b 2	; 2 bytes
zSFXTempoDivider:	ds.b 1
zCurrentPriority:	ds.b 1

	!org $1C3E
zConditionalJumpFlag: ds.b 1

; This is RAM for backup of songs (when 1-up jingle is playing)
; and for SFX channels. Note these two overlap.
; Max number of SFX channels: 4 FM + 3 PSG
zTracksSFXStart:
zSFX_FM1:		zTrack
zSFX_FM2:		zTrack
zSFX_FM3:		zTrack
zSFX_FM4:		zTrack
zSFX_FM5:		zTrack
zSFX_FM6:		zTrack
zSFX_PSG1:		zTrack
zSFX_PSG2:		zTrack
zSFX_PSG3:		zTrack
zTracksSFXEnd:

zTempVariablesEnd:

	if * > z80_stack_end	; Don't declare more space than the RAM can contain!
		fatal "The RAM variable declarations are too large. It's \{*-z80_stack_end}h bytes past the start of the bottom of the stack, at \{z80_stack_end}h."
	endif
zVariables	ENDSTRUCT
; ---------------------------------------------------------------------------
; z80_SoundDriver:
Z80_SoundDriver:
		save
		CPU Z80
		listing purecode
; ---------------------------------------------------------------------------
SndID__First			= 90h
SndID__End				= 0E0h
; ---------------------------------------------------------------------------
FirstCoordFlag			= 0EBh
; ---------------------------------------------------------------------------

; macro to make a certain error message clearer should you happen to get it...
rsttarget macro {INTLABEL}
	if ($&7)||($>38h)
		fatal "Function __LABEL__ is at 0\{$}h, but must be at a multiple of 8 bytes <= 38h to be used with the rst instruction."
	endif
	if "__LABEL__"<>""
__LABEL__ label $
	endif
    endm

; ---------------------------------------------------------------------------
; ===========================================================================
; Entry Point
; ===========================================================================

; EntryPoint:
		di									; Disable interrupts
		ld	sp, z80_stack					; set the stack pointer to 0x2000 (end of z80 RAM)
		jr	zInitAudioDriver
; ---------------------------------------------------------------------------

; =============== S U B	R O U T	I N E =======================================
;
; Reads	an offset into a pointer table and returns dereferenced pointer.
;
;
; Input:  a    Index into pointer table
;	      hl   Pointer to pointer table
; Output: hl   Selected	pointer	in pointer table
;         bc   Trashed

; sub_18
	align	8
PointerTableOffset:	rsttarget
		ld	c, a							; c = index into pointer table
		ld	b, 0							; b = 0
		add	hl, bc							; hl += bc
		add	hl, bc							; hl += bc
		rst	ReadPointer
		ret
; End of function PointerTableOffset

; =============== S U B	R O U T	I N E =======================================
;
; Dereferences a pointer.
;
; Input:  hl	Pointer
; output: hl	Equal to what that was being pointed to by hl

; loc_20
	align	8
ReadPointer:	rsttarget
		ld	a, (hl)							; Read low byte of pointer into a
		inc	hl
		ld	h, (hl)							; Read high byte of pointer into h
		ld	l, a							; Put low byte of pointer into l
		ret
; End of function PointerTableOffset

; =============== S U B	R O U T	I N E =======================================
;
; Writes a reg/data pair to part I or II
;
; Input:  a    Value for register
;         c    Value for data
;         ix   Pointer to track RAM

;sub_AF
	align	8
zWriteFMIorII:	rsttarget
		bit	2, (ix+zTrack.PlaybackControl)	; Is SFX overriding this track?
		ret	nz								; Return if yes
		add	a, (ix+zTrack.VoiceControl)		; Add the channel bits to the register address
		bit	2, (ix+zTrack.VoiceControl)		; Is this the DAC channel or FM4 or FM5 or FM6?
		jr	z, zWriteFMI			; If yes, write reg/data pair to part II;
											; otherwise, write reg/data pair as is to part I.
; End of function zWriteFMIorII

;loc_CB
zWriteFMII_reduced:
		sub	4								; Strip 'bound to part II regs' bit

; =============== S U B	R O U T	I N E =======================================
;
; Writes a reg/data pair to part II
;
; Input:  a    Value for register
;         c    Value for data

;sub_CD
	align	8
zWriteFMII:	rsttarget
		ld	(zYM2612_A1), a					; Select YM2612 register
		ld	a, c							; a = data to send
		ld	(zYM2612_D1), a					; Send data to register
		ret
; End of function zWriteFMII

; =============== S U B	R O U T	I N E =======================================
;
; Writes a reg/data pair to part I
;
; Input:  a    Value for register
;         c    Value for data

;sub_C2
	align	8
zWriteFMI:	rsttarget
		ld	(zYM2612_A0), a					; Select YM2612 register
		ld	a, c							; a = data to send
		ld	(zYM2612_D0), a					; Send data to register
		ret
; End of function zWriteFMI
; ---------------------------------------------------------------------------

; ---------------------------------------------------------------------------
; There is room for two more 'rsttarget's here
; ---------------------------------------------------------------------------
;loc_85
zInitAudioDriver:
			; The following instruction block keeps the z80 in a tight loop.
		ld	c, 0							; c = 0

.loop:
		ld	b, 0							; b = 0
		djnz	$							; Loop in this instruction, decrementing b each iteration, until b = 0
		dec	c								; c--
		jr	z, .loop						; Loop if c = 0

		call	zStopAllSound					; Stop all music

.forever_loop:
		call	zDoSoundQueue
		ld	a, (zYM2612_A0)
		bit	1, a		; Has YM2612 Timer B expired?
		call	nz,zUpdateEverything			; Update all tracks
		jr	.forever_loop

ResetYMTimerB:
		ld	c, 0C8h
		ld	a, 26h
; YM2612 Timer B Frequency is (256-val)	* 16 * Prescale	/ Chip Clock
; The YM2612 prescaler (clock divider) is fixed	to 6 * 24 = 144
; The YM2612 Clock is 7670454.
; Final	formula: Hz = (256-val)	* 2304 / 7670454
; --> Timer B =	200 -> 59.45 Hz
		rst	zWriteFMI	; set YM2612 Timer B = C8
		ld	a, 2Fh
		ld	c, a
		ld	a, 27h
		rst	zWriteFMI
		ret
; End of function ResetYMTimerB

; ---------------------------------------------------------------------------
; =============== S U B	R O U T	I N E =======================================
;
;sub_11B
zUpdateEverything:
		call	ResetYMTimerB
		call	zPlaySoundByIndex
;		jp	zUpdateSFXTracks			; Do SFX tracks

; =============== S U B	R O U T	I N E =======================================
;
;sub_19E
; zUpdateSFXTracks:
		ld	ix, zTracksSFXStart				; ix = start of SFX track RAM
		ld	b, (zTracksSFXEnd-zTracksSFXStart)/zTrack.len	; Number of channels

zTrackUpdLoop:
		push	bc							; Save bc
		bit	7, (ix+zTrack.PlaybackControl)	; Is track playing?
		call	nz, zUpdateFMorPSGTrack		; Call routine if yes
		ld	de, zTrack.len					; Spacing between tracks
		add	ix, de							; Advance to next track
		pop	bc								; Restore bc
		djnz	zTrackUpdLoop				; Loop for all tracks

		ret
; End of function zUpdateSFXTracks


; =============== S U B	R O U T	I N E =======================================
; Updates FM or PSG track.
;
;sub_1E9
zUpdateFMorPSGTrack:
		bit	7, (ix+zTrack.VoiceControl)		; Is this a PSG channel?
		jp	nz, zUpdatePSGTrack				; Branch if yes
		dec	(ix+zTrack.DurationTimeout)		; Run note timer
		jr	nz, .note_going					; Branch if note hasn't expired yet
		call	zGetNextNote				; Get next note for FM track
		bit	4, (ix+zTrack.PlaybackControl)	; Is track resting?
		ret	nz								; Return if yes
		call	zPrepareModulation			; Initialize modulation
		call	zUpdateFreq					; Add frequency displacement to frequency
		call	zDoModulation				; Apply modulation
		call	zFMSendFreq					; Send frequency to YM2612
		jp	zFMNoteOn						; Note on on all operators
; ---------------------------------------------------------------------------
.note_going:
		bit	4, (ix+zTrack.PlaybackControl)	; Is track resting?
		ret	nz								; Return if yes
		call	zUpdateFreq					; Add frequency displacement to frequency
		bit	6, (ix+zTrack.PlaybackControl)	; Is 'sustain frequency' bit set?
		ret	nz								; Return if yes
		call	zDoModulation				; Apply modulation then fall through
		; Fall through to next function
; End of function zUpdateFMorPSGTrack


; =============== S U B	R O U T	I N E =======================================
; Uploads track's frequency to YM2612.
;
; Input:   ix    Pointer to track RAM
;          hl    Frequency to upload
;          de    For FM3 in special mode, pointer to extra FM3 frequency data (never correctly set)
; Output:  a     Trashed
;          bc    Trashed
;          hl    Trashed
;          de    Increased by 8
;
;sub_22B
zFMSendFreq:
		bit	2, (ix+zTrack.PlaybackControl)	; Is SFX overriding this track?
		ret	nz								; Return if yes

.not_fm3:
		ld	a, 0A4h							; Command to update frequency MSB
		ld	c, h							; High byte of frequency
		rst	zWriteFMIorII				; Send it to YM2612
		ld	a, 0A0h							; Command to update frequency LSB
		ld	c, l							; Low byte of frequency
		rst	zWriteFMIorII					; Send it to YM2612
		ret
; ---------------------------------------------------------------------------

; =============== S U B	R O U T	I N E =======================================
; Gets next note from the track's data stream. If any coordination flags are
; found, they are handled and then the function keeps looping until a note is
; found.
;
; Input:   ix    Pointer to track's RAM
; Output:  de    Pointer to current position on track data
;          hl    Note frequency
;          All others possibly trashed due to coordination flags
;
;sub_277
zGetNextNote:
		ld	e, (ix+zTrack.DataPointerLow)	; e = low byte of track data pointer
		ld	d, (ix+zTrack.DataPointerHigh)	; d = high byte of track data pointer
		res	1, (ix+zTrack.PlaybackControl)	; Clear 'do not attack next note' flag
		res	4, (ix+zTrack.PlaybackControl)	; Clear 'track is at rest' flag

;loc_285
zGetNextNote_cont:
		ld	a, (de)							; Get next byte from track
		inc	de								; Advance pointer
		cp	FirstCoordFlag					; Is it a coordination flag?
		jp	nc, zHandleFMorPSGCoordFlag		; Branch if yes
		ex	af, af'							; Save af
		call	zKeyOffIfActive				; Kill note
		ex	af, af'							; Restore af
		or	a								; Is this a duration?
		jp	p, zStoreDuration				; Branch if yes
		sub	81h								; Make the note into a 0-based index
		jp	p, .got_note					; Branch if it is a note and not a rest
		call	zRestTrack					; Put track at rest
		jr	zGetNoteDuration
; ---------------------------------------------------------------------------
.got_note:
		add	a, (ix+zTrack.Transpose)		; Add in transposition
		ld	hl, zPSGFrequencies				; PSG frequency lookup table
		push	af							; Save af
		rst	PointerTableOffset				; hl = frequency value for note
		pop	af								; Restore af
		bit	7, (ix+zTrack.VoiceControl)		; Is this a PSG track?
		jr	nz, zGotNoteFreq				; Branch if yes
		push	de							; Save de
		ld	d, 8							; Each octave above the first adds this to frequency high bits
		ld	e, 0Ch							; 12 notes per octave
		ex	af, af'							; Exchange af with af'
		xor	a								; Clear a (which will clear a')

.loop:
		ex	af, af'							; Exchange af with af'
		sub	e								; Subtract 1 octave from the note
		jr	c, .got_displacement			; If this is less than zero, we are done
		ex	af, af'							; Exchange af with af'
		add	a, d							; One octave up
		jr	.loop							; Loop
; ---------------------------------------------------------------------------
.got_displacement:
		add	a, e							; Add 1 octave back (so note index is positive)
		ld	hl, zFMFrequencies				; FM first octave frequency lookup table
		rst	PointerTableOffset				; hl = frequency of the note on the first octave
		ex	af, af'							; Exchange af with af'
		or	h								; a = high bits of frequency (including octave bits, which were in a)
		ld	h, a							; h = high bits of frequency (including octave bits)
		pop	de								; Restore de

;loc_2CE
zGotNoteFreq:
		ld	(ix+zTrack.FreqLow), l			; Store low byte of note frequency
		ld	(ix+zTrack.FreqHigh), h			; Store high byte of note frequency

;loc_2D4
zGetNoteDuration:
		ld	a, (de)							; Get duration from the track
		or	a								; Is it an actual duration?
		jp	p, zGotNoteDuration				; Branch if yes
		jr	zFinishTrackUpdate

;loc_307
zGotNoteDuration:
		inc	de								; Advance to next byte in track

;loc_308
zStoreDuration:
		call	zComputeNoteDuration		; Multiply note by tempo divider
		ld	(ix+zTrack.SavedDuration), a	; Store it for next note

;loc_30E
zFinishTrackUpdate:
		ld	(ix+zTrack.DataPointerLow), e	; Save low byte of current location in song
		ld	(ix+zTrack.DataPointerHigh), d	; Save high byte of current location in song
		ld	a, (ix+zTrack.SavedDuration)	; Get current saved duration
		ld	(ix+zTrack.DurationTimeout), a	; Set it as duration timeout
		bit	1, (ix+zTrack.PlaybackControl)	; Is 'do not attack next note' flag set?
		ret	nz								; Return if yes
		xor	a								; Clear a
		ld	(ix+zTrack.ModulationSpeed), a	; Clear modulation speed
		ld	(ix+zTrack.ModulationValLow), a	; Clear low byte of accumulated modulation
		ld	(ix+zTrack.VolEnv), a			; Reset volume envelope
		ret
; End of function zGetNextNote


; =============== S U B	R O U T	I N E =======================================
; This routine multiplies the note duration by the tempo divider. This can
; easily overflow, as the result is stored in a byte.
;
; Input:   a    Note duration
; Output:  a    Final note duration
;          b    zero
;          c    Damaged
;sub_330
zComputeNoteDuration:
		ld	b, (ix+zTrack.TempoDivider)		; Get tempo divider for this track
		dec	b								; Make it into a loop counter
		ret	z								; Return if it was 1
		ld	c, a							; c = a

.loop:
		add	a, c							; a += c
		djnz	.loop						; Loop
		ret
; End of function zComputeNoteDuration


; ---------------------------------------------------------------------------
;loc_342
zFMNoteOn:
		ld	a, (ix+zTrack.FreqLow)			; Get low byte of note frequency
		or	(ix+zTrack.FreqHigh)			; Is the note frequency zero?
		ret	z								; Return if yes
		ld	a, (ix+zTrack.PlaybackControl)	; Get playback control byte for track
		and	16h								; Is either bit 4 ("track at rest") or 2 ("SFX overriding this track") or bit 1 ("do not attack next note") set?
		ret	nz								; Return if yes
		ld	a, (ix+zTrack.VoiceControl)		; Get voice control byte from track
		or	0F0h							; We want only the FM channel assignment bits
		ld	c, a							; Key on for all operators
		ld	a, 28h							; Select key on/of register
		rst	zWriteFMI						; Send command to YM2612
		ret
; ---------------------------------------------------------------------------

; =============== S U B	R O U T	I N E =======================================
; Writes reg/data pair to register 28h (key on/off) if track active
;
; Input:   ix   Track data
; Output:  a    Damaged
;          c    Damaged
;sub_35B
zKeyOffIfActive:
		ld	a, (ix+zTrack.PlaybackControl)	; Get playback control byte for track
		and	6								; Is either bit 1 ("do not attack next note") or 2 ("SFX overriding this track") set?
		ret	nz								; Return if yes
; End of function zKeyOffIfActive

; =============== S U B	R O U T	I N E =======================================
; Writes reg/data pair to register 28h (key on/off)
;
; Input:   ix   Track data
; Output:  a    Damaged
;          c    Damaged
;loc_361
zKeyOff:
		ld	c, (ix+zTrack.VoiceControl)		; Get voice control byte for track (this will turn off all operators as high nibble = 0)
		bit	7, c							; Is this a PSG track?
		ret	nz								; Return if yes
; End of function zKeyOff

; =============== S U B	R O U T	I N E =======================================
; Writes reg/data pair to register 28h (key on/off)
;
; Input:   c    Data to write
; Output:  a    Damaged
;loc_367
zKeyOnOff:
		ld	a, 28h							; Write to KEY ON/OFF port
		res	6, (ix+zTrack.PlaybackControl)	; From Dyna Brothers 2, but in a better place; clear flag to sustain frequency
		rst	zWriteFMI						; Send it
		ret
; End of function zKeyOnOff

; =============== S U B	R O U T	I N E =======================================
; Initializes normal modulation.
;
; Input:   ix    Pointer to track's RAM
; Output:  de    If modulation control has bit 7 set and track is to attack next note, pointer to modulation steps in track RAM
;          hl    If modulation control has bit 7 set and track is to attack next note, pointer to modulation steps in track data
;
;sub_39E
zPrepareModulation:
		bit	7, (ix+zTrack.ModulationCtrl)	; Is modulation on?
		ret	z								; Return if not
		bit	1, (ix+zTrack.PlaybackControl)	; Is 'do not attack next note' bit set?
		ret	nz								; Return if yes
		ld	e, (ix+zTrack.ModulationPtrLow)	; e = low byte of pointer to modulation data
		ld	d, (ix+zTrack.ModulationPtrHigh)	; d = high byte of pointer to modulation data
		push	ix							; Save ix
		pop	hl								; hl = pointer to track data
		ld	b, 0							; b = 0
		ld	c, zTrack.ModulationWait		; c = offset in track RAM for modulation data
		add	hl, bc							; hl = pointer to modulation data in track RAM
		ex	de, hl							; Exchange de and hl
		ldi									; *de++ = *hl++
		ldi									; *de++ = *hl++
		ldi									; *de++ = *hl++
		ld	a, (hl)							; a = number of modulation steps
		srl	a								; Divide by 2
		ld	(de), a							; Store in track RAM
		xor	a								; a = 0
		ld	(ix+zTrack.ModulationValLow), a	; Clear low byte of accumulated modulation
		ld	(ix+zTrack.ModulationValHigh), a	; Clear high byte of accumulated modulation
		ret
; End of function zPrepareModulation


; =============== S U B	R O U T	I N E =======================================
; Applies modulation.
;
; Input:   ix    Pointer to track's RAM
;          hl    Note frequency
; Output:
;    If modulation control is 80h (normal modulation):
;          hl    Final note frequency
;          de    Pointer to modulation data in track RAM
;          iy    Pointer to modulation data in track RAM
;          bc    Unmodulated note frequency
;
;    If modulation control is nonzero and not 80h (modulation envelope effects):
;
;
;sub_3C9
zDoModulation:
		ld	a, (ix+zTrack.ModulationCtrl)	; Get modulation control byte
		or	a								; Is modulation active?
		ret	z								; Return if not
		dec	(ix+zTrack.ModulationWait)		; Decrement modulation wait
		ret	nz								; Return if nonzero
		inc	(ix+zTrack.ModulationWait)		; Increase it back to 1 for next frame
		push	hl							; Save hl
		ld	l, (ix+zTrack.ModulationValLow)	; l = low byte of accumulated modulation
		ld	h, (ix+zTrack.ModulationValHigh)	; h = high byte of accumulated modulation
		; In non-Type 2 DAC versions of SMPS Z80, the following four instructions were below the 'jr nz'
		; which could lead to a bug where iy isn't initialised, but still used as a pointer.
		ld	e, (ix+zTrack.ModulationPtrLow)	; e = low byte of modulation data pointer
		ld	d, (ix+zTrack.ModulationPtrHigh)	; d = high byte of modulation data pointer
		push	de							; Save de
		pop	iy								; iy = pointer to modulation data
		dec	(ix+zTrack.ModulationSpeed)		; Decrement modulation speed
		jr	nz, .mod_sustain				; Branch if nonzero
		ld	a, (iy+1)						; Get original modulation speed
		ld	(ix+zTrack.ModulationSpeed), a	; Reset modulation speed timeout
		ld	a, (ix+zTrack.ModulationDelta)	; Get modulation delta per step
		ld	c, a							; c = modulation delta per step
		rla									; Carry contains sign of delta
		sbc	a, a							; a = 0 or -1 if carry is 0 or 1
		ld	b, a							; bc = sign extension of delta
		add	hl, bc							; hl += bc
		ld	(ix+zTrack.ModulationValLow), l	; Store low byte of accumulated modulation
		ld	(ix+zTrack.ModulationValHigh), h	; Store high byte of accumulated modulation

.mod_sustain:
		pop	bc								; bc = note frequency
		add	hl, bc							; hl = modulated note frequency
		dec	(ix+zTrack.ModulationSteps)		; Reduce number of modulation steps
		ret	nz								; Return if nonzero
		ld	a, (iy+3)						; Get number of steps from track data
		ld	(ix+zTrack.ModulationSteps), a	; Reset modulation steps in track RAM
		ld	a, (ix+zTrack.ModulationDelta)	; Load modulation delta
		neg									; Change its sign
		ld	(ix+zTrack.ModulationDelta), a	; Store it back
		ret

; =============== S U B	R O U T	I N E =======================================
; Adds the current frequency displacement (signed) to note frequency.
;
; Input:   ix    Track RAM
; Output:  hl    Shifted frequency
;          a     Damaged
;          bc    Damaged
;
;sub_46F
;zDoPitchSlide
zUpdateFreq:
		ld	h, (ix+zTrack.FreqHigh)			; h = high byte of note frequency
		ld	l, (ix+zTrack.FreqLow)			; l = low byte of note frequency
		ld	a, (ix+zTrack.Detune)			; a = detune
		ld	c, a							; bc = sign extension of frequency displacement
		rla									; Carry contains sign of frequency displacement
		sbc	a, a							; a = 0 or -1 if carry is 0 or 1
		ld	b, a							; bc = sign extension of frequency displacement
		add	hl, bc							; Add to frequency
		ret
; End of function zUpdateFreq

; =============== S U B	R O U T	I N E =======================================
; Gets offset for requested FM instrument.
;
;sub_483
zGetFMInstrumentPointer:
		ld	l, (ix+zTrack.VoicesLow)		; l = low byte of track's voice pointer
		ld	h, (ix+zTrack.VoicesHigh)		; h = high byte of track's voice pointer
		or	a								; Is FM instrument the first one (zero)?
		ret	z								; Return if so
		ld	b, a
		ld	de, 25							; Size of each FM instrument

.loop:
		add	hl, de							; Advance pointer to next instrument
		djnz	.loop						; Loop until instrument offset is found
		ret
; End of function zGetFMInstrumentPointer

; ---------------------------------------------------------------------------
;loc_49C
zFMInstrumentRegTable:
		db 0B0h								; Feedback/Algorithm
zFMInstrumentOperatorTable:
		db  30h								; Detune/multiple operator 1
		db  38h								; Detune/multiple operator 3
		db  34h								; Detune/multiple operator 2
		db  3Ch								; Detune/multiple operator 4
zFMInstrumentRSARTable:
		db  50h								; Rate scaling/attack rate operator 1
		db  58h								; Rate scaling/attack rate operator 3
		db  54h								; Rate scaling/attack rate operator 2
		db  5Ch								; Rate scaling/attack rate operator 4
zFMInstrumentAMD1RTable:
		db  60h								; Amplitude modulation/first decay rate operator 1
		db  68h								; Amplitude modulation/first decay rate operator 3
		db  64h								; Amplitude modulation/first decay rate operator 2
		db  6Ch								; Amplitude modulation/first decay rate operator 4
zFMInstrumentD2RTable:
		db  70h								; Secondary decay rate operator 1
		db  78h								; Secondary decay rate operator 3
		db  74h								; Secondary decay rate operator 2
		db  7Ch								; Secondary decay rate operator 4
zFMInstrumentD1LRRTable:
		db  80h								; Secondary amplitude/release rate operator 1
		db  88h								; Secondary amplitude/release rate operator 3
		db  84h								; Secondary amplitude/release rate operator 2
		db  8Ch								; Secondary amplitude/release rate operator 4
zFMInstrumentOperatorTable_End
;loc_4B1
zFMInstrumentTLTable:
		db  40h								; Total level operator 1
		db  48h								; Total level operator 3
		db  44h								; Total level operator 2
		db  4Ch								; Total level operator 4
zFMInstrumentTLTable_End

; =============== S U B	R O U T	I N E =======================================
; Subroutine to send FM instrument data to YM2612 chip.
;
;sub_4B9
zSendFMInstrument:
		ld	de, zFMInstrumentRegTable		; de = pointer to register output table
		ld	c, (ix+zTrack.AMSFMSPan)		; Send track AMS/FMS/panning
		ld	a, 0B4h							; Select AMS/FMS/panning register
		rst	zWriteFMIorII				; Set track data
		call	zSendFMInstrData			; Send data to register

		; Start with detune/multiplier operators
		ld	b, zFMInstrumentRSARTable-zFMInstrumentOperatorTable	; Number of commands to issue

.loop1:
		call	zSendFMInstrData			; Send FM instrument data
		djnz	.loop1						; Loop

		; Now for rate scaling/attack rate. The attack rate must be 1Fh if using
		; SSG-EG, which is the reason for the split.
		ld	b, zFMInstrumentAMD1RTable-zFMInstrumentRSARTable	; Number of commands to issue

.loop2:
		call	zSendFMInstrData		; Send FM instrument data
		djnz	.loop2						; Loop

		; Finalize with all the other operators.
		ld	b, zFMInstrumentOperatorTable_End-zFMInstrumentAMD1RTable	; Number of commands to issue

.loop3:
		call	zSendFMInstrData			; Send FM instrument data
		djnz	.loop3						; Loop
		ld	(ix+zTrack.TLPtrLow), l			; Save low byte of pointer to (not yet uploaded) TL data
		ld	(ix+zTrack.TLPtrHigh), h		; Save high byte of pointer to (not yet uploaded) TL data
		jp	zSendTL							; Send TL data
; End of function zSendFMInstrument

; =============== S U B	R O U T	I N E =======================================
; Sends FM instrument data to YM2612.
;
; Input:   ix    Track data
;          hl    Pointer to instrument data
;          de    Pointer to register selector table
; Output:   a    Value written to the register
;           c    Value written to the register
;
;sub_4DA
zSendFMInstrData:
		ld	a, (de)							; Get register output
		inc	de								; Advance pointer
		ld	c, (hl)							; Get value from instrument RAM
		inc	hl								; Advance pointer
		rst	zWriteFMIorII					; Write track data
		ret
; End of function zSendFMInstrData


; ===========================================================================
; Type Check
; ===========================================================================
; 1-32, DC = Music
; 33-DB, DD-DF = SFX
; E1-E6 = Fade Effects
; FF = SEGA Scream

; TypeCheck:
;sub_4FB
zPlaySoundByIndex:
		ld	a, (zNextSound)					; a = next sound to play
		cp	SndID__First						; Is this a music?
		ret	c					; Branch if yes
		cp	SndID__End						; Is this a sound effect?
		jr	c, zPlaySound_CheckRing			; Branch if yes
		jp	zStopAllSound							; Handle fade effect
; End of function zSilenceStopTrack
; ---------------------------------------------------------------------------
; =============== S U B	R O U T	I N E =======================================
; Clears next sound to play.
;sub_690
zClearNextSound:
		xor	a
		ld	(zNextSound), a
		ret
; End of function zClearNextSound

; =============== S U B	R O U T	I N E =======================================
;
;sub_78F
zGetSFXChannelPointers:
		bit	7, c							; Is this a PSG track?
		jr	nz, .is_psg						; Branch if yes
		ld	a, c							; a = c
		jr	.get_ptrs
; ---------------------------------------------------------------------------
.is_psg:
		call	zSilencePSGChannel			; Silence channel at ix
		ld	a, c							; a = channel identifier
		; Shift high 3 bits to low bits so that we can convert it to a table index
		rlca
		rlca
		rlca
		and	7
		add	a, 3							; Compensate for subtraction below

.get_ptrs:
		ld	hl, zSFXChannelData				; Pointer table for track RAM
		rst	PointerTableOffset				; hl = track RAM
		push	hl							; Save hl
		pop	ix								; ix = track RAM
		ret
; End of function zGetSFXChannelPointers

zSFXChannelData:
		dw  zSFX_FM1						; FM1
		dw  zSFX_FM2						; FM2
		dw  zSFX_FM3						; FM3
		dw  0000h
		dw  zSFX_FM4						; FM4
		dw  zSFX_FM5						; FM5
		dw  zSFX_FM6						; FM6 or DAC
		dw  zSFX_PSG1						; PSG1
		dw  zSFX_PSG2						; PSG2
		dw  zSFX_PSG3						; PSG3
		dw  zSFX_PSG3						; PSG3/Noise
		
; ---------------------------------------------------------------------------
;loc_6A9
zPlaySound_CheckRing:
		cp	95h
		jr	nz, +
		ld	a, (zRingSpeaker)
		xor	1
		ld	(zRingSpeaker), a	; so toggle (00	-> 80, L -> R)
		ld	a, 95h
		jr	z, +
		ld	a, 0A8h
+
		sub	SndID__First					; Make it a 0-based index
		ld	hl, z80_SFXPointers				; SFX table index
		rst	PointerTableOffset				; hl = pointer to SFX data
		push	hl							; Save hl
		rst	ReadPointer						; hl = voice table pointer
		ld	(zSFXVoiceTblPtr), hl			; Save to RAM
		pop	hl								; hl = pointer to SFX data
		push	hl							; Save it again
		pop	iy								; iy = pointer to SFX data
		ld	a, (iy+2)						; a = tempo divider
		ld	(zSFXTempoDivider), a			; Save to RAM
		ld	de, 4							; de = 4
		add	hl, de							; hl = pointer to SFX track data
		ld	b, (iy+3)						; b = number of tracks (FM + PSG) used by SFX

;loc_72C
zSFXTrackInitLoop:
		push	bc							; Save bc; damaged by ldi instructions below
		push	hl
		
		ld	c, (hl)							; c = channel identifier
		call	zGetSFXChannelPointers		; Get track pointers for track RAM (ix) and overridden song track (hl)

		pop	hl
		push	ix							; Save pointer to SFX track data in RAM
		pop	de							; de = pointer to SFX track data in RAM (unless you consider the above effectively dead code)
		ld	a,80h
		ld	(de),a									; *de++ = *hl++ (initial playback control)
		inc	de
		ld	a, (de)							; Get the voice control byte from track RAM (to deal with SFX already there)
		ldi									; *de++ = *hl++ (copy channel identifier)
		ld	a, (zSFXTempoDivider)			; Get SFX tempo divider
		ld	(de), a							; Store it to RAM
		inc	de								; Advance pointer
		ldi									; *de++ = *hl++ (low byte of channel data pointer)
		ldi									; *de++ = *hl++ (high byte of channel data pointer)
		ldi									; *de++ = *hl++ (key displacement)
		ldi									; *de++ = *hl++ (channel volume)
		call	zInitFMDACTrack				; Init the remainder of the track RAM


		push	hl							; Save hl
		ld	hl, (zSFXVoiceTblPtr)			; hl = pointer to voice data

		ld	(ix+zTrack.VoicesLow), l		; Low byte of voice pointer
		ld	(ix+zTrack.VoicesHigh), h		; High byte of voice pointer
		call	zKeyOffIfActive				; Kill channel notes
		bit	7, (ix+zTrack.VoiceControl)		; Is this an FM track?
		call	z, zFMClearSSGEGOps			; If so, clear SSG-EG operators for track's channels
		pop		hl							; Restore hl
		pop		bc							; Restore bc
		djnz	zSFXTrackInitLoop			; Loop for all SFX tracks
		jp	zClearNextSound

; =============== S U B	R O U T	I N E =======================================
;
; =============== S U B	R O U T	I N E =======================================
;
;sub_7C5
zInitFMDACTrack:
		ex	af, af'							; Save af
		xor	a								; a = 0
		ld	(de), a							; Set modulation to inactive
		inc	de								; Advance to next byte
		ld	(de), a							; Set FM instrument/PSG tone to zero too
		inc	de								; Advance to next byte again
		ex	af, af'							; Restore af

;loc_7CC
zZeroFillTrackRAM:
		ex	de, hl							; Exchange the contents of de and hl
		ld	(hl), zTrack.len				; Call subroutine stack pointer
		inc	hl								; Advance to next byte
		ld	(hl), 0C0h						; default Panning / AMS / FMS settings (only stereo L/R enabled)
		inc	hl								; Advance to next byte
		ld	(hl), 1							; Current note duration timeout

		ld	b, zTrack.len-zTrack.DurationTimeout-1	; Loop counter

.loop:
		inc	hl								; Advance to next byte
		ld	(hl), 0							; Put 0 into this byte
		djnz	.loop						; Loop until end of track

		inc	hl								; Make hl point to next track
		ex	de, hl							; Exchange the contents of de and hl
		ret
; End of function zInitFMDACTrack


; =============== S U B	R O U T	I N E =======================================
; Wipes music data and fades all FM, PSG and DAC channels.
;sub_944 zMusicFade
zStopAllSound:
		; The following block sets to zero the z80 RAM from 1C0Dh to 1FD4h
		ld	hl, zTempVariablesStart				; Starting source address for copy
		ld	de, zTempVariablesStart+1					; Starting destination address for copy
		ld	bc, zTempVariablesEnd-zTempVariablesStart-1	; Length of copy
		ld	(hl), 0							; Initial value of zero
		ldir								; while (--length) *de++ = *hl++

		ld	ix, zFMDACInitBytes-1				; Initialization data for channels
		ld	b, 6	; Number of FM channels

.loop:
		push	bc							; Save bc for loop

		call	zSetMaxRelRate
		ld	a, 40h							; Set total level...
		ld	c, 7Fh							; ... to minimum envelope amplitude...
		call	zFMOperatorWriteLoop		; ... for all operators of this track's channel
		ld	a, 28h							; Write to KEY ON/OFF port
		ld	c, (ix+zTrack.VoiceControl)		; Send key off
		rst	zWriteFMI						; Send it

		call	zFMClearSSGEGOps			; Clears the SSG-EG operators for this channel
		inc	ix								; Go to next channel byte
		pop	bc								; Restore bc for loop counter
		djnz	.loop						; Loop while b > 0

		call	zPSGSilenceAll				; Silence PSG

		jp	zClearNextSound
; End of function zStopAllSound

zFMDACInitBytes:
		db   0,1,2,4,5,6

; =============== S U B	R O U T	I N E =======================================
; Sets the SSG-EG registers (90h+) for all operators on this track to 0.
;
; Input:  ix    Pointer to track RAM
; Output: a     Damaged
;         b     Damaged
;         c     Damaged
;sub_986
zFMClearSSGEGOps:
		ld	a, 90h							; Set SSG-EG registers...
		ld	c, 0							; ... set to zero (as docs say it should)...
		jp	zFMOperatorWriteLoop			; ... for all operators of this track's channel
; End of function zFMClearSSGEGOps


; =============== S U B	R O U T	I N E =======================================
; Silences all PSG channels, including the noise channel.
;
; Output: a    Damaged
;sub_9BC
zPSGSilenceAll:
		push	bc							; Save bc
		ld	b, 4	; Loop 4 times: 3 PSG channels + noise channel
		ld	a, 9Fh							; Command to silence PSG1

.loop:
		ld	(zPSG), a						; Write command
		add	a, 20h							; Next channel
		djnz	.loop						; Loop for all PSG channels
		pop	bc								; Restore bc
		jp	zClearNextSound
; End of function zPSGSilenceAll


zDoSoundQueue:
		ld	de, zMusicNumber
		call	zDoOneSndQueue
		call	zDoOneSndQueue
; End of function DoSoundQueue


; =============== S U B	R O U T	I N E =======================================


zDoOneSndQueue:
		ld	a, (de)
		sub	SndID__First
		ret	c;, .remove_sound_from_queue
		ld	hl, zSndPriorities
		ld	c, a
		ld	b, 0
		add	hl, bc
		ld	a, (zCurrentPriority)
		cp	(hl)
		jr	z, .queue_sound
		jr	nc, .remove_sound_from_queue

.queue_sound:
		ld	a, (de)
		ld	(zNextSound), a
		ld	a, (hl)
		and	7Fh
		ld	(zCurrentPriority), a

.remove_sound_from_queue:
		xor	a
		ld	(de), a
		inc	de
		ret
; End of function DoOneSndQueue

; =============== S U B	R O U T	I N E =======================================
; Sets D1L to minimum and RR to maximum for all operators on this track's
; channel.
;
; Input:  ix    Pointer to track RAM
; Output: a     Damaged
;         b     Damaged
;         c     Damaged
;sub_A06
;zSetFMMinD1LRR
zSetMaxRelRate:
		ld	a, 80h							; Set D1L to minimum and RR to maximum...
		ld	c, 0FFh							; ... for all operators on this track's channel (fall through)
; End of function zSetMaxRelRate


; =============== S U B	R O U T	I N E =======================================
; Loops through all of a channel's operators and sets them to the desired value.
;
; Input:  ix    Pointer to track RAM
;         a     YM2612 register to write to
;         c     Value to write to register
; Output: b     Damaged
;sub_A0A
zFMOperatorWriteLoop:
		ld	b, 4							; Loop 4 times

.loop:
		push	af							; Save af
		rst	zWriteFMIorII				; Write to part I or II, as appropriate
		pop	af								; Restore af
		add	a, 4							; a += 4
		djnz	.loop						; Loop
		ret
; End of function zFMOperatorWriteLoop
; ---------------------------------------------------------------------------
;loc_AA5
zPSGFrequencies:
		; This table starts with 12 notes not in S1 or S2:
		dw 3FFh,3FFh,3FFh,3FFh,3FFh,3FFh,3FFh,3FFh,3FFh,3F7h,3BEh,388h
		; The following notes are present on S1 and S2 too:
		dw 356h,326h,2F9h,2CEh,2A5h,280h,25Ch,23Ah,21Ah,1FBh,1DFh,1C4h
		dw 1ABh,193h,17Dh,167h,153h,140h,12Eh,11Dh,10Dh,0FEh,0EFh,0E2h
		dw 0D6h,0C9h,0BEh,0B4h,0A9h,0A0h,097h,08Fh,087h,07Fh,078h,071h
		dw 06Bh,065h,05Fh,05Ah,055h,050h,04Bh,047h,043h,040h,03Ch,039h
		dw 036h,033h,030h,02Dh,02Bh,028h,026h,024h,022h,020h,01Fh,01Dh
		dw 01Bh,01Ah,018h,017h,016h,015h,013h,012h,011h,010h,000h,000h
		; Then, it falls through to the 12 base notes from FM octaves.
;loc_B4D
zFMFrequencies:
		dw 284h,2ABh,2D3h,2FEh,32Dh,35Ch,38Fh,3C5h,3FFh,43Ch,47Ch,4C0h
; ---------------------------------------------------------------------------
; ---------------------------------------------------------------------------
;loc_BED
zHandleFMorPSGCoordFlag:
		ld	hl, loc_BF9						; hl = desired return address

;loc_BF0
zHandleCoordFlag:
		push	hl							; Set return location (ret) to location stored in hl
		sub	FirstCoordFlag					; Make it a zero-based index
		ld	hl, zCoordFlagSwitchTable		; Load switch table into hl
		rst	PointerTableOffset				; hl = pointer to target location
		ld	a, (de)							; a = coordination flag parameter
		jp	(hl)							; Indirect jump to coordination flag handler
; End of function zUpdateDACTrack
; ---------------------------------------------------------------------------
loc_BF9:
		inc	de								; Advance to next byte in track
		jp	zGetNextNote_cont		; Continue processing FM/PSG track
; ---------------------------------------------------------------------------
;loc_BFD
zCoordFlagSwitchTable:
		dw cfPanningAMSFMS					; 0E0h
		dw cfDetune							; 0E1h
		dw cfSetComm				; 0E2h
		dw cfSetVolume						; 0E4h
		dw cfChangeVolume2					; 0E5h
		dw cfChangeVolume					; 0E6h
		dw cfPreventAttack					; 0E7h
		dw cfActualConditionalJump;cfPlayDACSample					; 0EAh
		dw cfChangePSGVolume				; 0ECh
		dw cfSetKey							; 0EDh
		dw cfSetVoice						; 0EFh
		dw cfModulation						; 0F0h
		dw cfStopTrack						; 0F2h
		dw cfSetPSGNoise					; 0F3h
		dw cfSetPSGVolEnv					; 0F5h
		dw cfJumpTo							; 0F6h
		dw cfRepeatAtPos					; 0F7h
		dw cfJumpToGosub					; 0F8h
		dw cfJumpReturn						; 0F9h
		dw cfDisableModulation				; 0FAh
		dw cfChangeTransposition			; 0FBh

cfActualConditionalJump:
		ld	a, (zConditionalJumpFlag)
		or	a
		jp	z, cfJumpTo
		inc	de
		ret
; =============== S U B	R O U T	I N E =======================================
; Sets panning for track. By accident, you can sometimes set AMS and FMS
; flags -- but only if the bits in question were zero.
;
; Has one parameter byte, the AMS/FMS/panning bits.
;
;sub_C51
cfPanningAMSFMS:
		ld	c, 3Fh							; Mask for all but panning

zDoChangePan:
		ld	a, (ix+zTrack.AMSFMSPan)		; Get current AMS/FMS/panning
		and	c								; Mask out L+R
		push	de							; Store de
		ex	de, hl							; Exchange de and hl
		or	(hl)							; Mask in the new panning; may also add AMS/FMS
		ld	(ix+zTrack.AMSFMSPan), a		; Store new value in track RAM
		ld	c, a							; c = new AMS/FMS/panning
		ld	a, 0B4h							; a = YM2612 register to write to
		rst	zWriteFMIorII				; Set new panning/AMS/FMS
		pop	de								; Restore de
		ret
; End of function cfPanningAMSFMS

; =============== S U B	R O U T	I N E =======================================
; Sets frequency displacement (signed). The final note frequency is shifted
; by this value.
;
; Has one parameter byte, the new frequency displacement.
;
;sub_C77 cfAlterNoteFreq
cfDetune:
		ld	(ix+zTrack.Detune), a	; Set detune
		ret
; End of function cfDetune


; =============== S U B	R O U T	I N E =======================================
; Fade in to previous song.
;
; Has one parameter byte. If the parameter byte if FFh, the engine will fade
; to the previous song. If the parameter byte is equal to 29h (1-Up ID - 1),
; the driver will prevent new music or SFX from playing as long as the 1-Up
; music is playing (but will not clear the M68K queue). For all other values,
; the queue will work as normal, but no fade-in will be done.
;
;sub_C7B
cfSetComm:
		ld	(zCommunication), a
		ret
; End of function cfFadeInToPrevious


; =============== S U B	R O U T	I N E =======================================
; Sets track volume.
;
; Has one parameter byte, the volume.
;
; For FM tracks, this is a 7-bit value from 0 (lowest volume) to 127 (highest
; volume). The value is XOR'ed with 7Fh before being sent, then stripped of the
; sign bit. The volume change takes effect immediately.
;
; For PSG tracks, this is a 4-bit value ranging from 8 (lowest volume) to 78h
; (highest volume). The value is shifted 3 bits to the right, XOR'ed with 0Fh
; and AND'ed with 0Fh before being uploaded, so the sign bit and the lower 3
; bits are discarded.
;
;loc_C85
cfSetVolume:
		bit	7, (ix+zTrack.VoiceControl)		; Is this a PSG channel?
		jr	z, .not_psg						; Branch if not
		; The following code gets bits 3, 4, 5 and 6 from the parameter byte,
		; puts them in positions 0 to 3 and inverts them, discarding all other
		; bits in the parameter byte.
		; Shift the parameter byte 3 bits to the right
		srl	a
		srl	a
		srl	a
		xor	0Fh								; Invert lower nibble's bits
		and	0Fh								; Clear out high nibble
		jp	zStoreTrackVolume
; ---------------------------------------------------------------------------
.not_psg:
		xor	7Fh								; Invert parameter byte (except irrelevant sign bit)
		and	7Fh								; Strip sign bit
		ld	(ix+zTrack.Volume), a			; Set as new track volume
		jr	zSendTL							; Begin using new volume immediately

; =============== S U B	R O U T	I N E =======================================
; Change track volume for a FM track.
;
; Has two parameter bytes: the first byte is ignored, the second is the signed
; change in volume. Positive lowers volume, negative increases it.
;
;loc_CA1
cfChangeVolume2:
		inc	de								; Advance pointer
		ld	a, (de)							; Get change in volume then fall-through


; =============== S U B	R O U T	I N E =======================================
; Change track volume for a FM track.
;
; Has one parameter byte, the signed change in volume. Positive lowers volume,
; negative increases it.
;
;loc_CA3
cfChangeVolume:
		; S2 places this check further down (and S1 lacks it altogether),
		; allowing PSG channels to change their volume. This means the
		; likes of S2's SFX $F0 will sound different in this driver
		bit	7, (ix+zTrack.VoiceControl)		; Is this a PSG track?
		ret	nz								; Return if yes
		add	a, (ix+zTrack.Volume)			; Add in track's current volume
		jp	p, .set_vol						; Branch if result is still positive
		jp	pe, .underflow					; Branch if addition overflowed into more than 127 positive
		xor	a								; Set maximum volume
		jp	.set_vol
; ---------------------------------------------------------------------------
.underflow:
		ld	a, 7Fh							; Set minimum volume

.set_vol:
		ld	(ix+zTrack.Volume), a			; Store new volume

; =============== S U B	R O U T	I N E =======================================
; Subroutine to send TL information to YM2612.
;
;sub_CBA
zSendTL:
		push	de							; Save de
		ld	de, zFMInstrumentTLTable		; de = pointer to FM TL register table
		ld	l, (ix+zTrack.TLPtrLow)			; l = low byte of pointer to instrument's TL data
		ld	h, (ix+zTrack.TLPtrHigh)		; h = high byte of pointer to instrument's TL data
		ld	b, zFMInstrumentTLTable_End-zFMInstrumentTLTable	; Number of entries

.loop:
		ld	a, (hl)							; a = register data
		or	a								; Is it positive?
		jp	p, .skip_track_vol				; Branch if yes
		add	a, (ix+zTrack.Volume)			; Add track's volume to it

.skip_track_vol:
		and	7Fh								; Strip sign bit
		ld	c, a							; c = new volume for operator
		ld	a, (de)							; a = register write command
		rst	zWriteFMIorII				; Send it to YM2612
		inc	de								; Advance pointer
		inc	hl								; Advance pointer
		djnz	.loop						; Loop

		pop	de								; Restore de
		ret
; End of function zSendTL

; =============== S U B	R O U T	I N E =======================================
; Prevents next note from attacking.
;
; Has no parameter bytes.
;
;loc_CDB
cfPreventAttack:
		set	1, (ix+zTrack.PlaybackControl)	; Set flag to prevent attack
		dec	de								; Put parameter byte back
		ret


; =============== S U B	R O U T	I N E =======================================
; Change PSG volume. Has no effect on FM or DAC channels.
;
; Has one parameter byte, the change in volume. The value is signed, but any
; result greater than 0Fh (or lower than 0) will result in no output.
;
;loc_D01
cfChangePSGVolume:
		bit	7, (ix+zTrack.VoiceControl)		; Is this a PSG channel?
		ret	z								; Return if not
		res	4, (ix+zTrack.PlaybackControl)	; Clear 'track is resting' flag
		dec	(ix+zTrack.VolEnv)				; Decrement envelope index
		add	a, (ix+zTrack.Volume)			; Add track's current volume
		cp	0Fh								; Is it 0Fh or more?
		jp	c, zStoreTrackVolume			; Branch if not
		ld	a, 0Fh							; Limit to 0Fh (silence)

;loc_D17
zStoreTrackVolume:
		ld	(ix+zTrack.Volume), a			; Store new volume
		ret

; =============== S U B	R O U T	I N E =======================================
; Changes the track's key displacement.
;
; There is a single parameter byte, the new track key offset + 40h (that is,
; 40h is subtracted from the parameter byte before the key displacement is set)
;
;loc_D1B
cfSetKey:
		sub	40h								; Subtract 40h from key displacement
		ld	(ix+zTrack.Transpose), a		; Store result as new transposition
		ret


; =============== S U B	R O U T	I N E =======================================
; Change current instrument (FM), tone (PSG) or sample (DAC).
;
; Has either a single positive parameter byte or a pair of parameter bytes of
; which the first is negative.
;
; If positive, the first parameter byte is the index of voice to use.
;
; If negative, and on a PSG track, then the first parameter byte is the index
; of voice to use while the second parameter byte is ignored.
;
; If negative and on a FM or DAC track, the first parameter byte is 80h + index
; of voice to use, while the second parameter byte is 7Fh + index of song whose
; voice bank is to be used (here, the AIZ1 song is index 1, not zero).
;
; The uploading of an FM instrument is irrelevant for the DAC.
;
;loc_D2E
cfSetVoice:
		bit	7, (ix+zTrack.VoiceControl)		; Is this a PSG track?
		jr	nz, zSetVoicePSG				; Branch if yes
		call	zSetMaxRelRate				; Set minimum D1L/RR for channel
		ld	a, (de)							; Get voice index
		ld	(ix+zTrack.VoiceIndex), a		; Store to track RAM
		push	de							; Save de
		call	zGetFMInstrumentPointer		; hl = pointer to instrument data

zSetVoiceDoUpload:
		call	zSendFMInstrument			; Upload instrument data to YM2612
		pop	de								; Restore de
		ret
; End of function cfSetVoice
; ---------------------------------------------------------------------------
;loc_D64:
zSetVoicePSG:
		or	a								; Is the voice index positive?
		jp	p, cfStoreNewVoice				; Branch if yes
		inc	de								; Otherwise, advance song data pointer
		jp	cfStoreNewVoice

; =============== S U B	R O U T	I N E =======================================
; Turns on modulation on the channel.
;
; Has four 1-byte parameters: delay before modulation starts, modulation speed,
; modulation change per step, number of steps in modulation.
;
;loc_D6D
cfModulation:
		ld	(ix+zTrack.ModulationPtrLow), e		; Store low byte of modulation data pointer
		ld	(ix+zTrack.ModulationPtrHigh), d	; Store high byte of modulation data pointer
		ld	(ix+zTrack.ModulationCtrl), 80h	; Toggle modulation on
		inc	de								; Advance pointer...
		inc	de								; ... again...
		inc	de								; ... and again.
		ret


; =============== S U B	R O U T	I N E =======================================
; Stops the current track.
;
; Technically, it has a parameter byte, but it is irrelevant and unused.
;
;loc_D87
cfStopTrack:
		res	7, (ix+zTrack.PlaybackControl)	; Clear 'track playing' flag
		call	zKeyOffIfActive				; Send key off for track channel
		bit	7, (ix+zTrack.VoiceControl)
		call	nz, zSilencePSGChannel
		xor	a
		ld	(zCurrentPriority), a
		pop	hl								; Pop return value from stack
		pop	hl								; Pop another return value from stack
		ret
; =============== S U B	R O U T	I N E =======================================
; Change PSG noise to parameter, and silences PSG3 channel.
;
; Has one parameter byte: if zero, the channel is changed back to a normal PSG
; channel and the noise is silenced; if non-zero, it must be in the 0E0h-0E7h
; range, and sets the noise type to use (and sets the channel as being a noise
; channel).
;
;loc_E39
cfSetPSGNoise:
		bit	7, (ix+zTrack.VoiceControl)		; Is this a PSG track?
		ret	z								; Return if not
		ld	(ix+zTrack.PSGNoise), a			; Store to track RAM
		set	0, (ix+zTrack.PlaybackControl)	; Mark PSG track as being noise
		or	a								; Test noise value
		ld	a, 0DFh							; Command to silence PSG3
		jr	nz, .skip_noise_silence			; If nonzero, branch
		res	0, (ix+zTrack.PlaybackControl)	; Otherwise, mark track as not being a noise channel
		ld	a, 0FFh							; Command to silence the noise channel

.skip_noise_silence:
		bit	2, (ix+zTrack.PlaybackControl)	; Is SFX overriding this track?
		ret	nz								; Return if yes
		ld	(zPSG), a						; Execute it
		ld	a, (de)							; Get PSG noise value
		ld	(zPSG), a						; Send command to PSG
		ret

; =============== S U B	R O U T	I N E =======================================
; Set PSG tone.
;
; Has one parameter byte, the new PSG tone to use.
;
;loc_E58
;cfSetPSGTone
cfSetPSGVolEnv:
		bit	7, (ix+zTrack.VoiceControl)		; Is this a PSG track?
		ret	z								; Return if not

;loc_E5D
cfStoreNewVoice:
		ld	(ix+zTrack.VoiceIndex), a		; Store voice
		ret

; =============== S U B	R O U T	I N E =======================================
; Jump to specified location.
;
; Has a 2-byte parameter, indicating target location for jump.
;
;loc_E61
cfJumpTo:
		ex	de, hl							; Exchange de and hl
		ld	e, (hl)							; e = low byte of target location
		inc	hl								; Advance pointer
		ld	d, (hl)							; d = high byte of target location
		dec	de								; Put destination byte back
		ret

; =============== S U B	R O U T	I N E =======================================
; Loop section of music.
;
; Has four parameter bytes: a loop counter index (exactly like coord. flag 0EBh),
; a repeat count, and a 2-byte jump target.
;
;loc_E67
cfRepeatAtPos:
		inc	de								; Advance track pointer
		add	a, zTrack.LoopCounters			; Add offset into loop counters
		ld	c, a							; c = offset of current loop counter
		ld	b, 0							; bc = sign-extended offset to current loop counter
		push	ix							; Save track RAM pointer
		pop	hl								; hl = pointer to track RAM
		add	hl, bc							; hl = pointer in RAM to current loop counter
		ld	a, (hl)							; a = value of current loop counter
		or	a								; Is loop counter zero?
		jr	nz, .run_counter				; Branch if not
		ld	a, (de)							; Get repeat counter
		ld	(hl), a							; Reset loop counter to it

.run_counter:
		inc	de								; Advance track pointer
		dec	(hl)							; Decrement loop counter
		jp	nz, cfJumpTo					; Loop if it is nonzero
		inc	de								; Advance track pointer
		ret

; =============== S U B	R O U T	I N E =======================================
; Call subroutine. Stores current location on track-specific stack so that
; coord. flag 0F9h can be used to return to current location.
;
; Has one 2-byte parameter, the target subroutine's address.
;
;loc_E7E
cfJumpToGosub:
		ld	c, a							; c = low byte of target address
		inc	de								; Advance track pointer
		ld	a, (de)							; a = high byte of target address
		ld	b, a							; bc = target address
		push	bc							; Save bc
		push	ix							; Save ix
		pop	hl								; hl = pointer to track RAM
		dec	(ix+zTrack.StackPointer)		; Decrement track stack pointer
		ld	c, (ix+zTrack.StackPointer)		; c = track stack pointer
		dec	(ix+zTrack.StackPointer)		; Decrement track stack pointer again
		ld	b, 0							; b = 0
		add	hl, bc							; hl = offset of high byte of return address
		ld	(hl), d							; Store high byte of return address
		dec	hl								; Move pointer to target location
		ld	(hl), e							; Store low byte of return address
		pop	de								; de = jump target address
		dec	de								; Put back the byte
		ret

; =============== S U B	R O U T	I N E =======================================
; Returns from subroutine call. Does NOT check for stack overflows!
;
; Has no parameter bytes.
;
;loc_E98
cfJumpReturn:
		push	ix							; Save track RAM address
		pop	hl								; hl = pointer to track RAM
		ld	c, (ix+zTrack.StackPointer)		; c = offset to top of return stack
		ld	b, 0							; b = 0
		add	hl, bc							; hl = pointer to top of return stack
		ld	e, (hl)							; e = low byte of return address
		inc	hl								; Advance pointer
		ld	d, (hl)							; de = return address
		inc	(ix+zTrack.StackPointer)		; Pop byte from return stack
		inc	(ix+zTrack.StackPointer)		; Pop byte from return stack
		ret

; =============== S U B	R O U T	I N E =======================================
; Clears sign bit of modulation control, disabling normal modulation.
;
; Has no parameter bytes.
;
;loc_EAB
cfDisableModulation:
		res	7, (ix+zTrack.ModulationCtrl)	; Clear bit 7 of modulation control
		dec	de								; Put byte back
		ret

; =============== S U B	R O U T	I N E =======================================
; Adds a signed value to channel key displacement.
;
; Has one parameter byte, the change in channel key displacement.
;
;loc_EB1 cfAddKey
cfChangeTransposition:
		add	a, (ix+zTrack.Transpose)		; Add current transposition to parameter
		ld	(ix+zTrack.Transpose), a		; Store result as new transposition
		ret



; =============== S U B	R O U T	I N E =======================================
; Updates a PSG track.
;
; Input:   ix    Pointer to track RAM
;
;loc_FC4
zUpdatePSGTrack:
		dec	(ix+zTrack.DurationTimeout)		; Run note timer
		jr	nz, .note_going					; Branch if note hasn't expired yet
		call	zGetNextNote				; Get next note for PSG track
		bit	4, (ix+zTrack.PlaybackControl)	; Is track resting?
		ret	nz								; Return if yes
		call	zPrepareModulation			; Initialize modulation
; ---------------------------------------------------------------------------
.note_going:
		call	zUpdateFreq					; Add frequency displacement to frequency
		call	zDoModulation				; Do modulation
		bit	2, (ix+zTrack.PlaybackControl)	; Is SFX overriding this track?
		ret	nz								; Return if yes
		ld	c, (ix+zTrack.VoiceControl)		; c = voice control byte
		ld	a, l							; a = low byte of new frequency
		and	0Fh								; Get only lower nibble
		or	c								; OR in PSG channel bits
		ld	(zPSG), a						; Send to PSG, latching register
		ld	a, l							; a = low byte of new frequency
		and	0F0h							; Get high nibble now
		or	h								; OR in the high byte of the new frequency
		; Swap nibbles
		rrca
		rrca
		rrca
		rrca
		ld	(zPSG), a						; Send to PSG, to latched register
		ld	a, (ix+zTrack.VoiceIndex)		; Get PSG tone
		or	a								; Test if it is zero
		ld	c, 0							; c = 0
		jr	z, .no_volenv					; Branch if no PSG tone
		dec	a								; Make it into a 0-based index
		ld	hl, z80_VolEnvPointers			; Value for volume envelope pointer table
		rst	PointerTableOffset				; hl = pointer to volume envelope for track
		call	zDoVolEnv					; Get new volume envelope
		ld	c, a							; c = new volume envelope

.no_volenv:
		bit	4, (ix+zTrack.PlaybackControl)	; Is track resting?
		ret	nz								; Return if yes
		ld	a, (ix+zTrack.Volume)			; Get track volume
		add	a, c							; Add volume envelope to it
		bit	4, a							; Is bit 4 set?
		jr	z, .no_underflow				; Branch if not
		ld	a, 0Fh							; Set silence on PSG track

.no_underflow:
		or	(ix+zTrack.VoiceControl)		; Mask in the PSG channel bits
		add	a, 10h							; Flag to latch volume
		bit	0, (ix+zTrack.PlaybackControl)	; Is this a noise channel?
		jr	z, .not_noise					; Branch if not
		add	a, 20h							; Change to noise channel
.not_noise:
		ld	(zPSG), a						; Set noise channel volume
		ret
; ---------------------------------------------------------------------------
;loc_1037
;zDoFlutterSetValue
zDoVolEnvSetValue:
		ld	(ix+zTrack.VolEnv), a			; Set new value for PSG envelope index and fall through

; =============== S U B	R O U T	I N E =======================================
; Get next PSG volume envelope value.
;
; Input:   ix    Pointer to track RAM
;          hl    Pointer to current PSG volume envelope
; Output:  a     New volume envelope value
;          bc    Trashed
;
;sub_103A
;zDoFlutter
zDoVolEnv:
		push	hl							; Save hl
		ld	c, (ix+zTrack.VolEnv)			; Get current PSG envelope index
		ld	b, 0							; b = 0
		add	hl, bc							; Offset into PSG envelope table
		; Fix based on similar code from Space Harrier II's sound driver.
		; This fixes the "DANGER!" bug below. This is better than the
		; previous fix, which was based on Ristar's driver.
		ld	c, l
		ld	b, h
		ld	a, (bc)							; a = PSG volume envelope
		pop	hl								; Restore hl
		bit	7, a							; Is it a terminator?
		jr	z, zDoVolEnvAdvance				; Branch if not
		cp	83h								; Is it a command to put PSG channel to rest?
		jr	z, zDoVolEnvFullRest			; Branch if yes
		cp	81h								; Is it a command to set rest flag on PSG channel?
		jr	z, zDoVolEnvRest				; Branch if yes
		cp	80h								; Is it a command to reset envelope?
		jr	z, zDoVolEnvReset				; Branch if yes

		inc	bc								; Increment envelope index
		; DANGER! Will read data from code segment and use it as if it were valid!
		; In order to get here, the flutter value would have to be:
		; (1) negative;
		; (2) not 80h, 81h or 83h.
		; As it stands, none of the entries in the flutter tables will allow
		; this code to execute.
		ld	a, (bc)							; Get value from wherever the hell bc is pointing to
		jr	zDoVolEnvSetValue				; Use this as new envelope index
; ---------------------------------------------------------------------------
;loc_1057
;zDoFlutterFullRest
zDoVolEnvFullRest:
		pop	hl								; Pop return value from stack (causes a 'ret' to return to caller of zUpdatePSGTrack)
		jp	zRestTrack						; Put track at rest
; ---------------------------------------------------------------------------
;loc_105F
;zDoFlutterReset
zDoVolEnvReset:
		xor	a								; a = 0
		jr	zDoVolEnvSetValue
; ---------------------------------------------------------------------------
;loc_1062
;zDoFlutterRest
zDoVolEnvRest:
		pop	hl								; Pop return value from stack (causes a 'ret' to return to caller of zUpdatePSGTrack)
		set	4, (ix+zTrack.PlaybackControl)	; Set 'track is resting' bit
		ret									; Do NOT silence PSG channel
; ---------------------------------------------------------------------------
;loc_1068
;zDoFlutterAdvance
zDoVolEnvAdvance:
		inc	(ix+zTrack.VolEnv)				; Advance envelope
		ret
; End of function zDoVolEnv


; =============== S U B	R O U T	I N E =======================================
;
;sub_106C
zRestTrack:
		set	4, (ix+zTrack.PlaybackControl)	; Set 'track is resting' bit
		bit	2, (ix+zTrack.PlaybackControl)	; Is SFX overriding this track?
		ret	nz								; Return if so
; End of function zRestTrack


; =============== S U B	R O U T	I N E =======================================
;
;sub_1075
zSilencePSGChannel:
		ld	a, 1Fh							; Set volume to zero on PSG channel
		add	a, (ix+zTrack.VoiceControl)		; Add in the PSG channel selector
		or	a								; Is it an actual PSG channel?
		ret	p								; Return if not
		ld	(zPSG), a						; Silence this channel
		cp	0DFh							; Was this PSG3?
		ret	nz								; Return if not
		ld	a, 0FFh							; Command to silence Noise channel
		ld	(zPSG), a						; Do it
		ret
; End of function zSilencePSGChannel


; ---------------------------------------------------------------------------
; ===========================================================================
; Volume Envelope Pointers
; ===========================================================================
;z80_PSGTonePointers
z80_VolEnvPointers:
;		dw		VolEnv_00,VolEnv_01,VolEnv_02,VolEnv_03,VolEnv_04,VolEnv_05
;		dw		VolEnv_06,VolEnv_07,VolEnv_08,VolEnv_09,VolEnv_0A,VolEnv_0B
;		dw		VolEnv_0C,VolEnv_0D,VolEnv_0E,VolEnv_0F,VolEnv_10,VolEnv_11
;		dw		VolEnv_12,VolEnv_13,VolEnv_14,VolEnv_15,VolEnv_16,VolEnv_17
;		dw		VolEnv_18,VolEnv_19,VolEnv_1A,VolEnv_1B,VolEnv_1C,VolEnv_1D
;		dw		VolEnv_1E,VolEnv_1F,VolEnv_20,VolEnv_21,VolEnv_22,VolEnv_23
;		dw		VolEnv_24,VolEnv_25,VolEnv_26
;
;VolEnv_00:	db    2, 83h
;VolEnv_01:	db    0,   2,   4,   6,   8, 10h, 83h
;VolEnv_02:	db    2,   1,   0,   0,   1,   2,   2,   2,   2,   2,   2,   2,   2,   2,   2,   2
;			db    2,   3,   3,   3,   4,   4,   4,   5, 81h
;VolEnv_03:	db    0,   0,   2,   3,   4,   4,   5,   5,   5,   6,   6, 81h
;VolEnv_04:	db    3,   0,   1,   1,   1,   2,   3,   4,   4,   5, 81h
;VolEnv_05:	db    0,   0,   1,   1,   2,   3,   4,   5,   5,   6,   8,   7,   7,   6, 81h
;VolEnv_06:	db    1, 0Ch,   3, 0Fh,   2,   7,   3, 0Fh, 80h
;VolEnv_07:	db    0,   0,   0,   2,   3,   3,   4,   5,   6,   7,   8,   9, 0Ah, 0Bh, 0Eh, 0Fh
;			db  83h
;VolEnv_08:	db    3,   2,   1,   1,   0,   0,   1,   2,   3,   4, 81h
;VolEnv_09:	db    1,   0,   0,   0,   0,   1,   1,   1,   2,   2,   2,   3,   3,   3,   3,   4
;			db    4,   4,   5,   5, 81h
;VolEnv_0A:	db  10h, 20h, 30h, 40h, 30h, 20h, 10h,   0,0F0h, 80h
;VolEnv_0B:	db    0,   0,   1,   1,   3,   3,   4,   5, 83h
;VolEnv_0C:	db    0, 81h
;VolEnv_0D:	db    2, 83h
;VolEnv_0E:	db    0,   2,   4,   6,   8, 10h, 83h
;VolEnv_0F:	db    9,   9,   9,   8,   8,   8,   7,   7,   7,   6,   6,   6,   5,   5,   5,   4
;			db    4,   4,   3,   3,   3,   2,   2,   2,   1,   1,   1,   0,   0,   0, 81h
;VolEnv_10:	db    1,   1,   1,   0,   0,   0, 81h
;VolEnv_11:	db    3,   0,   1,   1,   1,   2,   3,   4,   4,   5, 81h
;VolEnv_12:	db    0,   0,   1,   1,   2,   3,   4,   5,   5,   6,   8,   7,   7,   6, 81h
;VolEnv_13:	db  0Ah,   5,   0,   4,   8, 83h
;VolEnv_14:	db    0,   0,   0,   2,   3,   3,   4,   5,   6,   7,   8,   9, 0Ah, 0Bh, 0Eh, 0Fh
;			db  83h
;VolEnv_15:	db    3,   2,   1,   1,   0,   0,   1,   2,   3,   4, 81h
;VolEnv_16:	db    1,   0,   0,   0,   0,   1,   1,   1,   2,   2,   2,   3,   3,   3,   3,   4
;			db    4,   4,   5,   5, 81h
;VolEnv_17:	db  10h, 20h, 30h, 40h, 30h, 20h, 10h,   0, 10h, 20h, 30h, 40h, 30h, 20h, 10h,   0
;			db  10h, 20h, 30h, 40h, 30h, 20h, 10h,   0, 80h
;VolEnv_18:	db    0,   0,   1,   1,   3,   3,   4,   5, 83h
;VolEnv_19:	db    0,   2,   4,   6,   8, 16h, 83h
;VolEnv_1A:	db    0,   0,   1,   1,   3,   3,   4,   5, 83h
;VolEnv_1B:	db    4,   4,   4,   4,   3,   3,   3,   3,   2,   2,   2,   2,   1,   1,   1,   1
;			db  83h
;VolEnv_1C:	db    0,   0,   0,   0,   1,   1,   1,   1,   2,   2,   2,   2,   3,   3,   3,   3
;			db    4,   4,   4,   4,   5,   5,   5,   5,   6,   6,   6,   6,   7,   7,   7,   7
;			db    8,   8,   8,   8,   9,   9,   9,   9, 0Ah, 0Ah, 0Ah, 0Ah, 81h
;VolEnv_1D:	db    0, 0Ah, 83h
;VolEnv_1E:	db    0,   2,   4, 81h
;VolEnv_1F:	db  30h, 20h, 10h,   0,   0,   0,   0,   0,   8, 10h, 20h, 30h, 81h
;VolEnv_20:	db    0,   4,   4,   4,   4,   4,   4,   4,   4,   4,   4,   6,   6,   6,   8,   8
;			db  0Ah, 83h
;VolEnv_21:	db    0,   2,   3,   4,   6,   7, 81h
;VolEnv_22:	db    2,   1,   0,   0,   0,   2,   4,   7, 81h
;VolEnv_23:	db  0Fh,   1,   5, 83h
;VolEnv_24:	db    8,   6,   2,   3,   4,   5,   6,   7,   8,   9, 0Ah, 0Bh, 0Ch, 0Dh, 0Eh, 0Fh
;			db  10h, 83h
;VolEnv_25:	db    0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   1,   1,   1,   1,   1,   1
;			db    1,   1,   1,   1,   2,   2,   2,   2,   2,   2,   2,   2,   2,   2,   3,   3
;			db    3,   3,   3,   3,   3,   3,   3,   3,   4,   4,   4,   4,   4,   4,   4,   4
;			db    4,   4,   5,   5,   5,   5,   5,   5,   5,   5,   5,   5,   6,   6,   6,   6
;			db    6,   6,   6,   6,   6,   6,   7,   7,   7,   7,   7,   7,   7,   7,   7,   7
;			db    8,   8,   8,   8,   8,   8,   8,   8,   8,   8,   9,   9,   9,   9,   9,   9
;			db    9,   9, 83h
;VolEnv_26:	db    0,   2,   2,   2,   3,   3,   3,   4,   4,   4,   5,   5, 83h

; ---------------------------------------------------------------------------
; ===========================================================================
; SFX Pointers
; ===========================================================================

z80_SFXPointers:
		dw	Sound_33,Sound_34,Sound_35,Sound_36
		dw	Sound_37,Sound_38,Sound_39,Sound_3A
		dw	Sound_3B,Sound_3C,Sound_3D,Sound_3E
		dw	Sound_3F

		dw	Sound_40,Sound_41,Sound_42,Sound_43
		dw	Sound_44,Sound_45,Sound_46,Sound_47
		dw	Sound_48,Sound_49,Sound_4A,Sound_4B
		dw	Sound_4C,Sound_4D,Sound_4E,Sound_4F

		dw	Sound_50,Sound_51,Sound_52,Sound_53
		dw	Sound_54,Sound_55,Sound_56,Sound_57
		dw	Sound_58,Sound_59,Sound_5A,Sound_5B
		dw	Sound_5C,Sound_5D,Sound_5E,Sound_5F

		dw	Sound_60,Sound_61,Sound_62,Sound_63
		dw	Sound_64,Sound_65,Sound_66,Sound_67
		dw	Sound_68,Sound_69,Sound_6A,Sound_6B
		dw	Sound_6C,Sound_6D,Sound_6E,Sound_6F

		dw	Sound_70,Sound_71,Sound_72,Sound_73
		dw	Sound_74,Sound_75,Sound_76,Sound_77
		dw	Sound_78,Sound_79,Sound_7A,Sound_7B
		dw	Sound_7C,Sound_7D,Sound_7E,Sound_7F

		dw	Sound_80,Sound_81,Sound_82

zSndPriorities:	db  70h, 7Ah, 70h, 7Dh, 7Dh, 70h, 70h, 7Ah
		db  70h, 6Dh, 7Dh, 7Ah, 7Ah, 70h, 7Ah, 6Dh
		db  70h, 6Dh, 70h, 70h, 6Dh, 6Dh, 6Dh, 70h
		db  70h, 7Dh, 70h, 7Dh, 70h, 7Dh, 7Ah, 7Ah
		db  70h, 70h, 70h, 6Dh, 70h, 70h, 7Ah, 70h
		db  7Dh, 7Dh, 6Ah, 6Dh, 7Dh, 6Dh, 6Dh, 6Dh
		db  70h, 70h, 70h, 7Ah, 70h, 70h, 70h, 70h
		db  7Dh, 70h, 70h, 6Dh, 6Dh, 70h, 7Ah, 70h
		db  6Dh, 6Dh, 7Ah, 70h, 70h, 6Dh, 6Ah, 6Dh
		db  70h, 70h, 70h, 70h, 70h, 70h, 70h, 04h
		db  08h, 01h, 02h, 80h, 00h,0EAh, 07h,0F6h
		db  10h, 80h, 01h,0F6h, 07h,0F7h, 10h,0EFh
		db  00h,0AFh, 01h, 80h, 01h,0F7h, 00h, 0Bh
		db 0ECh, 07h,0F2h,0EFh, 00h, 80h, 01h,0ADh
; ---------------------------------------------------------------------------
	restore
	padding off

Sound_33:	include "SFX/S2/A4 - Skidding.asm"
;Sound_33:	include "SFX/SCD/Snd90_Skid.asm"
Sound_34:	include "SFX/SCD/Snd91.asm"
Sound_35:	include "SFX/S2/A0 - Jump.asm"
;Sound_35:	include "SFX/SCD/Snd92.asm"
Sound_36:	include "SFX/SCD/Snd93_Death.asm"
Sound_37:	include "SFX/SCD/Snd94_RingLoss.asm"
Sound_38:	include "SFX/SCD/Snd95_RingR.asm"
Sound_39:	include "SFX/SCD/Snd96_Destroy.asm"
Sound_3A:	include "SFX/SCD/Snd97_Shield.asm"
Sound_3B:	include "SFX/SCD/Snd98_Spring.asm"
Sound_3C:	include "SFX/SCD/Snd99.asm"
Sound_3D:	include "SFX/SCD/Snd9A.asm"
Sound_3E:	include "SFX/SCD/Snd9B.asm"
Sound_3F:	include "SFX/SCD/Snd9C.asm"
Sound_40:	include "SFX/SCD/Snd9D.asm"
Sound_41:	include "SFX/SCD/Snd9E.asm"
Sound_42:	include "SFX/SCD/Snd9F.asm"
Sound_43:	include "SFX/SCD/SndA0.asm"
Sound_44:	include "SFX/SCD/SndA1.asm"
Sound_45:	include "SFX/SCD/SndA2.asm"
Sound_46:	include "SFX/SCD/SndA3.asm"
Sound_47:	include "SFX/SCD/SndA4.asm"
Sound_48:	include "SFX/SCD/SndA5.asm"
Sound_49:	include "SFX/SCD/SndA6.asm"
Sound_4A:	include "SFX/SCD/SndA7.asm"
Sound_4B:	include "SFX/SCD/SndA8_RingL.asm"
Sound_4C:	include "SFX/SCD/SndA9.asm"
Sound_4D:	include "SFX/SCD/SndAA.asm"
Sound_4E:	include "SFX/SCD/SndAB.asm"
Sound_4F:	include "SFX/SCD/SndAC.asm"
Sound_50:	include "SFX/SCD/SndAD.asm"
Sound_51:	include "SFX/SCD/SndAE_StarPost.asm"
Sound_52:	include "SFX/SCD/SndAF.asm"
Sound_53:	include "SFX/SCD/SndB0.asm"
Sound_54:	include "SFX/SCD/SndB1.asm"
Sound_55:	include "SFX/SCD/SndB2.asm"
Sound_56:	include "SFX/SCD/SndB3.asm"
Sound_57:	include "SFX/SCD/SndB4.asm"
Sound_58:	include "SFX/SCD/SndB5.asm"
Sound_59:	include "SFX/SCD/SndB6.asm"
Sound_5A:	include "SFX/SCD/SndB7.asm"
Sound_5B:	include "SFX/SCD/SndB8.asm"
Sound_5C:	include "SFX/SCD/SndB9.asm"
Sound_5D:	include "SFX/SCD/SndBA.asm"
Sound_5E:	include "SFX/SCD/SndBB.asm"
Sound_5F:	include "SFX/SCD/SndBC.asm"
Sound_60:	include "SFX/SCD/SndBD.asm"
Sound_61:	include "SFX/SCD/SndBE.asm"
Sound_62:	include "SFX/SCD/SndBF.asm"
Sound_63:	include "SFX/SCD/SndC0.asm"
Sound_64:	include "SFX/SCD/SndC1.asm"
Sound_65:	include "SFX/SCD/SndC2.asm"
Sound_66:	include "SFX/SCD/SndC3.asm"
Sound_67:	include "SFX/SCD/SndC4.asm"
Sound_68:	include "SFX/SCD/SndC5.asm"
Sound_69:	include "SFX/SCD/SndC6.asm"
Sound_6A:	include "SFX/SCD/SndC7.asm"
Sound_6B:	include "SFX/SCD/SndC8_SSEnter.asm"
Sound_6C:	include "SFX/SCD/SndC9.asm"
Sound_6D:	include "SFX/SCD/SndCA.asm"
Sound_6E:	include "SFX/SCD/SndCB.asm"
Sound_6F:	include "SFX/SCD/SndCC.asm"
Sound_70:	include "SFX/SCD/SndCD.asm"
Sound_71:	include "SFX/SCD/SndCE.asm"
Sound_72:	include "SFX/SCD/SndCF.asm"
Sound_73:	include "SFX/SCD/SndD0.asm"
Sound_74:	include "SFX/SCD/SndD1.asm"
Sound_75:	include "SFX/SCD/SndD2.asm"
Sound_76:	include "SFX/SCD/SndD3.asm"
Sound_77:	include "SFX/SCD/SndD4.asm"
Sound_78:	include "SFX/SCD/SndD5.asm"
Sound_79:	include "SFX/SCD/SndD6.asm"
Sound_7A:	include "SFX/SCD/SndD7.asm"
Sound_7B:	include "SFX/SCD/SndD8.asm"
Sound_7C:	include "SFX/SCD/SndD9.asm"
Sound_7D:	include "SFX/SCD/SndDA.asm"
Sound_7E:	include "SFX/SCD/SndDB.asm"
Sound_7F:	include "SFX/SCD/SndDC.asm"
Sound_80:	include "SFX/SCD/SndDD.asm"
Sound_81:	include "SFX/SCD/SndDE.asm"
Sound_82:	include "SFX/SCD/SndDF.asm"

	if * > zDataStart
		fatal "Your Z80 tables won't fit before its variables. It's \{$-zDataStart}h bytes past the start of the variables, at \{zDataStart}h"
	endif
