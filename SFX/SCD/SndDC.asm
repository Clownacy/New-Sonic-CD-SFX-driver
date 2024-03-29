SndDC_Header:
	smpsHeaderStartSong 3, 1
	smpsHeaderVoice     SndDC_Voices
	smpsHeaderTempoSFX  $01
	smpsHeaderChanSFX   $02

	smpsHeaderSFXChannel cFM5, SndDC_FM5,	$00, $00
	smpsHeaderSFXChannel cFM6, SndDC_FM6,	$00, $00

; FM6 Data
SndDC_FM6:
	dc.b	nRst, $03

; FM5 Data
SndDC_FM5:
	smpsSetvoice        $00
	dc.b	nG7, $05, nRst, $06, nAb7, $11
	smpsStop

SndDC_Voices:
;	Voice $00
;	$34
;	$0C, $0A, $04, $03, 	$1F, $1F, $1F, $1F, 	$0C, $0D, $09, $0C
;	$0A, $0E, $0D, $0E, 	$35, $1A, $55, $3A, 	$0F, $80, $0F, $80
	smpsVcAlgorithm     $04
	smpsVcFeedback      $06
	smpsVcUnusedBits    $00
	smpsVcDetune        $00, $00, $00, $00
	smpsVcCoarseFreq    $03, $04, $0A, $0C
	smpsVcRateScale     $00, $00, $00, $00
	smpsVcAttackRate    $1F, $1F, $1F, $1F
	smpsVcAmpMod        $00, $00, $00, $00
	smpsVcDecayRate1    $0C, $09, $0D, $0C
	smpsVcDecayRate2    $0E, $0D, $0E, $0A
	smpsVcDecayLevel    $03, $05, $01, $03
	smpsVcReleaseRate   $0A, $05, $0A, $05
	smpsVcTotalLevel    $80, $0F, $80, $0F

