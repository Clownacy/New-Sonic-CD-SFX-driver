SndC4_Header:
	smpsHeaderStartSong 3, 1
	smpsHeaderVoice     SndC4_Voices
	smpsHeaderTempoSFX  $01
	smpsHeaderChanSFX   $01

	smpsHeaderSFXChannel cFM6, SndC4_FM6,	$00, $05

; FM6 Data
SndC4_FM6:
	smpsSetvoice        $00
	dc.b	nG6, $02

SndC4_Loop00:
	dc.b	smpsNoAttack, $01
	smpsActualConditionalJump SndC4_Loop00

SndC4_Loop01:
	dc.b	smpsNoAttack, $01
	smpsFMAlterVol      $01
	smpsLoop            $00, $22, SndC4_Loop01
	dc.b	nRst, $01
	smpsSetComm         $00
	smpsStop

SndC4_Voices:
;	Voice $00
;	$38
;	$0F, $0F, $0F, $0F, 	$1F, $1F, $1F, $0E, 	$00, $00, $00, $00
;	$00, $00, $00, $00, 	$0F, $0F, $0F, $1F, 	$1A, $0C, $00, $80
	smpsVcAlgorithm     $00
	smpsVcFeedback      $07
	smpsVcUnusedBits    $00
	smpsVcDetune        $00, $00, $00, $00
	smpsVcCoarseFreq    $0F, $0F, $0F, $0F
	smpsVcRateScale     $00, $00, $00, $00
	smpsVcAttackRate    $0E, $1F, $1F, $1F
	smpsVcAmpMod        $00, $00, $00, $00
	smpsVcDecayRate1    $00, $00, $00, $00
	smpsVcDecayRate2    $00, $00, $00, $00
	smpsVcDecayLevel    $01, $00, $00, $00
	smpsVcReleaseRate   $0F, $0F, $0F, $0F
	smpsVcTotalLevel    $80, $00, $0C, $1A

