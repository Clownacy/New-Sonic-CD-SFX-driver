This is a replacement SFX sound driver for Sonic CD, featuring PSG
support and fixed ring SFX behaviour.

Sonic CD has separate sound drivers for SFX and music: music uses
SMPS-PCM, and SFX uses a stripped-down version of SMPS Z80 Type 1 FM.

The problem with the SFX driver is that its support for the Mega Drive's
PSG sound chip was removed. This makes it incompatible with the original
jump and skidding SFX from Sonic 1. This is why Sonic CD has its own
version of these sounds.

This replacement driver lacks that problem, allowing for the use of the
original sounds.

Additionally, the ring SFX was poorly-implemented in the original
driver, causing only one instance to be able to play at a time. This has
been corrected, so that two can play at a time instead, as is the case
in other Sonic games.

This driver was made by taking the sound driver from Sonic & Knuckles
(SMPS Z80 Type 2 DAC), and stripping it down to fit in Sonic CD's
limited memory arrangement. Extra features from Sonic CD's original
driver were ported or reimplemented.
