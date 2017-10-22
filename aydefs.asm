

; useful definitions for programming the ay-3-8910 in the spectrum

AYCTRL      EQU     65533 
AYDATA      EQU     49149 

CHAFINE     EQU     0 
CHACRSE     EQU     1 
CHBFINE     EQU     2 
CHBCRSE     EQU     3 
CHCFINE     EQU     4 
CHCCRSE     EQU     5 
NOISE       EQU     6 
MIXER       EQU     7 
CHAVOL      EQU     8 
CHBVOL      EQU     9 
CHCVOL      EQU     10 
ENVFINE     EQU     11 
ENVCRSE     EQU     12 
ENVSHP      EQU     13 
IOA         EQU     14 
IOB         EQU     15 

; c is C1 - note 24 in MIDI. This is the lowest complete octave that
; the ay can generate

zC          EQU     3421 ; c
zCShrp      EQU     3228; c#
zD          EQU     3047; d
zDShrp      EQU     2876; d#
zE          EQU     2715; e
zF          EQU     2562; f
zFShrp      EQU     2419; f#
zG          EQU     2283; g
zGShrp      EQU     2155; g#
zA          EQU     2034; a
zAShrp      EQU     1920; a#
zB          EQU     1892; b