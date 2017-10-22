            zeusemulate "128K"              ; Set a model with 128K of RAM

            ; fill video memory with a zelda logo 

            import_bin "zelda.scr",$4000
            
            ; structure definition for a track

            ORG     $0

dTrackPtr:  dw      $00       ; pointe to the next event
dDTime:     db      $0        ; delta time before the next event
dChnMask:   db      $0        ; the mixer mask for this channel
Track       EQU     .       

            ; structure definition for a song

            ; a song may have to 4 tracks, the last of which will be noise so useful only as percussion 
            ORG    $0

dTrack0:    dw     $0         ; pointer to track0 song data
dTrack1:    dw     $0         ; pointer to track1 song data
dTrack2:    dw     $0         ; pointer to track2 song data
dTrack3:    dw     $0         ; pointer to track3 song data
dTrack0Vol: db     $0         ; volume 0-15 for track0
dTrack1Vol: db     $0         ; volume 0-15 for track1
dTrack2Vol: db     $0         ; volume 0-15 for track2 (track 3's volume comes from track0)
dNoicePtch: db     $0         ; noise pitch
Song        EQU    .


            ORG     $8000 

            include "aydefs.asm"

            ; zelda overworld theme converted from MIDI. Pitch conversion is straight forward; timing is more problematic. To keep tracks
            ; in sync, each midi delta time must be an exact multiple of 1/50. The formula used in coversion from midi is:
            ;
            ; delta_time_in_interrupts = note_delta_time / quater_note_divisor * quater_note_length_in_interrupts
            ;
            ; the numerator and denominator in the expression comes from the midi file and just using these numbers raw will result in
            ; quite nasty drift between tracks. 
            ;
            ; the simplest thing to do is to quantise the midi song ( in anvil studio in my case ) to - say - 1/32s, so making the expression
            ; below:
            ;
            ; delta_time_in_interrupts = ( quarter_note_divisor*4*x/32  )/ quater_note_divisor * quarter_note_length_in_interrupts
            ;                          = x * quarter_note_length_in_interrupts/8
            ;                           
            ; 
            ; .. so as long quarter note length used for playback is exactly divisible by 8 ( for quantisation of 1/32 ) then all will be well
            ; for PAL, a quarter note length of 24 interrupts ( 24/50s ) has the right properties and gives a bpm of 125. For higher bpm,
            ; need coarse quantisation.
            ;
            ; this means that tempo in the file is not respected ( zelda is 148bpm, so this is quite a bit slower), but quantising to 1/16 
            ; changed the song too much. 



            include "overworld.asm"

            ; note table for one octave starting at c1 

notes:      dw zC, zCShrp, zD, zDShrp, zE, zF, zFShrp, zG, zGShrp, zA, zAShrp, zB          

            ; song definition for the zelds overworld theme

zelda:      dw    melody
            dw    harmony
            dw    bass
            dw    percussion
            db    $0f, $0f, $0f, $10


            ; set ay register: d = register, e = value

outer:      push    bc 
            ld      bc,AYCTRL 
            out     (c),d 
            ld      bc,AYDATA 
            out     (c),e 
            pop     bc 
            ret    

            ; set pitch. bc = pitch, a = fine register

setPitch:   ld      d,a 
            ld      e,c 
            call    outer
            inc a
            ld      d,a
            ld      e,b 
            call    outer
            ret

            ; play note. a = channel(0-2), b = octave (1-6), c = note (0-11)

playNote:   add a,a                 ; double a to covert to fine register
            push af
            ld hl, notes            ; look-up note pitch in table
            ld d, 0
            ld e, c
            add hl, de
            add hl, de
            ld  e, (hl)
            inc hl
            ld  d, (hl)
            dec b                   ; lowest octave is 1, so dec b
            jr z, playNote0         ; do not need to adjust pitch
            xor a
playNote1:  rr d                    ; scale pitch by 2^octave
            rr e
            djnz playNote1
playNote0:  ld b, d
            ld c, e
            pop af
            call setPitch
            ret

            ; record state of currently playing song

tracks:     ds Track*4
shadmix:    db $ff

            ; start playing a new song. ix = pointer to new song definition

strtSong:   di                      ; disable interupts as we are going to changing the currently playing song 
            ld hl, tracks
            ld b,4                  ; always supply 4 tracks ( but pointers can be null)
            ld c,1                  ; c holds mask for current track
strtSong0:  ld d, (ix+dTrack0+1)    ; load de with pointer to track
            ld e, (ix+dTrack0)
            ex de,hl
            ld a,(hl)               ; time delta of first event
            ex de,hl
            inc de                  ; advance to first event
            ld (hl), e              ; store pointer to first event in the current song record
            inc hl
            ld (hl), d
            inc hl
            ld (hl),a               ; store time delta in the timer field 
            inc hl
            ld (hl),c               ; store the channel mask 
            inc hl
            xor a
            rl c                    ; next channel
            inc ix                  ; next song track
            inc ix
            djnz strtSong0
            ld      d, CHAVOL
            ld      e, (ix+0)
            CALL    outer           ; set volume for track0 / channel A - assumed constant 
            ld      d, CHBVOL 
            ld      e, (ix+1)
            call    outer           ; set volume for track1 / channel B - assumed constant 
            ld      d, CHCVOL
            ld      e, (ix+2)
            call    outer           ; set volume for track0 / channel C - assumed constant 
            ld      d, NOISE
            ld      e, (ix+3)
            call    outer           ; set noise pitch
            ld      d, MIXER
            ld      a, $ff          ; all channels off to start with
            ld      e, a
            ld      (shadmix),a     ; also keep this is a shadow register so we don't need to read it from the chip
            call    outer
            ei
            ret  

            ; is there a song currently playing? carry flag set on return if true

isPlayng:   ld ix, tracks
            xor a
            ld b,4                  ; 4 tracks
isPlayng1:  ld h, (ix+1)            ; track pointer is set to null on reaching end of track ...
            ld l, (ix+0)
            or h                    ; ... so just or all the pointers together
            or l
            ld de, Track            ; Track is sizeof Track struct
            add ix,de
            djnz isPlayng1
            and a
            jr z, isPlayng0
            xor a                   ; clear and complement carry to set
            ccf 
            ret
isPlayng0:  xor a                   ; clear carry
            ret 

            ; start the zelda song if there is not another song already playing

trystart:   call isPlayng
            ret c
            ld ix, zelda
            call strtSong
            ret  

            ; turnn on a channel. e = channel mask

turnon:     ld a,e
            cpl 
            ld e,a
            ld a,(shadmix)
            and e
            ld e,a
            ld (shadmix),a
            ld d,MIXER
            call outer
            ret

            ; turn off a channel. e = channel mask

turnoff:    ld a,(shadmix)
            or e
            ld e,a
            ld (shadmix),a
            ld d,MIXER
            call outer
            ret

            ; "local" var to store the current track

trckindex:  db 0

            ; play the current song. ix = current tracks
            ; This routine runs as the ISR and updates the song every 1/50s

playsong:   xor a                         
            ld (trckindex),a              ; set current track to 0
play2:      ld h,(ix+dTrackPtr+1)         ; get the pointer into the track
            ld l,(ix+dTrackPtr)
            ld a,h                        ; test for null
            or l
            jr z, play0                   ; null, so go to next track
            ld a,(ix+dDTime)              ; load the delta time for this track
            and a                         ; check for 0
            jr z,  play6                  ; if 0, read the next event
            dec a                         ; decrease time to next event
            ld (ix+dDTime),a              ; store
            jr nz, play0                  ; if 0, read the next event

play6:      ld a,(hl)                     ; load next event
            cp $ff                        ; end track event?
            jr nz, play1                  ; not end track event

            xor a                         ; end of track, null pointer ..
            ld (ix+dTrackPtr), a
            ld (ix+dTrackPtr+1),a
            ld e, (ix+dChnMask)           ; ... and turn off track
            call turnoff
            jr play0                      ; goto next track

 play1:     cp $80                        ; note off event?
            jr nz, play4                  ; no, so goto note on
            ld e, (ix+dChnMask)           ; turn off note
            call  turnoff                 
            jr play5                      ; read the delta for the next event 

 play4:     ld c,a                         ; note on - load note from a into c 
            inc hl                        ; load octave into b
            ld b, (hl)
            push hl                       ; save hl as it won't be preserved by playNote
            ld a, (ix+dChnMask)           ; is this a tone channel?
            and 7
            jr z, play7                   ; no, its noise so skip setting pitch and just turn the channel on
            ld a,(trckindex)              ; otherwise set the pitch of the note...
            call playNote
play7:      ld e, (ix+dChnMask)           ; .. and turn the channel on
            call turnon
            pop hl
play5:      inc hl                        ; advance hl to next delta time
            ld a,(hl) 
            ld (ix+dDTime),a              ; store the delta time
            inc hl                        ; adavance hl to point at next event
            ld (ix+dTrackPtr), l
            ld (ix+dTrackPtr+1), h
            and a                         ; check that the delta time is not 0 - if it is, process this event now!
            jr z, play6
 play0:     ld de, Track                  ; sizeof Track
            add ix,de                     ; advanace to next track
            ld a, (trckindex)             ; inc track number
            inc a
            ld (trckindex),a
            cp 4                          ; done?
            jr nz, play2                  ; ... if not, next track
            ret

            ; wait for the space key to be pressed

waitKey:    ld a, $7f
            in a, ($fe)
            cpl 
            and 1
            jr z, waitKey
waitkey0:   ld a, $7f
            in a, ($fe)
            and 1
            jr nz, waitkey0
            ret

            ; entry point. set stack to top of memory ( first entry will be at $ffff and grow downwards), setup the ISR
            ; wait for a key to be pressed, then continuously try to start a song

main:       ld sp,0
            call    setupIM2
            call    waitKey
main0:      halt
            call trystart
            jr main0

            ; standard setup for IM2. Create a vector table where every entry pointe to $fefe

setupIM2:   di
            im 2
            ld a, $fd
            ld i, a
            ei
            ret

            org    $fdfd

            loop 257
            db $fe
            lend

            ; actual ISR. play the current song

            org    $fefe
           
            di
            ld ix,tracks
            call playsong
            ei
            ret


            ; export a snapshot

            output_szx "ay.szx",$0000,main 
