INCLUDE "hardware.inc"

; OBJECT COUNT (MAX: 40)
DEF OBJCOUNT EQU 40

; BOUNCE AREA LIMITS
DEF WESTLIMIT EQU 8
DEF EASTLIMIT EQU 160
DEF NORTHLIMIT EQU 16
DEF SOUTHLIMIT EQU 144+8 

; obj Size
DEF PLAYERSZIE EQU 2 * 2
DEF BULLETSIZE EQU 1 * 1
DEF KILLERSIZE EQU 1 * 2
DEF NUMBERSIZE EQU 1 * 1


; Where objs are located in tile
DEF TILENNUMBER EQU 0
DEF TILEPLAYER EQU 10
DEF TILEBULLET EQU 14
DEF TILEKILLER EQU 15


; Where objs are located in OAM
DEF OAMPLAYER EQU 0
DEF OAMKILLER EQU 4*4
DEF OAMSCORES EQU 10*4
DEF OAMBULLETS EQU 14*4

DEF GROUND EQU 130

DEF Y EQU 0
DEF X EQU 1
DEF TILE EQU 2
DEF FLAGES EQU 3

; See `man 5 rgbasm` and search `SECTIONS`
SECTION "Variables", WRAM0
shadowOAM:    DS 40*4 
acceleration: DS 1
speed:        DS 1
bullet_num:   DS 1
key_state:    DS 1
score:        DS 6

this_bullet:  DS 2
last_bullet:  DS 2

previous:     DS 1
current:      DS 1

SECTION "Header", ROM0[$100]
  jp EntryPoint

SECTION "Main", ROM0[$150]
EntryPoint:

; 0. turn off LCD so we can access VRAM whenever we want
; 1. copy tiles data to VRAM tiles location
; 2. copy tile map
; 3. set object and background palette
; 4. reset OAM and shadowOAM values to display sprites
; 5. initialize the data we need 
; 6. set LCD and go to infinite loop

;turn off audio
  ld a, 0
  ld [rNR52], a
.wait:
  ld a, [rLY]
  cp 144
  jr c, .wait
  ld a, %00010000
  ld [rLCDC], a

CopyTileDataToVRAM:
  ld de, Tiles
  ld hl, _VRAM
  ld bc,TilesEnd - Tiles
.copy:
  ld a,[de]
  inc de
  ldi [hl],a
  dec bc
  ld a,b
  or c
  jr nz, .copy

Copythetilemap:
  ld de, Tilemap
  ld hl, $9800
  ld bc, TilemapEnd - Tilemap
CopyTilemap:
  ld a, [de]
  ldi [hl], a
  inc de
  dec bc
  ld a, b
  or a, c
  jp nz, CopyTilemap

; set a black and white palette
  ld a, %11100100
  ld [rOBP0], a
  ld [rBGP], a

ResetOAM:
  ld hl, _OAMRAM
  ld b, 40*4
  ld a, 0
.loop:
  ldi [hl],a
  dec b
  jr nz,.loop

ResetShadowOAM:
  ld hl, shadowOAM
  ld b, 40*4
  ld a, 0
.loop:
  ldi [hl],a
  dec b
  jr nz,.loop

; initialize the data we need
  call init_data
 
; turn LCD on, only enable object and background layer, store OBJ and BG data at same block
  ld a, %10010011
  ld [rLCDC], a

MainLoop:
  call move_and_shoot
  call update_bullet
  call update_killers
  call death_or_score
  call show_score
  call waitVBlank
  call copyShadowOAMtoOAM
  jr MainLoop


SECTION "Routines", ROM0

waitVBlank:
  .wait:
    ld a, [rLY]
    cp 144
    jr nz, .wait
    ret
  
copyShadowOAMtoOAM:
  ld hl,shadowOAM
  ld de,_OAMRAM
  ld b, OBJCOUNT
.loop:
  ldi a,[hl]
  ld [de],a
  inc e
  ldi a,[hl]
  ld [de],a
  inc e
  ldi a,[hl]
  ld [de],a
  inc e
  ldi a,[hl]
  ld [de],a
  inc e
  dec b
  jr nz, .loop
  ret

random_byte:
  push bc
  ld c,a
  ld a,[rDIV]
  adc l
  add c
  xor %10101011
  pop bc
  ret

x_random:
  call random_byte
  cp EASTLIMIT - WESTLIMIT
  jp c, x_random_1
  sub EASTLIMIT - WESTLIMIT
x_random_1:
  add WESTLIMIT
  ret

y_random:
  call random_byte
  cp SOUTHLIMIT - NORTHLIMIT
  jp c, y_random_1
  sub SOUTHLIMIT - NORTHLIMIT
y_random_1:
  add NORTHLIMIT
  ret
  
dir_random:
  call random_byte
  and %00000011
  ret

init_data:
  ld a, 9
  ld hl, acceleration
.loop_init:
  ld [hl], 0
  inc hl
  dec a
  jr nz, .loop_init
  ld hl, this_bullet
  ld de , shadowOAM + OAMBULLETS
  ld [hl],d
  inc hl
  ld [hl],e
  inc hl
  ld [hl],d
  inc hl
  ld [hl],e

; PLAYER
  ld hl, shadowOAM + OAMPLAYER + Y
  ld bc, TILE - Y
  ld [hl], GROUND
  add hl, bc
  ld [hl], TILEPLAYER + 0
  add hl, bc
  ld [hl], GROUND
  add hl, bc
  ld [hl], TILEPLAYER + 1
  add hl, bc
  ld [hl], GROUND + 8
  add hl, bc
  ld [hl], TILEPLAYER + 2
  add hl, bc
  ld [hl], GROUND + 8
  add hl, bc
  ld [hl], TILEPLAYER + 3

; Killer
  ld hl, shadowOAM + OAMKILLER + X
  ld bc, 4
  ld [hl], 46
  add hl, bc
  ld [hl], 46
  add hl, bc
  ld [hl], 84
  add hl, bc
  ld [hl], 84
  add hl, bc
  ld [hl], 122
  add hl, bc
  ld [hl], 122

  ld hl, shadowOAM + OAMKILLER + TILE
  ld [hl], TILEKILLER + 0
  add hl, bc
  ld [hl], TILEKILLER + 1
  add hl, bc
  ld [hl], TILEKILLER + 0
  add hl, bc
  ld [hl], TILEKILLER + 1
  add hl, bc
  ld [hl], TILEKILLER + 0
  add hl, bc
  ld [hl], TILEKILLER + 1
  add hl, bc
  
; Score
  ld hl, shadowOAM + OAMSCORES + Y
  ld [hl], NORTHLIMIT + 24 + 30
  add hl, bc
  ld [hl], NORTHLIMIT + 24 + 20
  add hl, bc
  ld [hl], NORTHLIMIT + 24 + 10
  add hl, bc
  ld [hl], NORTHLIMIT + 24 + 00
  ld hl, shadowOAM + OAMSCORES + X
  ld [hl], WESTLIMIT + 8
  add hl, bc
  ld [hl], WESTLIMIT + 8
  add hl, bc
  ld [hl], WESTLIMIT + 8
  add hl, bc
  ld [hl], WESTLIMIT + 8

  ret




shoot:
  ld a, [bullet_num]
  cp 10
  ret nc
  
  ld hl, last_bullet
  ld b, [hl]
  inc hl
  ld c, [hl]
  ld h, b
  ld l, c;last_bullet

  ld a, [shadowOAM+OAMPLAYER+X];x
  add 5
  ld [hl], GROUND
  inc hl
  ld [hl+], a
  ld [hl], TILEBULLET
  inc hl
  inc hl

  ld b,h
  ld c,l
  ld hl, last_bullet
  ld [hl],b
  inc hl
  ld [hl],c
  


  ld hl, bullet_num
  inc [hl]
  ret

sync_player:
  ld hl, shadowOAM + OAMPLAYER + X
  ld a, [hl]
  ld d, a
  add 8
  ld bc, 4
  add hl, bc
  ld [hl], a
  add hl, bc
  ld [hl], d
  add hl, bc
  ld [hl], a
  ret

sync_killer:
  ld hl, shadowOAM + OAMKILLER + Y
  ld bc, 4
  ld a, [hl]
  add 8
  add hl, bc
  ld [hl], a
  ld hl, shadowOAM + OAMKILLER + 8 + Y
  ld a, [hl]
  add 8
  add hl, bc
  ld [hl], a
  ld hl, shadowOAM + OAMKILLER + 16 + Y
  ld a, [hl]
  add 8
  add hl, bc
  ld [hl], a
  ret 

update_bullet:
  ld hl, shadowOAM + OAMBULLETS ; this
  ld b, 1
;for b in 1..=bullet_num
.loop_update_bullet_start:
  ld a, [bullet_num]
  cp b
  ret c
;{
  ld a, [hl]; this->y
  cp GROUND + 8
  jr c, .move_bullet
  ;{
    push bc
    push hl;this
      ld hl, last_bullet
      ld d, [hl]
      inc hl
      ld e, [hl]
      ld h, d
      ld l, e ;de = last_bullet
      ;hl = last
      ld b, [hl]
      ld [hl], 0
      inc hl
      ld c, [hl]
      ld [hl], 0
      ;bc = last.yx
      pop hl;this
      dec sp
      dec sp
      ld [hl], b
      inc hl
      ld [hl], c

      dec de
      dec de
      dec de
      dec de
      ld hl, last_bullet
      ld [hl], d 
      inc hl
      ld [hl], e;
      ;last_bullet -= sizeof(struct Obj);

      ld hl, bullet_num
      dec [hl]
    pop hl
    pop bc
    jr .loop_update_bullet_start
  ;}
.move_bullet:
  ;hl = this
  
  dec [hl]
  dec [hl]
  dec [hl]

  ld c, 1
  ld de, shadowOAM + OAMKILLER
.loop_check_hit:
  push hl
  push bc
  push de
    ld b, [hl]; this->ySOUTHLIMIT
    inc hl
    ld c, [hl]; this->x
    push hl
      ld h, d
      ld l, e
      ld d, [hl]; killer->y
      inc hl
      ld e, [hl]; killer->x
    pop hl
    ld a, d
    cp SOUTHLIMIT
    jr nc, .next_killer
    add 8
    cp b
    jr c, .next_killer ;killer->y + 8 < this->y
    ld a, c
    sub e
    jr nc, .already_positive
    xor %11111111
    inc a
.already_positive:
    cp 10
    jr nc, .next_killer; abs()>8
    ;hit{
      ;hl = this
      ld [hl], 0;
      pop de
      dec sp
      dec sp
      ld h, d
      ld l, e; hl = killer
      ld [hl], SOUTHLIMIT + 8

      ld hl, score
      inc hl
      inc hl
      inc [hl]
      call show_score

    ;}
.next_killer:
  pop de
    inc de
    inc de
    inc de
    inc de
    inc de
    inc de
    inc de
    inc de
    ; killer += 2 * sizeof(struct Obj);
  pop bc
  pop hl
  ld a, 3
  inc c
  cp c
  jr nc, .loop_check_hit

  inc b
  inc hl
  inc hl
  inc hl
  inc hl
;}
  jp .loop_update_bullet_start
  
move_and_shoot:
  call read_keys
  xor a
  
.key_left
  bit PADB_LEFT, b
  jr z, .key_right
  sub 2
.key_right
  bit PADB_RIGHT, b
  jr z, .key_shoot
  add 2
.key_shoot
  bit PADB_A, b
  jr z, .key_none
  push af
  push bc
  call shoot
  pop bc
  pop af
.key_none
  ld b, a
  ld a, [speed]
  cp 128
  jr c, .speed_negative
  cp 80
  jr nc, .speed_limite_done
  ld a, 39
.speed_negative:
  cp -80
  jr c, .speed_limite_done
  ld a, -79
.speed_limite_done:
  add b
  ld [speed], a
  sra a
  sra a
  sra a
  ld hl, shadowOAM + OAMPLAYER + X
  add [hl]
  cp EASTLIMIT - 8
  jr c, .limit_fine_east
  ld a, [speed]
  xor %11111111
  inc a
  sra a
  ld [speed], a
  ld a, EASTLIMIT - 8
  jr .limit_fine_all
.limit_fine_east:
  cp WESTLIMIT
  jr nc, .limit_fine_all
  ld a, [speed]
  xor %11111111
  inc a
  sra a
  ld [speed], a
  ld a, WESTLIMIT 
.limit_fine_all:
  ld [hl], a
  jp sync_player

update_killers:
  ld d, 0
  ld hl, shadowOAM + OAMKILLER + Y
  ld bc, 8
.loop_update_killer:
  ld a, [hl]
  cp SOUTHLIMIT
  jr nc, .killer_fine
  cp GROUND - 8
  jr c, .killer_fine
  call random_byte
  or SOUTHLIMIT + 8
  ld [hl],a
.killer_fine:
  inc [hl]
  add hl, bc
  inc d
  ld a, d
  cp 3
  jr nz, .loop_update_killer
  jp sync_killer

death_or_score:
  ld hl, score
  inc [hl]
  ld bc, 8
  ld e, 0
  ld hl, shadowOAM + OAMKILLER + Y
.loop_score:
  ld a, [hl]
  cp GROUND - 8 
  jr z, clear_score
  add hl, bc
  inc e
  ld a, e
  cp 3
  jr nz, .loop_score
  ret

clear_score:
  ld hl, score
  xor a
  ld [hl+], a
  ld [hl+], a
  ld [hl+], a
  ld [hl+], a
  ld [hl+], a
  ld [hl+], a
  ret 
  
show_score:
  ld hl, score
  ld a, [hl+]
  bit 4, a
  jr z, .show
  xor a
  ld e, a
  ld [score], a
  inc [hl]
.show:
  ld b, 0
.loop_carry
  ld a, [hl]
  cp 10
  jr c, .no_carry
  ld a, [hl]
  sub 10
  ld [hl], a
  inc hl
  inc [hl]
  dec hl
.no_carry:
  inc b
  inc hl
  ld a, b
  cp 4
  jr nz, .loop_carry

  ld a, [score+1]
  ld [shadowOAM + OAMSCORES + TILE], a
  ld a, [score+2]
  ld [shadowOAM + OAMSCORES + 4 + TILE], a
  ld a, [score+3]
  ld [shadowOAM + OAMSCORES + 8 + TILE], a
  ld a, [score+4]
  ld [shadowOAM + OAMSCORES + 12 + TILE], a
  

  ret


;-------------------------------------------------------------------------------
read_keys:
;-------------------------------------------------------------------------------
; this function returns two different values in b and c registers:
; b - returns raw state (pressing key triggers given action continuously as long as it's pressed - it does not prevent bouncing)
; c - returns debounced state (pressing key triggers given action only once - key must be released and pressed again)

        ld      a,$20				; read P15 - returns a, b, select, start
        ldh     [rP1],a        
        ldh     a,[rP1]				; mandatory
        ldh     a,[rP1]
	cpl					; rP1 returns not pressed keys as 1 and pressed as 0, invert it to make result more readable
        and     $0f				; lower nibble has a, b, select, start state
	swap	a				
	ld	b,a

        ld      a,$10				; read P14 - returns up, down, left, right
        ldh     [rP1],a
        ldh     a,[rP1]				; mandatory
        ldh     a,[rP1]
        ldh     a,[rP1]
        ldh     a,[rP1]
        ldh     a,[rP1]
        ldh     a,[rP1]
	cpl					; rP1 returns not pressed keys as 1 and pressed as 0, invert it to make result more readable
        and     $0f				; lower nibble has up, down, left, right state
	or	b				; combine P15 and P14 states in one byte
        ld      b,a				; store it

	ld	a,[previous]			; this is when important part begins, load previous P15 & P14 state
	xor	b				; result will be 0 if it's the same as current read
	and	b				; keep buttons that were pressed during this read only
	ld	[current],a			; store final result in variable and register
	ld	c,a
	ld	a,b				; current P15 & P14 state will be previous in next read
	ld	[previous],a

	ld	a,$30				; reset rP1
        ldh     [rP1],a

	ret

SECTION "TilesData", ROM0
; see https://gbdev.io/pandocs/Tile_Data.html
; here we only use color IDs of %00 and %10 for pixels,
; so we can easily "draw" tiles in our .asm file

Tiles:
; tile id 0
 DB %00000000,%00000000
 DB %01111110,%00000000
 DB %01000010,%00000000
 DB %01000010,%00000000
 DB %01000010,%00000000
 DB %01000010,%00000000
 DB %01111110,%00000000
 DB %00000000,%00000000
; tile id 1
 DB %00000000,%00000000
 DB %00011000,%00000000
 DB %00111000,%00000000
 DB %00011000,%00000000
 DB %00011000,%00000000
 DB %00011000,%00000000
 DB %01111110,%00000000
 DB %00000000,%00000000
; tile id 2
 DB %00000000,%00000000
 DB %01111110,%00000000
 DB %00000010,%00000000
 DB %01111110,%00000000
 DB %01000000,%00000000
 DB %01000000,%00000000
 DB %01111110,%00000000
 DB %00000000,%00000000
; tile id 3
 DB %00000000,%00000000
 DB %01111110,%00000000
 DB %00000010,%00000000
 DB %01111110,%00000000
 DB %00000010,%00000000
 DB %00000010,%00000000
 DB %01111110,%00000000
 DB %00000000,%00000000
; tile id 4
 DB %00000000,%00000000
 DB %01000100,%00000000
 DB %01000100,%00000000
 DB %01111110,%00000000
 DB %00000100,%00000000
 DB %00000100,%00000000
 DB %00000100,%00000000
 DB %00000000,%00000000
; tile id 5
 DB %00000000,%00000000
 DB %01111110,%00000000
 DB %01000000,%00000000
 DB %01111110,%00000000
 DB %00000010,%00000000
 DB %00000010,%00000000
 DB %01111110,%00000000
 DB %00000000,%00000000
; tile id 6
 DB %00000000,%00000000
 DB %01111110,%00000000
 DB %01000000,%00000000
 DB %01111110,%00000000
 DB %01000010,%00000000
 DB %01000010,%00000000
 DB %01111110,%00000000
 DB %00000000,%00000000
; tile id 7
 DB %00000000,%00000000
 DB %01111110,%00000000
 DB %00000010,%00000000
 DB %00000010,%00000000
 DB %00000100,%00000000
 DB %00000100,%00000000
 DB %00000100,%00000000
 DB %00000000,%00000000
; tile id 8
 DB %00000000,%00000000
 DB %01111110,%00000000
 DB %01000010,%00000000
 DB %01111110,%00000000
 DB %01000010,%00000000
 DB %01000010,%00000000
 DB %01111110,%00000000
 DB %00000000,%00000000
; tile id 9
 DB %00000000,%00000000
 DB %01111110,%00000000
 DB %01000010,%00000000
 DB %01111110,%00000000
 DB %00000010,%00000000
 DB %00000010,%00000000
 DB %01111110,%00000000
 DB %00000000,%00000000
; tile id 10 there stay---------------------
 DB %00000001,%00000001
 DB %00000001,%00000001
 DB %00000001,%00000101
 DB %00000001,%00000101
 DB %00000001,%00000101
 DB %00000001,%00000101
 DB %00000001,%00000111
 DB %00000001,%00000011

 DB %00000000,%00000000
 DB %00000000,%00000000
 DB %00000000,%01000000
 DB %00000000,%01000000
 DB %00000000,%01000000
 DB %00000000,%01000000
 DB %00000000,%11000000
 DB %00000000,%10000000

 DB %00000011,%00000001
 DB %00000001,%00000001
 DB %00000001,%00000001
 DB %00000001,%00000011
 DB %00110011,%00110011
 DB %00010101,%00010101
 DB %00011101,%00011101
 DB %00011001,%00001001

 DB %10000000,%00000000
 DB %00000000,%00000000
 DB %00000000,%00000000
 DB %00000000,%10000000
 DB %10011000,%10011000
 DB %01010000,%01010000
 DB %01110000,%01110000
 DB %00110000,%00100000

 DB %00000000,%00000000
 DB %00011000,%00000000
 DB %00100100,%00011000
 DB %00011000,%00111100
 DB %00100100,%00011000
 DB %00000000,%00000000
 DB %00000000,%00000000
 DB %00000000,%00000000

; tile id 15
 DB %00000000,%00000000
 DB %01000000,%00000000
 DB %01010010,%00000000
 DB %01011010,%00000000
 DB %01001010,%00000000
 DB %01001010,%00000000
 DB %01000010,%00000000
 DB %00000010,%00000000

 DB %01111100,%01111100
 DB %11000110,%11111110
 DB %10101111,%11010011
 DB %11000001,%10111111
 DB %10011001,%11100111
 DB %11001011,%11110111
 DB %01111110,%01111110
 DB %00000000,%00000000
; tile id 17
 DB %10000000,%10000000
 DB %01000000,%01000000
 DB %00100000,%00100000
 DB %00010000,%00010000
 DB %11100000,%11100000
 DB %00000000,%00000000
 DB %00000000,%00000000
 DB %00000000,%00000000
; tile id 18
 DB %00111000,%00111000
 DB %00000111,%00000111
 DB %00000000,%00000000
 DB %00000000,%00000000
 DB %00000000,%00000000
 DB %00000000,%00000000
 DB %00000000,%00000000
 DB %00000000,%00000000
; tile id 19
 DB %00011000,%00011000
 DB %11100000,%11100000
 DB %00000000,%00000000
 DB %00000000,%00000000
 DB %00000000,%00000000
 DB %00000000,%00000000
 DB %00000000,%00000000
 DB %00000000,%00000000
; tile id 20
 DB %00000000,%00000000
 DB %00000000,%00000000
 DB %00000000,%00000000
 DB %00000000,%00000000
 DB %00000000,%00000000
 DB %00000000,%00000000
 DB %00000000,%00000000
 DB %00000000,%00000000
 
 DB %00000000,%00000000
 DB %00000000,%00000000
 DB %00000000,%00000000
 DB %00000000,%00000000
 DB %00000000,%00000000
 DB %00000000,%00000000
 DB %00000000,%00000000
 DB %00000000,%00000000
; tile id 22 finger
 DB %00001010,%00000100
 DB %00000000,%00001010
 DB %00000000,%00001010
 DB %00000000,%00001010
 DB %00010010,%00001011
 DB %00101011,%00111010
 DB %00101010,%01101010
 DB %00000010,%10101010

 DB %00000000,%00000000
 DB %00000000,%00000000
 DB %00000000,%00000000
 DB %00000000,%00000000
 DB %00000000,%00000000
 DB %00000000,%10000000
 DB %10000000,%10000000
 DB %10100000,%11000000

 DB %10001010,%10100000
 DB %10000000,%10000000
 DB %00000000,%10000000
 DB %11000000,%10000000
 DB %00100000,%01000000
 DB %00100000,%00100000
 DB %00000001,%00100000
 DB %00100001,%00100000

 DB %10000000,%10100000
 DB %00100000,%10100000
 DB %01100000,%00100000
 DB %01000000,%00100000
 DB %10000000,%01000000
 DB %10000000,%10000000
 DB %00000000,%10000000
 DB %00000000,%10000000

;background 
 DB %00000000,%00000000
 DB %00000000,%00000000
 DB %00000000,%00000000
 DB %00000000,%00000000
 DB %00000000,%00000000
 DB %00000000,%00000000
 DB %00000000,%00000000
 DB %11111111,%11111111

 ;blank
 DB %00000000,%00000000
 DB %00000000,%00000000
 DB %00000000,%00000000
 DB %00000000,%00000000
 DB %00000000,%00000000
 DB %00000000,%00000000
 DB %00000000,%00000000
 DB %00000000,%00000000

;cloud
 DB %00000001,%00000000
 DB %00000001,%00000000
 DB %00000110,%00000000
 DB %00001000,%00000000
 DB %00011000,%00000000
 DB %01100000,%00000000
 DB %10000000,%00000000
 DB %11111111,%00000000

 DB %11100000,%00000000
 DB %00010000,%00000000
 DB %00001000,%00000000
 DB %00001000,%00000000
 DB %00001100,%00000000
 DB %00000110,%00000000
 DB %00000001,%00000000
 DB %11111111,%00000000

 DB %00001000,%00000000
 DB %00010010,%00000000
 DB %00100100,%00000000
 DB %00001000,%00000000
 DB %00010001,%00000000
 DB %00000010,%00000000
 DB %00000000,%00000000
 DB %00000000,%00000000

 DB %00100000,%00000000
 DB %01000000,%00000000
 DB %00001111,%00000000
 DB %10001001,%00000000
 DB %00001101,%00000000
 DB %00000001,%00000000
 DB %01111110,%00000000
 DB %00000000,%00000000

TilesEnd:


SECTION "Tilemap", ROM0

Tilemap:
  DB $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B
  DB $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B
  DB $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B
  DB $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1C, $1D, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B
  DB $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1E, $1F, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B
  DB $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1C, $1D, $1B, $1B, $1B, $1C, $1D, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B
  DB $1B, $1B, $1C, $1D, $1B, $1B, $1B, $1B, $1E, $1F, $1B, $1B, $1B, $1E, $1F, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B
  DB $1B, $1B, $1E, $1F, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B
  DB $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B
  DB $1B, $1B, $1B, $1B, $1B, $1C, $1D, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B
  DB $1B, $1B, $1B, $1B, $1B, $1E, $1F, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B
  DB $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B
  DB $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B
  DB $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B
  DB $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B
  DB $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A
  DB $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B
  DB $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B
  DB $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B
  DB $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B
  DB $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B
  DB $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B
  DB $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B
  DB $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B
  DB $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B
  DB $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B
  DB $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B
  DB $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B
  DB $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B
  DB $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B
  DB $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B
  DB $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B
  DB $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B
  DB $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B, $1B
TilemapEnd:
