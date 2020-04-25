push_regs macro r1,r2,r3,r4,r5,r6,r7,r8,r9 ;push to all registers you want (doesnt have to be 9).
	irp reg,<r9,r8,r7,r6,r5,r4,r3,r2,r1>
		ifnb <reg>
			push reg
		endif
	endm
endm

pop_regs macro r1,r2,r3,r4,r5,r6,r7,r8,r9 ;push to all registers you want (doesnt have to be 9).
	irp reg,<r1,r2,r3,r4,r5,r6,r7,r8,r9>
		ifnb <reg>
			pop reg
		endif
	endm
endm

JE_F macro label ;je command, but with jmp limits
   local SKIP
   jne SKIP
   jmp label
SKIP:       
endm

JNE_F macro label ;jne command, but with jmp limits
   local SKIP
   je SKIP
   jmp label
SKIP:       
endm

JNZ_F macro label ;jnz command, but far (jmp limits)
	local skip
	jz skip 
	jmp label
skip:
endm

;Print any pic (bmp). Values: offset of variable that keeps pic name.
PrintBmp macro NameOfPic
	push NameOfPic ; the offset has to be in the variable. The 'pop' of this is in OpenFile proc.
	call OpenFile 
	call ReadHeader
	call ReadPalette
	call CopyPal
	call CopyBitmap
endm

;Print any pic (bmp). Values: name of variable that keeps pic name.
PrintBmp1 macro NameOfPic
	push offset NameOfPic ;Here, we just need the name of the pic in order to print it.
	call OpenFile 
	call ReadHeader
	call ReadPalette
	call CopyPal
	call CopyBitmap
endm
	
IDEAL
MODEL small
STACK 100h
DATASEG

BombPic db 'Bomb.bmp',0
Menu db 'Menu.bmp',0
Inst db 'Inst.bmp',0
Lost db 'Lose.bmp',0
Passed db 'Passed.bmp', 0

One db 'One.bmp', 0
Two db 'Two.bmp', 0
Three db 'Three.bmp', 0
Four db 'Four.bmp', 0
Five db 'Five.bmp', 0
Six db 'Six.bmp', 0
Seven db 'Seven.bmp', 0
Eight db 'Eight.bmp', 0
Nine db 'Nine.bmp', 0
Ten db 'Ten.bmp', 0

Winner db 'Winner.bmp', 0

filehandle dw ?
Header db 54 dup (0)
Palette db 256*4 dup (0)
ScrLine db 320 dup (0)
ErrorMsg db 'Error', 13, 10,'$'

;Mouse shape:
SpaceMouse 	dw 1111111011111111b
            dw 1111111011111111b
            dw 1111111011111111b
            dw 1111111011111111b
            dw 1111111011111111b
            dw 1111111011111111b
            dw 1111111011111111b
            dw 0000000100000000b
            dw 1111111011111111b
            dw 1111111011111111b
            dw 1111111011111111b
            dw 1111111011111111b
            dw 1111111011111111b
            dw 1111111011111111b
            dw 1111111011111111b
            dw 1111111011111111b

            dw 1111111011111110b
            dw 1100000100000110b
            dw 1010000100001010b
            dw 1001000100010010b
            dw 1000100100100010b
            dw 1000010101000010b
            dw 1000001110000010b
            dw 0111111011111100b
            dw 1000001110000010b
            dw 1000010101000010b
            dw 1000100100100010b
            dw 1001000100010010b
            dw 1010000100001010b
            dw 1100000100000110b
            dw 1111111011111110b
            dw 0000000000000000b
			
ShotsArr 	dw 150 dup(0), '$';up to 50 shots together.
;This array order: kind of shot, x pos, y pos, kind, x, y,....


Shot 		db 14, 14, 14, 14, 14, 14, 14, 14 
					
ShotSide_width equ 8 ;shot from left or right
ShotSide_Height equ 1

ShotUp_width 	equ 1 ;shot from up or down
ShotUp_Height 	equ 8

Pic_X 			dw ? ;variables for the printing proc - changed every time we print something.
Pic_Y 			dw ? ;have to be pushed.
;This might be changed to 'Pic_X/Y' because it is not just for shots.

WidthOfPic 		dw ? ;this is a variable to hold widths of pics
HeightOfPic 	dw ? ;this is a variable to hold heights of pics

;The Asteroid pic
Ast db 0, 0, 0, 0, 0, 28, 0, 31, 0, 30, 0, 0, 0, 0, 0, 0
	db 0, 0, 30, 0, 27, 0, 31, 30, 0, 29, 30, 27, 0, 29, 0, 0
	db 0, 31, 27, 29, 28, 30, 29, 27, 26, 29, 0, 28, 0, 0, 0, 0
	db 0, 23, 0, 30, 0, 26, 31, 28, 29, 27, 29, 0, 28, 0, 29, 0
	db 0, 29, 0, 27, 29, 0, 25, 29, 28, 28, 26, 27, 29, 0, 25, 0
	db 0, 0, 0, 24, 28, 29, 0, 24, 27, 0, 26, 29, 25, 24, 27, 0
	db 0, 23, 30, 0, 26, 27, 24, 0, 28, 0, 25, 30, 0, 24, 0, 0
	db 0, 25, 29, 24, 0, 30, 26, 24, 28, 27, 25, 0, 26, 0, 26, 24
	db 28, 24, 25, 27, 28, 0, 25, 23, 26, 27, 23, 27, 24, 0, 25, 22
	db 24, 23, 0, 25, 22, 27, 24, 0, 25, 23, 22, 28, 24, 26, 30, 23
	db 25, 0, 24, 22, 0, 25, 23, 22, 0, 25, 25, 0, 26, 24, 27, 0
	db 25, 0, 31, 23, 26, 0, 29, 24, 23, 0, 26, 24, 27, 26, 23, 0
	db 22, 25, 26, 0, 29, 26, 24, 0, 26, 0, 24, 26, 0, 25, 0, 0
	db 0, 25, 20, 0, 23, 25, 22, 25, 0, 23, 0, 30, 0, 0, 0, 0
	db 0, 0, 27, 19, 28, 26, 0, 20, 24, 22, 21, 25, 0, 0, 0, 0
	db 0, 0, 23, 24, 20, 0, 24, 23, 20, 21, 22, 24, 25, 0, 0, 0
	db 0, 22, 19, 0, 24, 23, 24, 22, 25, 0, 21, 0, 23, 20, 0, 0
	
	AstArr dw 3*16 dup(0), '!' ;Place for up to 15 (16-1) asts.
	
	Ast_Width equ 16
	Ast_Height equ 15
	
	BombsRemaining db 2 
	
CODESEG

;--------------------------------------------------------------------------------------------------
;This is a game called "space survivor".
;Move with the mouse, shoot with A,S,W,D.

;The code is devided to 4 different types of procs,
;according to their uses.
;
;An explanation about each proc and what it does is 
;above each proc.
;
;The code itself is combined with notes that explain
;it, too.
;--------------------------------------------------------------------------------------------------

;--------------------------------------------------------------------------------------------------
;  _____      _       _     ____  __  __ _____  
; |  __ \    (_)     | |   |  _ \|  \/  |  __ \ 
; | |__) | __ _ _ __ | |_  | |_) | \  / | |__) |
; |  ___/ '__| | '_ \| __| |  _ <| |\/| |  ___/ 
; | |   | |  | | | | | |_  | |_) | |  | | |     
; |_|   |_|  |_|_| |_|\__| |____/|_|  |_|_|     
;                                               
;This procs take care of printing the bmp files.
;I took those from the book. These are not mine!
;--------------------------------------------------------------------------------------------------                                               

proc OpenFile
	push bp
	mov bp, sp

	; Open file
	mov ah, 3Dh
	xor al, al
	mov dx, [word bp+4] ;offset filename
	int 21h
	jc openerror
	mov [filehandle], ax
	pop bp
	ret 2
	openerror:
	mov dx, offset ErrorMsg
	mov ah, 9h
	int 21h
	pop bp
	ret 2
	endp OpenFile
	
	
	proc ReadHeader
	; Read BMP file header, 54 bytes
	mov ah,3fh
	mov bx, [filehandle]
	mov cx,54
	mov dx,offset Header
	int 21h
	ret
	endp ReadHeader
	
	
	proc ReadPalette
	; Read BMP file color palette, 256 colors * 4 bytes (400h)
	mov ah,3fh
	mov cx,400h
	mov dx,offset Palette
	int 21h
	ret
	endp ReadPalette
	
	
	proc CopyPal
	; Copy the colors palette to the video memory
	; The number of the first color should be sent to port 3C8h
	; The palette is sent to port 3C9h
	mov si,offset Palette
	mov cx,256
	mov dx,3C8h
	mov al,0
	; Copy starting color to port 3C8h
	out dx,al
	; Copy palette itself to port 3C9h
	inc dx
	PalLoop:
	; Note: Colors in a BMP file are saved as BGR values rather than RGB.
	mov al,[si+2] ; Get red value.
	shr al,2 ; Max. is 255, but video palette maximal
	; value is 63. Therefore dividing by 4.
	out dx,al ; Send it.
	mov al,[si+1] ; Get green value.
	shr al,2
	out dx,al ; Send it.
	mov al,[si] ; Get blue value.
	shr al,2
	out dx,al ; Send it.
	add si,4 ; Point to next color.
	; (There is a null chr. after every color.)
	loop PalLoop
	ret
	endp CopyPal
	
	
	proc CopyBitmap
	; BMP graphics are saved upside-down.
	; Read the graphic line by line (200 lines in VGA format),
	; displaying the lines from bottom to top.
	mov ax, 0A000h
	mov es, ax
	mov cx,200
	PrintBMPLoop:
	push cx
	; di = cx*320, point to the correct screen line
	mov di,cx
	shl cx,6
	shl di,8
	add di,cx
	; Read one line
	mov ah,3fh
	mov cx,320
	mov dx,offset ScrLine
	int 21h
	; Copy one line into video memory
	cld ; Clear direction flag, for movsb
	mov cx,320
	mov si,offset ScrLine
	rep movsb ; Copy line to the screen
	;rep movsb is same as the following code:
	;mov es:di, ds:si
	;inc si
	;inc di
	;dec cx
	;loop until cx=0
	pop cx
	loop PrintBMPLoop
	
	mov ah, 3eh ;Close file
	mov bx, [filehandle]
	int 21h
	
	ret
	endp CopyBitmap
	
;--------------------------------------------------------------------------------------------------
;All of the following procs was written by Gefen Hajaj.
;--------------------------------------------------------------------------------------------------

;--------------------------------------------------------------------------------------------------
;  _                 _        _____                    
; | |               (_)      |  __ \                   
; | |     ___   __ _ _  ___  | |__) | __ ___   ___ ___ 
; | |    / _ \ / _` | |/ __| |  ___/ '__/ _ \ / __/ __|
; | |___| (_) | (_| | | (__  | |   | | | (_) | (__\__ \
; |______\___/ \__, |_|\___| |_|   |_|  \___/ \___|___/
;               __/ |                                  
;              |___/                                   
;
; This proc manage all of the logical area of the game.
;--------------------------------------------------------------------------------------------------


;----------------------------------------------
;This proc return a random number in dx:
;0 -> the number we want.
;
;Values: The maximum number+1 (Through stack).
;----------------------------------------------
proc Rand 
	push bp
	mov bp, sp
	push_regs ax, bx, cx
	
	xor ax, ax
	mov ah,2Ch
	int 21h ;ch - hour, cl - minute, dh - second, dl - milisecond.
	cmp dl, dh ;Random moves to create a random number
	jne @@continue
	add ax, 201
	shr ax, 1
@@continue:
	sub dl, 30
	cmp dl, dh
	jne continue
	xor ax, cx
continue:
	mov ax, cx 
	mul dx ;multiply by random number --> ax(dx)
	shl dx, 3
	mul dx
	shr cx, 3
	add ax, dx
	shl cx, 3 
	add ch, cl
	add ax, cx
	mov bx, 505
	xor bx, cx
	sub ax, bx
	add ax, dx
	shl dx, 2
	add ax, dx
	add dh, dl
	shr dx, 4
	add ax, dx 
	or cx, ax
	add ah, al
	or cx, dx
IncLoop:
	inc ax
	loop IncLoop
	inc ax
	xor dx, dx
	div [word ptr bp+4] ;now, there is a completely random number in ax. we want a number that is not bigger than the numer we pushed in the beginning...
	;...so we devide the random number in ax by the number we entered (we checked - it's not 0) and the remainder (dx) will not be bigger than that number-1.
	;Now, the random number is in dx
	
	pop_regs ax, bx, cx
	pop bp
	ret 2
endp Rand

;---------------------------------
;this proc changes the mouse shape.
; gets the offset of the mouse data 
; through stack.
;
;This proc was written with help
;from one of the presentations.
;----------------------------------
proc ChangeMouse
	push bp
	mov bp, sp
	push_regs ax, es, bx, cx, dx 
	
	mov ax, ds
	mov es, ax
 
    mov ax,00h ;Initiate Mouse
    int 33h
; ----------------------= Define graphics cursor = the location of the point 
	mov ax,09h
	mov bx,0	; col <-16 +16> 	(0,0 is the middle of the mouse)
	mov cx,0	; row <-16 +16>
	mov dx,offset SpaceMouse
	int 33h

    mov ax,01h ;Show mouse
    int 33h
	
	pop_regs ax, es, bx, cx, dx 
	pop bp
	ret 2
endp ChangeMouse

;----------------------------------
;this proc checks if the spaceship
;hit the edge of the screen.
;
;it returns 1 in ax if it is,
;and 0 if it is not.
;
;No values needed.
;----------------------------------
proc CheckIfHitEdge
	push_regs bx, cx, dx ; int33/ax=3 affects bx too
	
	mov ax, 3h ;Get mouse info
	int 33h ;cx<-- x position, dx<--y position and something in bx.
	
	
	xor ax, ax
	shr cx, 1 ;cx contains double the real value!
	cmp cx, 0
	je hit
	cmp cx, 319-14
	jae hit
	cmp dx, 0
	je hit
	cmp dx, 199-14
	jae hit
	jmp @@EndOfProc
	
hit:
	mov ax, 1

@@EndOfProc:
	pop_regs bx, cx, dx
	ret
endp CheckIfHitEdge

;------------------------------------------------
;This proc checks the input and adds a new shot
;to the array of shots accordingly.
;
;No values needed. No values are changed.
;------------------------------------------------
proc NewShot
	push bx
	cmp al, 20h ;D
	jne @@continue
	mov bx, 1
	push bx
	call AddNewShotToArr
	jmp @@continue3
	
@@continue:
	cmp al, 1eh ;A
	jne @@continue1
	mov bx, 2
	push bx
	call AddNewShotToArr
	jmp @@continue3
	
@@continue1:
	cmp al, 11h ;W
	jne @@continue2
	mov bx, 3
	push bx
	call AddNewShotToArr
	jmp @@continue3
	
@@continue2:
	cmp al, 1fh ;S
	jne @@continue3
	mov bx, 4
	push bx
	call AddNewShotToArr
@@continue3:
	pop bx
	ret
endp NewShot

;------------------------------------------
;This proc adds a new shot to ShotsArr.
;
;Values: the number of the shot:
; - shot from right -> 1.
; -	shot from left -> 2.
; - a shot from up -> 3.
; - a shot from down -> 4.
; (Through stack)
;------------------------------------------
proc AddNewShotToArr
	push bp 
	mov bp, sp
	push_regs ax, bx, cx, dx, si 

	mov ax, 3h ;Get mouse info
	int 33h ;cx<-- x position, dx<--y position.
	shr cx, 1
	add cx, 7
	add dx, 7
	
	mov bx, offset ShotsArr
	xor si, si 
	
@@FindFirstAvailablePlace:
	cmp [word ptr bx+si], 0
	je @@continue
	cmp [word ptr bx+si], '$' ;Check if there is enough space.
	je @@EndOfProc
	add si, 2
	jmp @@FindFirstAvailablePlace
	
@@continue:
	mov ax, [bp+4]
	mov [word ptr bx+si], ax ;The shot number
	mov [word ptr bx+si+2], cx ;X pos
	mov [word ptr bx+si+4], dx ;Y pos
	
@@EndOfProc:

	pop_regs ax, bx, cx, dx, si 
	pop bp
	ret 2
endp AddNewShotToArr

;-----------------------------------------------------------
;This proc prints a bitmap that we created (we can see
; those in DATASEG.
;
;Values:
;Push: offset of pic, x, y (according to that order)
; variables: [HeightOfPic], [WidthOfPic] - have to be set.
;
;This proc was written with inspiration from a proc I 
;found in one of the presentations.
;-----------------------------------------------------------
proc DrawPic
	push bp
	mov bp, sp
	push_regs ax, bx, cx, dx, si
	
	;Preparing for printing - getting the right values.
	mov dh, [byte ptr HeightOfPic]
	mov dl, [byte ptr WidthOfPic]
    mov cx, [bp+6] ;X
    mov bx, [bp+4] ;Y
	
    mov si, [bp+8]  ;mov si,offset pic
drawing:
    mov al, [byte ptr si]
	
    call PutPixel
	
    inc si ;move to the next pixel
    inc cx ;move to the next x position (on screen)
	dec dl 
    jnz drawing                  ; X loop
	
    mov dl, [byte ptr WidthOfPic]
    sub cx, [WidthOfPic]
    inc bx
	
    dec dh
    jnz drawing                  ; Y loop
	
	pop_regs ax, bx, cx, dx, si
	pop bp
	ret 6
endp DrawPic

;------------------------------------------------
;This proc puts a pixel according to the 
;values in - bx (y), cx (x) and al (color).
;
;This proc works with "DrawPic".
;------------------------------------------------
proc PutPixel 
		push_regs di, bx, dx
        xor di, di
		mov dx, bx ;claculating the offset where we want to draw the pixel
		shl dx, 8 ;y*2^8
		shl bx, 6 ;y*2^6
		add di, cx ;0+x
		add di, bx ;0+x+y*2^6
		add di, dx ;0+x+y*2^6+y*2^8 - the final address (offset)
		stosb		; mov es:[di]<--al, inc di
		pop_regs di, bx, dx
		ret
endp PutPixel

;------------------------------------------------
;This proc prints all of the shots currenlty 
;in the array of shots.
;
;Values: offset of ShotsArr (through stack).
;------------------------------------------------
proc PrintShots
	push bp
	mov bp, sp
	push_regs ax, bx, dx, es, si
	
	mov bx, [bp+4] ;ShotsArr
	xor si, si
	
Printing:
	cmp [word ptr bx+si], 0 ;check if reached the end
	je @@Finished
	cmp [word ptr bx+si], '$'
	je @@Finished
	
	cmp [word ptr bx+si], 2 ;checks type of shot (right - 1, left - 2, up - 3, down - 4)
	ja UpShotPrint
	
;side shot: (Horizontal shot)
	mov dx, [word ptr bx+si+2] ;x pos
	mov [Pic_X], dx
	mov dx, [word ptr bx+si+4] ;y pos
	mov [Pic_Y], dx 
	
	mov ax, 0A000h ;Graphical Memory
	mov es, ax
	
	;Preparing for printing:
	mov [byte ptr HeightOfPic], ShotSide_Height
	mov [byte ptr WidthOfPic], ShotSide_width
	push offset Shot
	push [word ptr Pic_X]
	push [word ptr Pic_Y]
	jmp @@continue
	
UpShotPrint: ;Vertical shot
	mov dx, [word ptr bx+si+2] ;x pos
	mov [Pic_X], dx
	mov dx, [word ptr bx+si+4] ;y pos
	mov [Pic_Y], dx 
	
	mov ax, 0A000h ;Graphical Memory
	mov es, ax
	
	;Preparing for printing
	mov [byte ptr HeightOfPic], ShotUp_Height
	mov [byte ptr WidthOfPic], ShotUp_width
	push offset Shot
	push [word ptr Pic_X]
	push [word ptr Pic_Y]
	
	
@@continue:
	call DrawPic
	add si, 6 ;move to the next shot
	jmp Printing
	
@@Finished:
	pop_regs ax, bx, dx, es, si
	pop bp
	ret 2
endp PrintShots

;--------------------------------------------------
;This proc updates the positions of the shots
;in order for them to move.
;
;Values: offset of ShotsArr (through stack)
;--------------------------------------------------
proc MoveShots
	push bp
	mov bp, sp
	push_regs bx, si
	
	mov bx, [bp+4] ;beginnig of proc (shots)
	xor si, si ;start from the beginning
	
Updating:
	cmp [word ptr bx+si], 0
	je @@Finished
	cmp [word ptr bx+si], '$'
	je @@Finished
	
	cmp [word ptr bx+si], 1 ;Right shot
	jne @@ContinueChecking
	add [word ptr bx+si+2], 10 ;x pos
	jmp @@continue
	
@@ContinueChecking:
	cmp [word ptr bx+si], 2 ;Left shot
	jne @@ContinueChecking1
	sub [word ptr bx+si+2], 10
	jmp @@continue
	
@@ContinueChecking1:
	cmp [word ptr bx+si], 3 ;Up shot
	jne @@ContinueChecking2
	sub [word ptr bx+si+4], 10 ;y pos
	jmp @@continue
	
@@ContinueChecking2:
	add [word ptr bx+si+4], 10 ;Has to be down shot
	
@@continue:
	add si, 6 ;move to the next shot
	jmp Updating
	
@@Finished:
	pop_regs bx, si
	pop bp
	ret 2
endp MoveShots

;------------------------------------------------
;This proc "covers" the whole screen 
;with black pixels = cleans the 
;screen.
;It also print 10 white pixels = stars.
;
;No values needed.
;------------------------------------------------
proc CleanScreen 
	push_regs ax, cx, di, es
	
	mov ax, 2
	int 33h ;hide mouse (the CleanScreen proc does not work well when the mouse is visible)
	
	mov ax, 0A000h
	mov es, ax
	xor di,di ;move the the first offset of the part of the video memory
	mov cx,320*200 ;cover the whole screen - 320*200 pixels.
	mov al,0 ;color (black)
	rep stosb ; mov es:[di]<--al ,add di,1 (rep=cx times)
	
	mov cx, 10
	mov al, 0fh
	mov di, 5
@@Stars:
	stosb
	add di, 2000h
	loop @@Stars
	
	mov ax, 1
	int 33h ;show mouse again

	pop_regs ax, cx, di, es
	ret 
endp CleanScreen

;-----------------------------------------
;This proc checks if one of the shots
;hit the edge of the screen.
;If it is, it deletes it from the array,
;and moves the other shots back.
;
;Values:
;Offset of ShotsArr (through stack)
;-----------------------------------------
proc CheckIfShotHitEdge
	push bp
	mov bp, sp
	push_regs bx, si
	
	mov bx, [bp+4] ;The beginning of the array of shots
	xor si, si
	
@@Checking:
	cmp [word ptr bx+si], 0 ;Check if end of arr or last shot
	je @@Finished
	cmp [word ptr bx+si], '$'
	je @@Finished
	
	cmp [word ptr bx+si+2], 10 ;x pos
	jbe @@HitFound
	cmp [word ptr bx+si+2], 319-16
	jae @@HitFound
	cmp [word ptr bx+si+4], 5 ;y pos
	jbe @@HitFound
	cmp [word ptr+bx+si+4], 199-10
	jb @@continue
@@HitFound:
	call DeleteShot
	
@@continue:
	add si, 6 ;Move to the next shot
	jmp @@Checking
	
@@Finished:
	pop_regs bx, si
	pop bp
	ret 2
endp CheckIfShotHitEdge

;----------------------------------------
;This proc deletes a shot from the array
;of shots and moves the other shots to 
;fill its place
;
;Values: the offset of the requiered 
;shot - in bx and si.
;----------------------------------------
proc DeleteShot
	push_regs bx, si, cx 
	
@@Deleting:
	cmp [word ptr bx+si+6], '$' ;Check if we hit the end of the proc
	jne @@continue
	mov [word ptr bx+si], 0
	mov [word ptr bx+si+2], 0
	mov [word ptr bx+si+4], 0
	jmp @@Finished
	
@@continue:
	mov cx, [word ptr bx+si+6]
	mov [word ptr bx+si], cx ;The type of the next shot
	
	mov cx, [word ptr bx+si+8]
	mov [word ptr bx+si+2], cx ;The x pos of the next shot
	
	mov cx, [word ptr bx+si+10]
	mov [word ptr bx+si+4], cx ;The y pos of the next shot
	
	cmp [word ptr bx+si], 0
	je @@Finished
	
	add si, 6 ;move to the next shot
	jmp @@Deleting
	
@@Finished:
	pop_regs bx, si, cx
	ret
endp DeleteShot

;-----------------------------------
;This proc adds one new random ast
;to the array of ast.
;
;Values: offset of the array of
;ast (AstArr - Through stack).
;-----------------------------------
proc CreateNewAst
	push bp
	mov bp, sp
	push_regs bx, si , dx 
	
	mov bx, [bp+4] ;The beginnig of the array
	xor si, si
	
@@FindFirstAvailablePlace:
	cmp [word ptr bx+si], 0
	je @@continue
	cmp [word ptr bx+si], '$' ;Not enough space
	JE_F @@EndOfProc
	add si, 2
	jmp @@FindFirstAvailablePlace
	
@@continue:
	;Find Direction (From right - 1, left - 2, up - 3, down 4):
	push 4
	call Rand ;Now, dx holds 0-3
	inc dx ; 1-4
	mov [word ptr bx+si], dx
	
	;Check direction. X,Y values for each one:
	cmp dx, 1
	jne @@check
	mov [word ptr bx+si+2], 310-16 ;x pos
	push 170
	call Rand
	add dx, 7
	mov [word ptr bx+si+4], dx ;y pos
	jmp @@EndOfProc
	
@@Check:
	cmp dx, 2
	jne @@Check1
	mov [word ptr bx+si+2], 7 ;x pos
	push 170
	call Rand
	add dx, 7
	mov [word ptr bx+si+4], dx ;y pos
	jmp @@EndOfProc
	
@@check1:
	cmp dx, 3
	jne @@check2
	mov [word ptr bx+si+4], 10 ;y pos
	push 305-16
	call Rand
	add dx, 7
	mov [word ptr bx+si+2], dx ;x pos
	jmp @@EndOfProc
	
@@check2:
	mov [word ptr bx+si+4], 190-15 ;y pos
	push 305 -16
	call Rand
	add dx, 7
	mov [word ptr bx+si+2], dx ;x pos
	
@@EndOfProc:
	pop_regs bx, si ,dx
	pop bp
	ret 2
endp CreateNewAst

;-------------------------------------------------
;This proc prints all the asteroids in their 
;places. 
;
;Values: The offset of the beginnig of the array
;of asteroids - AstArr (Through stack).
;-------------------------------------------------
proc PrintAsts
	push bp
	mov bp, sp
	push_regs ax, bx, es, si 
	
	mov ax, 0A000h ;Graphical Memory
	mov es, ax
	
	mov bx, [bp+4] ;The beginning of the array of Asts
	xor si, si
	
PrintingAst:
	cmp [word ptr bx+si], 0 ;check if reached the end
	je @@Finished
	cmp [word ptr bx+si], '$'
	je @@Finished
	
	;Preparing for printing:
	mov [byte ptr HeightOfPic], Ast_Height
	mov [byte ptr WidthOfPic], Ast_Width
	push offset Ast
	push [word ptr bx+si+2] ;x pos
	push [word ptr bx+si+4] ;y pos
	
	call DrawPic
	add si, 6 ;Move to the next ast
	jmp PrintingAst
	
@@Finished:
	pop_regs ax, bx, es, si
	pop bp
	ret 2
endp PrintAsts


;-----------------------------------------
;This proc changes the x, y pos
;of the asts in order for them to move.
;
;Values: The offset of the beginnig of
;the asts array - AstArr (Through stack).
;-----------------------------------------
proc MoveAst
	push bp
	mov bp, sp
	push_regs bx, si
	
	mov bx, [bp+4] ;The offset of the beginnig of AstArr
	xor si, si
	
UpdatingPos:
	cmp [word ptr bx+si], 0 ;Check if finished
	je @@Finished
	cmp [word ptr bx+si] , '$'
	je @@Finished
	
	cmp [word ptr bx+si], 1 ;Right Ast
	jne @@ContinueChecking
	sub [word ptr bx+si+2], 5 ;x pos
	inc [word ptr bx+si+4]
	jmp @@continue
	
@@ContinueChecking:
	cmp [word ptr bx+si], 2 ;Left Ast
	jne @@ContinueChecking1
	add [word ptr bx+si+2], 5
	sub [word ptr bx+si+4], 2
	jmp @@continue
	
@@ContinueChecking1:
	cmp [word ptr bx+si], 3 ;Up Ast
	jne @@ContinueChecking2
	add [word ptr bx+si+4], 5 ;y pos
	inc [word ptr bx+si+2]
	jmp @@continue
	
@@ContinueChecking2:
	sub [word ptr bx+si+4], 5 ;Down Ast
	sub [word ptr bx+si+2], 2
	
@@continue:
	add si, 6 ;move to the next Ast
	jmp UpdatingPos

@@Finished:
	pop_regs bx, si
	pop bp
	ret 2
endp MoveAst


;----------------------------------------------
;This proc checks if an ast hit one of the 
;edges. If it did, it deletes it from the
;array and moves the whole array back to save
;space.
;
;Values: offset of beginning of array of
;asts - AstArr (Through stack).
;----------------------------------------------
proc CheckIfAstHitEdge
	push bp
	mov bp, sp
	push_regs bx, si 
	
	mov bx, [bp+4] ;The beginning of the array of ast
	xor si, si
	
@@Checking:
	cmp [word ptr bx+si], 0 ;Check if end of arr or last ast
	je @@Finished
	cmp [word ptr bx+si], '$'
	je @@Finished
	
	cmp [word ptr bx+si+2], 6 ;x pos
	jbe @@HitFound
	cmp [word ptr bx+si+2], 298 ;319-6-15
	jae @@HitFound
	cmp [word ptr bx+si+4], 6 ;y pos
	jbe @@HitFound
	cmp [word ptr+bx+si+4], 178 ;199-6-15
	jb @@continue
@@HitFound:
	call DeleteAst
	
@@continue:
	add si, 6 ;Move to the next ast
	jmp @@Checking
	
@@Finished:
	pop_regs bx, si
	pop bp
	ret 2
endp CheckIfAstHitEdge


;----------------------------------
;Deletes the requested ast and 
;aranges the array.
;
;Values: the right address of ast
;in bx and si.
;----------------------------------
proc DeleteAst
	push_regs bx, si, cx
	
@@Deleting:
	cmp [word ptr bx+si+6], '$' ;Check if we hit the end of the proc
	jne @@continue
	mov [word ptr bx+si], 0
	mov [word ptr bx+si+2], 0
	mov [word ptr bx+si+4], 0
	jmp @@Finished
	
@@continue:
	mov cx, [word ptr bx+si+6]
	mov [word ptr bx+si], cx ;The type of the next ast
	
	mov cx, [word ptr bx+si+8]
	mov [word ptr bx+si+2], cx ;The x pos of the next ast
	
	mov cx, [word ptr bx+si+10]
	mov [word ptr bx+si+4], cx ;The y pos of the next ast
	
	cmp [word ptr bx+si], 0
	je @@Finished
	
	add si, 6 ;move to the next shot
	jmp @@Deleting
	
@@Finished:
	pop_regs bx, si, cx
	ret
endp DeleteAst


;------------------------------------
;This proc checks if the mouse hit
;one of the asteroids.
;If yes - 1 in ax. If no - 0 in ax.
;
;Values: offset of beginning of ast
; array - AstArr (Through stack).
;------------------------------------
proc CheckIfMouseHitAst
	push bp
	mov bp, sp
	push_regs bx, cx, dx, di, si
	
	mov ax, 3h ;Get mouse info
	int 33h ;cx<-- x position, dx<--y position and something in bx.
	shr cx, 1 ;cx contains double the real value!
	
	mov bx, [bp+4] ;The beginning of AstArr
	xor si, si
	xor ax, ax
	
@@Checking:
	cmp [word ptr bx+si], 0 ;Check if reached the end of the array of ast
	je @@Finished
	cmp [word ptr bx+si], '$'
	je @@Finished
	
;Check if x pos is in range:
	mov di, [word ptr bx+si+2] ;x pos
	add di, Ast_Width
	cmp di, cx
	jb @@Next
	
	add cx, 15
	cmp [word ptr bx+si+2], cx ;x pos
	
	pushf ;keep the real value in cx
	sub cx, 15
	popf
	
	ja @@Next 
	
;Check if y pos is in range:
	mov di, [word ptr bx+si+4] ;y pos
	add di, Ast_Height
	cmp di, dx
	jb @@Next
	
	add dx, 15
	cmp [word ptr bx+si+4], dx ;y pos
	
	pushf
	sub dx, 15
	popf
	
	ja @@Next
	
;Hit:
	mov ax, 1
	jmp @@Finished
	
	
@@Next:
	add si, 6 ;Move to the next ast
	jmp @@Checking
	

	
@@Finished:
	pop_regs bx, cx, dx, di, si
	pop bp
	ret 2
endp CheckIfMouseHitAst

;--------------------------------------------
;This proc checks if a shot hit an ast.
;If it did, it deletes the ast from the 
;array and the shot from the array.
;
;Values: offset of ShotsArr and offset of
;AstArr (Through stack, that order).
;--------------------------------------------
proc CheckIfShotHitAst
	push bp
	mov bp, sp
	push_regs ax, bx, cx, dx, si, di
	
	
	mov bx, [bp+6] ;ShotsArr
	xor si, si
	
@@CheckingOut:
	xor al, al ;Like a flag
	cmp [word ptr bx+si], 0 ;Check if we reached the end of the shots array
	JE_F @@Finished
	cmp [word ptr bx+si], '$'
	JE_F @@Finished
	
	mov cx, [word ptr bx+si+2] ;X pos
	mov dx, [word ptr bx+si+4] ;Y pos
	
	push bx
	push si
	
	cmp [word ptr bx+si], 2
	ja @@ItIsUpShot ;(or down)
	
	mov bx, [bp+4] ;AstArr
	xor si, si
@@Checking:
	cmp [word ptr bx+si], 0 ;Check if reached the end of the array of ast
	je @@OutOfThisLoop
	cmp [word ptr bx+si], '$'
	je @@OutOfThisLoop
	
;Check if x pos is in range:
	mov di, [word ptr bx+si+2] ;x pos
	add di, Ast_Width
	cmp di, cx
	jb @@Next
	
	add cx, ShotSide_width
	cmp [word ptr bx+si+2], cx ;x pos
	
	pushf ;keep the real value in cx
	sub cx, ShotSide_width
	popf
	
	ja @@Next 
	
;Check if y pos is in range:
	mov di, [word ptr bx+si+4] ;y pos
	add di, Ast_Height
	cmp di, dx
	jb @@Next
	
	add dx, ShotSide_Height
	cmp [word ptr bx+si+4], dx ;y pos
	
	pushf
	sub dx, ShotSide_Height
	popf
	
	ja @@Next
	
;Hit:
	call DeleteAst ;values that we need are already set
	mov al, 1
	jmp @@OutOfThisLoop
	
@@Next:
	add si, 6 ;Move to the next ast
	jmp @@Checking
	
@@OutOfThisLoop:
	pop si
	pop bx
	
	cmp al, 1
	jne @@continue
	xor al, al
	call DeleteShot
	jmp @@DontAdd
	
	
@@continue:
	add si, 6
@@DontAdd:
	jmp @@CheckingOut
	
	
@@ItIsUpShot: ;(or down. Vertical shot.)
	mov bx, [bp+4] ;AstArr
	xor si, si
@@Checking1:
	cmp [word ptr bx+si], 0 ;Check if reached the end of the array of ast
	je @@OutOfThisLoop1
	cmp [word ptr bx+si], '$'
	je @@OutOfThisLoop1
	
;check if x pos is in range:
	mov di, [word ptr bx+si+2] ;x pos
	add di, Ast_Width
	cmp di, cx
	jb @@Next1
	
	add cx, ShotUp_width
	cmp [word ptr bx+si+2], cx ;x pos
	
	pushf ;keep the real value in cx
	sub cx, ShotUp_width
	popf
	
	ja @@Next1 
	
;Check if y pos is in range:
	mov di, [word ptr bx+si+4] ;y pos
	add di, Ast_Height
	cmp di, dx
	jb @@Next1
	
	add dx, ShotUp_Height
	cmp [word ptr bx+si+4], dx ;y pos
	
	pushf
	sub dx, ShotUp_Height
	popf
	
	ja @@Next1
	
;Hit:
	call DeleteAst ;all the values we need are already set.
	mov al, 1
	jmp @@OutOfThisLoop1
	
@@Next1:
	add si, 6 ;Move to the next ast
	jmp @@Checking1
	
@@OutOfThisLoop1:
	pop si
	pop bx
	
	cmp al, 1
	jne @@continue1
	xor al, al
	call DeleteShot
	jmp @@DontAdd1
	
@@continue1:
	add si, 6 ;Move to next shot
@@DontAdd1:
	jmp @@CheckingOut
	
	
@@Finished:
	pop_regs ax, bx, cx, dx, si, di
	pop bp
	ret 4
endp CheckIfShotHitAst

;---------------------------------
;This proc allows the user
; to clear all of the 
;asteroids by pressing B.
;Inseting 0 to the whole
;arr.
;
;Values: the beginning of 
;the ast arr (AstArr) - stack.
;And, the current input 
;in al (scan code).
;---------------------------------
proc Bomb
	cmp al, 30h ;B - check if the user hit B, before doing anything.
	JNE_F @@EndOfProc1
	
	push bp
	mov bp, sp
	push_regs ax, bx, cx, dx, di, es, si
	
	cmp [byte ptr BombsRemaining], 0 ;Check if the user still has bombs to use
	JE_F @@EndOfProc
	
	; make a sound
	in 	al, 61h
	or 	al, 00000011b
	out 	61h, al
	mov 	al, 0b6h
	out 	43h, al
	mov 	ax, 5500h
	out 	42h, al
	mov 	al, ah
	out 	42h, al
	
	mov si, 2
	push si
	call Delaying
	
	; stop sound
	in 	al, 61h
	and	al, 11111100b
	out 	61h, al
	
	mov bx, [bp+4] ;The offset of the AstArr
	xor si, si
	
Cleaning:
	cmp [word ptr bx+si], 0
	je @@EndOfProcS
	cmp [word ptr bx+si], '$'
	je @@EndOfProcS
	
	mov [word ptr bx+si], 0
	mov [word ptr bx+si+2], 0
	mov [word ptr bx+si+4], 0
	add si, 6 ;move to the next ast
	jmp Cleaning
	
@@EndOfProcS:
	
	;Print the bomb pic, and delay (50 instead of 25 - in the regular PrintPic proc)
	mov ax, 2
	int 33h ;hide mouse (the CleanScreen proc does not work well when the mouse is visible)
	
	PrintBmp1 BombPic ;printing bomb screen
	
	mov bx, 50 ;delay
	push bx
	call Delaying 
	
	mov ax,13h ;Graphic mode (going back to original palette)
    int 10h
	push offset SpaceMouse ;Changing the mouse shape
	call ChangeMouse
	
	mov ax, 1
	int 33h ;show mouse again
	
	dec [byte ptr BombsRemaining]
	
@@EndOfProc:
	pop_regs ax, bx, cx, dx, di, es, si
	pop bp
	
@@EndOfProc1:
	ret 2
endp Bomb

;---------------------------------
;delay by x second/100.
;
;push the number (x) you want.
;---------------------------------
proc Delaying
	push bp
	mov bp, sp
	push_regs ax, bx, cx, dx

	
	;reads the time:
	;ch - hours
	;cl - minutes
	;dh - seconds 
	;dl - seconds/100
	mov ah, 2ch
	int 21h
	mov al, dl ;for delay
	
	mov bx, [bp+4] ;the num of seconds/100 to wait
	
@@Waiting:
	;reads the time:
	;ch - hours
	;cl - minutes
	;dh - seconds 
	;dl - seconds/100
	mov ah, 2ch
	int 21h
	
	cmp dl, al 
	je @@Waiting
	
	mov al, dl ;To stay relevant for next check.
	
	dec bx
	cmp bx, 0 ;check if the time has passed.
	jne @@Waiting

	pop_regs ax, bx, cx, dx
	pop bp
	ret 2
endp Delaying

;--------------------------------------
;This proc arranges the array of asts 
;for every new game, according to the 
;num of asts we want.
;
;Values: push num of asts you want.
;--------------------------------------
proc ArngArr 
	push bp
	mov bp, sp
	push_regs bx, si, cx
	
	;First, we have to clean the array of asteroids.
	;Asteroid cleaning.
	xor si,si
	mov bx, offset AstArr
@@TakeCareOfAsts:
	cmp [word ptr bx+si], 0
	je @@Done1
	cmp [word ptr bx+si], '!'
	je @@Done1
	
	mov [word ptr bx+si], 0
	mov [word ptr bx+si+2], 0
	mov [word ptr bx+si+4], 0
	add si, 6
	
	jmp @@TakeCareOfAsts
	
@@Done1:
	
	xor si, si
	mov cx, [bp+4] ;num of asts we need.
	
@@MakingRoom:
	add si, 6
	loop @@MakingRoom
	
	;Marking the end:
	mov [word ptr bx+si], '$' ;End
	
	pop_regs bx, si ,cx
	pop bp
	ret 2
endp ArngArr
	

	
;---------------------------------------------------------------------------------------------------------
;  __  __       _         _____  _             _               _____                    
; |  \/  |     (_)       |  __ \| |           (_)             |  __ \                   
; | \  / | __ _ _ _ __   | |__) | | __ _ _   _ _ _ __   __ _  | |__) | __ ___   ___ ___ 
; | |\/| |/ _` | | '_ \  |  ___/| |/ _` | | | | | '_ \ / _` | |  ___/ '__/ _ \ / __/ __|
; | |  | | (_| | | | | | | |    | | (_| | |_| | | | | | (_| | | |   | | | (_) | (__\__ \
; |_|  |_|\__,_|_|_| |_| |_|    |_|\__,_|\__, |_|_| |_|\__, | |_|   |_|  \___/ \___|___/
;                                         __/ |         __/ |                           
;                                        |___/         |___/                            
;
;These are the main procs that manage the game. This is the game itself.
;----------------------------------------------------------------------------------------------------------

	
;---------------------------------
;Main playing proc.
;
;No values needed.
;---------------------------------
proc Play
	
	mov ax,13h ;Graphic mode
    int 10h
	push offset SpaceMouse ;Changing the mouse shape
	call ChangeMouse
	
	
BefPlaying:

	call PlayNewGame
	mov ax, 2
	int 33h ;hide mouse (the CleanScreen proc does not work well when the mouse is visible)
	
BefPlaying1:
	xor ax, ax
	
	PrintBmp1 Menu ;Printning MainMenu
	
	
WaitForData1:
	mov ah, 1 ;Checks if there is something new
	int 16h
	jz WaitForData1
	mov ah, 0
	int 16h
	mov al, ah ;the scan code is in al.
	
	cmp al, 1 ;ESC
	je @@Ending
	
	cmp al, 17h ;I
	jne @@Conti
	
	PrintBmp1 Inst ;Print instructions
	
	push ax
	mov	ah, 1h ;Wait for key press
	int	21h
	pop ax
	
	jmp BefPlaying1
@@Conti:
	cmp al, 21h ;F
	jne @@Conti1
	mov ax,01h ;Show mouse
    int 33h
	
	push bx ;To make sure that there would only be 7 asts.
	mov bx, 7
	push bx
	call ArngArr
	pop bx
	
	call FreeMode
	jmp BefPlaying
	
@@Conti1:
	cmp al, 19h ;P
	jne WaitForData1 ;if nothing was chosen.
	mov ax,01h ;Show mouse
    int 33h
	call FullGame
	jmp BefPlaying

@@Ending:
	ret
endp Play
	
;---------------------------------
;This proc allows the player to 
;play freely, until he dies.
;When he dies, he is notified.
;Press ESC at any time to go out.
;
;No values needed.
;---------------------------------
proc FreeMode

	mov ax,13h ;Graphic mode
    int 10h
	push offset SpaceMouse ;Changing the mouse shape
	call ChangeMouse
	
	;reads the time:
	;ch - hours
	;cl - minutes
	;dh - seconds 
	;dl - seconds/100
	mov ah, 2ch
	int 21h
	mov bl, dl ;For Delay later
	;mov bh, dh

L1: 
	xor ax, ax
	call CheckIfHitEdge ;return 1 in ax if true, 0 if not
	cmp ax, 1
	JE_F OutOfLoop
	
	push offset AstArr
	call CheckIfMouseHitAst
	cmp ax, 1
	JE_F OutOfLoop
	
	;Get a char from the user
	mov ah, 1 ;Checks if there is something new
	int 16h
	jz NothingInKey
	mov ah, 0
	int 16h
	mov al, ah ;the scan code is in al.
NothingInKey:
	
	push offset AstArr
	call Bomb
	
	;Insert a new shot to array. If nothing new, 0 is inserted.
	call NewShot 
	
	push offset ShotsArr
	call CheckIfShotHitEdge
	
	push offset AstArr
	call CheckIfAstHitEdge
	
	push offset ShotsArr
	push offset AstArr
	call CheckIfShotHitAst
	
	push ax ;do it only every sec/100.
	;reads the time:
	;ch - hours
	;cl - minutes
	;dh - seconds 
	;dl - seconds/100
	mov ah, 2ch
	int 21h
	cmp dl, bl
	JE_F Delay
	mov bl, dl
	
	push offset AstArr
	call CreateNewAst
	
	;Updates the values of the shots
	push offset ShotsArr
	call MoveShots
	
	push offset AstArr
	call MoveAst
	
	call CleanScreen ;clears the whole screen
	
	;Prints the shots
	push offset ShotsArr 
	call PrintShots
	
	push offset AstArr
	call PrintAsts
Delay:
	pop ax
	
	
	dec al ;checks if ESC was pressed.
	jz EscPressed
	jmp L1
	
OutOfLoop:
	; make a sound (player lost)
	in 	al, 61h
	or 	al, 00000011b
	out 	61h, al
	mov 	al, 0b6h
	out 	43h, al
	mov 	ax, 5000h
	out 	42h, al
	mov 	al, ah
	out 	42h, al
	
	mov si, 2
	push si
	call Delaying
	
	; stop sound
	in 	al, 61h
	and	al, 11111100b
	out 	61h, al
	
	push offset Lost
	call PrintPic
EscPressed:

	ret 
endp FreeMode

;---------------------------------
;This proc is the full game proc.
;
;No values needed.
;---------------------------------
proc FullGame

;Level One:
	mov bx, 2 ;Preparing the AstArr for this level.
	push bx
	call ArngArr

	push offset One ;Printing level instructions
	call PrintPic
	mov bx, 5 ;Preparing for playing
	push bx
	push offset AstArr 
	call PlayLevel
	
	cmp ax, 1 ;ax=1 if he lost.
	JE_F Loser
	cmp ax, 3 ;ax=3 if he pressed ESC (he did not lose, but want to get out)
	JE_F PressedEsc
	push offset Passed ;ax is not 1 or 3 - so the user succeeded.
	call PrintPic ;Printing "Passed" massege.
	call CleanShotsArr ;Cleaning the ShotsArr for next level.
	
;Level Two
	mov bx, 4
	push bx
	call ArngArr

	push offset Two
	call PrintPic
	mov bx, 7
	push bx
	push offset AstArr
	call PlayLevel
	
	cmp ax, 1
	JE_F Loser
	cmp ax, 3
	JE_F PressedEsc
	push offset Passed
	call PrintPic
	call CleanShotsArr
	
;Level Three
	mov bx, 6
	push bx
	call ArngArr

	push offset Three
	call PrintPic
	mov bx, 7
	push bx
	push offset AstArr
	call PlayLevel
	
	cmp ax, 1
	JE_F Loser
	cmp ax, 3
	JE_F PressedEsc
	push offset Passed
	call PrintPic
	call CleanShotsArr
	
;Level Four:
	mov bx, 7
	push bx
	call ArngArr
	
	push offset Four
	call PrintPic
	mov bx, 7
	push bx
	push offset AstArr
	call PlayLevel
	
	cmp ax, 1
	JE_F Loser
	cmp ax, 3
	JE_F PressedEsc
	push offset Passed
	call PrintPic
	call CleanShotsArr
	
;Level Five
	mov bx, 10
	push bx
	call ArngArr
	
	push offset Five
	call PrintPic
	mov bx, 10
	push bx
	push offset AstArr
	call PlayLevel
	
	cmp ax, 1
	JE_F Loser
	cmp ax, 3
	JE_F PressedEsc
	push offset Passed
	call PrintPic
	inc [BombsRemaining]
	call CleanShotsArr
	
;Level Six
	mov bx, 11
	push bx
	call ArngArr
	
	push offset Six
	call PrintPic
	mov bx, 10
	push bx
	push offset AstArr
	call PlayLevel
	
	cmp ax, 1
	JE_F Loser
	cmp ax, 3
	JE_F PressedEsc
	push offset Passed
	call PrintPic
	call CleanShotsArr
	
;Level Seven
	mov bx, 12
	push bx
	call ArngArr
	
	push offset Seven
	call PrintPic
	mov bx, 13
	push bx
	push offset AstArr
	call PlayLevel
	
	cmp ax, 1
	JE_F Loser
	cmp ax, 3
	JE_F PressedEsc
	push offset Passed
	call PrintPic
	call CleanShotsArr
	
;Level Eight
	mov bx, 13
	push bx
	call ArngArr
	
	push offset Eight
	call PrintPic
	mov bx, 13
	push bx
	push offset AstArr
	call PlayLevel
	
	cmp ax, 1
	JE_F Loser
	cmp ax, 3
	JE_F PressedEsc
	push offset Passed
	call PrintPic
	call CleanShotsArr
	
;Level Nine
	mov bx, 14
	push bx
	call ArngArr

	push offset Nine
	call PrintPic
	mov bx, 15
	push bx
	push offset AstArr
	call PlayLevel
	
	cmp ax, 1
	je Loser
	cmp ax, 3
	je PressedEsc
	push offset Passed
	call PrintPic
	call CleanShotsArr
	
;Level Ten
	mov bx, 15
	push bx
	call ArngArr
	
	push offset Ten
	call PrintPic
	mov bx, 20
	push bx
	push offset AstArr
	call PlayLevel
	
	cmp ax, 1
	je Loser
	cmp ax, 3
	je PressedEsc
	
	push offset Winner ;If he got through this level, the user won!
	call PrintPic ;Printing winner massege.
	jmp PressedEsc
	
Loser:
	;nothing here. "Loser" exists just for better understanding of the code.
	;The pic and the sound of losing are taken care of in the "PlayLevel" proc.
PressedEsc:
	
	ret
endp FullGame

;---------------------------------
;This proc is one level proc.
;Returns: 
;If he won - ax=0.
;If he lost - ax=1.
;If he pressed ESC - ax = 2.
;
;Values: push (by that order):
;1. Num of seconds of level.
;2. The offset of proc (num of ast).
;---------------------------------
proc PlayLevel
	push bp
	mov bp, sp
	push_regs bx, cx, dx, di
	
	;clears the buffer
	mov ah,0ch
	mov al,0
	int 21h

	mov ax,13h ;Graphic mode
    int 10h
	push offset SpaceMouse ;Changing the mouse shape
	call ChangeMouse
	
	mov di, [bp+4]
	
	mov bx, [bp+6] ;num of seconds to wait (bl)
	;reads the time:
	;ch - hours
	;cl - minutes
	;dh - seconds 
	;dl - seconds/100
	mov ah, 2ch
	int 21h
	mov cl, dl
	mov bh, dh ;seconds
	
L2:
	
;THE GAME:

	push bx ;save its value (count seconds)
	
	xor ax, ax
	call CheckIfHitEdge ;return 1 in ax if true, 0 if not
	cmp ax, 1
	JE_F OutOfLoop1
	
	push di ;offset of proc
	call CheckIfMouseHitAst
	cmp ax, 1
	JE_F OutOfLoop1
	
	;Get a char from the user
	mov ah, 1 ;Checks if there is something new
	int 16h
	jz NothingInKey1
	mov ah, 0
	int 16h
	mov al, ah ;the scan code is in al.
NothingInKey1:
	
	push di ;AstArr
	call Bomb
	
	;Insert a new shot to array. If nothing new, 0 is inserted.
	call NewShot ;Think about inserting it into delayed loop?
	
	push offset ShotsArr
	call CheckIfShotHitEdge
	
	push di ;AstArr
	call CheckIfAstHitEdge
	
	push offset ShotsArr
	push di ;AstArr
	call CheckIfShotHitAst
	
	push ax ;do this thing only every sec/100.
	push cx
	;reads the time:
	;ch - hours
	;cl - minutes
	;dh - seconds 
	;dl - seconds/100
	mov ah, 2ch
	int 21h
	
	mov ax, dx
	
	pop cx
	cmp dl, cl
	JE_F Delay1
	mov cl, dl
	
	push di;AstArr
	call CreateNewAst
	
	;Updates the values of the shots
	push offset ShotsArr
	call MoveShots
	
	push di ;AstArr
	call MoveAst
	
	call CleanScreen ;clears the whole screen
	
	;Prints the shots
	push offset ShotsArr 
	call PrintShots
	
	push di ;AstArr
	call PrintAsts
Delay1:
	mov dx, ax
	pop ax
	
	dec al ;checks if ESC was pressed.
	jz EscPressed1
	
	pop bx
	cmp bh, dh ;seconds
	je Conti2
	dec bl
	cmp bl, 0
	je Won
	mov bh, dh
	
Conti2:
	jmp L2
	
OutOfLoop1:
	pop bx
	
	; make a sound (lost)
	in 	al, 61h
	or 	al, 00000011b
	out 	61h, al
	mov 	al, 0b6h
	out 	43h, al
	mov 	ax, 5000h
	out 	42h, al
	mov 	al, ah
	out 	42h, al
	
	mov si, 2
	push si
	call Delaying
	
	; stop sound
	in 	al, 61h
	and	al, 11111100b
	out 	61h, al
	
	push offset Lost
	call PrintPic
	
	mov ax, 1
	jmp EndEnd
Won: ;(next level)
	; make a sound
	in 	al, 61h
	or 	al, 00000011b
	out 	61h, al
	mov 	al, 0b6h
	out 	43h, al
	mov 	ax, 1000h
	out 	42h, al
	mov 	al, ah
	out 	42h, al
	
	mov si, 2
	push si
	call Delaying
	
	; stop sound
	in 	al, 61h
	and	al, 11111100b
	out 	61h, al
	
	xor ax, ax
	jmp EndEnd
	
EscPressed1:
	pop bx
	mov ax, 3
	
EndEnd:
	pop_regs bx, cx, dx, di
	pop bp
	ret 4
endp PlayLevel

;---------------------------------
;This proc resets everything 
;before playing a new game.
;
;No values needed.
;---------------------------------
proc PlayNewGame
	
	mov [BombsRemaining], 2 ;Number of bombs the user has is 2.
	call CleanShotsArr ;Cleaning the ShotsArr for the next game.
	;The AstArr is taken care of before every game, anyway.

	ret
endp PlayNewGame

;---------------------------------
;This proc clears the shots array
;before every new level.
;
;No values needed.
;---------------------------------
proc CleanShotsArr
	push_regs si, bx
	
	xor si, si
	mov bx, offset ShotsArr
@@TakeCareOfShots:
	cmp [word ptr bx+si], 0 ;Check if we reached the end of the array, or just got to a
	je @@Done               ; place where there are no more values of shots. 
	cmp [word ptr bx+si], '$'
	je @@Done
	
	mov [word ptr bx+si], 0
	mov [word ptr bx+si+2], 0
	mov [word ptr bx+si+4], 0
	add si, 6
	
	jmp @@TakeCareOfShots
	
@@Done:
	
	pop_regs si, bx
	ret
endp CleanShotsArr

;-------------------------------------------------------------------------------------------------------------------
;  _____      _       _   _               _____                    
; |  __ \    (_)     | | (_)             |  __ \                   
; | |__) | __ _ _ __ | |_ _ _ __   __ _  | |__) | __ ___   ___ 
; |  ___/ '__| | '_ \| __| | '_ \ / _` | |  ___/ '__/ _ \ / __/
; | |   | |  | | | | | |_| | | | | (_| | | |   | | | (_) | (__
; |_|   |_|  |_|_| |_|\__|_|_| |_|\__, | |_|   |_|  \___/ \___|
;                                  __/ |                           
;                                 |___/                            
;
;This proc prints images and apply a delay, according to the time of delay needed.
;--------------------------------------------------------------------------------------------------------------------

;---------------------------------
;Prints image (level image) and
;applies a delay.
;
;push the offset of image's name.
;---------------------------------
proc PrintPic
	push bp
	mov bp, sp
	push_regs ax, bx
	
	mov ax, 2
	int 33h ;hide mouse (the CleanScreen proc does not work well when the mouse is visible)
	
	mov bx, [bp+4]
	PrintBmp bx
	
	mov bx, 30
	push bx
	call Delaying
	
	mov ax, 1
	int 33h ;show mouse again
	
	pop_regs ax, bx
	pop bp
	ret 2
endp PrintPic
	
start:
	mov ax, @data
	mov ds, ax
	
	call Play ;Main playing proc.
	
    mov ax,03h ;return to text mode
    int 10h

	exit:
	mov ax, 4c00h
	int 21h
END start