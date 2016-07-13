.586p

codesg SEGMENT PARA USE16 'CODE'
	begincodesg = $
	
	db 0EAh ; jmp
	dw offset main
	dw 7C0h
	
	key db  0h, 1h, 2h, 3h, 4h, 5h, 6h, 7h,
			 8h, 9h, 0ah, 0bh, 0ch, 0dh, 0eh, 0fh,
			 10h, 11h, 12h, 13h, 14h, 15h, 16h, 17h,
			 18h, 19h, 01ah, 01bh, 01ch, 01dh, 01eh, 01fh
	
	keyStr db '0x00 0x00 0x00 0x00 0x00 0x00 0x00 0x00 '
		   db '0x00 0x00 0x00 0x00 0x00 0x00 0x00 0x00 '
		   db '0x00 0x00 0x00 0x00 0x00 0x00 0x00 0x00 '
		   db '0x00 0x00 0x00 0x00 0x00 0x00 0x00 0x00'
    keyStrSize = $ - keyStr
	
main:
	mov ax, 7C0h
	mov ds, ax
	mov es, ax
	mov ax, 0D00h
	mov ss, ax
	mov sp, 0FFFFh
	
	lea si, key
	lea di, keyStr
	add di, 2 ; shift '0x'
	mov cx, 32
fillKeyStr:
	xor eax, eax
	mov al, ds:[si]
	push eax
	push di
	push 2
	call itoah
	
	inc si
	add di, 5 ; shift '00 0x'
	dec cx
	jnz fillKeyStr
	
	mov ax, keyStrSize
	push ax
	lea bp, keyStr
	push bp
	call print
	
	; call itoah
	
	jmp $
	
	print proc near
		mov bp, sp
		
		; get current cursor position
		; mov ah, 03h
		; mov bh, 0h
		; int 10h
		; output buffer
		xor dx, dx ; row and column 0
		
		mov ah, 13h
		mov al, 1h
		mov cx, [bp+4]
		mov bh, 0
		mov bl, 07Ch ; background and character color
		mov bp, [bp+2]
		int 10h
		
		ret 4
	print endp
	
	; work in current ds
	; doubleword - value; pointer - buff; word - size
	itoah proc near
		push bp
		mov bp, sp
		push eax
		push bx
		push cx
		push si
		
		mov cx, [bp+4] ; size
		mov bx, [bp+6] ; buff
		mov eax, [bp+8] ; value
		mov si, cx
		
		push eax ; tmp
		mov bp, sp
		
		itoahloop:
			and ax, 0fh
			xor ax, 30h
			cmp ax, 39h
			jbe end_conversion
			add ax, 7h
			end_conversion:

			dec si
			mov ds:[bx+si], al
			
			; shift number
			mov eax, [bp]
			shr eax, 4h
			mov [bp], eax
		loop itoahloop
		
		pop eax ; tmp
		
		pop si
		pop cx
		pop bx
		pop eax
		pop bp
		
		ret 8
	itoah endp
codesg ENDS

END main