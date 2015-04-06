.386p
stacksg SEGMENT PARA stack 'STACK'
	stack db 100h dup(?)
stacksg ENDS

datasg SEGMENT PARA USE16 'DATA'
	inter9seg dw 0
	inter9off dw 0
datasg ENDS

extcodesg SEGMENT PARA USE16 'CODE'

_begin = $

	forwardoffset dw 0
	forwardsegment dw 0
	counter dw 0
	outcolor db 7Ch
	outputbuff db '0x1234',0
	intercept PROC far
		push ax
		push bx
		push cx
		push dx
		push ds
		push es
		push si
		push di
	
		mov ax, cs
		mov ds, ax
		
		; increase counter
		mov ax, [counter]
		inc ax
		mov [counter], ax
		
		; outputd
		call fillBuff

		push ax
		mov bx, 0b800h
		mov es, bx
		mov bx, 148
		push bx
		mov bp, sp
		mov cx, 6h
		output:
			; current index
			mov ax, 6h
			sub ax, cx
			mov si, ax
			; form symbol
			mov ah, outcolor
			lea bx, outputbuff
			mov al, [bx+si]
			mov [bp+2], ax ; save symbol
			
			mov dh, ah
			and dh, 0fh
			cmp dh, 0fh
			je only_one_inc
			add ah, 10h
		only_one_inc:
			inc ah
			mov outcolor, ah
			; send in video
			mov ax, si
			shl ax, 1
			mov si, ax
			mov ax, [bp+2]
			mov bx, [bp]
			mov es:[bx+si], ax
		loop output
		pop bx
		pop ax
		
		; prepare stack
		pushf
		lea bx,[forwardoffset]
		call far ptr[bx]
		
		pop di
		pop si
		pop es
		pop ds
		pop dx
		pop cx
		pop bx
		pop ax
		
		iret
	intercept ENDP
	
	fillBuff PROC near
		push bp
		push ax
		mov bp, sp
		
		lea si, outputbuff+1
		mov cx, 4h
		cycle:
			; get number for conversion
			mov ax, [bp]
			and ax, 0fh
			xor ax, 30h
			cmp ax, 39h
			jbe end_conversion
			add ax, 7h
			end_conversion:
			; write in buff
			mov bx, cx
			mov [si+bx], al
			
			; shift number
			mov bx, [bp]
			shr bx, 4h
			mov [bp], bx
		loop cycle
			
		pop ax
		mov bp, sp
		pop bp
		
		ret
	fillBuff ENDP
	
_end = $
_size = _end - _begin
	
extcodesg ENDS


codesg SEGMENT PARA USE16 'CODE'
main:
	ASSUME SS:stacksg, ES:datasg, CS:codesg

	mov bx, datasg
	mov es, bx
	xor bx, bx
	mov ds, bx
	; save original int 9h address
	mov ax, 9h
	mov bx, 4h
	mul bx
	mov bx, ax
	mov ax, [bx]
	mov [es:inter9off], ax
	add bx, 2h
	mov ax, [bx]
	mov [es:inter9seg], ax
	; write in segment variables
	mov bx, seg extcodesg
	mov ds, bx
	mov ax, [es:inter9off]
	mov [ds:forwardoffset], ax
	mov ax, [es:inter9seg]
	mov [ds:forwardsegment], ax
	; compute rest size
	mov ax, _size
	test ax, 400h-1
	jz e1
	add ax, 400h
e1:
	shr ax, 10
	mov dx, ax
	; write rest size
	int 12h
	xor bx, bx
	mov ds, bx
	sub ax, dx
	mov bx, 413h
	mov [bx], ax
	; segment in memory
	cld
	shl ax, 6h
	mov cx, seg extcodesg
	mov ds, cx
	xor si, si
	mov es, ax
	xor di, di
	mov cx, _size
	rep movsb
	
	; change int 9h
	xor bx, bx
	mov ds, bx
	mov ax, 9h
	mov bx, 4h
	mul bx
	mov bx, ax
	cli
	mov [bx], offset intercept
	mov [bx+2], seg extcodesg
	sti
	
exit_from_app:
	mov ax,4c00h
	int 21h
	
codesg ENDS

END main

