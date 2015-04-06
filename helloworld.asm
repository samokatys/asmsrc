.386

codesg SEGMENT PARA USE16 'CODE'
	ASSUME CS:codesg
	ORG 100h
	
main:
	jmp start

	buff db	256 dup(?)
	buff_size dw 0h
	
	
start:
	push 256
	push offset buff
	call fillBuffer
	
	mov buff_size, ax

	; get current cursor position
	mov ah, 03h
	mov bh, 0h
	int 10h
	; output buffer
	mov ah, 13h
	mov al, 1h
	mov cx, buff_size
	mov bh, 0
	mov bl, 07Ch ; background and character color
	lea bp, [buff]
	int 10h
	
	ret
	
	
fillBuffer PROC near
	mov bp, sp
	
	add bp, 4
	mov cx, [bp]
	dec cx ;terminate null
	mov si, 0
	
	mov bp, sp
	add bp, 2
	mov bx, [bp]
	cycle:
		mov ah, 0
		int 16h
		cmp al, 0dh
		je cycle_end
		mov [bx+si],al
		inc si
	loop cycle
cycle_end:
	xor ax,ax
	mov [bx+si],ax
	mov ax, si
	
	mov si, 0
	mov bp, 0
	
	ret 4
fillBuffer ENDP

codesg ENDS

END main
