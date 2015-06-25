.386

.MODEL FLAT, STDCALL
.STACK 10000h
.DATA
.CODE
stub:
start:
	push eax
	push ebx
	push ecx
	push edx
	push ebp
	push esi
	push edi
	
	call here
	
	sub eax, 0Ch
	
	push 0h
	mov ebx, eax
	add ebx, offset msgBoxCaption - 401000h
	push ebx ; addr caption
	mov ebx, eax
	add ebx, offset msgBoxText - 401000h
	push ebx ; addr text
	push 0h
	; call stub
	db 0FFh, 15h, 0A0h, 0A0h, 0A0h, 0A0h ; call for araxis merge
	
	pop edi
	pop esi
	pop ebp
	pop edx
	pop ecx
	pop ebx
	pop eax
	
	mov eax, 10h
	
	db 0E9h
	db 0Ah, 0Ah, 0Ah, 0Ah

here:
	mov eax, [esp]
	ret
	
;msgBoxCaption db "Uncle Soso", 0
;msgBoxText db "Hello, from uncle Soso!", 0	
msgBoxCaption dw 'U', 'n', 'c', 'l', 'e', ' ', 'S', 'o', 's', 'o', 0
msgBoxText dw 'H', 'e', 'l', 'l', 'o', ' ', 'f', 'r', 'o', 'm', ' ', 'u', 'n', 'c', 'l', 'e', ' ', 'S', 'o', 's', 'o', '!', 0
END start