public start
.DATA
.CODE
stub:
start:
	push rax
	push rbx
	push rcx
	push rdx
	push rbp
	push rsi
	push rdi
	push r8
	push r9
	push r10
	push r11
	push r12
	push r13
	push r14
	push r15
	
	sub rsp, 8h
	
	call here
	
	sub rax, 20h
	
	mov r8, offset msgBoxCaption - 140001000h
	add r8, rax
	mov rdx, offset msgBoxText - 140001000h
	add rdx, rax
	xor rcx, rcx  ; hWnd = null
	xor r9d, r9d  ; uType = 0
	
	sub rsp, 20h
	db 0FFh, 15h, 0A0h, 0A0h, 0A0h, 0A0h
	; call __imp_MessageBoxW
	add rsp, 28h ; + 8 from (sub rsp, 8h)
	
	pop r15
	pop r14
	pop r13
	pop r12
	pop r11
	pop r10
	pop r9
	pop r8
	pop rdi
	pop rsi
	pop rbp
	pop rdx
	pop rcx
	pop rbx
	pop rax
	
	call stub
	
	db 0E9h
	db 0Ah, 0Ah, 0Ah, 0Ah

here:
	mov rax, [rsp]
	ret
	
msgBoxCaption dw 'U', 'n', 'c', 'l', 'e', ' ', 'S', 'o', 's', 'o', 0
msgBoxText dw 'H', 'e', 'l', 'l', 'o', ' ', 'f', 'r', 'o', 'm', ' ', 'u', 'n', 'c', 'l', 'e', ' ', 'S', 'o', 's', 'o', '!', 0
END