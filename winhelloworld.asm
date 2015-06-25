.386

.MODEL FLAT, STDCALL
ExitProcess PROTO ,:DWORD 
MessageBoxA PROTO ,:DWORD, :DWORD, :DWORD, :DWORD 
SetUnhandledExceptionFilter PROTO ,:DWORD
.STACK 10000h
.DATA
	msgBoxCaption db "Asm message box!", 0
	msgBoxText db "My first assembly message box! Hello, World!", 0
	finalCaption db "Final handler", 0
	finalText db "Final handler executed", 0
	localCaption db "Local handler", 0
	localText db "Local handler executed", 0
	localCaption1 db "Local1 handler", 0
	localText1 db "Local1 handler executed", 0
	internalCaption db "internal handler", 0
	internalText db "internal handler executed", 0
.CONST 
   NULL equ 0 
   MB_OK equ 0
   
   assume nothing
.CODE
finalHandler proc near
	invoke MessageBoxA, NULL, ADDR finalText, ADDR finalCaption, MB_OK
	mov eax, 1
	ret
finalHandler endp

localHandler proc near
	invoke MessageBoxA, NULL, ADDR localText, ADDR localCaption, MB_OK
	mov eax, 1
	ret
localHandler endp

localHandler1 proc near
	invoke MessageBoxA, NULL, ADDR localText1, ADDR localCaption1, MB_OK
	
	mov ebx, [esp+0Ch]
	mov [ebx+0B8h], offset safe_place
	mov eax, 0
	ret
localHandler1 endp

internalHandler proc near
	invoke MessageBoxA, NULL, ADDR internalText, ADDR internalCaption, MB_OK
	
	mov ebx, [esp+0Ch]
	mov [ebx+0B8h], offset internal_safe_place
	mov eax, 0
	ret
internalHandler endp

start:
	mov eax, 10h
	mov ebx, 15h
	add eax, ebx
	mul ebx

	invoke SetUnhandledExceptionFilter, ADDR finalHandler
	
	push offset localHandler1
	push fs:[0]
	mov fs:[0], esp
	push offset localHandler
	push fs:[0]
	mov fs:[0], esp
		invoke MessageBoxA, NULL, ADDR msgBoxText, ADDR msgBoxCaption, MB_OK
		
			push offset internalHandler
			push fs:[0]
			mov fs:[0], esp
				invoke MessageBoxA, NULL, ADDR msgBoxText, 0FFFFFFFFh, MB_OK
			internal_safe_place:
			mov esp, fs:[0]
			pop fs:[0]
			add esp, 4h
		
		invoke MessageBoxA, NULL, ADDR msgBoxText, 0FFFFFFFFh, MB_OK
	safe_place:
	mov esp, fs:[0]
	pop fs:[0]
	add esp, 4h
	pop fs:[0]
	add esp, 4h
	
	invoke MessageBoxA, NULL, ADDR msgBoxText, ADDR msgBoxCaption, MB_OK
	
	invoke ExitProcess, 0
	
	
END start