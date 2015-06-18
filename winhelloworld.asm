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
.CONST 
   NULL equ 0 
   MB_OK equ 0
.CODE
finalHandler proc near
	invoke MessageBoxA, NULL, ADDR finalText, ADDR finalCaption, MB_OK
	mov eax, 1
	ret
finalHandler endp
start:
	invoke SetUnhandledExceptionFilter, ADDR finalHandler	
	invoke MessageBoxA, NULL, ADDR msgBoxText, ADDR msgBoxCaption, MB_OK
	invoke ExitProcess, 0
	
	
END start