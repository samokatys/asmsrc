.586p

codesg SEGMENT PARA USE16 'CODE'
	begincodesg = $
	
	db 0EAh ; jmp
	dw offset main
	dw 7C0h
	
	nullTextForEnc db 32 * 6 dup(0)
	
	key db  0h, 1h, 2h, 3h, 4h, 5h, 6h, 7h,
			 8h, 9h, 0ah, 0bh, 0ch, 0dh, 0eh, 0fh,
			 10h, 11h, 12h, 13h, 14h, 15h, 16h, 17h,
			 18h, 19h, 01ah, 01bh, 01ch, 01dh, 01eh, 01fh
	
main:
	mov ax, 7C0h
	mov ds, ax
	mov es, ax
	mov ax, 0D00h
	mov ss, ax
	mov sp, 0FFFFh
	
	lea dx, key
	lea di, nullTextForEnc
	
	mov ebx, 32 * 6
	xor cx, cx
	xor ax, ax
	
aes_encrypt_sector_next:
	cmp cx, 32
	jb aes_encrypt_byte_xor
	xor cx, cx

aes_encrypt_byte_xor:
	mov si, dx
	add si, cx
	mov al, [si]
	xor es:[di], al
        
	inc di
	inc cx
	dec ebx
	jnz aes_encrypt_sector_next
	
	jmp $
codesg ENDS

END main