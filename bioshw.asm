.386p

codesg SEGMENT PARA USE16 'CODE'
	begincodesg = $
	
	db 0EAh ; jmp
	dw offset main
	dw 7C0h

	diskid db 0
	
	notequalstr db 'Not equal',0Ah,0Dh
	notequalstrsize = $ - notequalstr

	equalstr db 'Equal',0Ah,0Dh
	equalstrsize = $ - equalstr

	firsttest20str db 'First test A20',0Ah,0Dh
	firsttest20strsize = $ - firsttest20str
	
	afterkbrda20str db 'Test A20 after keyboard xor',0Ah,0Dh
	afterkbrda20strsize = $ - afterkbrda20str
	
	afterfasta20str db 'Test A20 after fast xor',0Ah,0Dh
	afterfasta20strsize = $ - afterfasta20str
	
	afterint15a20str db 'Test A20 after int15 xor',0Ah,0Dh
	afterint15a20strsize = $ - afterint15a20str
	
	initpestr db 'Start initialize protected mode',0Ah,0Dh
	initpestrsize = $ - initpestr
	
	endinitpestr db 'Initialize protected mode finished' ;,0Ah,0Dh
	endinitpestrsize = $ - endinitpestr
	
	dapstruct struct 
		dsize 			db 1
		unuse 			db 1
		numsec 			dw 1
		membuffoffset 	dw 1
		membuffsegment 	dw 1
		absnumstartsec	db 8 dup(?)
	dapstruct ends
	
	readdap dapstruct {10h, 0h, 0004h, 0h, 07E0h, {01h, 00h, 00h, 00h, 00h, 00h, 00h, 00h} }
	
main:
	mov ax, 7C0h
	mov ds, ax
	mov es, ax
	mov ax, 0D00h
	mov ss, ax
	mov sp, 0FFFFh
	
	mov diskid, dl
	mov ah, 42h
	lea si, readdap
	int 13h
	
	; call testA20methods
	call enableA20fastON
	call testA20
	
	call workInProtectedMode
	
	jmp $
	
	; methods for work with A20
	testA20methods proc near
		mov ax, firsttest20strsize
		push ax
		lea bp, firsttest20str
		push bp
		call print
		
		call testA20
		
		; keyboard a20 test
		call enableA20
		
		mov ax, afterkbrda20strsize
		push ax
		lea bp, afterkbrda20str
		push bp
		call print

		call testA20
		
		; keyboard a20 test
		call enableA20fast
		
		mov ax, afterfasta20strsize
		push ax
		lea bp, afterfasta20str
		push bp
		call print

		call testA20
		
		; keyboard a20 test
		call enableA20int15
		
		mov ax, afterint15a20strsize
		push ax
		lea bp, afterint15a20str
		push bp
		call print

		call testA20
		
		ret		
	testA20methods endp
	
	testA20 proc near
		push ds
		
		mov bx, 0h
		mov ds, bx
		mov ax, ds:[bx]
		push ax

		mov bx, 0FFFFh
		mov ds, bx
		mov bx, 10h
		mov dx, ds:[bx]
		not dx
		cli
		mov ds:[bx], dx
		
		mov bx, 0h
		mov ds, bx
		mov cx, ds:[bx]
		;restore
		pop ax
		mov ds:[bx], ax
		sti
		
		pop ds
		cmp cx, dx
		jne not_equal_result
			mov ax, equalstrsize
			push ax
			lea bp, equalstr
			push bp
			call print
			jmp end_result
	not_equal_result:
			mov ax, notequalstrsize
			push ax
			lea bp, notequalstr
			push bp
			call print
	end_result:
		ret
		
	testA20 endp
	
	print proc near
		mov bp, sp
		
		; get current cursor position
		mov ah, 03h
		mov bh, 0h
		int 10h
		; output buffer
		mov ah, 13h
		mov al, 1h
		mov cx, [bp+4]
		mov bh, 0
		mov bl, 07Ch ; background and character color
		mov bp, [bp+2]
		int 10h
		
		ret 4
		
	print endp
	
	waitkeyboard proc near
	   push cx
       xor     cx, cx
WK1:   in      al, 64h
       test    al, 1
       jz      short WK2
       in      al, 60h
       jmp     short WK1
WK2:
       test    al, 2
       loopnz  WK1
	   pop cx
       ret
	waitkeyboard endp

	enableA20 proc near
		call    waitkeyboard
		mov     al, 0D0H
		out     64h, al
		in      al, 60h
		xor     al, 2
		mov     cl, al
					
		call    waitkeyboard
		mov     al, 0D1H
		out     64h, al
		mov     al, cl
		out     60h, al
		
		ret
	enableA20 endp
	
	enableA20fast proc near
		in      al, 92h
		xor     al, 2
		out     92h, al

		ret
	enableA20fast endp
	
	enableA20fastON proc near
		in      al, 92h
		or   	al, 2
		out     92h, al

		ret
	enableA20fastON endp
	
	enableA20int15 proc near
		mov ax, 2400h
		int 15h
		ret
	enableA20int15 endp
	
	ORG	1feh
	db 55h, 0aah
	
	;code for protected mode
	segmentdescriptor struct
		seglimit1 dw 0
		segbase1 dw 0
		segbase2 db 0
		params dw 0
		segbase3 db 0
	segmentdescriptor ends
	
	gatedescriptor struct
		offset1 dw 0h
		segselector dw 0h
		params dw 0h
		offset2 dw 0h
	gatedescriptor ends
	
	; GDT
	nulldesc segmentdescriptor {}
	codedesc segmentdescriptor { codesgsize, 7C00h, 0, 109Ah, 0 }
	datadesc segmentdescriptor { codesgsize, 7C00h, 0, 1092h, 0 }
	stackdesc segmentdescriptor { 0h, 0D000h, 0h, 1096h, 0h }
	videotextdesc segmentdescriptor { 0, 8000h, 0Bh, 1192h, 0 }
	; user mode segments
	usermodecodedesc segmentdescriptor { usermodecodesgsize, 0, 0, 10FAh, 0 }
	usermodedatadesc segmentdescriptor { usermodedatasgsize, 0, 0, 10F2h, 0 }
	usermodestackdesc segmentdescriptor { 0h, 0D000h, 1h, 10F6h, 0h }
	; simply have function that multiply two numbers and return result in ax
	coreconformcodedesc segmentdescriptor { conformcodesgsize, 0, 0, 109Eh, 0 } 
	; call gates
	printcallgate gatedescriptor { offset printMultiplyResultStr, 08h, 0EC01h, 0h } 
	; tss 
	tssdesc segmentdescriptor { sizeof tssseg16 - 1, 7c00h + tsssegment, 0h, 1089h, 0h }
	gdtsize = $ - nulldesc
	

	; IDT
	inthndls gatedescriptor 32 dup ({ 0h, 0h, 8700h, 0h }) ; 8600h - interrupt gate
	idtsize = $ - inthndls
	
	inthndlfunctions dd offset int0hndl, offset int1hndl, offset int2hndl, offset int3hndl, offset int6hndl, 
						offset int5hndl, offset int6hndl, offset int7hndl, offset int8hndl, offset int9hndl,
						offset int10hndl, offset int11hndl, offset int12hndl, offset int13hndl, offset int14hndl, 
						offset int15hndl, offset int16hndl, offset int17hndl, offset int18hndl, offset int19hndl
	
	regstruct struct
		limit dw 0h
		baselinearaddr dd 0 
	regstruct ends
	gdtr regstruct { gdtsize - 1, 0 }
	idtr regstruct { idtsize - 1, 0 }
	
	taskreg dw 50h ; 10 in GDT
	
	inthndlstr db 'Int handle called 0x00'
	inthndlstrsize = $ - inthndlstr
	
	intstubhndlstr db 'Int stub handle called'
	intstubhndlstrsize = $ - intstubhndlstr
	
	multiplyresultstr db 'Multiply result = 0x00'
	multiplyresultstrsize = $ - multiplyresultstr
	
	tssseg32 struct
		prevtasklink dw 0h
		reserved0 dw 0h
		
		esp0 dd 0h
		ss0 dw 0h
		reserved1 dw 0h
		
		esp1 dd 0h
		ss1 dw 0h
		reserved2 dw 0h
		
		esp2 dd 0h
		ss2 dw 0h
		reserved3 dw 0h
		
		cr3_pdbr dd 0h
		eip_reg dd 0h
		eflags dd 0h
		eax_reg dd 0h
		ecx_reg dd 0h
		edx_reg dd 0h
		ebx_reg dd 0h
		esp_reg dd 0h
		ebp_reg dd 0h
		esi_reg dd 0h
		edi_reg dd 0h
		
		es_reg dw 0h
		reserved5 dw 0h
		
		cs_reg dw 0h
		reserved6 dw 0h
		
		ss_reg dw 0h
		reserved7 dw 0h
		
		ds_reg dw 0h
		reserved8 dw 0h
		
		fs_reg dw 0h
		reserved9 dw 0h
		
		gs_reg dw 0h
		reserved10 dw 0h
		
		ldtselector dw 0h
		reserved11 dw 0h
		
		reserved12 dw 0h ; 0 bit - flag T for debug
		iomapbaseaddr dw 0h
	tssseg32 ends
	
	tssseg16 struct
		prevtasklink dw 0h
		
		sp0 dw 0h
		ss0 dw 0h
	
		sp1 dw 0h
		ss1 dw 0h
		
		sp2 dw 0h
		ss2 dw 0h
		
		ip_reg dw 0h
		eflags dw 0h
		ax_reg dw 0h
		cx_reg dw 0h
		dx_reg dw 0h
		bx_reg dw 0h
		sp_reg dw 0h
		bp_reg dw 0h
		si_reg dw 0h
		di_reg dw 0h
		
		es_reg dw 0h
		cs_reg dw 0h
		ss_reg dw 0h
		ds_reg dw 0h
		
		taskldtselector dw 0h
	tssseg16 ends
	
	tsssegment tssseg16 {}
	
	; main function for protected mode
	workInProtectedMode proc near
		mov ax, initpestrsize
		push ax
		lea bp, initpestr
		push bp
		call print
		
		call prepareGDTR
		call prepareIDTR
		
		cli
		
		; disable and save NMI state
		in al,70h
		or al,80h
		out 70h,al
		in al, 71h

		lgdt gdtr
		lidt idtr
		
		mov eax, cr0
		or al, 1
		mov cr0, eax
		
		db 0EAh ; jmp
		dw offset pstart
		dw 8h
		
	pstart:	
		mov ax, 10h
		mov ds, ax
		mov es, ax
		mov ax, 18h
		mov ss, ax	
		
		call prepareTSS
		ltr taskreg
		; when we load task in register
		; cpu load stored data in segment like cs:ip
		nexttaskstep = $
		
		push 14h
		push 18h
		mov ax, endinitpestrsize
		push ax
		lea bp, endinitpestr
		push bp
		call printProtectedVGA
		
		int 3h
		
		xor bx, bx
		div bx
		
		mov ax, 30h ; 6 in GDT
		mov es, ax ; user mode data segment
		mov es:callgatetocore, 4B0000h ; call gate
		xor eax, eax
		mov eax, offset multiplyTwoNumbers
		xor ebx, ebx
		mov ebx, 430000h
		add ebx, eax
		mov es:conformsegfunc, ebx
		
		push 3Bh ; stack
		push 0FFFFh
		push 2Bh ; user mode code 
		push offset workInUserMode
		retf
		
		ret
	workInProtectedMode endp
	
	prepareGDTR proc
		; compute gdtr start 
		xor eax, eax
		mov bx, cs
		shl bx, 4
		lea ax, nulldesc
		add ax, bx
		mov gdtr.baselinearaddr, eax
		
		mov ax, 7c00h
		
		mov bx, seg usermodecodesg
		shl bx, 4
		add bx, ax
		mov usermodecodedesc.segbase1, bx
		
		mov bx, seg usermodedatasg
		shl bx, 4
		add bx, ax
		mov usermodedatadesc.segbase1, bx
		
		mov bx, seg conformcodesg
		shl bx, 4
		add bx, ax
		mov coreconformcodedesc.segbase1, bx
		
		ret
	prepareGDTR endp
	
	prepareIDTR proc
		; compute idtr start 
		xor eax, eax
		mov bx, cs
		shl bx, 4
		lea ax, inthndls
		add ax, bx
		mov idtr.baselinearaddr, eax
		
		xor eax, eax
		xor bx, bx
		mov cx, 20h
		mov si, 0h
		prepareIDTRloop:
			ifstart:
				cmp cx, 0Bh ; cx >= 12 
				ja notstub
				; else
					mov eax, offset intStubHndl
					jmp ifend
				notstub:
					mov eax, inthndlfunctions[si]
					add si, 4
			ifend:
		
			mov inthndls[bx].offset1, ax
			shr eax, 8
			mov inthndls[bx].offset2, ax
			mov inthndls[bx].segselector, 8h
			
			add bx, sizeof gatedescriptor ; idt row byte offset
		
		loop prepareIDTRloop
		
		ret
	prepareIDTR endp
	
	prepareTSS proc near
		mov ds:tsssegment.cs_reg, cs
		mov ds:tsssegment.ip_reg, nexttaskstep
		
		mov ds:tsssegment.ss_reg, ss
		mov ds:tsssegment.sp_reg, sp
		
		mov ds:tsssegment.ds_reg, ds
		mov ds:tsssegment.es_reg, es
		
		pushf
		pop ax
		mov ds:tsssegment.eflags, ax 
		
		mov ds:tsssegment.ss0, ss
		mov ds:tsssegment.sp0, 0FFFFh
		
		ret
	prepareTSS endp
	
	; stack:
	; no checks out of buff, out vga buff
	; 0 <= col < 80 0 <= row < 25(bochsrc default)
	; col, row, buff_size, buff_ptr, ret_addr
	printProtectedVGA proc near
		push es
		mov bp, sp
		
		mov ax, [bp+8] ; row
		mov bx, 50h
		mul bx
		mov bx, [bp+10]
		add bx, ax ; start pointer to vga buffer
		shl bx, 1
		
		mov ax, 20h
		mov es, ax
		mov cx, [bp+6]
		mov dx, cx
		mov bp, [bp+4] ;pointer to string
		output:
			mov ah, 07Ch ; background and character color
			mov si, dx
			sub si, cx
			mov al, ds:[bp+si]
			
			shl si, 1
			mov es:[bx+si], ax
		loop output
		
		pop es
		ret 8
	printProtectedVGA endp
	
	printIntStr proc near
		push ds
		mov bp, sp
		mov ax, 10h
		mov ds, ax
		
		mov ax, [bp+4]
		push ax ; tmp
		
		lea bx, ds:inthndlstr
		add bx, inthndlstrsize - 3h
		mov cx, 2h
		printintloop:
			and ax, 0fh
			xor ax, 30h
			cmp ax, 39h
			jbe end_conversion
			add ax, 7h
			end_conversion:

			mov si, cx
			mov [bx+si], al
			
			; shift number
			mov ax, [bp-2]
			shr ax, 4h
			mov [bp-2], ax
		loop printintloop
		
		pop ax ; clear tmp
		
		push 4fh - inthndlstrsize
		push [bp+4]
		mov ax, inthndlstrsize
		push ax
		lea bp, inthndlstr
		push bp
		call printProtectedVGA
		
		pop ds
		ret 2
	printIntStr endp
	
	printMultiplyResultStr proc far
		push ds
		mov bp, sp
		mov ax, 10h
		mov ds, ax
		
		mov ax, [bp+4]
		push ax ; tmp
		
		lea bx, ds:mulresultstr
		add bx, mulresultstrsize - 3h
		mov cx, 2h
		printintloop:
			and ax, 0fh
			xor ax, 30h
			cmp ax, 39h
			jbe end_conversion
			add ax, 7h
			end_conversion:

			mov si, cx
			mov [bx+si], al
			
			; shift number
			mov ax, [bp-2]
			shr ax, 4h
			mov [bp-2], ax
		loop printintloop
		
		pop ax ; clear tmp
		
		push 18h
		push 10h
		mov ax, mulresultstrsize
		push ax
		lea bp, mulresultstr
		push bp
		call printProtectedVGA
		
		pop ds
		retf 2
	printMultiplyResultStr endp
	
	; interrupt handles
	
	intStubHndl proc far
		push 4fh - intstubhndlstrsize
		push 04h
		mov ax, intstubhndlstrsize
		push ax
		lea bp, intstubhndlstr
		push bp
		call printProtectedVGA
		
		iret
	intStubHndl endp
	
	int0hndl proc far
		push 0h
		call printIntStr
		iret
	int0hndl endp
	
	int1hndl proc far
		push 1h
		call printIntStr
		iret
	int1hndl endp
	
	int2hndl proc far
		push 2h
		call printIntStr
		iret
	int2hndl endp
	
	int3hndl proc far
		push 03h
		call printIntStr
		iret
	int3hndl endp
	
	int4hndl proc far
		push 04h
		call printIntStr
		iret
	int4hndl endp
	
	int5hndl proc far
		push 05h
		call printIntStr
		iret
	int5hndl endp
	
	int6hndl proc far
		push 06h
		call printIntStr
		iret
	int6hndl endp
	
	int7hndl proc far
		push 07h
		call printIntStr
		iret
	int7hndl endp
	
	int8hndl proc far
		push 03h
		call printIntStr
		iret
	int8hndl endp
	
	int9hndl proc far
		push 09h
		call printIntStr
		iret
	int9hndl endp
	
	int10hndl proc far
		push 0Ah
		call printIntStr
		iret
	int10hndl endp
	
	int11hndl proc far
		push 0Bh
		call printIntStr
		iret
	int11hndl endp
	
	int12hndl proc far
		push 0Ch
		call printIntStr
		iret
	int12hndl endp
	
	int13hndl proc far
		push 0Dh
		call printIntStr
		iret
	int13hndl endp
	
	int14hndl proc far
		push 0Eh
		call printIntStr
		iret
	int14hndl endp
	
	int15hndl proc far
		push 0Fh
		call printIntStr
		iret
	int15hndl endp
	
	int16hndl proc far
		push 10h
		call printIntStr
		iret
	int16hndl endp
	
	int17hndl proc far
		push 11h
		call printIntStr
		iret
	int17hndl endp
	
	int18hndl proc far
		push 12h
		call printIntStr
		iret
	int18hndl endp
	
	int19hndl proc far
		push 13h
		call printIntStr
		iret
	int19hndl endp
	
	int20hndl proc far
		push 14h
		call printIntStr
		iret
	int20hndl endp
	
	codesgsize = $ - begincodesg
codesg ENDS



conformcodesg SEGMENT PARA USE16 'CODE'
	beginconformcodesg = $
	; conforming core code segment
	multiplyTwoNumbers proc far
		mov bp, sp
		mov ax, [bp+4]
		mov bx, [bp+6]
		mul bx
		
		retf 4
	multiplyTwoNumbers endp
	
	conformcodesgsize = $ - beginconformcodesg
conformcodesg ends	

	
	
usermodedatasg SEGMENT PARA USE16 'DATA'
	beginusermodedatasg = $
	; user mode data segment
	var1 dw 0Ah
	var2 dw 0Bh
	mulresultstr db '0xA * 0xB = 0x00'
	mulresultstrsize = $ - mulresultstr
	
	conformsegfunc dd 0h
	callgatetocore dd 0h
	
	usermodedatasgsize = $ - beginusermodedatasg
usermodedatasg ends
	
usermodecodesg SEGMENT PARA USE16 'CODE'
	beginusermodecodesg = $
	; user mode code segment
	workInUserMode proc far
		mov ax, 33h ; 7 number in GDT
		mov ds, ax

		; call multiply
		push ds:var1
		push ds:var2
		lea ebx, ds:conformsegfunc
		call far ptr [ebx]
		
		; output result
		push ax
		lea ebx, ds:callgatetocore
		call far ptr [ebx]
		
		jmp $
	workInUserMode endp
	
	usermodecodesgsize = $ - beginusermodecodesg
usermodecodesg ends
	
	

END