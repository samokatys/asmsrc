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
	
	readdap dapstruct {10h, 0h, 000ah, 0h, 07E0h, {01h, 00h, 00h, 00h, 00h, 00h, 00h, 00h} }
	
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
	printcallgate gatedescriptor { offset printMultiplyResultStr, 08h, 0E401h, 0h } 
	; tss 
	;tssdesc segmentdescriptor { sizeof tssseg16, 7c00h + tsssegment, 0h, 1081h, 0h }
	tssdesc segmentdescriptor { sizeof tssseg32 - 1, 7c00h + tsssegment, 0h, 50E9h, 0h }
	; print counter callgate
	printcountercallgate gatedescriptor { offset printUserModeCounter, 08h, 0E403h, 0h } 
	; fone segments
	fonecodedesc segmentdescriptor { fonecodesgsize, 0, 0, 10FAh, 0 }
	fonedatadesc segmentdescriptor { fonedatasgsize, 0, 0, 10F2h, 0 }
	fonestackdesc segmentdescriptor { 0h, 0D000h, 2h, 10F6h, 0h }
	fonestack0desc segmentdescriptor { 0h, 0D000h, 3h, 1096h, 0h }
	; fone tss
	fonetssdesc segmentdescriptor { sizeof tssseg32 - 1, 7c00h + fonetsssegment, 0h, 50E9h, 0h }
	;fonetssdesc segmentdescriptor { sizeof tssseg16, 7c00h + fonetsssegment, 0h, 1081h, 0h }
	; callgates for keyboard
	putcharcallgate gatedescriptor { offset putchar, 08h, 0E401h, 0h } 
	getcharcallgate gatedescriptor { offset getchar, 08h, 0E400h, 0h } 
	addrowcallgate gatedescriptor { offset addrow, 08h, 0E401h, 0h } 
	addcolcallgate gatedescriptor { offset addcol, 08h, 0E401h, 0h } 
	getcurrowcallgate gatedescriptor { offset getcurrow, 08h, 0E400h, 0h } 
	getcurcolcallgate gatedescriptor { offset getcurcol, 08h, 0E400h, 0h } 
	gdtsize = $ - nulldesc
	
	; IDT
	inthndls gatedescriptor 32 dup ({ 0h, 0h, 8700h, 0h }) ; 8600h - interrupt gate
	pichndls gatedescriptor 16 dup ({ 0h, 0h, 8600h, 0h }) 
	idtsize = $ - inthndls
	idtdescnum = idtsize / sizeof gatedescriptor
	
	inthndlfunctions dd offset int0hndl, offset int1hndl, offset int2hndl, offset int3hndl, offset int6hndl, 
						offset int5hndl, offset int6hndl, offset int7hndl, offset int8hndl, offset int9hndl,
						offset int10hndl, offset int11hndl, offset int12hndl, offset int13hndl, offset int14hndl, 
						offset int15hndl, offset int16hndl, offset int17hndl, offset int18hndl, offset int19hndl
	inthndlfuncnum = ($ - inthndlfunctions) / 4
						
	pichndlfunctions dd offset int32hndl, offset int33hndl, offset int34hndl, offset int35hndl, 
						offset int36hndl, offset int37hndl, offset int38hndl, offset int39hndl, 
						offset int40hndl, offset int41hndl, offset int42hndl, offset int43hndl,
						offset int44hndl, offset int45hndl, offset int46hndl, offset int47hndl
	pichndlfuncnum = ($ - pichndlfunctions) / 4
	
	regstruct struct
		limit dw 0h
		baselinearaddr dd 0 
	regstruct ends
	gdtr regstruct { gdtsize - 1, 0 }
	idtr regstruct { idtsize - 1, 0 }
	
	taskreg dw 50h ; 10 in GDT
	fonetaskreg dw 80h ; 16 in GDT
	
	itoahcallptr dd 0h
	mulcallptr dd 0h
	
	inthndlstr db 'Int handle called 0x00'
	inthndlstrsize = $ - inthndlstr
	
	pichndlstr db 'pic 0x00'
	pichndlstrsize = $ - pichndlstr
	
	intstubhndlstr db 'Int stub handle called'
	intstubhndlstrsize = $ - intstubhndlstr
	
	multiplyresultstr db 'Multiply result = 0x00'
	multiplyresultstrsize = $ - multiplyresultstr
	
	int8counter dd 0h
	int8outputstr db '0x00000000'
	int8outputstrsize = $ - int8outputstr
	
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
	
	tsssegment tssseg32 {}
	fonetsssegment tssseg32 {}
	
	; main function for protected mode
	workInProtectedMode proc near
		mov ax, initpestrsize
		push ax
		lea bp, initpestr
		push bp
		call print
		
		mov ebx, 430000h
		mov eax, offset multiplyTwoNumbers
		add eax, ebx
		mov ds:mulcallptr, eax
		mov eax, offset itoah
		add eax, ebx
		mov ds:itoahcallptr, eax
		
		call prepareGDTR
		call prepareIDTR
		
		cli
		
		; disable and save NMI state
		in al,70h
		or al,80h
		out 70h,al
		in al, 71h
		
		call reinitPIC

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
		
		in al,70h
		and al,07fh
		out 70h,al
		in al, 71h
		
		sti
		
		push 14h
		push 18h
		mov ax, endinitpestrsize
		push ax
		lea bp, endinitpestr
		push bp
		call printProtectedVGA
		
		
		int 3h
		; test int 0 handlers
		;xor bx, bx
		;div bx
		
		call prepareFoneTSS
		
		mov ax, 30h ; 6 in GDT
		mov es, ax ; user mode data segment
		mov es:callgateprintmultiply, 4B0000h
		mov es:callgateprintcounter, 5B0000h 
		mov eax, ds:itoahcallptr
		mov es:itoahconformptr, eax
		mov eax, ds:mulcallptr
		mov es:mulconformptr, eax
		
		mov ax, 6Bh
		mov es, ax
		mov es:fonecallgateprintcounter, 5B0000h 
		mov es:foneputcharcallgate, 8B0000h
		mov es:fonegetcharcallgate, 930000h
		mov es:foneaddrowcallgate, 9B0000h
		mov es:foneaddcolcallgate, 0A30000h
		mov es:fonegetcurrowcallgate, 0AB0000h
		mov es:fonegetcurcolcallgate, 0B30000h
		mov eax, ds:itoahcallptr
		mov es:foneitoahconformptr, eax
		
		;lea ebx, ptrtofonetss ; fone gdt
		;jmp far ptr [ebx]
		
		push 3Bh ; stack
		push 0FFFFh
		push 2Bh ; user mode code 
		push offset workInUserMode
		;push 73h ; stack
		;push 0FFFFh
		;push 63h ; user mode code 
		;push 0h
		retf
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
		
		mov bx, seg conformcodesg
		shl bx, 4
		add bx, ax
		mov coreconformcodedesc.segbase1, bx
		
		mov bx, seg fonecodesg
		shl bx, 4
		add bx, ax
		mov fonecodedesc.segbase1, bx
		
		mov bx, seg fonedatasg
		shl bx, 4
		add bx, ax
		mov fonedatadesc.segbase1, bx
		
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
		xor di, di
		xor si, si
		mov cx, idtdescnum
		prepareIDTRloop:
			ifstart:
				cmp cx, idtdescnum - inthndlfuncnum
				ja inthndl
				cmp cx, pichndlfuncnum
				jbe pichndl
				; else
					mov eax, offset intStubHndl
					jmp ifend
				pichndl:
					mov eax, pichndlfunctions[di]
					add di, 4
					jmp ifend
				inthndl:
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
		mov ds:tsssegment.eip_reg, nexttaskstep
		
		mov ds:tsssegment.ss_reg, ss
		mov ds:tsssegment.esp_reg, esp
		
		mov ds:tsssegment.ds_reg, ds
		mov ds:tsssegment.es_reg, es
		
		xor eax, eax
		pushf
		pop ax
		mov ds:tsssegment.eflags, eax 

		mov ds:tsssegment.ss0, ss
		mov ds:tsssegment.esp0, 0FFFFh
		
		ret
	prepareTSS endp
	
	prepareFoneTSS proc near
		mov ds:fonetsssegment.cs_reg, 63h ; 12 GDT
	    mov ds:fonetsssegment.eip_reg, offset foneStart
		
		mov ds:fonetsssegment.ss_reg, 73h ; 14 GDT
		mov ds:fonetsssegment.esp_reg, 0FFFFh
		
		mov ds:fonetsssegment.ds_reg, 6Bh ; 13 GDT
		mov ds:fonetsssegment.es_reg, 6Bh ; 13 GDT
		
		xor eax, eax
		pushf
		pop ax
		mov ds:fonetsssegment.eflags, eax
		
		mov ds:fonetsssegment.ss0, 78h ; 15 GDT
		mov ds:fonetsssegment.esp0, 0FFFFh
		
		ret
	prepareFoneTSS endp
	
	; cli and sti must call outside this function
	reinitPIC proc near
		; icw1
		mov al, 15h 
		out 20h, al
		out 0A0h, al
		
		; icw2
		mov al, 20h ; shift to int 32
		out 21h, al
		mov al, 28h ; shift to int 40
		out 0A1h, al
		
		; icw3
		mov al, 04h ; irq2 - slave pic
		out 21h, al
		mov al, 02h ; id - 2 for slave pic
		out 0A1h, al
		
		; icw4
		mov al, 0Dh
		out 21h, al
		mov al, 09h
		out 0A1h, al
		
		ret
	reinitPIC endp
	
	; stack:
	; no checks out of buff, out vga buff
	; 0 <= col < 80 0 <= row < 25(bochsrc default)
	; col, row, buff_size, buff_ptr, ret_addr
	printProtectedVGA proc near
		push bp
		mov bp, sp
		push es
		push ax
		push bx
		push cx
		push dx
		push si
		
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
		
		pop si
		pop dx
		pop cx
		pop bx
		pop ax
		pop es
		pop bp
		
		ret 8
	printProtectedVGA endp
	
	printIntStr proc near
		push bp
		mov bp, sp
		push ds
		push eax
		push ebx
		
		mov ax, 10h
		mov ds, ax
		
		xor eax, eax
		mov ax, [bp+4]
		lea bx, ds:inthndlstr
		add bx, inthndlstrsize - 2h
		
		push eax
		push bx
		push 2h
		lea ebx, ds:itoahcallptr
		call far ptr [ebx]

		push 4fh - inthndlstrsize
		push [bp+4]
		mov ax, inthndlstrsize
		push ax
		lea bp, inthndlstr
		push bp
		call printProtectedVGA
		
		pop ebx
		pop eax
		pop ds
		pop bp
		
		ret 2
	printIntStr endp
	
	printPicStr proc near
		push bp
		mov bp, sp
		push ds
		push eax
		push ebx
		
		mov ax, 10h
		mov ds, ax

		xor eax, eax
		mov ax, [bp+4]
		lea bx, ds:pichndlstr
		add bx, pichndlstrsize - 2h
		
		push eax
		push bx
		push 2h
		lea ebx, ds:itoahcallptr
		call far ptr [ebx]
		
		push 28h
		push [bp+4]
		mov ax, pichndlstrsize
		push ax
		lea bp, pichndlstr
		push bp
		call printProtectedVGA
		
		pop ebx
		pop eax
		pop ds
		pop bp
		
		ret 2
	printPicStr endp
	
	printMultiplyResultStr proc far
		push bp
		mov bp, sp
		push ds
		push eax
		push ebx
		
		mov ax, 10h
		mov ds, ax
		
		xor eax, eax
		mov ax, [bp+4]
		lea bx, ds:multiplyresultstr
		add bx, multiplyresultstrsize - 2h
		
		push eax
		push bx
		push 2h
		lea ebx, ds:itoahcallptr
		call far ptr [ebx]	
		
		push 18h
		push 10h
		mov ax, multiplyresultstrsize
		push ax
		lea bp, multiplyresultstr
		push bp
		call printProtectedVGA
		
		pop ebx
		pop eax
		pop ds
		pop bp
		
		retf 2
	printMultiplyResultStr endp
	
	; word task id, ptr to string, size string
	printUserModeCounter proc far
		push bp
		mov bp, sp
		push ax
		push bx
		push cx
		
		mov ax, [bp+6] ; size
		mov bx, [bp+8] ; string ptr
		mov cx, [bp+10] ; task id
		
		add cx, 0Ch
		push 0h
		push cx
		push ax
		push bx
		call printProtectedVGA
		
		pop cx
		pop bx
		pop ax
		pop bp
		
		retf 6
	printUserModeCounter endp
	
	printInt8Counter proc near
		push bp
		mov bp, sp
		push ds
		push eax
		push ebx
		
		mov ax, 10h
		mov ds, ax
		
		mov eax, ds:int8counter
		inc eax
		mov ds:int8counter, eax
		lea bx, ds:int8outputstr
		add bx, 2h ; shift '0x'
		
		push eax
		push bx
		push 8h
		lea ebx, ds:itoahcallptr
		call far ptr [ebx]
		
		push 28h
		push 0h
		mov ax, int8outputstrsize
		push ax
		lea bp, int8outputstr
		push bp
		call printProtectedVGA
		
		pop ebx
		pop eax
		pop ds
		pop bp
		
		ret
	printInt8Counter endp
	
	; output console properties
	SCREEN_COLS = 40
	SCREEN_ROWS = 10
	
	cursorcolpos dw 0h
	cursorrowpos dw 0h
	
	putchar proc far
		push bp
		mov bp, sp
		push ax
		push bx
		push ds
		push es
		
		mov ax, 10h
		mov ds, ax
		mov ax, ds:cursorrowpos
		mov bx, 80
		mul bx
		mov bx, ds:cursorcolpos
		add bx, ax
		shl bx, 1
		
		mov ax, 20h
		mov es, ax
		mov ax, [bp+6]
		mov es:[bx], ax
		
		mov ax, ds:cursorcolpos
		inc ax
		cmp ax, SCREEN_COLS
		jne no_add_row
			mov ax, ds:cursorrowpos
			inc ax
			cmp ax, SCREEN_ROWS
			jne no_clear_row
				xor ax, ax
			no_clear_row:
			mov ds:cursorrowpos, ax 
			xor ax, ax
		no_add_row:
		mov ds:cursorcolpos, ax
		
		pop es
		pop ds
		pop bx
		pop ax
		pop bp
		retf 2
	putchar endp
	
	addrow proc far
		push bp
		mov bp, sp
		push ax
		push ds
		
		mov ax, 10h
		mov ds, ax
		
		mov ax, ds:cursorrowpos
		add ax, [bp+6]
		
		cmp ax, SCREEN_ROWS
		jb not_overflow
			xor ax, ax
		not_overflow:
		
		mov ds:cursorrowpos, ax
		
		pop ds
		pop ax
		pop bp
		retf 2
	addrow endp
	
	getcurrow proc far
		push ds
		
		mov ax, 10h
		mov ds, ax
		
		mov ax, ds:cursorrowpos

		pop ds
		retf
	getcurrow endp
	
	addcol proc far
		push bp
		mov bp, sp
		push ax
		push ds
		
		mov ax, 10h
		mov ds, ax
		
		mov ax, ds:cursorcolpos
		add ax, [bp+6]
		
		cmp ax, SCREEN_COLS
		jb not_overflow
			xor ax, ax
		not_overflow:
		
		mov ds:cursorcolpos, ax
		
		pop ds
		pop ax
		pop bp
		retf 2
	addcol endp
	
	getcurcol proc far
		push ds
		
		mov ax, 10h
		mov ds, ax
		
		mov ax, ds:cursorcolpos

		pop ds
		retf
	getcurcol endp
	
	; interrupt handles
	
	intStubHndl proc far
		push bp
		push ax
		
		push 4fh - intstubhndlstrsize
		push 04h
		mov ax, intstubhndlstrsize
		push ax
		lea bp, intstubhndlstr
		push bp
		call printProtectedVGA
		
		pop ax
		pop bp
		
		iret
	intStubHndl endp
	

	STUB_INT_HNDL macro number
		push number
		call printIntStr
	endm
	
	int0hndl proc far
		STUB_INT_HNDL 0h
		iret
	int0hndl endp
	
	int1hndl proc far
		STUB_INT_HNDL 1h
		iret
	int1hndl endp
	
	int2hndl proc far
		STUB_INT_HNDL 2h
		iret
	int2hndl endp
	
	int3hndl proc far
		STUB_INT_HNDL 03h
		iret
	int3hndl endp
	
	int4hndl proc far
		STUB_INT_HNDL 04h
		iret
	int4hndl endp
	
	int5hndl proc far
		STUB_INT_HNDL 05h
		iret
	int5hndl endp
	
	int6hndl proc far
		STUB_INT_HNDL 06h
		iret
	int6hndl endp
	
	int7hndl proc far
		STUB_INT_HNDL 07h
		iret
	int7hndl endp
	
	int8hndl proc far
		STUB_INT_HNDL 08h
		iret
	int8hndl endp
	
	int9hndl proc far
		STUB_INT_HNDL 09h
		iret
	int9hndl endp
	
	int10hndl proc far
		STUB_INT_HNDL 0Ah
		iret
	int10hndl endp
	
	int11hndl proc far
		STUB_INT_HNDL 0Bh
		iret
	int11hndl endp
	
	int12hndl proc far
		STUB_INT_HNDL 0Ch
		iret
	int12hndl endp
	
	int13hndl proc far
		STUB_INT_HNDL 0Dh
		iret
	int13hndl endp
	
	int14hndl proc far
		STUB_INT_HNDL 0Eh
		iret
	int14hndl endp
	
	int15hndl proc far
		STUB_INT_HNDL 0Fh
		iret
	int15hndl endp
	
	int16hndl proc far
		STUB_INT_HNDL 10h
		iret
	int16hndl endp
	
	int17hndl proc far
		STUB_INT_HNDL 11h
		iret
	int17hndl endp
	
	int18hndl proc far
		STUB_INT_HNDL 12h
		iret
	int18hndl endp
	
	int19hndl proc far
		STUB_INT_HNDL 13h
		iret
	int19hndl endp
	
	int20hndl proc far
		STUB_INT_HNDL 14h
		iret
	int20hndl endp
	
	; pic handlers 
	
	; macro stub for master pic
	STUB_MASTER_PIC_HNDL macro number
		push ax
		
		push number
		call printPicStr
		mov al, 20h
		out 20h, al
		
		pop ax
	endm
	
	ptrtomaintss dd 500000h
	ptrtofonetss dd 800000h
	
	; timer
	int32hndl proc far
		push ax
		push ebx
		push ds
		
		call printInt8Counter
		
		mov ax, 10h
		mov ds, ax
		
		str bx
		cmp bx, 50h
		je load_fone_tss
			lea ebx, ptrtomaintss ; main gdt
			jmp end_load
		load_fone_tss:
 			lea ebx, ptrtofonetss ; fone gdt
		end_load:
		
 		mov al, 20h
		out 20h, al
		
 		jmp far ptr [ebx]

		pop ds
		pop ebx
		pop ax
		
		iret
	int32hndl endp
	
	KBD_CMD_PORT = 64h
	KBD_DATA_PORT = 60h
	
	kbdbuffhead dw 0h
	kbdbufftail dw 0h
	; real size is 32 because if tail == head -> buff is empty
	kbdbuff db 33 dup(0)
	kdbbuffsize = $ - kbdbuff
	
	; keyboard
	int33hndl proc far
		push ax
		push bx
		push cx
		push si
		push ds
		
		mov ax, 10h
		mov ds, ax
		
		lea bx, ds:kbdbuff
		mov si, ds:kbdbufftail
		
		in al, KBD_CMD_PORT
		xor cx, cx
		test al, 1h
		jz output_empty
			read_kbd_buff:
				; write in buff
				in al, KBD_DATA_PORT
				mov ds:[bx+si], al
				
				; increase tail
				inc si
				cmp si, kdbbuffsize
				jne end_clear_tail
					xor si, si
				end_clear_tail:
				
				; if tail equal head don`t save tail
				; buff is overflow
				cmp si, ds:kbdbuffhead
				je end_save_tail
					mov ds:kbdbufftail, si
				end_save_tail:
				
				
				in al, KBD_CMD_PORT
				test al, 1h
			loopnz read_kbd_buff
		output_empty:
		
		mov al, 20h
		out 20h, al
		
		pop ds
		pop si
		pop cx
		pop bx
		pop ax		
		iret
	int33hndl endp
	
	getchar proc far
		push bx
		push si
		push es
		
		mov ax, 10h
		mov es, ax
		
		mov si, es:kbdbuffhead
		cmp si, es:kbdbufftail
		je empty_buffer
			lea bx, es:kbdbuff
			xor ax, ax
			mov al, es:[bx+si]			
			
			inc si
			cmp si, kdbbuffsize
			jne end_clear_tail
				xor si, si
			end_clear_tail:
			mov es:kbdbuffhead, si
			jmp end_getchar
		empty_buffer:
			xor ax, ax
		end_getchar:
		
		pop es
		pop si
		pop bx
		retf 
	getchar endp
	
	int34hndl proc far
		STUB_MASTER_PIC_HNDL 2h
		iret
	int34hndl endp
	
	int35hndl proc far
		STUB_MASTER_PIC_HNDL 3h
		iret
	int35hndl endp
	
	int36hndl proc far
		STUB_MASTER_PIC_HNDL 4h
		iret
	int36hndl endp
	
	int37hndl proc far
		STUB_MASTER_PIC_HNDL 5h
		iret
	int37hndl endp
	
	int38hndl proc far
		STUB_MASTER_PIC_HNDL 6h
		iret
	int38hndl endp
	
	int39hndl proc far
		STUB_MASTER_PIC_HNDL 7h
		iret
	int39hndl endp
	
	; macro stub for slave pic
	STUB_SLAVE_PIC_HNDL macro number
		;LOAD_ALL_REGISTERS_IN_STACK
		push ax
		
		push number
		call printPicStr
		mov al, 20h
		out 0A0h, al
		out 20h, al
		
		pop ax
		;UNLOAD_ALL_REGISTERS_IN_STACK
	endm
	
	int40hndl proc far
		STUB_SLAVE_PIC_HNDL 8h
		iret
	int40hndl endp
	
	int41hndl proc far
		STUB_SLAVE_PIC_HNDL 9h
		iret
	int41hndl endp
	
	int42hndl proc far
		STUB_SLAVE_PIC_HNDL 0Ah
		iret
	int42hndl endp
	
	int43hndl proc far
		STUB_SLAVE_PIC_HNDL 0Bh
		iret
	int43hndl endp
	
	int44hndl proc far
		STUB_SLAVE_PIC_HNDL 0Ch
		iret
	int44hndl endp
	
	int45hndl proc far
		STUB_SLAVE_PIC_HNDL 0Dh
		iret
	int45hndl endp
	
	int46hndl proc far
		STUB_SLAVE_PIC_HNDL 0Eh
		iret
	int46hndl endp
	
	int47hndl proc far
		STUB_SLAVE_PIC_HNDL 0Fh
		iret
	int47hndl endp
	
	codesgsize = $ - begincodesg
codesg ENDS



conformcodesg SEGMENT PARA USE16 'CODE'
	beginconformcodesg = $
	; conforming core code segment
	multiplyTwoNumbers proc far
		push bp
		mov bp, sp
		push ax
		push bx
		
		mov ax, [bp+6]
		mov bx, [bp+8]
		mul bx
		
		pop bx
		pop ax
		pop bp
		
		retf 4
	multiplyTwoNumbers endp
	
	; work in current ds
	; doubleword - value; pointer - buff; word - size
	itoah proc far
		push bp
		mov bp, sp
		push eax
		push bx
		push cx
		push si
		
		mov cx, [bp+6] ; size
		mov bx, [bp+8] ; buff
		mov eax, [bp+10] ; value
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
		
		retf 8
	itoah endp
	
	conformcodesgsize = $ - beginconformcodesg
conformcodesg ends	

	
	
usermodedatasg SEGMENT PARA USE16 'DATA'
	beginusermodedatasg = $
	; user mode data segment
	var1 dw 0Ah
	var2 dw 0Bh
	counter dd 1000h
	
	counterstr db 'I`m main task. My counter = 0x0000'
	counterstrsize = $ - counterstr
	
	mulconformptr dd 0h
	itoahconformptr dd 0h
	callgateprintmultiply dd 0h
	callgateprintcounter dd 0h

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
		lea ebx, ds:mulconformptr
		call far ptr [ebx]
		
		; output result
		push ax
		lea ebx, ds:callgateprintmultiply
		call far ptr [ebx]
		
		infinity_loop:
			mov ecx, 02FFFFh
			pause_loop:
				dec ecx
			jecxz pause_end
			jmp pause_loop
		pause_end:
			
			call printCounter
		jmp infinity_loop
	workInUserMode endp
	
	printCounter proc near
		mov eax, ds:counter
		inc eax
		mov ds:counter, eax
		
		; itoa
		lea bx, counterstr
		add bx, counterstrsize - 4
		
		push eax
		push bx
		push 4h
		lea ebx, ds:itoahconformptr
		call far ptr [ebx]
		
		; word task id, ptr to string, size string
		push 0h
		lea bx, counterstr
		push bx
		push counterstrsize
		lea ebx, ds:callgateprintcounter
		call far ptr [ebx]
		
		ret
	printCounter endp
	
	usermodecodesgsize = $ - beginusermodecodesg
usermodecodesg ends
	
fonedatasg SEGMENT PARA USE16 'DATA'
	beginfonedatasg = $
	
	fonecounter dd 0h
	
	fonecounterstr db 'I`m fone task. My counter = 0x0000'
	fonecounterstrsize = $ - fonecounterstr
	
	fonecallgateprintcounter dd 0h
	foneitoahconformptr dd 0h
	
	foneputcharcallgate dd 0h
	fonegetcharcallgate dd 0h
	foneaddrowcallgate dd 0h
	foneaddcolcallgate dd 0h
	fonegetcurrowcallgate dd 0h
	fonegetcurcolcallgate dd 0h
	
	kbdRegister db 0h
	KBD_REGISTER_NO_SHIFT = 0h
	KBD_REGISTER_SHIFT = 1h
	
	scancodehandlers dw 256 dup(offset scanCodeStub)
	
	fonedatasgsize = $ - beginfonedatasg
fonedatasg ends

fonecodesg SEGMENT PARA USE16 'CODE'
	beginfonecodesg = $
	
	foneStart proc far
		mov ax, 6Bh
		mov ds, ax
		
		call initKeyboardHandlers
		
		infinity_loop:
			mov ecx, 02FFFFh
			pause_loop:
				call foneKeyboardOutput
				dec ecx
			jecxz pause_end
			jmp pause_loop
		pause_end:
			call fonePrintCounter
		jmp infinity_loop
	foneStart endp
	
	fonePrintCounter proc near
		push eax
		push ebx
		
		mov eax, ds:fonecounter
		inc eax
		mov ds:fonecounter, eax
		
		; itoa
		lea bx, fonecounterstr
		add bx, fonecounterstrsize - 4
		
		push eax
		push bx
		push 4h
		lea ebx, ds:foneitoahconformptr
		call far ptr [ebx]
		
		; word task id, ptr to string, size string
		push 1h
		lea bx, fonecounterstr
		push bx
		push fonecounterstrsize
		lea ebx, ds:fonecallgateprintcounter
		call far ptr [ebx]
		
		pop ebx
		pop eax
		
		ret
	fonePrintCounter endp
	
	foneKeyboardOutput proc near
		push ax
		push ebx
		push si
		lea ebx, ds:fonegetcharcallgate
		call far ptr [ebx]
		
		cmp ax, 0h
		je end_output
			mov si, ax
			shl si, 1
			push ax
			lea bx, ds:scancodehandlers[si]
			; lea bx, offset scanCodeHndl0
			call near ptr [bx]
		end_output:
		
		pop si
		pop ebx
		pop ax
		ret
	foneKeyboardOutput endp
	
	initKeyboardHandlers proc near
		push bx
		
		lea bx, ds:scancodehandlers
		
		; all scan code multiply by 2 because address take 2 bytes
		mov ds:[bx+3ch], offset scanCodeHndlA
		mov ds:[bx+60h], offset scanCodeHndlB
		mov ds:[bx+5ch], offset scanCodeHndlC
		mov ds:[bx+40h], offset scanCodeHndlD
		mov ds:[bx+24h], offset scanCodeHndlE
		mov ds:[bx+42h], offset scanCodeHndlF
		mov ds:[bx+44h], offset scanCodeHndlG
		mov ds:[bx+46h], offset scanCodeHndlH
		mov ds:[bx+2eh], offset scanCodeHndlI
		mov ds:[bx+48h], offset scanCodeHndlJ
		mov ds:[bx+4ah], offset scanCodeHndlK
		mov ds:[bx+4ch], offset scanCodeHndlL
		mov ds:[bx+64h], offset scanCodeHndlM
		mov ds:[bx+62h], offset scanCodeHndlN
		mov ds:[bx+30h], offset scanCodeHndlO
		mov ds:[bx+32h], offset scanCodeHndlP
		mov ds:[bx+20h], offset scanCodeHndlQ
		mov ds:[bx+26h], offset scanCodeHndlR
		mov ds:[bx+3eh], offset scanCodeHndlS
		mov ds:[bx+28h], offset scanCodeHndlT
		mov ds:[bx+2ch], offset scanCodeHndlU
		mov ds:[bx+5eh], offset scanCodeHndlV
		mov ds:[bx+22h], offset scanCodeHndlW
		mov ds:[bx+5ah], offset scanCodeHndlX
		mov ds:[bx+2ah], offset scanCodeHndlY
		mov ds:[bx+58h], offset scanCodeHndlZ
		                 
		mov ds:[bx+16h], offset scanCodeHndl0
		mov ds:[bx+04h], offset scanCodeHndl1
		mov ds:[bx+06h], offset scanCodeHndl2
		mov ds:[bx+08h], offset scanCodeHndl3
		mov ds:[bx+0ah], offset scanCodeHndl4
		mov ds:[bx+0ch], offset scanCodeHndl5
		mov ds:[bx+0eh], offset scanCodeHndl6
		mov ds:[bx+10h], offset scanCodeHndl7
		mov ds:[bx+12h], offset scanCodeHndl8
		mov ds:[bx+14h], offset scanCodeHndl9
		
		mov ds:[bx+52h], offset scanCodeHndlApostrophe
		mov ds:[bx+18h], offset scanCodeHndlMinus
		mov ds:[bx+1ah], offset scanCodeHndlEqual
		mov ds:[bx+56h], offset scanCodeHndlSlash
		mov ds:[bx+34h], offset scanCodeHndlLBrace
		mov ds:[bx+36h], offset scanCodeHndlRBrace
		mov ds:[bx+4eh], offset scanCodeHndlSemicolon
		mov ds:[bx+50h], offset scanCodeHndlQuote
		mov ds:[bx+66h], offset scanCodeHndlComma
		mov ds:[bx+68h], offset scanCodeHndlPoint
		mov ds:[bx+6ah], offset scanCodeHndlBackSlash
		mov ds:[bx+72h], offset scanCodeHndlSpace
		
		mov ds:[bx+54h], offset scanCodeHndlShiftOn
		mov ds:[bx+154h], offset scanCodeHndlShiftOff
		mov ds:[bx+38h], offset scanCodeHndlEnter
		                  
		pop bx            
	                      
		ret 
	initKeyboardHandlers endp
	
	scanCodeStub proc near
		ret 2
	scanCodeStub endp
	
	printChar proc near
		push bp
		mov bp, sp
		push ax
		push bx
		
		cmp ds:kbdRegister, KBD_REGISTER_SHIFT
		je shift_char
			mov ax, [bp+6]
			jmp end_select_char
		shift_char:
			mov ax, [bp+4]	
		end_select_char:

		mov ah, 70h
		push ax
		lea bx, ds:foneputcharcallgate
		call far ptr [ebx]
		
		pop bx
		pop ax
		pop bp
		ret 4
	printChar endp
	
	PRINT_CHAR macro char, charSh
		push char
		push charSh
		call printChar
		ret 2
	endm
	
	scanCodeHndl0 proc near 
		PRINT_CHAR '0', ')'
	scanCodeHndl0 endp
	
	scanCodeHndl1 proc near 
		PRINT_CHAR '1', '!'
	scanCodeHndl1 endp
	
	scanCodeHndl2 proc near 
		PRINT_CHAR '2', '@'
	scanCodeHndl2 endp
	
	scanCodeHndl3 proc near 
		PRINT_CHAR '3', '#'
	scanCodeHndl3 endp
	
	scanCodeHndl4 proc near 
		PRINT_CHAR '4', '$'
	scanCodeHndl4 endp
	
	scanCodeHndl5 proc near 
		PRINT_CHAR '5', '%'
	scanCodeHndl5 endp
	
	scanCodeHndl6 proc near 
		PRINT_CHAR '6', '^'
	scanCodeHndl6 endp
	
	scanCodeHndl7 proc near 
		PRINT_CHAR '7', '&'
	scanCodeHndl7 endp
	
	scanCodeHndl8 proc near 
		PRINT_CHAR '8', '*'
	scanCodeHndl8 endp
	
	scanCodeHndl9 proc near 
		PRINT_CHAR '9', '('
	scanCodeHndl9 endp
	
	scanCodeHndlA proc near 
		PRINT_CHAR 'a', 'A'
	scanCodeHndlA endp
	
	scanCodeHndlB proc near 
		PRINT_CHAR 'b', 'B'
	scanCodeHndlB endp
	
	scanCodeHndlC proc near 
		PRINT_CHAR 'c', 'C'
	scanCodeHndlC endp
	
	scanCodeHndlD proc near 
		PRINT_CHAR 'd', 'D'
	scanCodeHndlD endp
	
	scanCodeHndlE proc near 
		PRINT_CHAR 'e', 'E'
	scanCodeHndlE endp
	
	scanCodeHndlF proc near 
		PRINT_CHAR 'f', 'F'
	scanCodeHndlF endp
	
	scanCodeHndlG proc near 
		PRINT_CHAR 'g', 'G'
	scanCodeHndlG endp
	
	scanCodeHndlH proc near 
		PRINT_CHAR 'h', 'H'
	scanCodeHndlH endp
	
	scanCodeHndlI proc near 
		PRINT_CHAR 'i', 'I'
	scanCodeHndlI endp
	
	scanCodeHndlJ proc near 
		PRINT_CHAR 'j', 'J'
	scanCodeHndlJ endp
	
	scanCodeHndlK proc near 
		PRINT_CHAR 'k', 'K'
	scanCodeHndlK endp
	
	scanCodeHndlL proc near 
		PRINT_CHAR 'l', 'L'
	scanCodeHndlL endp
	
	scanCodeHndlM proc near 
		PRINT_CHAR 'm', 'M'
	scanCodeHndlM endp
	
	scanCodeHndlN proc near 
		PRINT_CHAR 'n', 'N'
	scanCodeHndlN endp
	
	scanCodeHndlO proc near 
		PRINT_CHAR 'o', 'O'
	scanCodeHndlO endp
	
	scanCodeHndlP proc near 
		PRINT_CHAR 'p', 'P'
	scanCodeHndlP endp
	
	scanCodeHndlQ proc near 
		PRINT_CHAR 'q', 'Q'
	scanCodeHndlQ endp
	
	scanCodeHndlR proc near 
		PRINT_CHAR 'r', 'R'
	scanCodeHndlR endp
	
	scanCodeHndlS proc near 
		PRINT_CHAR 's', 'S'
	scanCodeHndlS endp
	
	scanCodeHndlT proc near 
		PRINT_CHAR 't', 'T'
	scanCodeHndlT endp
	
	scanCodeHndlU proc near 
		PRINT_CHAR 'u', 'U'
	scanCodeHndlU endp
	
	scanCodeHndlV proc near 
		PRINT_CHAR 'v', 'V'
	scanCodeHndlV endp
	
	scanCodeHndlW proc near 
		PRINT_CHAR 'w', 'W'
	scanCodeHndlW endp
	
	scanCodeHndlX proc near 
		PRINT_CHAR 'x', 'X'
	scanCodeHndlX endp
	
	scanCodeHndlY proc near 
		PRINT_CHAR 'y', 'Y'
	scanCodeHndlY endp
	
	scanCodeHndlZ proc near 
		PRINT_CHAR 'z', 'Z'
	scanCodeHndlZ endp
	
	scanCodeHndlApostrophe proc near 
		PRINT_CHAR '`', '~'
	scanCodeHndlApostrophe endp
	
	scanCodeHndlMinus proc near 
		PRINT_CHAR '-', '_'
	scanCodeHndlMinus endp
	
	scanCodeHndlEqual proc near 
		PRINT_CHAR '=', '+'
	scanCodeHndlEqual endp
	
	scanCodeHndlSlash proc near 
		PRINT_CHAR '\\', '|'
	scanCodeHndlSlash endp
	
	scanCodeHndlLBrace proc near 
		PRINT_CHAR '[', '{'
	scanCodeHndlLBrace endp
	
	scanCodeHndlRBrace proc near 
		PRINT_CHAR ']', '}'
	scanCodeHndlRBrace endp
	
	scanCodeHndlSemicolon proc near 
		PRINT_CHAR ';', ':'
	scanCodeHndlSemicolon endp
	
	scanCodeHndlQuote proc near 
		PRINT_CHAR '''', '"'
	scanCodeHndlQuote endp
	
	scanCodeHndlComma proc near 
		PRINT_CHAR ',', '<'
	scanCodeHndlComma endp
	
	scanCodeHndlPoint proc near 
		PRINT_CHAR '.', '>'
	scanCodeHndlPoint endp
	
	scanCodeHndlBackSlash proc near 
		PRINT_CHAR '/', '?'
	scanCodeHndlBackSlash endp
	
	scanCodeHndlSpace proc near 
		PRINT_CHAR ' ', ' '
	scanCodeHndlSpace endp
	
	scanCodeHndlShiftOn proc near 
		mov ds:kbdRegister, KBD_REGISTER_SHIFT
		ret 2
	scanCodeHndlShiftOn endp
	
	scanCodeHndlShiftOff proc near 
		mov ds:kbdRegister, KBD_REGISTER_NO_SHIFT
		ret 2
	scanCodeHndlShiftOff endp
	
	scanCodeHndlEnter proc near 
		push ax
		push ebx
		
		push 1h
		lea ebx, ds:foneaddrowcallgate
		call far ptr [ebx]
		
		lea ebx, ds:fonegetcurcolcallgate
		call far ptr [ebx]
		
		xor bx, bx
		sub bx, ax
		
		push bx
		lea ebx, ds:foneaddcolcallgate
		call far ptr [ebx]
		
		pop ebx
		pop ax
		ret 2
	scanCodeHndlEnter endp
	
	fonecodesgsize = $ - beginfonecodesg
fonecodesg ends

END