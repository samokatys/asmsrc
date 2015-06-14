.586p

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
	
	readdap dapstruct {10h, 0h, 0018h, 0h, 07E0h, {01h, 00h, 00h, 00h, 00h, 00h, 00h, 00h} }
	
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
	codedesc segmentdescriptor { codesgsize, 7C00h, 0h, 109Ah, 0 }
	datadesc segmentdescriptor { codesgsize, 7C00h, 0, 1092h, 0 }
	stackdesc segmentdescriptor { 0h, 0D000h, 0h, 1096h, 0h }
	videotextdesc segmentdescriptor { 0, 8000h, 0Bh, 1192h, 0 }
	; user mode segments
	usermodecodedesc segmentdescriptor { 0FFFFh, 0, 0, 10FAh, 10h } ; base addr 10000000h 
	usermodedatadesc segmentdescriptor { 0FFFFh, 0, 0, 10F2h, 10h } ; base addr 10000000h 
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
	; fonecodedesc segmentdescriptor { fonecodesgsize, 0, 0, 10FAh, 0 }
	; fonedatadesc segmentdescriptor { fonecodesgsize, 0, 0, 10F2h, 0 }
	fonecodedesc segmentdescriptor { fonecodesgsize, 0, 0, 10FAh, 10h } ; base addr 10000000h
	fonedatadesc segmentdescriptor { fonecodesgsize, 0, 0, 10F2h, 10h } ; base addr 10000000h
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
	; paging data segment
	pagingdatadesc segmentdescriptor { 0FFh, 0h, 20h, 9092h, 0 }  ; G = 1 1MByte size
	bigdatadesc segmentdescriptor { 0FFFFh, 0h, 0h, 9F92h, 0 } ; G = 1 4GByte size
	; allocate memory
	memallocatecallgate gatedescriptor { offset allocateUserMode, 08h, 0E401h, 0h } 
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
	
	virt1str db 'Virtual memory string 1'
	virt1strsize = $ - virt1str
	virt2str db 'Virtual memory string 2'
	virt2strsize = $ - virt2str
	
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
		
		call disablePIC
		
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
		
		call prepareVirtualAddressStrings
		
		call setPaging
		
		mov ebx, 0FEE00000h
		push 0h
		push ebx
		push ebx
		call setVirtualMemory
		
		push 1h
		push ebx
		push ebx
		call setVirtualMemory
		
		call prepareUserModeSegment
		call prepareFoneSegment
		
		call prepareTSS
		ltr taskreg
		; when we load task in register
		; cpu load stored data in segment like cs:ip
		nexttaskstep = $

		call prepareFoneTSS
		
		in al,70h
		and al,07fh
		out 70h,al
		in al, 71h
		
		sti
		
		call initAPIC
		
		push 14h
		push 18h
		mov ax, endinitpestrsize
		push ax
		lea ebp, endinitpestr
		push ebp
		call printProtectedVGA
		
		; test exception handlers
		int 3h
		
		; virtual memory work
		push 0h
		pushd 30000000h
		pushd 300000h
		call setVirtualMemory
		
		push 0h
		pushd 40000000h
		pushd 301000h
		call setVirtualMemory
		
		call printVirtualAddressStrings
		call testResetVirtualAddress
		
		call searchMPTable
		call makeInitProcPlace
		call startNewProcessor
		
		call resignKbdInterrupt
		
		push 3Bh ; stack
		push 0FFFFh
		push 2Bh ; user mode code 
		push offset workInUserMode

		retf
	workInProtectedMode endp
	
	floatingptrsignature dd 5F504D5Fh
	mptablesignature dd 504D4350h
	
	mptableptr dd 0h
	
	searchMPTable proc near
		push eax
		push ebx
		push ds
		push es
		
		mov ax, 0C0h
		mov ds, ax
		mov ax, 10h
		mov es, ax
		
		xor eax, eax
		xor ebx, ebx
		
		mov bx, 040Eh
		mov ax, ds:[bx]
		
		cmp ax, 0h
		jne ebda_exist
			mov eax, 9FC00h ; physical memory > 512 KB
			jmp end_init_address
		ebda_exist:
			shl eax, 4
		end_init_address:
		
		push eax
		add eax, 400h
		push eax
		mov eax, es:floatingptrsignature
		push eax
		call searchDWordInMemory
		
		cmp eax, 0h
		je float_ptr_found
			; search in bios memory
			pushd 0F0000h
			pushd 100000h
			pushd es:floatingptrsignature
			call searchDWordInMemory
			
			cmp eax, 0h
			je float_ptr_found
				; hndl this error state
				jmp end_of_search
		float_ptr_found:
			mov eax, ds:[ebx+4] ; ptr to floating ptr
			mov ebx, eax
			mov eax, ds:[ebx] ; signature of table
			cmp eax, es:mptablesignature
			jne bad_mp_table_signature
				mov es:mptableptr, ebx
				jmp end_of_search
			bad_mp_table_signature:
				; hndl this error state
		end_of_search:
	
		pop es
		pop ds
		pop ebx
		pop eax
		ret 
	searchMPTable endp
	
	; double word - start address in ds
	; double word - end address in ds
	; double word - signature
	; eax - error code 0 - no error, 1 - not found
	; ebx - address
	searchDWordInMemory proc near
		push bp
		mov bp, sp
		push ecx
		push esi
		
		mov eax, [bp+4] ; signature
		mov ecx, [bp+8] ; end
		mov ebx, [bp+12] ; start
		
		sub ecx, ebx ; number of bytes
		mov esi, ebx
		search_dword_loop:
			mov ebx, ds:[esi]
			cmp ebx, eax
			je dword_found
				sub ecx, 16
				add esi, 16
				cmp ecx, 0h
				ja search_dword_loop ; check next address
					mov eax, 1h
					jmp end_search_dword_loop
			dword_found:
				xor eax, eax
				mov ebx, esi
		end_search_dword_loop:
		
		pop ecx
		pop esi
		pop bp
		ret 12
	searchDWordInMemory endp
	
	startNewProcessor proc near
		push ds
		push es
		
		; set shutdown type
		mov al, 0Fh
		out 70h, al
		mov al, 0Ah
		out 71h, al
		
		; set init address
		mov ax, 0C0h
		mov es, ax
		mov ax, 10h
		mov ds, ax
		
		mov bx, 467h
		mov eax, ds:initaddress
		mov es:[bx], eax
		
		; send init
		mov ebx, 0FEE00000h
		check_ready_to_send_ipi:
			mov eax, es:[ebx+300h]
			test eax, 1000h
		jnz check_ready_to_send_ipi
		
		cli

		; init ipi
		mov eax, 0000000h
		mov es:[ebx+310h], eax
		mov eax, 0CC500h
		mov es:[ebx+300h], eax
		
		sti
		
		mov cx, 0FFFFh
		sleep_init_ipi:
		loop sleep_init_ipi
		
		mov eax, es:[ebx+30h] ; local apic version
		and ax, 0FFh
		cmp ax, 10h ; 0Xh - 82489DX discrete APIC
		jb end_startup_ipi
			; startup ipi
			mov dx, 2h
			startup_ipi:
				dec dx
				
				check_ready_to_send_sipi:
					mov eax, es:[ebx+300h]
					test eax, 1000h
				jnz check_ready_to_send_sipi
				
				cli 
				in al,70h
				or al,80h
				out 70h,al
				
				mov eax, 0000000h
				mov es:[ebx+310h], eax
				mov ecx, ds:initaddress
				shr ecx, 12
				and ecx, 0FFh
				mov eax, 0C4600h
				or eax, ecx
				mov es:[ebx+300h], eax
				
				in al,70h
				and al,07fh
				out 70h,al
				sti
				
				mov cx, 0FFFFh
				sleep_startup_ipi:
				loop sleep_startup_ipi
				
			cmp dx, 0h
			jne startup_ipi
		end_startup_ipi:
		
		
		pop es
		pop ds
		
		ret 
	startNewProcessor endp
	
	initaddress dd 088000h
	callbackptr dd 0h
	newprocstr db 'Hello, MP World!'
	newprocstrsize = $ - newprocstr
	
	printNewProcString proc near
		push 0h
		push 14h
		push newprocstrsize
		lea ebx, newprocstr
		pushd ebx
		call printRealModeVGA
		
		jmp $
	printNewProcString endp
	
	tmp db 'MARK'
	procInitProcedure proc near
		mov ax, 7c0h
		mov ds, ax
		mov es, ax
		mov ax, 8900h
		mov ss, ax
		mov sp, 0FFFFh

		lea ebx, ds:callbackptr
		call far ptr [ebx]
	procInitProcedure endp
	procInitProcedureSize = $ - procInitProcedure
	
	kbdintstr db 'Keyboard interrupt!'
	kbdintstrsize = $ - kbdintstr
	realModeKbdHndl proc near
		push ebx
		push ds
		
		mov bx, 7c0h
		mov ds, bx
		
		push 0h
		push 15h
		push ds:kbdintstrsize
		lea ebx, ds:kbdintstr
		pushd ebx
		call printRealModeVGA
		
		pop ds
		pop ebx
		
		iret
	realModeKbdHndl endp
	
	resignKbdInterrupt proc near
		push eax
		push ebx
		push edx
		push esi
		push ds
		push es
		
		mov ax, 10h
		mov ds, ax
		mov ax, 0C0h
		mov es, ax
		
		; reinit real mode ptr to interrupt handler
		mov ax, 21h
		mov bx, 4h
		mul bx
		mov bx, ax
		mov eax, 7c00000h
		mov ax, offset realModeKbdHndl
		mov es:[bx], eax
		
		; i/o apic reinit
		mov ebx, ds:apicioregsel
		mov eax, 21h
		mov esi, START_ADDRESS_OI_TBL + 2 ; kbd interrupt

		mov es:[ebx], esi
		mov edx, es:[ebx+APIC_WIN_OFFSET]
		mov es:[ebx+APIC_WIN_OFFSET], eax
		
		inc esi
		mov es:[ebx], esi
		mov edx, es:[ebx+APIC_WIN_OFFSET]
		mov edx, 1000000h
		mov es:[ebx+APIC_WIN_OFFSET], edx
		
		pop es
		pop ds
		pop esi
		pop edx
		pop ebx
		pop eax
			
		ret
	resignKbdInterrupt endp
	
	makeInitProcPlace proc near
		push es
		push ds
		
		mov ax, 10h
		mov ds, ax
		mov ax, 0C0h
		mov es, ax
		
		mov ebx, 07c00000h
		add ebx, offset printNewProcString
		mov ds:callbackptr, ebx
		
		mov esi, offset procInitProcedure
		mov edi, ds:initaddress
		mov cx, procInitProcedureSize
		rep movsb es:[edi], ds:[esi]
		
		pop ds
		pop es
		
		ret
	makeInitProcPlace endp
	
	
	prepareGDTR proc
		; compute gdtr start 
		xor eax, eax
		mov bx, cs
		shl bx, 4
		lea ax, nulldesc
		add ax, bx
		mov gdtr.baselinearaddr, eax
		
		mov ax, 7c00h
		
		; mov bx, seg usermodecodesg
		; shl bx, 4
		; add bx, ax
		; mov usermodecodedesc.segbase1, bx
		; mov usermodedatadesc.segbase1, bx
		
		mov bx, seg conformcodesg
		shl bx, 4
		add bx, ax
		mov coreconformcodedesc.segbase1, bx
		
		mov bx, seg conformcodesg
		shl bx, 4
		add bx, ax
		mov coreconformcodedesc.segbase1, bx
		
		; mov bx, seg fonecodesg
		; shl bx, 4
		; add bx, ax
		; mov fonecodedesc.segbase1, bx
		; mov fonedatadesc.segbase1, bx
		
		ret
	prepareGDTR endp
	
	prepareUserModeSegment proc near
		push eax
		push ebx
		push cx
		push edi
		push esi
		push ds
		push es
		
		mov ax, 10h
		mov ds, ax
		mov ax, 0C0h
		mov es, ax
		
		xor ebx, ebx
		mov bx, seg usermodecodesg
		shl bx, 4
		add bx, 7c00h ; temporary segment physical address
		
		mov es:[bx+callgateprintmultiply], 4B0000h
		mov es:[bx+callgateprintcounter], 5B0000h 
		mov es:[bx+callgatememallocate], 0C80000h 
		mov eax, ds:itoahcallptr
		mov es:[bx+itoahconformptr], eax
		mov eax, ds:mulcallptr
		mov es:[bx+mulconformptr], eax
		
		push 0h
		push usermodecodesgsize
		push ebx
		call setUserModeCode
		
		pop es
		pop ds
		pop esi
		pop edi
		pop cx
		pop ebx
		pop eax
		
		ret
	prepareUserModeSegment endp
	
	prepareFoneSegment proc near
		push eax
		push ebx
		push cx
		push edi
		push esi
		push ds
		push es
		
		mov ax, 10h
		mov ds, ax
		mov ax, 0C0h
		mov es, ax
		
		xor ebx, ebx
		mov bx, seg fonecodesg
		shl bx, 4
		add bx, 7c00h ; temporary segment physical address
		
		mov es:[bx+fonecallgateprintcounter], 5B0000h 
		mov es:[bx+foneputcharcallgate], 8B0000h
		mov es:[bx+fonegetcharcallgate], 930000h
		mov es:[bx+foneaddrowcallgate], 9B0000h
		mov es:[bx+foneaddcolcallgate], 0A30000h
		mov es:[bx+fonegetcurrowcallgate], 0AB0000h
		mov es:[bx+fonegetcurcolcallgate], 0B30000h
		mov eax, ds:itoahcallptr
		mov es:[bx+foneitoahconformptr], eax
		
		push 1h
		push fonecodesgsize
		push ebx
		call setUserModeCode		

		pop es
		pop ds
		pop esi
		pop edi
		pop cx
		pop ebx
		pop eax
		
		ret
	prepareFoneSegment endp
	
	; word - context number
	; word - size bytes
	; double word - code address physical
	; ret eax (0 - no error, 1 - not enough memory)
	setUserModeCode proc near
		push bp
		mov bp, sp
		push eax
		push ebx
		push ecx
		push esi
		push edi
		push es
		push ds
		
		mov ax, 0C0h
		mov es, ax
		mov ds, ax
		
		xor eax, eax
		xor ecx, ecx
		mov cx, [bp+8] ; size
		mov ax, cx
		shr ax, 12 ; pages count
		and cx, 0FFFh
		jz not_increment
			inc ax ; rest > 0
		not_increment:
		
		push [bp+10] ; context number
		push ax
		call allocateVirtualMemory
		
		cmp eax, 0h
		jne end_of_copy_code
			mov eax, cr3
			
			xor esi, esi
			mov si, [bp+10]
			shl esi, 12
			add esi, 200000h
			mov cr3, esi
		
			mov esi, [bp+4]
			mov edi, ebx
			mov cx, [bp+8] ; size
			rep movsb es:[edi], ds:[esi]
			
			mov cr3, eax
			xor eax, eax
		end_of_copy_code:
		
		pop ds
		pop es
		pop edi
		pop esi
		pop ecx
		pop ebx
		pop eax
		pop bp
		
		ret 8
	setUserModeCode endp
	
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
			shr eax, 16
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
		
		mov eax, cr3
		mov ds:tsssegment.cr3_pdbr, eax
		
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
		or ax, 200h ; int on
		mov ds:fonetsssegment.eflags, eax
		
		mov ds:fonetsssegment.ss0, 78h ; 15 GDT
		mov ds:fonetsssegment.esp0, 0FFFFh
		
		mov eax, cr3
		mov ds:fonetsssegment.cr3_pdbr, eax
		
		mov dx, ss
		mov ebx, esp
		
		mov ax, 78h
		mov ecx, 0FFFFh
		mov ss, ax
		mov esp, ecx
		
		; return stack
		push 73h
		push 0FFFFh
		pushf
		pop ax
		or ax, 200h ; int on
		push ax
		; push 63h
		push 2bh
		push offset foneStart
		; stack registers
		pushd 0h ; push eax
		pushd 0h ; push ebx
		pushd 0h ; push ecx
		pushd 0h ; push edx
		pushd 0h ; push esi
		pushd 0h ; push edi
		pushd 0h ; push ebp
		;push 6Bh ; push ds
		;push 6Bh ; push es
		push 33h
		push 33h
		
		
		mov ds:prevss, ss
		mov ds:prevesp, esp
		mov ds:prevss0, 78h
		mov ds:prevesp0, 0FFFFh
		
		mov ss, dx
		mov esp, ebx
		
		ret
	prepareFoneTSS endp
	
	maxpdenumber = 10h ; fixed cannot changed
	pagingsegmentselector = 0B8h
	usedptenumber dd 0h
	
	setPaging proc near
		push ds
		push es
		
		mov ax, 10h
		mov ds, ax
		
		push 0B8h
		call initPDE
		
		mov eax, 200000h ; last 12 bits - reserved or params
		mov cr3, eax
		
		mov eax, cr0
		or eax, 80000000h
		mov cr0, eax
		
		pop es
		pop ds
		
		ret
	setPaging endp
	
	
	
	; word - page segment num
	initPDE proc near
		push bp
		mov bp, sp
		push eax
		push ebx
		push cx
		push es
		push ds
		
		mov ax, 10h
		mov ds, ax
		mov es, [bp+4]
		
		push [bp+4]
		call segmentBaseAddress
		
		mov ebx, eax ; base segment address
		mov eax, maxpdenumber
		shl eax, 12
		add eax, ebx ; null pte address
		
		push eax
		call initNullPTE
		
		or eax, 7h
		mov cx, maxpdenumber
		pde_null_entry_init:
			mov bx, cx
			dec bx
			shl ebx, 12
			mov es:[ebx], eax
			
			shr ebx, 10 ; cx * 4 bytes
			; mov ds:lastvirtualaddr[bx], 3FF000h
			mov ds:lastvirtualaddr[bx], 0FFFF000h ; next 10000000h
		loop pde_null_entry_init
		
		mov eax, ds:usedptenumber
		inc eax
		mov ds:usedptenumber, eax		
		
		pop ds
		pop es
		pop cx
		pop ebx
		pop eax
		pop bp
		
		ret 2
	initPDE endp
	
	;double word - null pte address
	initNullPTE proc near
		push bp
		mov bp, sp
		push eax
		push ebx
		push cx
		push dx
		push esi
		push es
		
		mov ax, 0C0h
		mov es, ax
		
		mov ebx, [bp+4]
		xor dx, dx
		mov cx, 0400h 
		init_page_table:
			xor esi, esi
			mov si, dx
			shl si, 2 ; * 4 bytes
			
			xor eax, eax
			mov ax, dx
			shl eax, 12 ; 12(flags)
			or eax, 7h
			
			mov es:[ebx+esi], eax
			
			inc dx
		loop init_page_table
		
		pop es
		pop esi
		pop dx
		pop cx
		pop ebx
		pop eax
		pop bp
		
		ret 4
	initNullPTE endp
	
	prepareVirtualAddressStrings proc near
		push es
		
		mov ax, 0C0h
		mov es, ax
		
		lea esi, ds:virt1str
		mov edi, 300000h 
		mov cx, virt1strsize
		copy_1_virtstr:
			movsb es:[edi], ds:[esi]
		loop copy_1_virtstr
		
		lea esi, ds:virt2str
		mov edi, 301000h 
		mov cx, virt2strsize
		copy_2_virtstr:
			movsb es:[edi], ds:[esi]
		loop copy_2_virtstr
		
		pop es
		
		ret
	prepareVirtualAddressStrings endp
	
	printVirtualAddressStrings proc near
		push ds
		
		mov ax, 0C0h
		mov ds, ax
		
		push 0h
		push 17h
		push virt1strsize
		pushd 30000000h
		call printProtectedVGA
		
		push virt1strsize
		push 17h
		push virt2strsize
		pushd 40000000h
		call printProtectedVGA
		
		pop ds
		
		ret
	printVirtualAddressStrings endp
	
	testResetVirtualAddress proc near
		push ds
		
		push 0h
		pushd 40000000h
		pushd 300000h
		call setVirtualMemory
		
		mov ax, 0C0h
		mov ds, ax
		
		push 0h
		push 16h
		push virt1strsize
		pushd 40000000h
		call printProtectedVGA
		
		pop ds
		
		ret
	testResetVirtualAddress endp
	
	; word pde number(0,1,2...)
	; doubleword virtualAddr
	; doubleword physicalAddr
	; eax return error code 
	; 0 - no error
	; 1 - not enough space for table
	setVirtualMemory proc near
		push bp
		mov bp, sp
		push ebx
		push ecx
		push edx
		push es
		
		mov ax, 0B8h
		mov es, ax
		
		mov ax, [bp+12] ; pde number
		push ax 
		mov eax, [bp+8] ; virtual address
		push eax
		call ptePhysicalAddr
		
		cmp eax, 0h ; no error
		je already_allocate ; already had table for pte
			mov ecx, ebx
			call allocatePTE ; eax - error ebx - physical address of new pte
			cmp eax, 1
			je not_enough_pages
			
			or ebx, 7h ; p=1 r/w=1 u/s=1
			mov eax, ebx
			mov ebx, ecx
			mov es:[ebx], eax
			and eax, 0FFFFF000h
			
			mov ebx, [bp+8] ; virtual address
			and ebx, 3FF000h ; get pte
			shr ebx, 10 ; pte number * 4 bytes = offset
			add ebx, eax
		already_allocate:
		
		
		push ebx
		mov eax, [bp+4] ; physical address
		or eax, 7h
		push eax
		mov eax, [bp+8]
		push eax
		call setPTEEntryValue
		
		no_error:
			xor eax, eax
			jmp end_error_handling
		not_enough_pages:
			mov eax, 1
		end_error_handling:
		
		pop es
		pop edx
		pop ecx
		pop ebx
		pop bp
		
		ret 10
	setVirtualMemory endp
	
	; word pde number(0,1,2...)
	; doubleword virtualAddr
	; ret
	; eax - error code(0 - no error; 1 - not present)
	; ebx - eax=0 - pte physical address; eax=1 - pde offset from paging segment
	ptePhysicalAddr proc near
		push bp
		mov bp, sp
		push es
		
		mov ax, 0B8h
		mov es, ax
		
		xor ebx, ebx
		mov bx, [bp+8] ; pde number
		shl ebx, 12 ; pde null entry address
		mov eax, [bp+4] ; virtual address
		and eax, 0FFC00000h ; save first 10 bit
		shr eax, 20 ; pde number entry * 4 bytes = offset
		add ebx, eax
		mov eax, es:[ebx]
		test eax, 1h ; P == 1
		jz not_present ; pte present
			and eax, 0FFFFF000h
			mov ebx, eax
			
			mov eax, [bp+4] ; virtual address
			and eax, 3FF000h ; get pte
			shr eax, 10 ; pte number * 4 bytes = offset

			add ebx, eax ; physical address pte entry
			xor eax, eax
			
			jmp end_search_pte
		not_present:
			mov eax, 1h
		end_search_pte:
		
		pop es
		pop bp
		
		ret 6
	ptePhysicalAddr endp
	
	; ret 
	; eax - error code(0 - no error; 1 - not enough space for pte)
	; ebx - pte physical address
	allocatePTE proc near
		push ds
		
		mov ax, 10h
		mov ds, ax
		
		cli
		in al,70h
		or al,80h
		out 70h,al
		
		; allocate memory for table
		mov ebx, ds:usedptenumber
		add ebx, maxpdenumber
		; check enough space for new pte
		push 0B8h
		call segmentSize
		shr eax, 12 ; max number of tables
		cmp ebx, eax
		jae not_enough_space_for_pte ; error state
			shl ebx, 12
			
			push 0B8h
			call segmentBaseAddress
			
			add ebx, eax ; physical address of new pte
			
			; add to use page counter
			mov eax, ds:usedptenumber
			inc eax
			mov ds:usedptenumber, eax
		end_pte_allocate:
		
		in al,70h
		and al,07fh
		out 70h,al
		sti
		
		no_pte_alloc_error:
			xor eax, eax
			jmp end_virtual_error_handling
		not_enough_space_for_pte:
			mov eax, 1h
		end_virtual_error_handling:
		
		pop ds
		
		ret
	allocatePTE endp
	
	; double word - physical pte addr
	; double word - pte value
	; double word - virtual address(for clear cache)
	setPTEEntryValue proc near
		push bp
		mov bp, sp
		push eax
		push ebx
		push ecx
		push es
		
		mov ax, 0C0h
		mov es, ax
		
		mov eax, [bp+8]
		mov ebx, [bp+12]
		mov ecx, es:[ebx]
		mov es:[ebx], eax
		and ecx, 21h ; P = 1, A = 1 - entry cached in TLB
		cmp ecx, 21h 
		jne end_clear_tlb
			mov ebx, [bp+4]
			invlpg es:[ebx]
		end_clear_tlb:
		
		pop es
		pop ecx
		pop ebx
		pop eax
		pop bp
		
		ret 12
	setPTEEntryValue endp
	
	lastvirtualaddr dd maxpdenumber dup(0)
	; word - pde number(0,1,2...)
	; word - page count
	; ret 
	; eax - error code(0 - no error; 1 - not enough virtual memory; 2 - not enough space for pte)
	; ebx - virtual address
	allocateVirtualMemory proc near
		push bp
		mov bp, sp
		push ecx
		push edx
		push esi
		push ds
		push es
		
		mov ax, 10h
		mov ds, ax
		mov ax, 0B8h
		mov es, ax
		
		xor ebx, ebx
		mov bx, [bp+6] ; pde number
		shl ebx, 2
		mov eax, ds:lastvirtualaddr[ebx] ; last virtual address for given pde
		shr eax, 12
		mov edx, eax
		
		mov ebx, 0FFFFFFh
		sub ebx, eax ; rest of pages
		xor eax, eax
		mov ax, [bp+4] ; page count
		cmp ebx, eax
		jb no_virtual_pages_va
			mov cx, ax
			init_ptes_loop:
				inc edx 
				shl edx, 12 ; next virtual address
				
				mov ax, [bp+6] ; pde number
				push ax 
				push edx
				call ptePhysicalAddr
				
				cmp eax, 0h ; no error
				je already_allocate_va ; already had table for pte
					mov esi, ebx ; offset from paging segment for pde entry
					call allocatePTE ; eax - error ebx - physical address of new pte
					cmp eax, 1
					je not_enough_space_for_pte_va
					
					or ebx, 7h ; p=1 r/w=1 u/s=1
					mov es:[esi], ebx
					and ebx, 0FFFFF000h
					
					mov eax, edx ; virtual address
					and eax, 3FF000h ; get pte
					shr eax, 10 ; pte number * 4 bytes = offset
					add ebx, eax
				already_allocate_va:
				
				push ebx
				pushd 0FFFFFFFEh
				push edx
				call setPTEEntryValue
				
				shr edx, 12
			loop init_ptes_loop
			
			shl edx, 12
			xor ebx, ebx
			mov bx, [bp+6] ; pde number
			shl ebx, 2
			mov ds:lastvirtualaddr[ebx], edx
			
			xor eax, eax
			mov ax, [bp+4]
			dec ax
			shl eax, 12
			sub edx, eax
			mov ebx, edx
			
		no_virtual_error_va:
			xor eax, eax
			jmp end_virtual_error_handling_va
		no_virtual_pages_va:
			mov eax, 1h
			jmp end_virtual_error_handling_va
		not_enough_space_for_pte_va:
			mov eax, 2h
		end_virtual_error_handling_va:
		
		pop es
		pop ds
		pop esi
		pop edx
		pop ecx
		pop bp
		
		ret 4
	allocateVirtualMemory endp
	
	physicalAddrPageStart = 400000h
	maxPhysicalPages = 1C00h
	physicalpagesused dw 0h
	
	; word - page number
	; ret:
	; eax - error code (0-no error, 1-not enough memory)
	; ebx - physical address of 4KByte page
	allocatePhysicalMemory proc near
		push bp
		mov bp, sp
		push ds	
		
		mov ax, 10h
		mov ds, ax
		xor eax, eax
		
		mov ax, ds:physicalpagesused
		add ax, [bp+4] ; add number of pages
		cmp ax, maxPhysicalPages
		jae no_physical_pages
			mov ds:physicalpagesused, ax
			
			sub ax, [bp+4]
			mov ebx, physicalAddrPageStart
			shl eax, 12
			add ebx, eax
			
			xor eax, eax
			jmp end_physical_error_handling
		no_physical_pages:
			mov eax, 1h
		end_physical_error_handling:

		pop ds
		pop bp
		
		ret 2
	allocatePhysicalMemory endp
	
	; word - segment num
	; ret ax - ptr to descriptor
	getPointerToSegmentDescriptor proc near
		push bp
		mov bp, sp
		push bx
		push ds
		
		mov ax, 10h
		mov ds, ax
		
		mov ax, [bp+4]
		shr ax, 3 ; segment number
		mov bx, sizeof segmentdescriptor
		mul bx ; segment descriptor offset
		lea bx, ds:nulldesc
		add ax, bx
		
		pop ds
		pop bx
		pop bp
		
		ret 2
	getPointerToSegmentDescriptor endp
	
	; word - segment num
	; ret eax - size
	segmentSize proc near
		push bp
		mov bp, sp
		push ebx
		xor ebx, ebx
		push si
		push ds
		
		mov ax, 10h
		mov ds, ax
		
		push [bp+4]
		call getPointerToSegmentDescriptor
		
		mov si, ax ; pointer to descriptor
		mov bx, ds:[si].segmentdescriptor.params
		
		; most significant bits
		and bx, 0F00h
		xor eax, eax
		mov ax, bx
		shl eax, 8
		; other bits
		mov bx, ds:[si].segmentdescriptor.seglimit1
		or eax, ebx
		
		; check G == 1
		mov bx, ds:[si].segmentdescriptor.params
		test bx, 8000h
		jz end_of_calc
		; G = 1
			shl eax, 12
			or eax, 0FFFh
		end_of_calc:
		
		inc eax
		
		pop ds
		pop si
		pop ebx
		pop bp
		ret 2
	segmentSize endp
	
	; word - segment num
	; ret eax - base address
	segmentBaseAddress proc near
		push bp
		mov bp, sp
		push bx
		push ds
		
		mov ax, 10h
		mov ds, ax
		
		push [bp+4]
		call getPointerToSegmentDescriptor
		
		mov bx, ax
		xor eax, eax
		mov al, ds:[bx].segmentdescriptor.segbase3
		shl eax, 8
		mov al, ds:[bx].segmentdescriptor.segbase2
		shl eax, 16
		mov ax, ds:[bx].segmentdescriptor.segbase1
		
		pop ds
		pop bx
		pop bp
		ret 2
	segmentBaseAddress endp
	
	
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
	
	disablePIC proc near
		push ax
		
		mov al, 0FFh
		out 21h, al
		out 0A1h, al
		
		pop ax
		
		ret
	disablePIC endp
	
	enablePIC proc near
		push ax
		
		mov al, 0h
		out 21h, al
		out 0A1h, al
		
		pop ax
		
		ret
	enablePIC endp
	
	apicioregsel dd 0FEC00000h
	APIC_WIN_OFFSET = 10h
	START_ADDRESS_OI_TBL = 10h
	END_ADDRESS_OI_TBL = 40h
	IO_NUMBER = (END_ADDRESS_OI_TBL - START_ADDRESS_OI_TBL - 2) / 2
	initAPIC proc near
		push ds
		push es
		
		mov ax, 10h
		mov ds, ax
		mov ax, 0C0h
		mov es, ax
		
		mov ebx, ds:apicioregsel
		
		push 0h
		push ebx
		push ebx
		call setVirtualMemory
				
		cli
		mov eax, 21h
		mov esi, START_ADDRESS_OI_TBL + 2
		mov cx, IO_NUMBER - 1
		init_redirect_tbl_reg:
			mov es:[ebx], esi
			mov edx, es:[ebx+APIC_WIN_OFFSET]
			mov es:[ebx+APIC_WIN_OFFSET], eax
			
			inc esi
			mov es:[ebx], esi
			mov edx, es:[ebx+APIC_WIN_OFFSET]
			xor edx, edx
			mov es:[ebx+APIC_WIN_OFFSET], edx
			
			inc eax
			inc esi
			cmp esi, 14h
			jne end_init_timer
				mov eax, 20h
				mov es:[ebx], esi
				mov edx, es:[ebx+APIC_WIN_OFFSET]
				mov es:[ebx+APIC_WIN_OFFSET], eax
				
				inc esi
				mov es:[ebx], esi
				mov edx, es:[ebx+APIC_WIN_OFFSET]
				xor edx, edx
				mov es:[ebx+APIC_WIN_OFFSET], edx
				
				mov eax, 23h
				inc esi
			end_init_timer:
		loop init_redirect_tbl_reg
		sti
		pop es
		pop ds
		
		ret
	initAPIC endp
	
	; stack:
	; no checks out of buff, out vga buff
	; 0 <= col < 80 0 <= row < 25(bochsrc default)
	; col, row, buff_size, buff_ptr(double word), ret_addr
	printProtectedVGA proc near
		push ebp
		xor ebp, ebp
		mov bp, sp
		push es
		push ax
		push bx
		push cx
		push dx
		push esi
		
		xor esi, esi
		
		mov ax, [bp+12] ; row
		mov bx, 50h
		mul bx
		mov bx, [bp+14]
		add bx, ax ; start pointer to vga buffer
		shl bx, 1
		
		mov ax, 20h
		mov es, ax
		mov cx, [bp+10]
		mov dx, cx
		mov ebp, [bp+6] ;pointer to string
		output:
			mov ah, 07Ch ; background and character color
			mov si, dx
			sub si, cx
			mov al, ds:[ebp+esi]
			
			shl si, 1
			mov es:[bx+si], ax ; esi not need
		loop output
		
		pop esi
		pop dx
		pop cx
		pop bx
		pop ax
		pop es
		pop ebp
		
		ret 10
	printProtectedVGA endp
	
	printRealModeVGA proc near
		push ebp
		xor ebp, ebp
		mov bp, sp
		push es
		push ax
		push bx
		push cx
		push dx
		push esi
		
		xor esi, esi
		
		mov ax, [bp+12] ; row
		mov bx, 50h
		mul bx
		mov bx, [bp+14]
		add bx, ax ; start pointer to vga buffer
		shl bx, 1
		
		mov ax, 0B800h
		mov es, ax
		mov cx, [bp+10]
		mov dx, cx
		mov ebp, [bp+6] ;pointer to string
		output:
			mov ah, 07Ch ; background and character color
			mov si, dx
			sub si, cx
			mov al, ds:[ebp+esi]
			
			shl si, 1
			mov es:[bx+si], ax ; esi not need
		loop output
		
		pop esi
		pop dx
		pop cx
		pop bx
		pop ax
		pop es
		pop ebp
		
		ret 10
	printRealModeVGA endp
	
	printIntStr proc near
		push ebp
		mov bp, sp
		push ds
		push eax
		push ebx
		
		mov ax, 10h
		mov ds, ax
		
		xor eax, eax
		mov ax, [bp+6]
		lea bx, ds:inthndlstr
		add bx, inthndlstrsize - 2h
		
		push eax
		push bx
		push 2h
		lea ebx, ds:itoahcallptr
		call far ptr [ebx]

		push 4fh - inthndlstrsize
		push [bp+6]
		mov ax, inthndlstrsize
		push ax
		lea ebp, inthndlstr
		push ebp
		call printProtectedVGA
		
		pop ebx
		pop eax
		pop ds
		pop ebp
		
		ret 2
	printIntStr endp
	
	printPicStr proc near
		push ebp
		mov bp, sp
		push ds
		push eax
		push ebx
		
		mov ax, 10h
		mov ds, ax

		xor eax, eax
		mov ax, [bp+6]
		lea bx, ds:pichndlstr
		add bx, pichndlstrsize - 2h
		
		push eax
		push bx
		push 2h
		lea ebx, ds:itoahcallptr
		call far ptr [ebx]
		
		push 28h
		push [bp+6]
		mov ax, pichndlstrsize
		push ax
		lea ebp, pichndlstr
		push ebp
		call printProtectedVGA
		
		pop ebx
		pop eax
		pop ds
		pop ebp
		
		ret 2
	printPicStr endp
	
	; word - pages count
	; ret 
	; eax - error code(0 - no error; 1 - not enough virtual memory; 2 - not enough space for pte)
	; ebx - offset from 10000000h
	allocateUserMode proc far
		push bp
		mov bp, sp
		
		mov eax, cr3
		and ax, 0F000h
		shr ax, 12
		push ax
		push [bp+6]
		call allocateVirtualMemory
		
		sub ebx, 10000000h ; we must take segment base address
		
		pop bp
		retf 2
	allocateUserMode endp
	
	printMultiplyResultStr proc far
		push ebp
		mov bp, sp
		push ds
		push eax
		push ebx
		
		mov ax, 10h
		mov ds, ax
		
		xor eax, eax
		mov ax, [bp+6]
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
		lea ebp, multiplyresultstr
		push ebp
		call printProtectedVGA
		
		pop ebx
		pop eax
		pop ds
		pop ebp
		
		retf 2
	printMultiplyResultStr endp
	
	; word task id, ptr to string, size string
	printUserModeCounter proc far
		push ebp
		mov bp, sp
		push ax
		push bx
		push cx
		
		mov ax, [bp+8] ; size
		mov bx, [bp+10] ; string ptr
		mov cx, [bp+12] ; task id
		
		add cx, 0Ch
		push 0h
		push cx
		push ax
		pushd bx
		call printProtectedVGA
		
		pop cx
		pop bx
		pop ax
		pop ebp
		
		retf 6
	printUserModeCounter endp
	
	printInt8Counter proc near
		push ebp
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
		lea ebp, int8outputstr
		push ebp
		call printProtectedVGA
		
		pop ebx
		pop eax
		pop ds
		pop ebp
		
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
		push ebp
		push ax
		
		push 4fh - intstubhndlstrsize
		push 04h
		mov ax, intstubhndlstrsize
		push ax
		lea ebp, intstubhndlstr
		push ebp
		call printProtectedVGA
		
		pop ax
		pop ebp
		
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
	
	pagefaultstr db 'Page fault error 0x0000'
	pagefaultstrsize = $ - pagefaultstr
	printPageFault proc near
		push bp
		mov bp, sp
		push ds
		push eax
		push ebx
		
		mov ax, 10h
		mov ds, ax
		
		xor eax, eax
		mov ax, [bp+4] ; error code
		lea bx, ds:pagefaultstr
		add bx, pagefaultstrsize - 4h ; shift '0x'
		
		push eax
		push bx
		push 4h
		lea ebx, ds:itoahcallptr
		call far ptr [ebx]
		
		push 4fh - pagefaultstrsize
		push 0Eh
		push pagefaultstrsize
		lea ebx, ds:pagefaultstr
		push ebx
		call printProtectedVGA
		
		pop ebx
		pop eax
		pop ds
		pop bp
		
		ret 2
	printPageFault endp
	
	
	int14hndl proc far
		; STUB_INT_HNDL 0Eh
		push bp
		mov bp, sp
		push eax
		push ebx
		push esi
		push es
		
		mov eax, [bp+2] ; error code
		and eax, 01h
		jnz not_translation_error
			; error with translation
			mov eax, cr3
			and eax, 0F000h
			shr eax, 12 ; pde number
			push ax
			mov eax, cr2 ; virtual address
			push eax
			call ptePhysicalAddr
			
			cmp eax, 0h
			jne pte_not_present ; memory is not allocated
				mov ax, 0C0h
				mov es, ax
				
				mov eax, es:[ebx]
				mov esi, ebx
				cmp eax, 0FFFFFFFEh
				jne not_reserved_value
					push 1h 
					call allocatePhysicalMemory
					
					cmp eax, 0h
					jne not_enough_physical_memory
						or ebx, 7h
						mov es:[esi], ebx
						mov eax, 0
						jmp end_repair
					not_enough_physical_memory:
						mov eax, 4h
						jmp end_repair
				not_reserved_value:
					mov eax, 3h
					jmp end_repair
			pte_not_present:
				mov eax, 2h
				jmp end_repair
		not_translation_error:
			mov eax, 1h
			jmp end_repair
		end_repair:
		
		push ax
		call printPageFault
		
		pop es
		pop esi
		pop ebx
		pop eax
		pop bp
		
		add sp, 2
		
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
		push eax
		push ebx
		push es
		
		push number
		call printPicStr
		; mov al, 20h
		; out 20h, al
		mov ax, 0C0h
		mov es, ax
		mov ebx, 0FEE00000h
		
		xor eax, eax
		mov es:[ebx+0B0h], eax
		
		pop es
		pop ebx
		pop eax
	endm
	
	ptrtomaintss dd 500000h
	ptrtofonetss dd 800000h
	
	; timer
	int32hndl_tss proc far
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
	int32hndl_tss endp
	
	prevss dw 0h
	prevesp dd 0h
	prevss0 dw 0h
	prevesp0 dd 0h
	int32hndl proc far
		push eax
		push ebx
		push ecx
		push edx
		push esi
		push edi
		push ebp
		push ds
		push es
		
		call printInt8Counter
		
		mov ax, 10h
		mov ds, ax
		
		mov eax, esp
		mov dx, ss
		
		mov ebx, ds:prevesp
		mov cx, ds:prevss
		mov ss, cx
		mov esp, ebx
		
		mov ds:prevss, dx
		mov ds:prevesp, eax

		mov ebx, ds:prevesp0
		mov eax, ds:tsssegment.esp0
		mov ds:tsssegment.esp0, ebx
		mov ds:prevesp0, eax
		
		mov bx, ds:prevss0
		mov ax, ds:tsssegment.ss0
		mov ds:tsssegment.ss0, bx
		mov ds:prevss0, ax
		
		mov eax, cr3
		test eax, 1000h
		jz next_pde
			mov eax, 200000h
			jmp end_pde_select
		next_pde:
			mov eax, 201000h
		end_pde_select:
		mov cr3, eax
		
		;mov al, 20h
		;out 20h, al
		
		mov ax, 0C0h
		mov es, ax
		xor eax, eax
		mov ebx, 0FEE00000h
		mov es:[ebx+0B0h], eax
		
		pop es
		pop ds
		pop ebp
		pop edi
		pop esi
		pop edx
		pop ecx
		pop ebx
		pop eax
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
		push eax
		push ebx
		push cx
		push si
		push ds
		push es
		
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
				je tail_equal_head
					mov ds:kbdbufftail, si
					jmp end_save_tail
				tail_equal_head:
					mov si, ds:kbdbufftail
				end_save_tail:
				
				
				in al, KBD_CMD_PORT
				test al, 1h
			loopnz read_kbd_buff
		output_empty:
		
		;mov al, 20h
		;out 20h, al
		mov ax, 0C0h
		mov es, ax
		xor eax, eax
		mov ebx, 0FEE00000h
		mov es:[ebx+0B0h], eax
		
		pop es
		pop ds
		pop si
		pop cx
		pop ebx
		pop eax		
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
		push eax
		push ebx
		push es
		
		push number
		call printPicStr
		;mov al, 20h
		;out 0A0h, al
		;out 20h, al
		mov ax, 0C0h
		mov es, ax
		xor eax, eax
		mov ebx, 0FEE00000h
		mov es:[ebx+0B0h], eax
		
		pop es
		pop ebx
		pop eax
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

	
usermodecodesg SEGMENT PARA USE16 'CODE'
	beginusermodecodesg = $
	
	var1 dw 0Ah
	var2 dw 0Bh
	counter dd 1000h
	
	counterstr db 'I`m main task. My counter = 0x0000'
	counterstrsize = $ - counterstr
	
	mulconformptr dd 0h
	itoahconformptr dd 0h
	callgateprintmultiply dd 0h
	callgateprintcounter dd 0h
	callgatememallocate dd 0h
	
	newpageptr dd 0h
	
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
		
		; output result
		push 1h
		lea ebx, ds:callgatememallocate
		call far ptr [ebx]
		
		mov ds:newpageptr, ebx
		mov ax, 0ACACh
		mov ds:[ebx], ax
		
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
	
fonecodesg SEGMENT PARA USE16 'CODE'
	beginfonecodesg = $
	
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
	
	foneStart proc far
		mov ax, 33h
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
		mov ds:[bx+1eh], offset scanCodeHndlTab
		mov ds:[bx+1ch], offset scanCodeHndlBackspace
		                  
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
	
	scanCodeHndlTab proc near 
		push cx
		
		mov cx, 4
		tab_loop:
			push ' '
			push ' '
			call printChar
		loop tab_loop
		
		pop cx
		ret 2
	scanCodeHndlTab endp
	
	scanCodeHndlBackspace proc near 
		push ax
		
		mov ax, -1
		
		push ax
		lea ebx, ds:foneaddcolcallgate
		call far ptr [ebx]
		
		push ' '
		push ' '
		call printChar
		
		push ax
		lea ebx, ds:foneaddcolcallgate
		call far ptr [ebx]
		
		pop ax
		ret 2
	scanCodeHndlBackspace endp
	
	fonecodesgsize = $ - beginfonecodesg
fonecodesg ends

END