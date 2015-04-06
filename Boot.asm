.386p

codesg SEGMENT PARA USE16 'CODE'

	ASSUME CS:codesg

	ORG 100h

main:
	
	jmp start
	
	sdfsdf	dw		0
	
start:

	mov [sdfsdf],ax

codesg ENDS

END main