SECTION .text
global main
main:
	mov	ax, 0x10
	int	0x10

	mov	ax, 0xA000
	mov es, ax
	

	;mov bx, 161
	;mov ax, 150
	;call print_dot
;
	;mov bx, 161
	;mov ax, 150
	;call read_dot
	;mov bx, ax
	;mov ax, 0
	;call print_dot
	;mov ax, 0x09
	;mov bx, 0x57
	;mov [es:bx], al
	;mov ax, 1010b
	;call set_pane
	;mov bx, 0x57 
	;mov cl, [es: bx]
	;mov dx, 3CEh       ; address of sequencer data register
	;mov ah, byte 0x00
	;mov al, 8 
	;out dx, ax
	;mov bx, 0x07
	;mov [es:bx], byte cl
	;call await_input
	;call exit

	;task 1
	mov ax, 1001b
	call set_pane 

	mov al, 0x09
	mov bx, 0x57
	mov [es:bx], al

	;task 2
	mov ax, 1010b
	call set_pane

	mov bx, 303
	mov ax, 150
	call print_dot
	
	;task 3
	mov bx, 50
	mov ax, 50
	mov cx, 100
	call print_horizontal_line
	
	
	;task 4
	mov ax, 50
	mov bx, 75
	mov cx, 75
	call print_vertical_line


	;task 5
	mov ax, 1001b
	call set_pane

	mov ax, 330
	mov bx, 150
	mov cx, 500
	mov dx, 200
	call draw_slope


	mov ax, 356
	mov bx, 120
	mov cx, 472
	mov dx, 257
	call draw_slope


	mov ax, 342
	mov bx, 267
	mov cx, 500
	mov dx, 120
	call draw_slope

	mov ax, 1011b
	call set_pane	

	mov bx, 420
	mov ax, 200
	mov cx, 50
	call drawcircle

	mov ax, 1110b
	call set_pane
	
	mov bx, 421
	mov ax, 155
	call fill

	mov ax, 1010b
	call set_pane
	
	mov bx, 420
	mov ax, 234
	call fill

	call await_input
	call exit

write_ega:
	mov cx, ax
	mov dl, [es:bx]
	mov dx, 3CEh       ; address of sequencer data register
	mov ah, cl
	mov al, 8 
	out dx, ax
	mov [es:bx], ah;
	ret

; bx = X, ax = Y
print_dot:

		mov dx, 80
		mul dx

		mov cl, bl
		and cl, 0x7
		; cl = X % 8
		shr bx, 3
		; bx = X / 8
		add ax, bx

		mov bx, 128
		shr bx, cl
		mov cl, bl
		mov bx, ax
		mov al, cl
		call write_ega
		ret
; bx - X, ax - Y
read_dot:
	push ax
	mov dx, 3CEh       ; address of sequencer data register
	mov ah, 001000b
	mov al, 5 
	out dx, ax

	mov dx, 3CEh       ; address of sequencer data register
	mov ah, 0000b
	mov al, 2 
	out dx, ax
	
	mov dx, 3CEh       ; address of sequencer data register
	mov ah, 1111b
	mov al, 7 
	out dx, ax
	pop ax

	mov dx, 80
	mul dx
	mov cl, bl
	and cl, 0x7
	; cl = X % 8
	shr bx, 3
	; bx = X / 8
	add ax, bx
	mov bx, 128
	shr bx, cl
	mov cl, bl
	mov bx, ax
	mov ax, [es:bx]
	and al, cl
	push ax

	mov dx, 3CEh       ; address of sequencer data register
	mov ah, 0x00
	mov al, 5 
	out dx, ax

	pop ax
	ret
;  bx = X0, ax = Y0, cx = X1
print_horizontal_line:
		cmp bx, cx
		jle .compute
		xchg bx, cx
	.compute:
		push ax
		push bx
		push cx
		call print_dot
		pop cx
		pop bx
		pop ax

		cmp bx, cx
		jge .end
		add bx, 1
		jmp .compute
	.end:
		ret

;   bx = X0, ax = Y0, cx = Y1
print_vertical_line:
		cmp ax, cx
		jle .compute
		; ¥á«¨ bx (x0) > cx (x1),
		; ¬¥­ï¥¬ cx, bx ¬¥áâ ¬¨
		xchg ax, cx
	.compute:
		push ax
		push bx
		push cx
		call print_dot
		pop cx
		pop bx
		pop ax

		cmp ax, cx
		jge .end
		add ax, 1
		jmp .compute
	.end:
		ret

await_input:
	xor ax, ax        ; ¡«®ª¨àãîé¥¥ çâ¥­¨¥ á¨¬¢®«  á ª« ¢¨ âãàë
	int 0x16
	ret

exit:
	mov ax, 0x4c00	; § ¢¥àè¥­¨¥ ¯à¨«®¦¥­¨ï „Ž‘
	int 0x21		; á ª®¤®¬ ¢®§¢à â  0

	ret				; ­¥®¡ï§ â¥«ì­®

set_pane:
	mov bl, al
	
	mov dx, 3C4h       ; address of sequencer address register
	mov al, 2h         ; index of map mask register
	out dx, al

	mov dx, 3C5h       ; address of sequencer data register
	mov al, bl  		
	out dx, al
	ret



more_horizontal:
   rol dx, 2

   mov bp, cx
   rol bp, 2
   sub bp, dx
   neg bp 

   mov di, dx
   sub di, cx

hor_loop:
      
	push ax
	push bx
	pop  ax
	pop  bx
	push bx
	push ax 
	push cx
	push si
	push dx
	call print_dot
	pop dx
	pop si
	pop cx
	pop bx
	pop ax
    
	cmp di, 0
	jl hor_jl
	inc bx
	add di, bp
	jmp hor_loop_end
hor_jl:
	add di, dx
hor_loop_end:      
    add ax, si
	dec cx
	jnz hor_loop
	ret	


more_vertical:
   rol cx, 2
   
   mov bp, dx
   rol bp, 2
   sub bp, cx
   neg bp 
   
   mov di, cx
   sub di, dx

vert_loop:
      
	push ax
	push bx
	pop  ax
	pop  bx
	push bx
	push ax 
	push cx
	push si
	push dx
	call print_dot
	pop dx
	pop si
	pop cx
	pop bx
	pop ax


	cmp di, 0
	jl vert_jl
	add ax, si

	add di, bp
	jmp vert_loop_end
vert_jl:
	add di, cx

vert_loop_end:      
    inc bx
    dec dx
	jnz vert_loop
	ret	

draw_slope:
;X0 - ax, Y0 - bx, X1 - cx, Y1 - dx
	mov si, 1
	cmp bx, dx
	jle do_not_swap
	push bx
	mov bx, dx
	pop dx
	push ax
	mov ax, cx
	pop cx
do_not_swap:
	sub cx, ax
	sub dx, bx

	cmp cx, 0
	jge going_left
	neg cx
	neg si
going_left:
    cmp cx, dx
    jl vert
    call more_horizontal
    ret
vert:    
	call more_vertical
	ret



drawcircle: ;x0 - bx, y0 - ax, radius - cx)
    mov dx, cx 
    dec dx ; x
    mov si, 0; y
    
    mov di, 1
    sub di, cx; err

drawcircle_loop:
    call drawcircle_8_points
    inc si
    cmp di, 0
    jg drawcircle_positive_err
    mov bp, si
    rol bp, 1
    add di, bp
    inc di
    jmp drawcircle_loop_end
drawcircle_positive_err:
    dec dx
    mov bp, si
    rol bp, 1
    add di, bp
    inc di
    mov bp, dx
    rol bp, 1
    sub di, bp
drawcircle_loop_end:        
    cmp dx, si
    jge drawcircle_loop
    ret

drawcircle_8_points:
    push ax
    push bx
    push cx
    push si
    push dx
    add bx, dx
    add ax, si
    call print_dot
    pop dx
    pop si
    pop cx
    pop bx
    pop ax

    push ax
    push bx
    push cx
    push si
    push dx
    add bx, si
    add ax, dx
    call print_dot
    pop dx
    pop si
    pop cx
    pop bx
    pop ax

    push ax
    push bx
    push cx
    push si
    push dx
    sub bx, si
    add ax, dx
    call print_dot
    pop dx
    pop si
    pop cx
    pop bx
    pop ax

    push ax
    push bx
    push cx
    push si
    push dx
    sub bx, dx
    add ax, si
    call print_dot
    pop dx
    pop si
    pop cx
    pop bx
    pop ax

    push ax
    push bx
    push cx
    push si
    push dx
    add bx, si
    sub ax, dx
    call print_dot
    pop dx
    pop si
    pop cx
    pop bx
    pop ax

    push ax
    push bx
    push cx
    push si
    push dx
    add bx, dx
    sub ax, si
    call print_dot
    pop dx
    pop si
    pop cx
    pop bx
    pop ax

    push ax
    push bx
    push cx
    push si
    push dx
    sub bx, dx
    sub ax, si
    call print_dot
    pop dx
    pop si
    pop cx
    pop bx
    pop ax

    push ax
    push bx
    push cx
    push si
    push dx
    sub bx, si
    sub ax, dx
    call print_dot
    pop dx
    pop si
    pop cx
    pop bx
    pop ax
    ret

; bx - X, ax - Y
fill:
	cmp bx, 639	
	jge fill_end
	cmp ax, 349
	jge fill_end
	cmp bx, 0	
	jl fill_end
	cmp ax, 0
	jl fill_end
	
	push bx
	push ax
	call read_dot
	cmp al, 0
	je fill_free_end
	pop ax
	pop bx

	push bx
	push ax
	call print_dot
	pop ax
	pop bx

	push bx
	push ax
	dec ax
	call fill
	pop ax
	pop bx

	push bx
	push ax
	inc bx
	call fill
	pop ax
	pop bx
	
	push bx
	push ax
	dec bx
	call fill
	pop ax
	pop bx
	
	push bx
	push ax
	inc ax
	call fill
	pop ax
	pop bx

	
	ret
fill_free_end:
	pop bx
	pop ax
fill_end:
	ret
