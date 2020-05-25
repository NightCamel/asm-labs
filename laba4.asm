.model small
.stack 256h

.data
    ; add your data here! 
    miny equ 1
    ;Colors
    land_color equ 00100000b ;earth color green
    player_color equ 00101101b  ;purple on green
    player_color_nl equ 11101101b ;purple on grey
    enemy_color equ 00001100b     ;red
    path_color equ 11100000b ;grey
    score_color equ 00001000b ;temno seriy
    gameovercol1 equ 11100001b
    gameovercol2 equ 11100100b
                           
    ; Клавиши
    upkey		=	48h		; скан-код клавиши вверх
    downkey		=	50h		; скан-код клавиши вниз
    leftkey		=	4Bh		; скан-код клавиши влево
    rightkey	=	4Dh		; скан-код клавиши вправо
    escep = 01h ;ESC 
    
    ; структура координат врагов
	x	dw	80		; x координата врага
	y	db	12		; y координата врага
	d_x	db	2		; направление движения врага по x (1, -1)
	d_y	db	1		; направление движения врага по y (1, -1)
	
	pts dw 0
	tempDI dw 0
	mGameOver db 'G',gameovercol1, ' ',gameovercol2, 'A',gameovercol2, ' ',gameovercol2, 'M',gameovercol1, ' ',gameovercol2, 'E',gameovercol2
	          db ' ',gameovercol1, ' ',gameovercol2, 'O',gameovercol1, ' ',gameovercol2, 'V',gameovercol2, ' ',gameovercol2, 'E',gameovercol1, ' ',gameovercol2, 'R',gameovercol2
	          db ' ',gameovercol2, '!',gameovercol1
	mYouWin   db 'Y',gameovercol1, ' ',gameovercol2, 'O',gameovercol2, ' ',gameovercol2, 'U',gameovercol1, ' ',gameovercol2
	          db ' ',gameovercol2, 'W',gameovercol1, ' ',gameovercol2, 'I',gameovercol2, ' ',gameovercol2, 'N',gameovercol1, ' ',gameovercol2, '!',gameovercol2                  
                          
    granica_len equ 160  
     
.code

; Записать цвет точки с координатами x=dx, y=bx в al
; Меняет di!    
GetPixel	proc
		call	PixelAddr	; получаем адрес точки 
		mov	ax, es:[di]	; читаем точку
		ret
GetPixel	endp

; Вывести точку цветом al по координатам x=dx, y=bx
; Меняет di!
PutPixel	proc
		call	PixelAddr	; получаем адрес точки
		stosw			; ставим точку  
		ret
PutPixel	endp

; Записать в di адрес точки с координатами x=dx, y=bx
PixelAddr	proc
		push bx
		push cx
		xor	bh,bh
		xor cx, cx 
		mov cx, bx
		xor bx, bx
		PixelSum:
		    add bx, 160
		    loop PixelSum
		mov di, bx 
		add	di, dx
		pop cx
		pop	bx
		ret
PixelAddr	endp

ShowScore	proc 
    push ax
    push bx
    push cx
    push dx
		mov	ax,[pts]
		add	ax,bp
		xor cx, cx
		mov	cl,0
		mov	bx,10		; система счисления
	nextdigit:
		xor dx, dx			; dx = 0 
		div	bx		; ax = dx:ax/систему_счисления, dx = остаток
		push dx		
		inc	cx		; увеличиваем кол-во цифр
		or	ax,ax
		jnz	short nextdigit	; переход, если частное ещё не равно 0

		mov ah, 0Eh
		mov al, 0Dh		; символ перехода к началу строки
		int 10h
		mov	bl, score_color
	outdigit:
		pop	ax		; извлекаем цифру из стека
		mov	ah,0Eh
		add	al,'0'		; преобразуем её в символ
		int	10h		; вывод цифры
		loop	outdigit	; повторяем цикл вывода символов
	pop dx
    pop cx
    pop bx
    pop ax
		ret
  ShowScore	endp

PauseTimer proc
    push dx
    push ax
    push cx
    mov cx, 0
    mov dx, 45000 ;dx = 45000 microsec
    mov ah, 86h
    int 15h
    pop cx
    pop ax
    pop dx
    ret
  PauseTimer endp

; Очистить буфер клавиатуры
; Меняет ax, возвращая скан-код последней клавиши, если в буфере была хоть одна!
ClearKbd	proc 
	next:
		mov	ah, 01h
		int	16h		; проверяем наличие клавиши в буфере
		jz	short nokeys	; нет, выходим
		xor	ah,ah
		int	16h		; есть, сбрасываем её
		mov ch, ah 
		jmp	short next	; повторяем цикл
	nokeys:
		ret
ClearKbd	endp
                                                                               
                                                                               
;-------------------------------------------------------------------------------
; Залить все цвета al цветом ah (начиная с позиции di)
; Увеличить bp на кол-во залитых точек цветом landcol (если score<>0 и extmode=0)
; Регистр dx изменяется!
FloodFill	proc
        push dx
        push bx
		push di  ;zapomnili pervoe znazhenie
		push di
		xor bx, bx
		mov bx, sp
		
		mov al, ' '
		mov ah, land_color
	    cld       ;zakrasili rassmatrivaemie element
	    stosw
        xor cx, cx
        mov cx, 1

	proverka:
	    pop di	        
	    mov dx, es:[di+2] ;proverka pravogo
	    cmp dh, land_color
	    jz short sled1
	    xor dx, dx
	    mov dx, di
	    add di, 2
	    push di
	    cld       ;zakrasili rassmatrivaemie element
	    stosw
	    mov di, dx
	    inc cx
	    inc bp
	    
	sled1:    
	    mov dx, es:[di+160] ;proverka nizshnego
	    cmp dh, land_color
	    jz short sled2
	    xor dx, dx
	    mov dx, di
	    add di, 160
	    push di
	    inc cx
	    cld       ;zakrasili rassmatrivaemie element
	    stosw
	    mov di, dx
	            inc bp
	
	sled2:
	    mov dx, es:[di-2] ;proverka levogo
	    cmp dh, land_color
	    jz short sled3
	    xor dx, dx
	    mov dx, di
	    sub di, 2
	    push di
	    inc cx
	    cld       ;zakrasili rassmatrivaemie element
	    stosw
	    mov di, dx
	            inc bp
	    
	sled3:
	    mov dx, es:[di-160] ;proverka verhnego
	    cmp dh, land_color
	    jz short krainee
	    inc bp
	    xor dx, dx
	    mov dx, di
	    sub di, 160
	    push di
	    inc cx
	    cld       ;zakrasili rassmatrivaemie element
	    stosw
	    mov di, dx
	            inc bp  
	krainee:
	    loop proverka
    konec:
        mov sp, bx
	    pop di
	    pop di
	    pop bx 
	    pop dx
		ret
FloodFill	endp

; Заменить все цвета на экране al на ah
; Увеличить bp на кол-во залитых точек цветом landcol (если score<>0 и extmode<>0)
; Изменяет cx и di!
ChangeColor	proc
    push bx
    push cx
    push di
        mov di, 326
        mov cx, 3400
        mov al, path_color
        mov ah, land_color
        xor bx, bx
    repeat:
        repne scasb
        jne short ccexit  ;esli cx=0 to v ccexit
        dec di
        mov al, ' '
        cld
        stosw
        mov al, path_color
        sub di, 2
        inc bp
        jmp short repeat        
	ccexit:
	pop di
	pop cx
	pop bx
		ret
ChangeColor	endp

; Uznaet tochku vnutri figuri
; Изменяет cx и di!
FindPixel	proc
    push cx
    push ax
    push bx
        xor cx, cx 
        xor bx, bx
        mov cl, 160
        mov di, [tempDI]
        
        xor ax, ax
        mov ax, di
        div cl     ;bl ostatok di
        mov bl, ah
        mov ch, al     ;ch celoe di
        
        xor ax, ax
        mov ax, si
        div cl      ;bh ostatok si
        mov bh, ah
        mov cl, al   ;cl celoe si
        
        cmp bl, bh
        ja short di_bolshe
        cmp bl, bh
        jb short di_menshe
        jmp short ravno
    di_menshe:
        xor ax, ax
        mov ax, es:[si-2]
        cmp ah, land_color
        je short di_vverh
        xor ax, ax
        mov ax, si     ;vlevo
        sub ax, 2
        mov si, ax
        jmp short nideno
    di_vverh:
        cmp si, di ;esli di nizhe
        jbe short di_vverh_v     
        xor ax, ax
        mov ax, si      ;vverh
        sub ax, 160
        mov si, ax
        jmp short nideno
    di_vverh_v:
        xor ax, ax
        mov ax, si      ;vniz
        add ax, 160
        mov si, ax
        jmp short nideno
         
    di_bolshe:
        xor ax, ax
        mov ax, es:[si+2]
        cmp ah, land_color
        je short di_vniz
        xor ax, ax
        mov ax, si      ;vpravo
        add ax, 2
        mov si, ax
        jmp short nideno
        di_vniz:
        cmp si, di ;esli di nizhe
        jbe short di_vniz_v    
        xor ax, ax
        mov ax, si      ;vverh
        sub ax, 160
        mov si, ax
        jmp short nideno
    di_vniz_v:
        xor ax, ax
        mov ax, si      ;vniz
        add ax, 160
        mov si, ax
        jmp short nideno
        
    ravno:
        cmp si, di
        jb short si_menshe
    si_bolshe:
        xor ax, ax
        mov ax, es:[si-160]
        cmp ah, land_color
        je short di_menshe
        xor ax, ax
        mov ax, si
        sub ax, 160
        mov si, ax
        jmp short nideno
    si_menshe:
        xor ax, ax
        mov ax, es:[si+160]
        cmp ah, land_color
        je short di_raven
        xor ax, ax
        mov ax, si
        add ax, 160
        mov si, ax
        jmp short nideno
    di_raven:
        cmp bl, 80
        jb di_mensh
        xor ax, ax
        mov ax, si      ;vpravo
        add ax, 2
        mov si, ax
        jmp short nideno
    di_mensh:
        xor ax, ax
        mov ax, si     ;vlevo
        sub ax, 2
        mov si, ax
        jmp short nideno            
    nideno:
    mov di, si
    pop bx
    pop ax
	pop cx
		ret
FindPixel	endp

start:
; set segment registers:
    mov ax, @data
    mov ds, ax
    mov es, ax
    ; add your code here
    
    mov ah, 00   ;ustanovka 16 cvetovogo video reshima
    mov al, 03   
    int 10h    
    
    ;Otrisovka granic
    push 0B800h
    pop es 
    mov al, ' '                                      
    mov ah, land_color
    
    mov cx, granica_len/2          ;verh
    sub cx, 2
    mov di, granica_len+2 
    cld
    rep stosw 
     
    mov cx, granica_len/2          ;niz
    sub cx, 2
    mov di, granica_len*23
    add di, 2 
    cld
    rep stosw
    
    mov bx, 1
left:
    inc bx
    mov cx, bx 
    xor dx, dx
    left_pl:
        add dx, granica_len
        loop left_pl 
    add dx, 2
    mov di, dx 
    mov cx, 2
    cld                                                    
    rep stosw                                            
    cmp bx, 23
    jne left
    
    mov bx, 2
right:
    inc bx
    mov cx, bx 
    xor dx, dx
    right_pl:
        add dx, granica_len
        loop right_pl 
    sub dx, 5
    mov di, dx 
    mov cx, 2
    cld                                                    
    rep stosw                                            
    cmp bx, 24
    jne right
      
;Player spawn point
    mov bl, 1 
    mov bh, land_color
    mov dx, granica_len/2
    xor ax, ax  
    mov al, 04h
    mov ah, player_color
    call PutPixel
    xor bp, bp
    call ShowScore
    
    xor si, si
;-- Начало основного цикла игры ------------------------------------------------
; bl - y позиция игрока, bh - цвет под игроком
; dx - x позиция игрока
GameLoop:
        push cx
        call GetPixel	; ah=цвет точки на новом месте
        xor bp, bp
        test ah, 11010010b
        jz short nepodh
        cmp bh, land_color
        jne short nepodh
        mov si, di
    nepodh: 
		push dx		
		test ah, 11010010b
	    jz short landtouch  ;нет, игрок движется по полю
	    mov ah, player_color_nl
	    mov [tempDI], di
		jmp	show_score	
	; да (земля), игрок коснулся земли
    landtouch:
		mov	ah, player_color
		cmp bh, path_color  ;esli pred ne bil pole
		jne show_score
		
		push ax
		push di
		call ChangeColor
		call FindPixel
		call FloodFill
		pop di
		pop ax
			
	    add [pts], bp
	    xor bp, bp
	    inc bp
	show_score:
		call ShowScore	; выводим счёт на экран
		cmp [pts], 1710
		jae short YouWin
	drawplayer:
		pop	dx
		mov al, 04h
		call PutPixel	; рисуем игрока
; Отрисовка и перемещение врагов
;-----------------------------------------------------------
        push	dx
		push	bx		; сохраняем важные данные (координаты игрока)
		push ax
		mov dx, [x] ;x
		mov bl, [y] ;y
		cmp dx, 0
		je short ne_ok
		call GetPixel
		cmp ah, land_color
		jne short ne_ubit
		mov [x], 0
		mov [y], 0
		mov [d_y], 0
		mov [d_x], 0
		jmp short ne_ok
		ne_ubit:
		mov al, ' '
		mov ah, 00000111b
		call	PutPixel	; стираем врага
		mov al, [d_x]    ;d_x
		cbw             ;iz byte v word
		add dx, ax       ;izmenyaem x
		xchg cx, ax ;sohr d_x v cx
		mov al, [d_y]
		add	bl,al		; изменяем y
		mov	bh,al		; сохраняем d_y в bh

		call GetPixel	; читаем цвет по новым координатам
		cmp	ah,path_color	; это цвет пути?
		je	short GameOver
		cmp	ah,player_color_nl	; это цвет игрока?
		je	short GameOver
	nomoving:
		cmp ah, 00000111b		; это чёрный цвет? 
		je short ok	; да, всё ok

		; отскок, пробуем изменить d_x
		shl	cx,1		; d_x=d_x*2
		sub	dx,cx		; x=x-2*d_x
		call GetPixel	; читаем пиксель по этим координатам
		cmp ah, 00000111b		; теперь чёрный?
		jz short invdx	; да, меняем d_x
		; нет, пробуем изменить d_y
		add	dx,cx		; восстанавливаем x (x=x+2*d_x)
		shl	bh,1		; d_y=d_y*2
		sub	bl,bh		; y=y-2*d_y
		call GetPixel	; читаем пиксель по этим координатам
		cmp ah, 00000111b		; ну а теперь-то чёрный?
		jz short invdy	; да, меняем d_y
		; опять нет?
		sub	dx,cx		; тогда снова меняем x=x-2*d_x :)
		neg	byte ptr [d_y]	; и инвертируем и d_y, и...
	invdx:
	    neg	byte ptr [d_x]	; инвертируем d_x
	    jmp short ok
	invdy:
	    neg	byte ptr [d_y]	; инвертируем d_y
	ok:
		mov	[x], dx
		mov	[y], bl	; заносим новые координаты
		mov	al, 0Fh
		mov ah, enemy_color
		call PutPixel	; рисуем врага
    ne_ok:
		pop ax
		pop	bx		; восстанавливаем важные данные	
        pop dx
;------------------------------------------------------------------------------- 
	    call PauseTimer
	; Обработка клавиш и перемещение игрока
	    test ah, 11010000b ;esli trava
	    jz short deletepl
	    mov ah, path_color  ;esli pole to
	    jmp short del
	deletepl:
	    mov	ah, land_color
	del:
	    mov al, ' '
	    mov bh, ah
		call PutPixel	; затираем игрока 
	    pop cx
	    mov cl, ch
	    
	    test ah, 11010000b ;esli ne trava
	    jnz short pole
	    xor cl, cl       ;ubiraem lock knopock
	    
	pole:
				
		xor ax, ax
		mov ah, 01h
		int 16h
		
		call ClearKbd	; очищаем буфер клавиатуры	
		cmp ch, escep
		je short exit	; Esc
        
		cmp	ch, upkey
		jne	short notup
		cmp bl, 1		; вверх
		jbe	short notup
		cmp cl, downkey     ;bila li proshlaya vniz
		je short protiv
		dec bx
notup:
		cmp	ch,downkey                                        
		jne	short notdown
		cmp	bl, 23		; вниз
		jae	short notdown
		cmp cl, upkey     ;bila li proshlaya vverh
		je short protiv
		inc bx
notdown:
		cmp	ch,leftkey
		jne	short notleft
		cmp dx, 2		; влево
		jbe	short notleft
		cmp cl, rightkey     ;bila li proshlaya vniz
		je short protiv
		sub dx, 2
notleft:
		cmp	ch,rightkey
		jne	short notright
		cmp	dx, 156		; вправо
		jnb	short notright
		cmp cl, leftkey     ;bila li proshlaya vniz
		je short protiv
		add dx, 2
		jmp short notright
protiv:
        mov ch, cl
notright:
		jmp	GameLoop 

    ;-- Игра окончена (столкновение с врагом) --------------------------------------
GameOver:
        mov di, 62
        mov si, offset mGameOver
        mov cx, 36
        cld
        rep movsb
        jmp short exit
YouWin:
        mov di, 66
        mov si, offset mYouWin
        mov cx, 28
        cld
        rep movsb
exit:   
    mov ah, 1
    int 21h
    
    mov ax, 4c00h ; exit to operating system.
    int 21h    
end start ;
