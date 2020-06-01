data segment                             
    inFileDesc dw ?
    outFileDesc dw ?
    buf db 100 dup('$')
    bufLen dw ?
    word db 127 dup(0)
    wordLen db 0
    strBeginL dw 0
    strBeginH dw 0
    
    filePath db 127 dup(0)
    outputPath db 127 dup(0)
    fileNotFound db "Error! File not found!", 10, 13, '$'
    pathNotFound db "Error! Path not found!", 10, 13, '$'
    tooManyFiles db "Error! Too many opened files!", 10, 13, '$'
    accessDenied db "Error! Access denied!", 10, 13, '$'
    wrongAccessMode db "Error! Wrong access mode!", 10, 13, '$'
    wrongCL db "Wrong command line arguments!", 10, 13, '$'
    samePath db "Input and output files cant be same!", 10, 13, '$'
    strCount db 0
    newl db 13, 10
    argc db 0

    needEnter db 0
ends

stack segment
    dw   128  dup(0)
ends

code segment
start:
    mov ax, data
    mov ds, ax
    mov es, ax
    
    call readCommandLine
    
    lea si, filePath
    lea di, outputPath    
    mov cx, 127
    repe cmpsb
    jne notSame:
    lea ax, samePath
    push ax
    call print
    jmp endProgramm
    notSame:
    
    mov ah, 3Dh    ; Open file
    mov al, 0h    
    lea dx, filePath
    int 21h
    call checkErrors
    
    mov inFileDesc, ax
    
    mov ah, 3ch
    mov cx, 0
    lea dx, outputPath
    int 21h
    call checkErrors
    
    mov outFileDesc, ax
    
    mainLoop:
    
    mov ah, 3fh
    mov bx, inFileDesc
    mov cx, 70
    lea dx, buf
    int 21h
    call checkErrors
    mov bufLen, ax
    cmp ax, 0
    je endProgramm
    
    lea di, buf
    mov cx, bufLen
    skipSpaces:
    push cx
    cmp [di], ' '
    je isSpace
    cmp [di], 9
    je isSpace
    cmp [di], 10
    je isNewLine
    cmp [di], 13
    je isNewLine
    pop cx
    jmp endSkipSpaces
    isNewLine:
    mov ax, 4201h
    mov bx, inFileDesc
    xor cx, cx
    xor dx, dx
    int 21h
    pop cx
    push cx
    dec cx
    sub ax, cx
    jnc noOver1
    dec dx
    noOver1:
    mov strBeginL, ax
    mov strBeginH, dx
    isSpace:
    pop cx
    inc di
    loop skipSpaces
    endSkipSpaces:
    cmp cx, bufLen
    je noSpaces
    
    cmp cx, 0
    je mainLoop
    
    mov ah, 42h
    mov al, 1
    mov bx, inFileDesc
    mov dx, cx
    xor cx, cx
    neg dx
    not cx
    int 21h
    jmp mainLoop
        
    noSpaces:
    
    lea di, word
    lea si, buf
    xor cx, cx
    mov cl, wordLen
    repe cmpsb
    jne notThisWord:
    
    cmp [si], ' '
    je thisWord
    cmp [si], 10
    je thisWord
    cmp [si], 13
    je thisWord
    cmp [si], 9
    je thisWord
                
    jmp notThisWord
    thisWord:
    inc strCount
    call writeStr
    jmp mainLoop
    
    notThisWord:
    
    lea di, buf
    mov cx, bufLen
    goNextWord:
    cmp [di], ' '
    je isNotLetter
    cmp [di], 9
    je isNotLetter
    cmp [di], 10
    je isNotLetter
    cmp [di], 13
    je isNotLetter
    jmp isLetter    
    isLetter:
    inc di
    loop goNextWord
    isNotLetter:
    
    cmp cx, 0
    je mainLoop
    
    mov ah, 42h
    mov al, 1
    mov bx, inFileDesc
    mov dx, cx
    xor cx, cx
    neg dx
    not cx
    int 21h
    jmp mainLoop
    
    endProgramm:
    mov ax, 4c00h 
    int 21h
    
        
print proc
    push bp
    mov bp, sp
    var equ [bp]+4
    mov dx, var
    mov ah, 9
    int 21h    
    pop bp   
ret 2
    
writeStr proc
    cmp needEnter, 1
    jne enterNotNeded
    mov bx, outFileDesc
    lea dx, newl
    mov cx, 2
    mov ah, 40h
    int 21h
    
    enterNotNeded:
    mov needEnter, 1
    
    mov ah, 42h
    mov al, 0
    mov bx, inFileDesc
    mov cx, strBeginH
    mov dx, strBeginL
    int 21h

    writeLoop:    
    mov ah, 3fh
    mov bx, inFileDesc
    mov cx, 100
    lea dx, buf
    int 21h
    mov bufLen, ax
    
    mov cx, 0
    lea di, buf
    seekEnd:
    cmp [di], 10
    je endSeekEnd
    cmp [di], 13
    je endSeekEnd
    inc di
    inc cx
    cmp cx, bufLen
    jne seekEnd 
    endSeekEnd:
    
    mov bx, outFileDesc
    lea dx, buf
    mov ah, 40h
    int 21h
    
    cmp cx, 100
    je writeLoop
    push cx
    
    pop cx
    mov dx, bufLen
    sub dx, cx
    cmp dx, 0
    je noBack
    xor cx, cx
    mov al, 1
    mov ah, 42h
    mov bx, inFileDesc
    not cx
    neg dx
    int 21h
    noBack:    
ret

checkErrors proc
    jnc noErr
    cmp ax, 02h  
    je err1
    cmp ax, 03h
    je err2
    cmp ax, 04h
    je err3
    cmp ax, 05h
    je err4
    cmp ax, 0Ch
    je err5
    jmp noErr
    
    err1:
    lea ax, fileNotFound
    push ax
    call print
    jmp endProgramm
    err2:
    lea ax, pathNotFound
    push ax
    call print
    jmp endProgramm
    err3:
    lea ax, tooManyFiles
    push ax
    call print
    jmp endProgramm
    err4:
    lea ax, accessDenied
    push ax
    call print
    jmp endProgramm
    err5:
    lea ax, wrongAccessMode
    push ax
    call print
    jmp endProgramm    
    noErr:     
ret

readCommandLine proc
        mov ah, 62h
    int 21h    
    mov ds, bx
    
    mov si, 80h
    lea di, filePath
    lodsb
    
    skipSpacesCL:
    lodsb
    cmp al, ' '
    je skipSpacesCL
    
    cmp al, 0dh
    je errorCL
    
    mov es:di, al
    inc di
    
    readNextCL:
    lodsb
    cmp al, ' '
    je readSecondCL
    cmp al, 0dh
    je errorCL
    mov es:di, al
    inc di
    jmp readNextCL
    
    readSecondCL:
    skipSpacesCL2:
    lodsb
    cmp al, ' '
    je skipSpacesCL2
    
    cmp al, 0dh
    je errorCL
    
    lea di, outputPath
    mov es:di, al ;SECOND ARG
    inc di
    
    readNextCL2:
    lodsb
    cmp al, ' '
    je ReadThirdCL
    cmp al, 0dh
    je errorCL
    mov es:di, al
    inc di
    jmp readNextCL2
    
    
    ReadThirdCL:
    skipSpacesCL3:
    lodsb
    cmp al, ' '
    je skipSpacesCL3
    
    cmp al, 0dh
    je errorCL
    
    lea di, word
    mov es:di, al ;THIRD ARG
    inc di
        
    readNextCL3:
    lodsb
    cmp al, ' '
    je ReadThirdCL
    cmp al, 0dh
    je endReadCL
    mov es:di, al
    inc di
    jmp readNextCL3
        
    endReadCL:
    
    mov ax, data
    mov ds, ax    
    
    lea ax, word
    sub di, ax
    mov ax, di
    mov wordLen, al
    
    jmp noErrorCL
    errorCL:
    mov ax, data
    mov ds, ax    
    lea ax, wrongCL
    push ax
    call print
    jmp endProgramm
    noErrorCL:     
ret
    
ends

end start