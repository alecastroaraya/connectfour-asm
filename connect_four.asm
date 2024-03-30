;--------------------------------------------Portada-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
; Tarea de ASM: Connect Four
; Curso: Arquitectura de Computadores
; Grupo: 2
; Escuela de Computacion
; Instituto Tecnologico de Costa Rica
; Fecha de entrega: 4 de noviembre del 2020
; Estudiante: Alejandro Castro Araya
; Carne: 2020034944
; Profesor: Kirstein Gatjens
;--------------------------------------------Manual de Usuario-------------------------------------------------------------------------------------------------------------------------------------------------------------------
; Este programa permite jugar una partida de Connect Four por cada ejecucion. Recibe entradas mediante la presion de una tecla y tiene varias posibles opciones de entrada.
; Las posibles opciones son:
; F1      Despliega la ayuda del programa.
; A       Despliega el acerca de del programa.
; 1-7     Se indica que se eche una ficha en la columna escogida para jugar el turno (es del uno al siete).
; F5      Se genera una jugada aleatoria valida para el jugador que la pida, es decir genera un numero aleatorio con los tics de reloj y este numero sera la columna escogida aleatoriamente.
; V       Se voltea el tablero.
; ESC     Interrumpe la ejecucion de la partida y sale al sistema operativo.
;--------------------------------------------Analisis de resultados-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
;+-----------------------------------------------------------------------------------------------------+------+--------------------------------------------------------------------------------------------------------------------------------+
;|                                            Parte                                                    | Nota | Explicacion Adicional                                                                                                          |
;+-----------------------------------------------------------------------------------------------------+------+--------------------------------------------------------------------------------------------------------------------------------+
;| Desplegar mini acerca de con A.                                                                     | A    | Funciona correctamente                                                                                                         |
;| Desplegar tablero y tablero inicial.																   | A    | Funciona correctamente                                                                                                         |
;| Llevar la secuencia de jugadas correctamente.                                                       | A    | Funciona correctamente                                                                                                         |
;| Determinar el jugador ganador correctamente.                                                        | A    | Funciona correctamente                                                                                                         |
;| Despliegue de mensajes y errores con int 21h.                                                       | A    | Funciona correctamente                                                                                                         |
;| Recibir presiones de teclas con int 16h.                                                            | A    | Funciona correctamente                                                                                                         |
;| Verificar que la columna seleccionada no este llena.                                                | A    | Funciona correctamente                                                                                                         |
;| Desplegar la ayuda del programa con F1.                                                             | A    | Funciona correctamente                                                                                                         |
;| Echar una ficha en una columna del 1-7 dependiendo de la escogida.                                  | A    | Funciona correctamente                                                                                                         |
;| Generar una jugada aleatoria valida para el jugador pidiendo interrupcion de tics de reloj con F5.  | A    | Funciona correctamente                                                                                                         |
;| Voltear el tablero con V.                                                                           | B    | Las ultimas dos columnas no se voltean al presionar el boton de voltear el tablero.                                            |
;| Interrumpir la ejecuion de la partida con ESC.                                                      | A    | Funciona correctamente                                                                                                         |
;| Documentación (Portada, manual de usuario y analisis de resultados con ABCD y comentarios).         | A    | Escrita correctamente                                                                                                          |
;+-----------------------------------------------------------------------------------------------------+------+--------------------------------------------------------------------------------------------------------------------------------+

data segment

	acercade db 'Arquitectura de Computadores Gr 2 Alejandro Castro Araya ' ,13, 10, 'Carne 2020034944 Ver. 0.74-3 02/11/2020 Tarea Connect Four', 13, 10, ' ', 13, 10, '$'
	ayuda1 db 'Este programa permite jugar Connect Four mediante recibir una presion de tecla como entrada. Opciones:',13,10, 13,10, 'F1 -> Despliega la ayuda.',13,10, 'A -> Despliega el acerca de del programa.',13,10, '1-7 -> Echa una ficha en esa columna.',13,10, '',13,10,'$'
	ayuda2 db 'F5 -> Genera y escoge una jugada aleatoria valida para el jugador actual.',13,10, 'V -> Se voltea el tablero.',13,10, '',13,10,'$'
	errortecla db 'No presiono una tecla correcta. Terminando el programa...$'
	errorlleno db 'Esta columna esta llena. Por favor ingrese una que no lo este.' ,13,10, 13,10, '$'
	saliendo db 'La tecla ESC ha sido presionada. Saliendose...' ,13, 10, '$'
	presionetecla db 'Por favor presione una tecla.' ,13, 10, '' , 13 ,10, '$'
	columnas db 7 ; 7 columnas x 6 filas
	filas db 6
	connectfour dw 42 dup(0)
	clonconnectfour dw 42 dup(0)
	msgjugador1 db 'Jugador 1?',13,10, 13,10,'$'
	msgjugador2 db 'Jugador 2?',13,10, 13,10,'$'
	jugadoractual db 1
	indiceexterno db 0
	indiceinterno dw 0
	msgganador1 db 'El jugador 1 ha ganado la partida!' ,13,10, '',13,10,'$'
	msgganador2 db 'El jugador 2 ha ganado la partida!' ,13,10, '',13,10,'$'
	msgempate db 'Hubo un empate!' ,13,10, '',13,10,'$'
	contador1 db 0
	contador2 db 0
	contador3 db 0
	moverandom db 0
	contadorvoltear db 0
	voltear1 dw 0
	voltear2 dw 0

data ends


pila segment stack 'stack'
   dw 256 dup(?)

pila ends


code segment

        assume  cs:code, ds:data, ss:pila

printAX proc ; Esta es la rutina de printAX que el profesor usa en los ejemplos vistos en clase y subidos en el foro.
; imprime a la salida estándar un número que supone estar en el AX
; supone que es un número positivo y natural en 16 bits.
; lo imprime en decimal.  
    
    push AX
    push BX
    push CX
    push DX

    xor cx, cx
    mov bx, 10
ciclo1PAX: xor dx, dx
    div bx
    push dx
    inc cx
    cmp ax, 0
    jne ciclo1PAX
    mov ah, 02h
ciclo2PAX: pop DX
    add dl, 30h
    int 21h
    loop ciclo2PAX

    pop DX
    pop CX
    pop BX
    pop AX
    ret
printAX endP

space proc near ; Esta es la rutina de space que el profesor usa en los ejemplos vistos en clase y subidos en el foro.
; imprime a la salida estándar CX espacios en blanco
 
    push AX
    push CX
    push DX

    mov ah, 02h
    mov dl, 32
spacecic: int 21h
    loop spacecic

    pop DX
    pop CX
    pop AX
    ret
space endP

println proc near ; Esta es la rutina de println que el profesor usa en los ejemplos vistos en clase y subidos en el foro.
; imprime a la salida estándar un cambio de línea
 
    push AX
    push DX

    mov ah, 02h
    mov dl, 10
    int 21h
    mov dl, 13
    int 21h

    pop DX
    pop AX
    ret
println endP

CamLin proc ; Esta es la rutina de CamLin que el profesor usa en los ejemplos vistos en clase y subidos en el foro.
; despliega a la salida estandar un Cambio de Línea

  push ax
  push dx

  mov dl, 0Dh
  mov ah, 02h
  int 21h

  mov dl, 0Ah
  mov ah, 02h
  int 21h


  pop dx
  pop ax
  ret

CamLin endP

axezador proc near ; Esta es la rutina de axezador que el profesor usa en los ejemplos vistos en clase y subidos en el foro.
; supone los indices en cl y ch (fil y col)
; retorna en el ax el contenido
; supone en [si] un ptr a la matriz.
; supone los tamaños en dh y dl
; Si está el CF encendido lee y se está apagado escribe.
; si va a escribir recibe en el ax lo que quiere escribir en la matriz
 
    push bx
    push cx
    push dx
    push si

    push ax
    pushF

    mov al, cl
    mul dh
    mov bl, ch
    xor bh, bh
    add ax, bx
    shl ax, 1
    add si, ax

    popF
    pop ax
    jc axezador_lee
    mov word ptr [si],ax
    jmp axezador_salir    

axezador_lee: 
    mov ax, word ptr [si]    


axezador_salir:

    pop SI
    pop DX
    pop CX
    pop BX
    ret

axezador endp




printMat2D proc near ; Esta es la rutina de printMat2D para imprimir una matriz que el profesor usa en los ejemplos vistos en clase y subidos en el foro.
; recibe en DS:[si] un ptr a la matriz de enteros de 16 bits
; en el dh la cantidad de columnas
; en el dl la cantidad de filas

    push ax
    push bx
    push cx
    push dx


    stc

    xor cx, cx
cicfil: cmp cl, dl
    je salirprintmat2D
ciccol: cmp ch, dh
    jne contcol
    xor ch,ch
    inc cl
    call println
    jmp cicfil
contcol: stc
    call axezador
    call printax
    push cx
    mov cx, 2
    call space
    pop cx
    inc ch
    jmp ciccol   

salirprintmat2D:

    pop DX
    pop CX
    pop BX
    pop AX
    ret

printMat2D endp

ESCpresionadoP proc
	lea dx, saliendo
	mov ah,9h
	int 21h
	
	ret
ESCpresionadoP endp
	
F1presionadoP proc
	lea dx, ayuda1
	mov ah,9h
	int 21h
	lea dx, ayuda2
	mov ah,9h
	int 21h
	
	ret
F1presionadoP endp

ApresionadoP proc
	lea dx, acercade
	mov ah,9h
	int 21h
	
	ret
ApresionadoP endp

darErrorP proc ; Da un error si no se presiono una tecla.
	lea dx,errortecla ; Si no se presiono una opcion valida, se despliega un error diciendo eso y luego termina
	mov ah,9h
	int 21h
	
	ret
darErrorP endp

unopresionadoP proc	; se pone una ficha en la columna 1
	mov indiceexterno,5 ; casilla externa inicial   [0][5]<-
	mov indiceinterno,0 ; casilla interna inicial ->[0][5]
	
	mov al,indiceexterno		; indice externo [0][5]<-   (columna * fila)
	mov bl,7		; 7 columnas (meto el numero de columnas porque es 7 x 6 y las columnas van a ir primero en mi caso)
	mul bl			; 
	add ax,indiceinterno		; indice interno ->[0][5]
	shl ax,1		; multiplicamos por el tamano del dato de cada celda
	mov si,ax
	
	cmp word ptr connectfour[si],0 ; si hay un cero le sumo un 1 o un 2
	je sumar1uno
	
	cmp word ptr connectfour[si],1 ; si hay un 1 o un  2 va a la casilla de arriba porque esa ya esta llena
	je siguienteCasillaUnoPresionado1
	
	cmp word ptr connectfour[si],2
	je siguienteCasillaUnoPresionado1
	
	jmp darErrorUnoPresionado
sumar1uno:
	cmp jugadoractual,1 ; le sumo 1 y el jugador es el 1 y 2 si el jugador es el 2
	jne sumar2uno
	
	add word ptr connectfour[si],1
	jmp terminarunopresionadoP
sumar2uno:
	add word ptr connectfour[si],2
	jmp terminarunopresionadoP
siguienteCasillaUnoPresionado1:
	mov cl,indiceexterno
siguienteCasillaUnoPresionado2: ; aqui se hace un ciclo donde se busca la siguiente casilla vacia y si no hay una vacia entonces la columna esta llena
	dec cl

	mov al,cl	; indice externo [1][4]<-   (columna * fila)
	mov bl,7		; 7 columnas (meto el numero de columnas porque es 7 x 6 y las columnas van a ir primero en mi caso)
	mul bl			; 
	add ax,indiceinterno		; indice interno ->[1][4]
	shl ax,1		; multiplicamos por el tamano del dato de cada celda
	mov si,ax
	
	cmp word ptr connectfour[si],0
	je sumar1uno
	
	cmp cl,0
	je darErrorUnoPresionado
	
	jmp siguienteCasillaUnoPresionado2
darErrorUnoPresionado: ; se despliega la matriz y da error porque esta columna esta llena
	mov dl, filas
	mov dh, columnas
	lea si, connectfour
	lea di, clonconnectfour
	call printMat2D
	call println
	call println

	lea dx,errorlleno
	mov ah,9h
	int 21h
	
	cmp jugadoractual,1
	jne saltoConejojugador2Ciclo1
	jmp jugador1Ciclo
saltoConejojugador2Ciclo1:
	jmp jugador2Ciclo
terminarunopresionadoP: ; se despliega la matriz luego de insertar la ficha y luego se revisa verticalmente, horizontalmente y diagonalmente a ver si hay un ganador
	mov dl, filas
	mov dh, columnas
	lea si, connectfour
	lea di, clonconnectfour
	call printMat2D
	call println
	call println
	call revisarGanadorVertical
	mov indiceexterno,5 ; casilla externa inicial   [0][5]<- Le resto uno para subir una casilla verticalmente (FILA)
	mov indiceinterno,0 ; casilla interna inicial ->[0][5] Le sumo uno para ir a la casila de la derecha (COLUMNA)
	call revisarGanadorHorizontal
	mov indiceexterno,4 ; casilla externa inicial   [0][5]<- Le resto uno para subir una casilla verticalmente (COLUMNA)
	mov indiceinterno,0 ; casilla interna inicial ->[0][5] Le sumo uno para ir a la casila de la derecha (FILA)
	call revisarGanadorHorizontal
	mov indiceexterno,3 ; casilla externa inicial   [0][5]<- Le resto uno para subir una casilla verticalmente (COLUMNA)
	mov indiceinterno,0 ; casilla interna inicial ->[0][5] Le sumo uno para ir a la casila de la derecha (FILA)
	call revisarGanadorHorizontal
	mov indiceexterno,2 ; casilla externa inicial   [0][5]<- Le resto uno para subir una casilla verticalmente (COLUMNA)
	mov indiceinterno,0 ; casilla interna inicial ->[0][5] Le sumo uno para ir a la casila de la derecha (FILA)
	call revisarGanadorHorizontal
	mov indiceexterno,1 ; casilla externa inicial   [0][5]<- Le resto uno para subir una casilla verticalmente (COLUMNA)
	mov indiceinterno,0 ; casilla interna inicial ->[0][5] Le sumo uno para ir a la casila de la derecha (FILA)
	call revisarGanadorHorizontal
	mov indiceexterno,0 ; casilla externa inicial   [0][5]<- Le resto uno para subir una casilla verticalmente (COLUMNA)
	mov indiceinterno,0 ; casilla interna inicial ->[0][5] Le sumo uno para ir a la casila de la derecha (FILA)
	call revisarGanadorHorizontal
	mov indiceexterno,2 ; casilla externa inicial   [0][5]<- Le resto uno para subir una casilla verticalmente (COLUMNA)
	mov indiceinterno,0 ; casilla interna inicial ->[0][5] Le sumo uno para ir a la casila de la derecha (FILA)
	call revisarGanadorDiagonal
	mov indiceexterno,1 ; casilla externa inicial   [0][5]<- Le resto uno para subir una casilla verticalmente (COLUMNA)
	mov indiceinterno,0 ; casilla interna inicial ->[0][5] Le sumo uno para ir a la casila de la derecha (FILA)
	call revisarGanadorDiagonal
	mov indiceexterno,0 ; casilla externa inicial   [0][5]<- Le resto uno para subir una casilla verticalmente (COLUMNA)
	mov indiceinterno,0 ; casilla interna inicial ->[0][5] Le sumo uno para ir a la casila de la derecha (FILA)
	call revisarGanadorDiagonal
	mov indiceexterno,0 ; casilla externa inicial   [0][5]<- Le resto uno para subir una casilla verticalmente (COLUMNA)
	mov indiceinterno,1 ; casilla interna inicial ->[0][5] Le sumo uno para ir a la casila de la derecha (FILA)
	call revisarGanadorDiagonal
	mov indiceexterno,0 ; casilla externa inicial   [0][5]<- Le resto uno para subir una casilla verticalmente (COLUMNA)
	mov indiceinterno,2 ; casilla interna inicial ->[0][5] Le sumo uno para ir a la casila de la derecha (FILA)
	call revisarGanadorDiagonal
	
	mov indiceexterno,2 ; casilla externa inicial   [0][5]<- Le resto uno para subir una casilla verticalmente (COLUMNA)
	mov indiceinterno,0 ; casilla interna inicial ->[0][5] Le sumo uno para ir a la casila de la derecha (FILA)
	call revisarGanadorDiagonalJ2
	mov indiceexterno,1 ; casilla externa inicial   [0][5]<- Le resto uno para subir una casilla verticalmente (COLUMNA)
	mov indiceinterno,0 ; casilla interna inicial ->[0][5] Le sumo uno para ir a la casila de la derecha (FILA)
	call revisarGanadorDiagonalJ2
	mov indiceexterno,0 ; casilla externa inicial   [0][5]<- Le resto uno para subir una casilla verticalmente (COLUMNA)
	mov indiceinterno,0 ; casilla interna inicial ->[0][5] Le sumo uno para ir a la casila de la derecha (FILA)
	call revisarGanadorDiagonalJ2
	mov indiceexterno,0 ; casilla externa inicial   [0][5]<- Le resto uno para subir una casilla verticalmente (COLUMNA)
	mov indiceinterno,1 ; casilla interna inicial ->[0][5] Le sumo uno para ir a la casila de la derecha (FILA)
	call revisarGanadorDiagonalJ2
	mov indiceexterno,0 ; casilla externa inicial   [0][5]<- Le resto uno para subir una casilla verticalmente (COLUMNA)
	mov indiceinterno,2 ; casilla interna inicial ->[0][5] Le sumo uno para ir a la casila de la derecha (FILA)
	call revisarGanadorDiagonalJ2
	ret
unopresionadoP endp

dospresionadoP proc ; Es el mismo procedimiento que para la primera columna, solo que se hace con la segunda columna
	mov indiceexterno,5 ; casilla externa inicial   [1][5]<-
	mov indiceinterno,1 ; casilla interna inicial ->[1][5]
	
	mov al,indiceexterno		; indice externo [1][5]<-   (columna * fila)
	mov bl,7		; 7 columnas (meto el numero de columnas porque es 7 x 6 y las columnas van a ir primero en mi caso)
	mul bl			; 
	add ax,indiceinterno		; indice interno ->[1][5]
	shl ax,1		; multiplicamos por el tamano del dato de cada celda
	mov si,ax
	
	cmp word ptr connectfour[si],0
	je sumar1dos
	
	cmp word ptr connectfour[si],1
	je siguienteCasillaDosPresionado1
	
	cmp word ptr connectfour[si],2
	je siguienteCasillaDosPresionado1
	
	jmp darErrorDosPresionado
sumar1dos:
	cmp jugadoractual,1
	jne sumar2dos
	
	add word ptr connectfour[si],1
	jmp terminardospresionadoP
sumar2dos:
	add word ptr connectfour[si],2
	jmp terminarsietepresionadoP
siguienteCasillaDosPresionado1:
	mov cl,indiceexterno
siguienteCasillaDosPresionado2:
	dec cl

	mov al,cl	; indice externo [1][4]<-   (columna * fila)
	mov bl,7		; 7 columnas (meto el numero de columnas porque es 7 x 6 y las columnas van a ir primero en mi caso)
	mul bl			; 
	add ax,indiceinterno		; indice interno ->[1][4]
	shl ax,1		; multiplicamos por el tamano del dato de cada celda
	mov si,ax
	
	cmp word ptr connectfour[si],0
	je sumar1dos
	
	cmp cl,0
	je darErrorDosPresionado
	
	jmp siguienteCasillaDosPresionado2
darErrorDosPresionado:
	mov dl, filas
	mov dh, columnas
	lea si, connectfour
	lea di, clonconnectfour
	call printMat2D
	call println
	call println

	lea dx,errorlleno
	mov ah,9h
	int 21h
	
	cmp jugadoractual,1
	jne saltoConejojugador2Ciclo2
	jmp jugador1Ciclo
saltoConejojugador2Ciclo2:
	jmp jugador2Ciclo
terminardospresionadoP:
	mov dl, filas
	mov dh, columnas
	lea si, connectfour
	lea di, clonconnectfour
	call printMat2D
	call println
	call println
	
	call revisarGanadorVertical
	mov indiceexterno,5 ; casilla externa inicial   [0][5]<- Le resto uno para subir una casilla verticalmente (COLUMNA)
	mov indiceinterno,0 ; casilla interna inicial ->[0][5] Le sumo uno para ir a la casila de la derecha (FILA)
	call revisarGanadorHorizontal
	mov indiceexterno,4 ; casilla externa inicial   [0][5]<- Le resto uno para subir una casilla verticalmente (COLUMNA)
	mov indiceinterno,0 ; casilla interna inicial ->[0][5] Le sumo uno para ir a la casila de la derecha (FILA)
	call revisarGanadorHorizontal
	mov indiceexterno,3 ; casilla externa inicial   [0][5]<- Le resto uno para subir una casilla verticalmente (COLUMNA)
	mov indiceinterno,0 ; casilla interna inicial ->[0][5] Le sumo uno para ir a la casila de la derecha (FILA)
	call revisarGanadorHorizontal
	mov indiceexterno,2 ; casilla externa inicial   [0][5]<- Le resto uno para subir una casilla verticalmente (COLUMNA)
	mov indiceinterno,0 ; casilla interna inicial ->[0][5] Le sumo uno para ir a la casila de la derecha (FILA)
	call revisarGanadorHorizontal
	mov indiceexterno,1 ; casilla externa inicial   [0][5]<- Le resto uno para subir una casilla verticalmente (COLUMNA)
	mov indiceinterno,0 ; casilla interna inicial ->[0][5] Le sumo uno para ir a la casila de la derecha (FILA)
	call revisarGanadorHorizontal
	mov indiceexterno,0 ; casilla externa inicial   [0][5]<- Le resto uno para subir una casilla verticalmente (COLUMNA)
	mov indiceinterno,0 ; casilla interna inicial ->[0][5] Le sumo uno para ir a la casila de la derecha (FILA)
	call revisarGanadorHorizontal
	
	mov indiceexterno,2 ; casilla externa inicial   [0][5]<- Le resto uno para subir una casilla verticalmente (COLUMNA)
	mov indiceinterno,0 ; casilla interna inicial ->[0][5] Le sumo uno para ir a la casila de la derecha (FILA)
	call revisarGanadorDiagonal
	mov indiceexterno,1 ; casilla externa inicial   [0][5]<- Le resto uno para subir una casilla verticalmente (COLUMNA)
	mov indiceinterno,0 ; casilla interna inicial ->[0][5] Le sumo uno para ir a la casila de la derecha (FILA)
	call revisarGanadorDiagonal
	mov indiceexterno,0 ; casilla externa inicial   [0][5]<- Le resto uno para subir una casilla verticalmente (COLUMNA)
	mov indiceinterno,0 ; casilla interna inicial ->[0][5] Le sumo uno para ir a la casila de la derecha (FILA)
	call revisarGanadorDiagonal
	mov indiceexterno,0 ; casilla externa inicial   [0][5]<- Le resto uno para subir una casilla verticalmente (COLUMNA)
	mov indiceinterno,1 ; casilla interna inicial ->[0][5] Le sumo uno para ir a la casila de la derecha (FILA)
	call revisarGanadorDiagonal
	mov indiceexterno,0 ; casilla externa inicial   [0][5]<- Le resto uno para subir una casilla verticalmente (COLUMNA)
	mov indiceinterno,2 ; casilla interna inicial ->[0][5] Le sumo uno para ir a la casila de la derecha (FILA)
	call revisarGanadorDiagonal
	
	mov indiceexterno,2 ; casilla externa inicial   [0][5]<- Le resto uno para subir una casilla verticalmente (COLUMNA)
	mov indiceinterno,0 ; casilla interna inicial ->[0][5] Le sumo uno para ir a la casila de la derecha (FILA)
	call revisarGanadorDiagonalJ2
	mov indiceexterno,1 ; casilla externa inicial   [0][5]<- Le resto uno para subir una casilla verticalmente (COLUMNA)
	mov indiceinterno,0 ; casilla interna inicial ->[0][5] Le sumo uno para ir a la casila de la derecha (FILA)
	call revisarGanadorDiagonalJ2
	mov indiceexterno,0 ; casilla externa inicial   [0][5]<- Le resto uno para subir una casilla verticalmente (COLUMNA)
	mov indiceinterno,0 ; casilla interna inicial ->[0][5] Le sumo uno para ir a la casila de la derecha (FILA)
	call revisarGanadorDiagonalJ2
	mov indiceexterno,0 ; casilla externa inicial   [0][5]<- Le resto uno para subir una casilla verticalmente (COLUMNA)
	mov indiceinterno,1 ; casilla interna inicial ->[0][5] Le sumo uno para ir a la casila de la derecha (FILA)
	call revisarGanadorDiagonalJ2
	mov indiceexterno,0 ; casilla externa inicial   [0][5]<- Le resto uno para subir una casilla verticalmente (COLUMNA)
	mov indiceinterno,2 ; casilla interna inicial ->[0][5] Le sumo uno para ir a la casila de la derecha (FILA)
	call revisarGanadorDiagonalJ2
	
	mov indiceexterno,2 ; casilla externa inicial   [0][5]<- Le resto uno para subir una casilla verticalmente (COLUMNA)
	mov indiceinterno,6 ; casilla interna inicial ->[0][5] Le sumo uno para ir a la casila de la derecha (FILA)
	call revisarGanadorDiagonalMirror
	mov indiceexterno,1 ; casilla externa inicial   [0][5]<- Le resto uno para subir una casilla verticalmente (COLUMNA)
	mov indiceinterno,6 ; casilla interna inicial ->[0][5] Le sumo uno para ir a la casila de la derecha (FILA)
	call revisarGanadorDiagonalMirror
	mov indiceexterno,0 ; casilla externa inicial   [0][5]<- Le resto uno para subir una casilla verticalmente (COLUMNA)
	mov indiceinterno,6 ; casilla interna inicial ->[0][5] Le sumo uno para ir a la casila de la derecha (FILA)
	call revisarGanadorDiagonalMirror
	mov indiceexterno,0 ; casilla externa inicial   [0][5]<- Le resto uno para subir una casilla verticalmente (COLUMNA)
	mov indiceinterno,5 ; casilla interna inicial ->[0][5] Le sumo uno para ir a la casila de la derecha (FILA)
	call revisarGanadorDiagonalMirror
	mov indiceexterno,0 ; casilla externa inicial   [0][5]<- Le resto uno para subir una casilla verticalmente (COLUMNA)
	mov indiceinterno,4 ; casilla interna inicial ->[0][5] Le sumo uno para ir a la casila de la derecha (FILA)
	call revisarGanadorDiagonalMirror
	mov indiceexterno,0 ; casilla externa inicial   [0][5]<- Le resto uno para subir una casilla verticalmente (COLUMNA)
	mov indiceinterno,3 ; casilla interna inicial ->[0][5] Le sumo uno para ir a la casila de la derecha (FILA)
	call revisarGanadorDiagonalMirror
	
	mov indiceexterno,2 ; casilla externa inicial   [0][5]<- Le resto uno para subir una casilla verticalmente (COLUMNA)
	mov indiceinterno,6 ; casilla interna inicial ->[0][5] Le sumo uno para ir a la casila de la derecha (FILA)
	call revisarGanadorDiagonalJ2Mirror
	mov indiceexterno,1 ; casilla externa inicial   [0][5]<- Le resto uno para subir una casilla verticalmente (COLUMNA)
	mov indiceinterno,6 ; casilla interna inicial ->[0][5] Le sumo uno para ir a la casila de la derecha (FILA)
	call revisarGanadorDiagonalJ2Mirror
	mov indiceexterno,0 ; casilla externa inicial   [0][5]<- Le resto uno para subir una casilla verticalmente (COLUMNA)
	mov indiceinterno,6 ; casilla interna inicial ->[0][5] Le sumo uno para ir a la casila de la derecha (FILA)
	call revisarGanadorDiagonalJ2Mirror
	mov indiceexterno,0 ; casilla externa inicial   [0][5]<- Le resto uno para subir una casilla verticalmente (COLUMNA)
	mov indiceinterno,5 ; casilla interna inicial ->[0][5] Le sumo uno para ir a la casila de la derecha (FILA)
	call revisarGanadorDiagonalJ2Mirror
	mov indiceexterno,0 ; casilla externa inicial   [0][5]<- Le resto uno para subir una casilla verticalmente (COLUMNA)
	mov indiceinterno,4 ; casilla interna inicial ->[0][5] Le sumo uno para ir a la casila de la derecha (FILA)
	call revisarGanadorDiagonalJ2Mirror
	mov indiceexterno,0 ; casilla externa inicial   [0][5]<- Le resto uno para subir una casilla verticalmente (COLUMNA)
	mov indiceinterno,3 ; casilla interna inicial ->[0][5] Le sumo uno para ir a la casila de la derecha (FILA)
	call revisarGanadorDiagonalJ2Mirror
	ret
dospresionadoP endp

trespresionadoP proc ; Mismo procedimiento que con la primera y segunda columna, solo que para la tercera
	mov indiceexterno,5 ; casilla externa inicial   [1][5]<-
	mov indiceinterno,2 ; casilla interna inicial ->[1][5]
	
	mov al,indiceexterno		; indice externo [1][5]<-   (columna * fila)
	mov bl,7		; 7 columnas (meto el numero de columnas porque es 7 x 6 y las columnas van a ir primero en mi caso)
	mul bl			; 
	add ax,indiceinterno		; indice interno ->[1][5]
	shl ax,1		; multiplicamos por el tamano del dato de cada celda
	mov si,ax
	
	cmp word ptr connectfour[si],0
	je sumar1tres
	
	cmp word ptr connectfour[si],1
	je siguienteCasillaTresPresionado1
	
	cmp word ptr connectfour[si],2
	je siguienteCasillaTresPresionado1
	
	jmp darErrorTresPresionado
sumar1tres:
	cmp jugadoractual,1
	jne sumar2tres
	
	add word ptr connectfour[si],1
	jmp terminartrespresionadoP
sumar2tres:
	add word ptr connectfour[si],2
	jmp terminartrespresionadoP
siguienteCasillaTresPresionado1:
	mov cl,indiceexterno
siguienteCasillaTresPresionado2:
	dec cl

	mov al,cl	; indice externo [1][4]<-   (columna * fila)
	mov bl,7		; 7 columnas (meto el numero de columnas porque es 7 x 6 y las columnas van a ir primero en mi caso)
	mul bl			; 
	add ax,indiceinterno		; indice interno ->[1][4]
	shl ax,1		; multiplicamos por el tamano del dato de cada celda
	mov si,ax
	
	cmp word ptr connectfour[si],0
	je sumar1tres
	
	cmp cl,0
	je darErrorTresPresionado
	
	jmp siguienteCasillaTresPresionado2
darErrorTresPresionado:
	mov dl, filas
	mov dh, columnas
	lea si, connectfour
	lea di, clonconnectfour
	call printMat2D
	call println
	call println

	lea dx,errorlleno
	mov ah,9h
	int 21h
	
	cmp jugadoractual,1
	jne saltoConejojugador2Ciclo3
	jmp jugador1Ciclo
saltoConejojugador2Ciclo3:
	jmp jugador2Ciclo
terminartrespresionadoP:
	mov dl, filas
	mov dh, columnas
	lea si, connectfour
	lea di, clonconnectfour
	call printMat2D
	call println
	call println
	call revisarGanadorVertical
	mov indiceexterno,5 ; casilla externa inicial   [0][5]<- Le resto uno para subir una casilla verticalmente (COLUMNA)
	mov indiceinterno,0 ; casilla interna inicial ->[0][5] Le sumo uno para ir a la casila de la derecha (FILA)
	call revisarGanadorHorizontal
	mov indiceexterno,4 ; casilla externa inicial   [0][5]<- Le resto uno para subir una casilla verticalmente (COLUMNA)
	mov indiceinterno,0 ; casilla interna inicial ->[0][5] Le sumo uno para ir a la casila de la derecha (FILA)
	call revisarGanadorHorizontal
	mov indiceexterno,3 ; casilla externa inicial   [0][5]<- Le resto uno para subir una casilla verticalmente (COLUMNA)
	mov indiceinterno,0 ; casilla interna inicial ->[0][5] Le sumo uno para ir a la casila de la derecha (FILA)
	call revisarGanadorHorizontal
	mov indiceexterno,2 ; casilla externa inicial   [0][5]<- Le resto uno para subir una casilla verticalmente (COLUMNA)
	mov indiceinterno,0 ; casilla interna inicial ->[0][5] Le sumo uno para ir a la casila de la derecha (FILA)
	call revisarGanadorHorizontal
	mov indiceexterno,1 ; casilla externa inicial   [0][5]<- Le resto uno para subir una casilla verticalmente (COLUMNA)
	mov indiceinterno,0 ; casilla interna inicial ->[0][5] Le sumo uno para ir a la casila de la derecha (FILA)
	call revisarGanadorHorizontal
	mov indiceexterno,0 ; casilla externa inicial   [0][5]<- Le resto uno para subir una casilla verticalmente (COLUMNA)
	mov indiceinterno,0 ; casilla interna inicial ->[0][5] Le sumo uno para ir a la casila de la derecha (FILA)
	call revisarGanadorHorizontal
	mov indiceexterno,2 ; casilla externa inicial   [0][5]<- Le resto uno para subir una casilla verticalmente (COLUMNA)
	mov indiceinterno,0 ; casilla interna inicial ->[0][5] Le sumo uno para ir a la casila de la derecha (FILA)
	call revisarGanadorDiagonal
	mov indiceexterno,1 ; casilla externa inicial   [0][5]<- Le resto uno para subir una casilla verticalmente (COLUMNA)
	mov indiceinterno,0 ; casilla interna inicial ->[0][5] Le sumo uno para ir a la casila de la derecha (FILA)
	call revisarGanadorDiagonal
	mov indiceexterno,0 ; casilla externa inicial   [0][5]<- Le resto uno para subir una casilla verticalmente (COLUMNA)
	mov indiceinterno,0 ; casilla interna inicial ->[0][5] Le sumo uno para ir a la casila de la derecha (FILA)
	call revisarGanadorDiagonal
	mov indiceexterno,0 ; casilla externa inicial   [0][5]<- Le resto uno para subir una casilla verticalmente (COLUMNA)
	mov indiceinterno,1 ; casilla interna inicial ->[0][5] Le sumo uno para ir a la casila de la derecha (FILA)
	call revisarGanadorDiagonal
	mov indiceexterno,0 ; casilla externa inicial   [0][5]<- Le resto uno para subir una casilla verticalmente (COLUMNA)
	mov indiceinterno,2 ; casilla interna inicial ->[0][5] Le sumo uno para ir a la casila de la derecha (FILA)
	call revisarGanadorDiagonal
	
	mov indiceexterno,2 ; casilla externa inicial   [0][5]<- Le resto uno para subir una casilla verticalmente (COLUMNA)
	mov indiceinterno,0 ; casilla interna inicial ->[0][5] Le sumo uno para ir a la casila de la derecha (FILA)
	call revisarGanadorDiagonalJ2
	mov indiceexterno,1 ; casilla externa inicial   [0][5]<- Le resto uno para subir una casilla verticalmente (COLUMNA)
	mov indiceinterno,0 ; casilla interna inicial ->[0][5] Le sumo uno para ir a la casila de la derecha (FILA)
	call revisarGanadorDiagonalJ2
	mov indiceexterno,0 ; casilla externa inicial   [0][5]<- Le resto uno para subir una casilla verticalmente (COLUMNA)
	mov indiceinterno,0 ; casilla interna inicial ->[0][5] Le sumo uno para ir a la casila de la derecha (FILA)
	call revisarGanadorDiagonalJ2
	mov indiceexterno,0 ; casilla externa inicial   [0][5]<- Le resto uno para subir una casilla verticalmente (COLUMNA)
	mov indiceinterno,1 ; casilla interna inicial ->[0][5] Le sumo uno para ir a la casila de la derecha (FILA)
	call revisarGanadorDiagonalJ2
	mov indiceexterno,0 ; casilla externa inicial   [0][5]<- Le resto uno para subir una casilla verticalmente (COLUMNA)
	mov indiceinterno,2 ; casilla interna inicial ->[0][5] Le sumo uno para ir a la casila de la derecha (FILA)
	call revisarGanadorDiagonalJ2
	
	mov indiceexterno,2 ; casilla externa inicial   [0][5]<- Le resto uno para subir una casilla verticalmente (COLUMNA)
	mov indiceinterno,6 ; casilla interna inicial ->[0][5] Le sumo uno para ir a la casila de la derecha (FILA)
	call revisarGanadorDiagonalMirror
	mov indiceexterno,1 ; casilla externa inicial   [0][5]<- Le resto uno para subir una casilla verticalmente (COLUMNA)
	mov indiceinterno,6 ; casilla interna inicial ->[0][5] Le sumo uno para ir a la casila de la derecha (FILA)
	call revisarGanadorDiagonalMirror
	mov indiceexterno,0 ; casilla externa inicial   [0][5]<- Le resto uno para subir una casilla verticalmente (COLUMNA)
	mov indiceinterno,6 ; casilla interna inicial ->[0][5] Le sumo uno para ir a la casila de la derecha (FILA)
	call revisarGanadorDiagonalMirror
	mov indiceexterno,0 ; casilla externa inicial   [0][5]<- Le resto uno para subir una casilla verticalmente (COLUMNA)
	mov indiceinterno,5 ; casilla interna inicial ->[0][5] Le sumo uno para ir a la casila de la derecha (FILA)
	call revisarGanadorDiagonalMirror
	mov indiceexterno,0 ; casilla externa inicial   [0][5]<- Le resto uno para subir una casilla verticalmente (COLUMNA)
	mov indiceinterno,4 ; casilla interna inicial ->[0][5] Le sumo uno para ir a la casila de la derecha (FILA)
	call revisarGanadorDiagonalMirror
	mov indiceexterno,0 ; casilla externa inicial   [0][5]<- Le resto uno para subir una casilla verticalmente (COLUMNA)
	mov indiceinterno,3 ; casilla interna inicial ->[0][5] Le sumo uno para ir a la casila de la derecha (FILA)
	call revisarGanadorDiagonalMirror
	
	mov indiceexterno,2 ; casilla externa inicial   [0][5]<- Le resto uno para subir una casilla verticalmente (COLUMNA)
	mov indiceinterno,6 ; casilla interna inicial ->[0][5] Le sumo uno para ir a la casila de la derecha (FILA)
	call revisarGanadorDiagonalJ2Mirror
	mov indiceexterno,1 ; casilla externa inicial   [0][5]<- Le resto uno para subir una casilla verticalmente (COLUMNA)
	mov indiceinterno,6 ; casilla interna inicial ->[0][5] Le sumo uno para ir a la casila de la derecha (FILA)
	call revisarGanadorDiagonalJ2Mirror
	mov indiceexterno,0 ; casilla externa inicial   [0][5]<- Le resto uno para subir una casilla verticalmente (COLUMNA)
	mov indiceinterno,6 ; casilla interna inicial ->[0][5] Le sumo uno para ir a la casila de la derecha (FILA)
	call revisarGanadorDiagonalJ2Mirror
	mov indiceexterno,0 ; casilla externa inicial   [0][5]<- Le resto uno para subir una casilla verticalmente (COLUMNA)
	mov indiceinterno,5 ; casilla interna inicial ->[0][5] Le sumo uno para ir a la casila de la derecha (FILA)
	call revisarGanadorDiagonalJ2Mirror
	mov indiceexterno,0 ; casilla externa inicial   [0][5]<- Le resto uno para subir una casilla verticalmente (COLUMNA)
	mov indiceinterno,4 ; casilla interna inicial ->[0][5] Le sumo uno para ir a la casila de la derecha (FILA)
	call revisarGanadorDiagonalJ2Mirror
	mov indiceexterno,0 ; casilla externa inicial   [0][5]<- Le resto uno para subir una casilla verticalmente (COLUMNA)
	mov indiceinterno,3 ; casilla interna inicial ->[0][5] Le sumo uno para ir a la casila de la derecha (FILA)
	call revisarGanadorDiagonalJ2Mirror
	ret
trespresionadoP endp

cuatropresionadoP proc ; Mismo procedimiento que con la primera y segunda columna, solo que para la cuarta
	mov indiceexterno,5 ; casilla externa inicial   [1][5]<-
	mov indiceinterno,3 ; casilla interna inicial ->[1][5]
	
	mov al,indiceexterno		; indice externo [1][5]<-   (columna * fila)
	mov bl,7		; 7 columnas (meto el numero de columnas porque es 7 x 6 y las columnas van a ir primero en mi caso)
	mul bl			; 
	add ax,indiceinterno		; indice interno ->[1][5]
	shl ax,1		; multiplicamos por el tamano del dato de cada celda
	mov si,ax
	
	cmp word ptr connectfour[si],0
	je sumar1cuatro
	
	cmp word ptr connectfour[si],1
	je siguienteCasillaCuatroPresionado1
	
	cmp word ptr connectfour[si],2
	je siguienteCasillaCuatroPresionado1
	
	jmp darErrorCuatroPresionado
sumar1cuatro:
	cmp jugadoractual,1
	jne sumar2cuatro
	
	add word ptr connectfour[si],1
	jmp terminarcuatropresionadoP
sumar2cuatro:
	add word ptr connectfour[si],2
	jmp terminarcuatropresionadoP
siguienteCasillaCuatroPresionado1:
	mov cl,indiceexterno
siguienteCasillaCuatroPresionado2:
	dec cl

	mov al,cl	; indice externo [1][4]<-   (columna * fila)
	mov bl,7		; 7 columnas (meto el numero de columnas porque es 7 x 6 y las columnas van a ir primero en mi caso)
	mul bl			; 
	add ax,indiceinterno		; indice interno ->[1][4]
	shl ax,1		; multiplicamos por el tamano del dato de cada celda
	mov si,ax
	
	cmp word ptr connectfour[si],0
	je sumar1cuatro
	
	cmp cl,0
	je darErrorCuatroPresionado
	
	jmp siguienteCasillaCuatroPresionado2
darErrorCuatroPresionado:
	mov dl, filas
	mov dh, columnas
	lea si, connectfour
	lea di, clonconnectfour
	call printMat2D
	call println
	call println

	lea dx,errorlleno
	mov ah,9h
	int 21h
	
	cmp jugadoractual,1
	jne saltoConejojugador2Ciclo4
	jmp jugador1Ciclo
saltoConejojugador2Ciclo4:
	jmp jugador2Ciclo
terminarcuatropresionadoP:
	mov dl, filas
	mov dh, columnas
	lea si, connectfour
	lea di, clonconnectfour
	call printMat2D
	call println
	call println
	call revisarGanadorVertical
	mov indiceexterno,5 ; casilla externa inicial   [0][5]<- Le resto uno para subir una casilla verticalmente (COLUMNA)
	mov indiceinterno,0 ; casilla interna inicial ->[0][5] Le sumo uno para ir a la casila de la derecha (FILA)
	call revisarGanadorHorizontal
	mov indiceexterno,4 ; casilla externa inicial   [0][5]<- Le resto uno para subir una casilla verticalmente (COLUMNA)
	mov indiceinterno,0 ; casilla interna inicial ->[0][5] Le sumo uno para ir a la casila de la derecha (FILA)
	call revisarGanadorHorizontal
	mov indiceexterno,3 ; casilla externa inicial   [0][5]<- Le resto uno para subir una casilla verticalmente (COLUMNA)
	mov indiceinterno,0 ; casilla interna inicial ->[0][5] Le sumo uno para ir a la casila de la derecha (FILA)
	call revisarGanadorHorizontal
	mov indiceexterno,2 ; casilla externa inicial   [0][5]<- Le resto uno para subir una casilla verticalmente (COLUMNA)
	mov indiceinterno,0 ; casilla interna inicial ->[0][5] Le sumo uno para ir a la casila de la derecha (FILA)
	call revisarGanadorHorizontal
	mov indiceexterno,1 ; casilla externa inicial   [0][5]<- Le resto uno para subir una casilla verticalmente (COLUMNA)
	mov indiceinterno,0 ; casilla interna inicial ->[0][5] Le sumo uno para ir a la casila de la derecha (FILA)
	call revisarGanadorHorizontal
	mov indiceexterno,0 ; casilla externa inicial   [0][5]<- Le resto uno para subir una casilla verticalmente (COLUMNA)
	mov indiceinterno,0 ; casilla interna inicial ->[0][5] Le sumo uno para ir a la casila de la derecha (FILA)
	call revisarGanadorHorizontal
	mov indiceexterno,2 ; casilla externa inicial   [0][5]<- Le resto uno para subir una casilla verticalmente (COLUMNA)
	mov indiceinterno,0 ; casilla interna inicial ->[0][5] Le sumo uno para ir a la casila de la derecha (FILA)
	call revisarGanadorDiagonal
	mov indiceexterno,1 ; casilla externa inicial   [0][5]<- Le resto uno para subir una casilla verticalmente (COLUMNA)
	mov indiceinterno,0 ; casilla interna inicial ->[0][5] Le sumo uno para ir a la casila de la derecha (FILA)
	call revisarGanadorDiagonal
	mov indiceexterno,0 ; casilla externa inicial   [0][5]<- Le resto uno para subir una casilla verticalmente (COLUMNA)
	mov indiceinterno,0 ; casilla interna inicial ->[0][5] Le sumo uno para ir a la casila de la derecha (FILA)
	call revisarGanadorDiagonal
	mov indiceexterno,0 ; casilla externa inicial   [0][5]<- Le resto uno para subir una casilla verticalmente (COLUMNA)
	mov indiceinterno,1 ; casilla interna inicial ->[0][5] Le sumo uno para ir a la casila de la derecha (FILA)
	call revisarGanadorDiagonal
	mov indiceexterno,0 ; casilla externa inicial   [0][5]<- Le resto uno para subir una casilla verticalmente (COLUMNA)
	mov indiceinterno,2 ; casilla interna inicial ->[0][5] Le sumo uno para ir a la casila de la derecha (FILA)
	call revisarGanadorDiagonal
	
	mov indiceexterno,2 ; casilla externa inicial   [0][5]<- Le resto uno para subir una casilla verticalmente (COLUMNA)
	mov indiceinterno,0 ; casilla interna inicial ->[0][5] Le sumo uno para ir a la casila de la derecha (FILA)
	call revisarGanadorDiagonalJ2
	mov indiceexterno,1 ; casilla externa inicial   [0][5]<- Le resto uno para subir una casilla verticalmente (COLUMNA)
	mov indiceinterno,0 ; casilla interna inicial ->[0][5] Le sumo uno para ir a la casila de la derecha (FILA)
	call revisarGanadorDiagonalJ2
	mov indiceexterno,0 ; casilla externa inicial   [0][5]<- Le resto uno para subir una casilla verticalmente (COLUMNA)
	mov indiceinterno,0 ; casilla interna inicial ->[0][5] Le sumo uno para ir a la casila de la derecha (FILA)
	call revisarGanadorDiagonalJ2
	mov indiceexterno,0 ; casilla externa inicial   [0][5]<- Le resto uno para subir una casilla verticalmente (COLUMNA)
	mov indiceinterno,1 ; casilla interna inicial ->[0][5] Le sumo uno para ir a la casila de la derecha (FILA)
	call revisarGanadorDiagonalJ2
	mov indiceexterno,0 ; casilla externa inicial   [0][5]<- Le resto uno para subir una casilla verticalmente (COLUMNA)
	mov indiceinterno,2 ; casilla interna inicial ->[0][5] Le sumo uno para ir a la casila de la derecha (FILA)
	call revisarGanadorDiagonalJ2
	
	mov indiceexterno,2 ; casilla externa inicial   [0][5]<- Le resto uno para subir una casilla verticalmente (COLUMNA)
	mov indiceinterno,6 ; casilla interna inicial ->[0][5] Le sumo uno para ir a la casila de la derecha (FILA)
	call revisarGanadorDiagonalMirror
	mov indiceexterno,1 ; casilla externa inicial   [0][5]<- Le resto uno para subir una casilla verticalmente (COLUMNA)
	mov indiceinterno,6 ; casilla interna inicial ->[0][5] Le sumo uno para ir a la casila de la derecha (FILA)
	call revisarGanadorDiagonalMirror
	mov indiceexterno,0 ; casilla externa inicial   [0][5]<- Le resto uno para subir una casilla verticalmente (COLUMNA)
	mov indiceinterno,6 ; casilla interna inicial ->[0][5] Le sumo uno para ir a la casila de la derecha (FILA)
	call revisarGanadorDiagonalMirror
	mov indiceexterno,0 ; casilla externa inicial   [0][5]<- Le resto uno para subir una casilla verticalmente (COLUMNA)
	mov indiceinterno,5 ; casilla interna inicial ->[0][5] Le sumo uno para ir a la casila de la derecha (FILA)
	call revisarGanadorDiagonalMirror
	mov indiceexterno,0 ; casilla externa inicial   [0][5]<- Le resto uno para subir una casilla verticalmente (COLUMNA)
	mov indiceinterno,4 ; casilla interna inicial ->[0][5] Le sumo uno para ir a la casila de la derecha (FILA)
	call revisarGanadorDiagonalMirror
	mov indiceexterno,0 ; casilla externa inicial   [0][5]<- Le resto uno para subir una casilla verticalmente (COLUMNA)
	mov indiceinterno,3 ; casilla interna inicial ->[0][5] Le sumo uno para ir a la casila de la derecha (FILA)
	call revisarGanadorDiagonalMirror
	
	mov indiceexterno,2 ; casilla externa inicial   [0][5]<- Le resto uno para subir una casilla verticalmente (COLUMNA)
	mov indiceinterno,6 ; casilla interna inicial ->[0][5] Le sumo uno para ir a la casila de la derecha (FILA)
	call revisarGanadorDiagonalJ2Mirror
	mov indiceexterno,1 ; casilla externa inicial   [0][5]<- Le resto uno para subir una casilla verticalmente (COLUMNA)
	mov indiceinterno,6 ; casilla interna inicial ->[0][5] Le sumo uno para ir a la casila de la derecha (FILA)
	call revisarGanadorDiagonalJ2Mirror
	mov indiceexterno,0 ; casilla externa inicial   [0][5]<- Le resto uno para subir una casilla verticalmente (COLUMNA)
	mov indiceinterno,6 ; casilla interna inicial ->[0][5] Le sumo uno para ir a la casila de la derecha (FILA)
	call revisarGanadorDiagonalJ2Mirror
	mov indiceexterno,0 ; casilla externa inicial   [0][5]<- Le resto uno para subir una casilla verticalmente (COLUMNA)
	mov indiceinterno,5 ; casilla interna inicial ->[0][5] Le sumo uno para ir a la casila de la derecha (FILA)
	call revisarGanadorDiagonalJ2Mirror
	mov indiceexterno,0 ; casilla externa inicial   [0][5]<- Le resto uno para subir una casilla verticalmente (COLUMNA)
	mov indiceinterno,4 ; casilla interna inicial ->[0][5] Le sumo uno para ir a la casila de la derecha (FILA)
	call revisarGanadorDiagonalJ2Mirror
	mov indiceexterno,0 ; casilla externa inicial   [0][5]<- Le resto uno para subir una casilla verticalmente (COLUMNA)
	mov indiceinterno,3 ; casilla interna inicial ->[0][5] Le sumo uno para ir a la casila de la derecha (FILA)
	call revisarGanadorDiagonalJ2Mirror
	ret
cuatropresionadoP endp

cincopresionadoP proc ; Mismo procedimiento que con la primera y segunda columna, solo que para la quinta
	mov indiceexterno,5 ; casilla externa inicial   [1][5]<-
	mov indiceinterno,4 ; casilla interna inicial ->[1][5]
	
	mov al,indiceexterno		; indice externo [1][5]<-   (columna * fila)
	mov bl,7		; 7 columnas (meto el numero de columnas porque es 7 x 6 y las columnas van a ir primero en mi caso)
	mul bl			; 
	add ax,indiceinterno		; indice interno ->[1][5]
	shl ax,1		; multiplicamos por el tamano del dato de cada celda
	mov si,ax
	
	cmp word ptr connectfour[si],0
	je sumar1cinco
	
	cmp word ptr connectfour[si],1
	je siguienteCasillaCincoPresionado1
	
	cmp word ptr connectfour[si],2
	je siguienteCasillaCincoPresionado1
	
	jmp darErrorCincoPresionado
sumar1cinco:
	cmp jugadoractual,1
	jne sumar2cinco
	
	add word ptr connectfour[si],1
	jmp terminarcincopresionadoP
sumar2cinco:
	add word ptr connectfour[si],2
	jmp terminarcincopresionadoP
siguienteCasillaCincoPresionado1:
	mov cl,indiceexterno
siguienteCasillaCincoPresionado2:
	dec cl

	mov al,cl	; indice externo [1][4]<-   (columna * fila)
	mov bl,7		; 7 columnas (meto el numero de columnas porque es 7 x 6 y las columnas van a ir primero en mi caso)
	mul bl			; 
	add ax,indiceinterno		; indice interno ->[1][4]
	shl ax,1		; multiplicamos por el tamano del dato de cada celda
	mov si,ax
	
	cmp word ptr connectfour[si],0
	je sumar1cinco
	
	cmp cl,0
	je darErrorCincoPresionado
	
	jmp siguienteCasillaCincoPresionado2
darErrorCincoPresionado:
	mov dl, filas
	mov dh, columnas
	lea si, connectfour
	lea di, clonconnectfour
	call printMat2D
	call println
	call println

	lea dx,errorlleno
	mov ah,9h
	int 21h
	
	cmp jugadoractual,1
	jne saltoConejojugador2Ciclo5
	jmp jugador1Ciclo
saltoConejojugador2Ciclo5:
	jmp jugador2Ciclo
terminarcincopresionadoP:
	mov dl, filas
	mov dh, columnas
	lea si, connectfour
	lea di, clonconnectfour
	call printMat2D
	call println
	call println
	
	call revisarGanadorVertical
	mov indiceexterno,5 ; casilla externa inicial   [0][5]<- Le resto uno para subir una casilla verticalmente (COLUMNA)
	mov indiceinterno,0 ; casilla interna inicial ->[0][5] Le sumo uno para ir a la casila de la derecha (FILA)
	call revisarGanadorHorizontal
	mov indiceexterno,4 ; casilla externa inicial   [0][5]<- Le resto uno para subir una casilla verticalmente (COLUMNA)
	mov indiceinterno,0 ; casilla interna inicial ->[0][5] Le sumo uno para ir a la casila de la derecha (FILA)
	call revisarGanadorHorizontal
	mov indiceexterno,3 ; casilla externa inicial   [0][5]<- Le resto uno para subir una casilla verticalmente (COLUMNA)
	mov indiceinterno,0 ; casilla interna inicial ->[0][5] Le sumo uno para ir a la casila de la derecha (FILA)
	call revisarGanadorHorizontal
	mov indiceexterno,2 ; casilla externa inicial   [0][5]<- Le resto uno para subir una casilla verticalmente (COLUMNA)
	mov indiceinterno,0 ; casilla interna inicial ->[0][5] Le sumo uno para ir a la casila de la derecha (FILA)
	call revisarGanadorHorizontal
	mov indiceexterno,1 ; casilla externa inicial   [0][5]<- Le resto uno para subir una casilla verticalmente (COLUMNA)
	mov indiceinterno,0 ; casilla interna inicial ->[0][5] Le sumo uno para ir a la casila de la derecha (FILA)
	call revisarGanadorHorizontal
	mov indiceexterno,0 ; casilla externa inicial   [0][5]<- Le resto uno para subir una casilla verticalmente (COLUMNA)
	mov indiceinterno,0 ; casilla interna inicial ->[0][5] Le sumo uno para ir a la casila de la derecha (FILA)
	call revisarGanadorHorizontal
	mov indiceexterno,2 ; casilla externa inicial   [0][5]<- Le resto uno para subir una casilla verticalmente (COLUMNA)
	mov indiceinterno,0 ; casilla interna inicial ->[0][5] Le sumo uno para ir a la casila de la derecha (FILA)
	call revisarGanadorDiagonal
	mov indiceexterno,1 ; casilla externa inicial   [0][5]<- Le resto uno para subir una casilla verticalmente (COLUMNA)
	mov indiceinterno,0 ; casilla interna inicial ->[0][5] Le sumo uno para ir a la casila de la derecha (FILA)
	call revisarGanadorDiagonal
	mov indiceexterno,0 ; casilla externa inicial   [0][5]<- Le resto uno para subir una casilla verticalmente (COLUMNA)
	mov indiceinterno,0 ; casilla interna inicial ->[0][5] Le sumo uno para ir a la casila de la derecha (FILA)
	call revisarGanadorDiagonal
	mov indiceexterno,0 ; casilla externa inicial   [0][5]<- Le resto uno para subir una casilla verticalmente (COLUMNA)
	mov indiceinterno,1 ; casilla interna inicial ->[0][5] Le sumo uno para ir a la casila de la derecha (FILA)
	call revisarGanadorDiagonal
	mov indiceexterno,0 ; casilla externa inicial   [0][5]<- Le resto uno para subir una casilla verticalmente (COLUMNA)
	mov indiceinterno,2 ; casilla interna inicial ->[0][5] Le sumo uno para ir a la casila de la derecha (FILA)
	call revisarGanadorDiagonal
	
	mov indiceexterno,2 ; casilla externa inicial   [0][5]<- Le resto uno para subir una casilla verticalmente (COLUMNA)
	mov indiceinterno,0 ; casilla interna inicial ->[0][5] Le sumo uno para ir a la casila de la derecha (FILA)
	call revisarGanadorDiagonalJ2
	mov indiceexterno,1 ; casilla externa inicial   [0][5]<- Le resto uno para subir una casilla verticalmente (COLUMNA)
	mov indiceinterno,0 ; casilla interna inicial ->[0][5] Le sumo uno para ir a la casila de la derecha (FILA)
	call revisarGanadorDiagonalJ2
	mov indiceexterno,0 ; casilla externa inicial   [0][5]<- Le resto uno para subir una casilla verticalmente (COLUMNA)
	mov indiceinterno,0 ; casilla interna inicial ->[0][5] Le sumo uno para ir a la casila de la derecha (FILA)
	call revisarGanadorDiagonalJ2
	mov indiceexterno,0 ; casilla externa inicial   [0][5]<- Le resto uno para subir una casilla verticalmente (COLUMNA)
	mov indiceinterno,1 ; casilla interna inicial ->[0][5] Le sumo uno para ir a la casila de la derecha (FILA)
	call revisarGanadorDiagonalJ2
	mov indiceexterno,0 ; casilla externa inicial   [0][5]<- Le resto uno para subir una casilla verticalmente (COLUMNA)
	mov indiceinterno,2 ; casilla interna inicial ->[0][5] Le sumo uno para ir a la casila de la derecha (FILA)
	call revisarGanadorDiagonalJ2
	
	mov indiceexterno,2 ; casilla externa inicial   [0][5]<- Le resto uno para subir una casilla verticalmente (COLUMNA)
	mov indiceinterno,6 ; casilla interna inicial ->[0][5] Le sumo uno para ir a la casila de la derecha (FILA)
	call revisarGanadorDiagonalMirror
	mov indiceexterno,1 ; casilla externa inicial   [0][5]<- Le resto uno para subir una casilla verticalmente (COLUMNA)
	mov indiceinterno,6 ; casilla interna inicial ->[0][5] Le sumo uno para ir a la casila de la derecha (FILA)
	call revisarGanadorDiagonalMirror
	mov indiceexterno,0 ; casilla externa inicial   [0][5]<- Le resto uno para subir una casilla verticalmente (COLUMNA)
	mov indiceinterno,6 ; casilla interna inicial ->[0][5] Le sumo uno para ir a la casila de la derecha (FILA)
	call revisarGanadorDiagonalMirror
	mov indiceexterno,0 ; casilla externa inicial   [0][5]<- Le resto uno para subir una casilla verticalmente (COLUMNA)
	mov indiceinterno,5 ; casilla interna inicial ->[0][5] Le sumo uno para ir a la casila de la derecha (FILA)
	call revisarGanadorDiagonalMirror
	mov indiceexterno,0 ; casilla externa inicial   [0][5]<- Le resto uno para subir una casilla verticalmente (COLUMNA)
	mov indiceinterno,4 ; casilla interna inicial ->[0][5] Le sumo uno para ir a la casila de la derecha (FILA)
	call revisarGanadorDiagonalMirror
	mov indiceexterno,0 ; casilla externa inicial   [0][5]<- Le resto uno para subir una casilla verticalmente (COLUMNA)
	mov indiceinterno,3 ; casilla interna inicial ->[0][5] Le sumo uno para ir a la casila de la derecha (FILA)
	call revisarGanadorDiagonalMirror
	
	mov indiceexterno,2 ; casilla externa inicial   [0][5]<- Le resto uno para subir una casilla verticalmente (COLUMNA)
	mov indiceinterno,6 ; casilla interna inicial ->[0][5] Le sumo uno para ir a la casila de la derecha (FILA)
	call revisarGanadorDiagonalJ2Mirror
	mov indiceexterno,1 ; casilla externa inicial   [0][5]<- Le resto uno para subir una casilla verticalmente (COLUMNA)
	mov indiceinterno,6 ; casilla interna inicial ->[0][5] Le sumo uno para ir a la casila de la derecha (FILA)
	call revisarGanadorDiagonalJ2Mirror
	mov indiceexterno,0 ; casilla externa inicial   [0][5]<- Le resto uno para subir una casilla verticalmente (COLUMNA)
	mov indiceinterno,6 ; casilla interna inicial ->[0][5] Le sumo uno para ir a la casila de la derecha (FILA)
	call revisarGanadorDiagonalJ2Mirror
	mov indiceexterno,0 ; casilla externa inicial   [0][5]<- Le resto uno para subir una casilla verticalmente (COLUMNA)
	mov indiceinterno,5 ; casilla interna inicial ->[0][5] Le sumo uno para ir a la casila de la derecha (FILA)
	call revisarGanadorDiagonalJ2Mirror
	mov indiceexterno,0 ; casilla externa inicial   [0][5]<- Le resto uno para subir una casilla verticalmente (COLUMNA)
	mov indiceinterno,4 ; casilla interna inicial ->[0][5] Le sumo uno para ir a la casila de la derecha (FILA)
	call revisarGanadorDiagonalJ2Mirror
	mov indiceexterno,0 ; casilla externa inicial   [0][5]<- Le resto uno para subir una casilla verticalmente (COLUMNA)
	mov indiceinterno,3 ; casilla interna inicial ->[0][5] Le sumo uno para ir a la casila de la derecha (FILA)
	call revisarGanadorDiagonalJ2Mirror
	ret
cincopresionadoP endp

seispresionadoP proc ; Mismo procedimiento que con la primera y segunda columna, solo que para la sexta
	mov indiceexterno,5 ; casilla externa inicial   [1][5]<-
	mov indiceinterno,5 ; casilla interna inicial ->[1][5]
	
	mov al,indiceexterno		; indice externo [1][5]<-   (columna * fila)
	mov bl,7		; 7 columnas (meto el numero de columnas porque es 7 x 6 y las columnas van a ir primero en mi caso)
	mul bl			; 
	add ax,indiceinterno		; indice interno ->[1][5]
	shl ax,1		; multiplicamos por el tamano del dato de cada celda
	mov si,ax
	
	cmp word ptr connectfour[si],0
	je sumar1seis
	
	cmp word ptr connectfour[si],1
	je siguienteCasillaSeisPresionado1
	
	cmp word ptr connectfour[si],2
	je siguienteCasillaSeisPresionado1
	
	jmp darErrorSeisPresionado
sumar1seis:
	cmp jugadoractual,1
	jne sumar2seis
	
	add word ptr connectfour[si],1
	jmp terminarseispresionadoP
sumar2seis:
	add word ptr connectfour[si],2
	jmp terminarseispresionadoP
siguienteCasillaSeisPresionado1:
	mov cl,indiceexterno
siguienteCasillaSeisPresionado2:
	dec cl

	mov al,cl	; indice externo [1][4]<-   (columna * fila)
	mov bl,7		; 7 columnas (meto el numero de columnas porque es 7 x 6 y las columnas van a ir primero en mi caso)
	mul bl			; 
	add ax,indiceinterno		; indice interno ->[1][4]
	shl ax,1		; multiplicamos por el tamano del dato de cada celda
	mov si,ax
	
	cmp word ptr connectfour[si],0
	je sumar1seis
	
	cmp cl,0
	je darErrorSeisPresionado
	
	jmp siguienteCasillaSeisPresionado2
darErrorSeisPresionado:
	mov dl, filas
	mov dh, columnas
	lea si, connectfour
	lea di, clonconnectfour
	call printMat2D
	call println
	call println

	lea dx,errorlleno
	mov ah,9h
	int 21h
	
	cmp jugadoractual,1
	jne saltoConejojugador2Ciclo6
	jmp jugador1Ciclo
saltoConejojugador2Ciclo6:
	jmp jugador2Ciclo
terminarseispresionadoP:
	mov dl, filas
	mov dh, columnas
	lea si, connectfour
	lea di, clonconnectfour
	call printMat2D
	call println
	call println
	
	call revisarGanadorVertical
	mov indiceexterno,5 ; casilla externa inicial   [0][5]<- Le resto uno para subir una casilla verticalmente (COLUMNA)
	mov indiceinterno,0 ; casilla interna inicial ->[0][5] Le sumo uno para ir a la casila de la derecha (FILA)
	call revisarGanadorHorizontal
	mov indiceexterno,4 ; casilla externa inicial   [0][5]<- Le resto uno para subir una casilla verticalmente (COLUMNA)
	mov indiceinterno,0 ; casilla interna inicial ->[0][5] Le sumo uno para ir a la casila de la derecha (FILA)
	call revisarGanadorHorizontal
	mov indiceexterno,3 ; casilla externa inicial   [0][5]<- Le resto uno para subir una casilla verticalmente (COLUMNA)
	mov indiceinterno,0 ; casilla interna inicial ->[0][5] Le sumo uno para ir a la casila de la derecha (FILA)
	call revisarGanadorHorizontal
	mov indiceexterno,2 ; casilla externa inicial   [0][5]<- Le resto uno para subir una casilla verticalmente (COLUMNA)
	mov indiceinterno,0 ; casilla interna inicial ->[0][5] Le sumo uno para ir a la casila de la derecha (FILA)
	call revisarGanadorHorizontal
	mov indiceexterno,1 ; casilla externa inicial   [0][5]<- Le resto uno para subir una casilla verticalmente (COLUMNA)
	mov indiceinterno,0 ; casilla interna inicial ->[0][5] Le sumo uno para ir a la casila de la derecha (FILA)
	call revisarGanadorHorizontal
	mov indiceexterno,0 ; casilla externa inicial   [0][5]<- Le resto uno para subir una casilla verticalmente (COLUMNA)
	mov indiceinterno,0 ; casilla interna inicial ->[0][5] Le sumo uno para ir a la casila de la derecha (FILA)
	call revisarGanadorHorizontal
	mov indiceexterno,2 ; casilla externa inicial   [0][5]<- Le resto uno para subir una casilla verticalmente (COLUMNA)
	mov indiceinterno,0 ; casilla interna inicial ->[0][5] Le sumo uno para ir a la casila de la derecha (FILA)
	call revisarGanadorDiagonal
	mov indiceexterno,1 ; casilla externa inicial   [0][5]<- Le resto uno para subir una casilla verticalmente (COLUMNA)
	mov indiceinterno,0 ; casilla interna inicial ->[0][5] Le sumo uno para ir a la casila de la derecha (FILA)
	call revisarGanadorDiagonal
	mov indiceexterno,0 ; casilla externa inicial   [0][5]<- Le resto uno para subir una casilla verticalmente (COLUMNA)
	mov indiceinterno,0 ; casilla interna inicial ->[0][5] Le sumo uno para ir a la casila de la derecha (FILA)
	call revisarGanadorDiagonal
	mov indiceexterno,0 ; casilla externa inicial   [0][5]<- Le resto uno para subir una casilla verticalmente (COLUMNA)
	mov indiceinterno,1 ; casilla interna inicial ->[0][5] Le sumo uno para ir a la casila de la derecha (FILA)
	call revisarGanadorDiagonal
	mov indiceexterno,0 ; casilla externa inicial   [0][5]<- Le resto uno para subir una casilla verticalmente (COLUMNA)
	mov indiceinterno,2 ; casilla interna inicial ->[0][5] Le sumo uno para ir a la casila de la derecha (FILA)
	call revisarGanadorDiagonal
	
	mov indiceexterno,2 ; casilla externa inicial   [0][5]<- Le resto uno para subir una casilla verticalmente (COLUMNA)
	mov indiceinterno,0 ; casilla interna inicial ->[0][5] Le sumo uno para ir a la casila de la derecha (FILA)
	call revisarGanadorDiagonalJ2
	mov indiceexterno,1 ; casilla externa inicial   [0][5]<- Le resto uno para subir una casilla verticalmente (COLUMNA)
	mov indiceinterno,0 ; casilla interna inicial ->[0][5] Le sumo uno para ir a la casila de la derecha (FILA)
	call revisarGanadorDiagonalJ2
	mov indiceexterno,0 ; casilla externa inicial   [0][5]<- Le resto uno para subir una casilla verticalmente (COLUMNA)
	mov indiceinterno,0 ; casilla interna inicial ->[0][5] Le sumo uno para ir a la casila de la derecha (FILA)
	call revisarGanadorDiagonalJ2
	mov indiceexterno,0 ; casilla externa inicial   [0][5]<- Le resto uno para subir una casilla verticalmente (COLUMNA)
	mov indiceinterno,1 ; casilla interna inicial ->[0][5] Le sumo uno para ir a la casila de la derecha (FILA)
	call revisarGanadorDiagonalJ2
	mov indiceexterno,0 ; casilla externa inicial   [0][5]<- Le resto uno para subir una casilla verticalmente (COLUMNA)
	mov indiceinterno,2 ; casilla interna inicial ->[0][5] Le sumo uno para ir a la casila de la derecha (FILA)
	call revisarGanadorDiagonalJ2
	
	mov indiceexterno,2 ; casilla externa inicial   [0][5]<- Le resto uno para subir una casilla verticalmente (COLUMNA)
	mov indiceinterno,6 ; casilla interna inicial ->[0][5] Le sumo uno para ir a la casila de la derecha (FILA)
	call revisarGanadorDiagonalMirror
	mov indiceexterno,1 ; casilla externa inicial   [0][5]<- Le resto uno para subir una casilla verticalmente (COLUMNA)
	mov indiceinterno,6 ; casilla interna inicial ->[0][5] Le sumo uno para ir a la casila de la derecha (FILA)
	call revisarGanadorDiagonalMirror
	mov indiceexterno,0 ; casilla externa inicial   [0][5]<- Le resto uno para subir una casilla verticalmente (COLUMNA)
	mov indiceinterno,6 ; casilla interna inicial ->[0][5] Le sumo uno para ir a la casila de la derecha (FILA)
	call revisarGanadorDiagonalMirror
	mov indiceexterno,0 ; casilla externa inicial   [0][5]<- Le resto uno para subir una casilla verticalmente (COLUMNA)
	mov indiceinterno,5 ; casilla interna inicial ->[0][5] Le sumo uno para ir a la casila de la derecha (FILA)
	call revisarGanadorDiagonalMirror
	mov indiceexterno,0 ; casilla externa inicial   [0][5]<- Le resto uno para subir una casilla verticalmente (COLUMNA)
	mov indiceinterno,4 ; casilla interna inicial ->[0][5] Le sumo uno para ir a la casila de la derecha (FILA)
	call revisarGanadorDiagonalMirror
	mov indiceexterno,0 ; casilla externa inicial   [0][5]<- Le resto uno para subir una casilla verticalmente (COLUMNA)
	mov indiceinterno,3 ; casilla interna inicial ->[0][5] Le sumo uno para ir a la casila de la derecha (FILA)
	call revisarGanadorDiagonalMirror
	
	mov indiceexterno,2 ; casilla externa inicial   [0][5]<- Le resto uno para subir una casilla verticalmente (COLUMNA)
	mov indiceinterno,6 ; casilla interna inicial ->[0][5] Le sumo uno para ir a la casila de la derecha (FILA)
	call revisarGanadorDiagonalJ2Mirror
	mov indiceexterno,1 ; casilla externa inicial   [0][5]<- Le resto uno para subir una casilla verticalmente (COLUMNA)
	mov indiceinterno,6 ; casilla interna inicial ->[0][5] Le sumo uno para ir a la casila de la derecha (FILA)
	call revisarGanadorDiagonalJ2Mirror
	mov indiceexterno,0 ; casilla externa inicial   [0][5]<- Le resto uno para subir una casilla verticalmente (COLUMNA)
	mov indiceinterno,6 ; casilla interna inicial ->[0][5] Le sumo uno para ir a la casila de la derecha (FILA)
	call revisarGanadorDiagonalJ2Mirror
	mov indiceexterno,0 ; casilla externa inicial   [0][5]<- Le resto uno para subir una casilla verticalmente (COLUMNA)
	mov indiceinterno,5 ; casilla interna inicial ->[0][5] Le sumo uno para ir a la casila de la derecha (FILA)
	call revisarGanadorDiagonalJ2Mirror
	mov indiceexterno,0 ; casilla externa inicial   [0][5]<- Le resto uno para subir una casilla verticalmente (COLUMNA)
	mov indiceinterno,4 ; casilla interna inicial ->[0][5] Le sumo uno para ir a la casila de la derecha (FILA)
	call revisarGanadorDiagonalJ2Mirror
	mov indiceexterno,0 ; casilla externa inicial   [0][5]<- Le resto uno para subir una casilla verticalmente (COLUMNA)
	mov indiceinterno,3 ; casilla interna inicial ->[0][5] Le sumo uno para ir a la casila de la derecha (FILA)
	call revisarGanadorDiagonalJ2Mirror
	ret
seispresionadoP endp

sietepresionadoP proc ; Mismo procedimiento que con la primera y segunda columna, solo que para la setima
	mov indiceexterno,5 ; casilla externa inicial   [1][5]<- ; FILA
	mov indiceinterno,6 ; casilla interna inicial ->[1][5] ; COLUMNA
	
	mov al,indiceexterno		; indice externo [1][5]<-   (columna * fila)
	mov bl,7		; 7 columnas (meto el numero de columnas porque es 7 x 6 y las columnas van a ir primero en mi caso)
	mul bl			; 
	add ax,indiceinterno		; indice interno ->[1][5]
	shl ax,1		; multiplicamos por el tamano del dato de cada celda
	mov si,ax
	
	cmp word ptr connectfour[si],0
	je sumar1siete
	
	cmp word ptr connectfour[si],1
	je siguienteCasillaSietePresionado1
	
	cmp word ptr connectfour[si],2
	je siguienteCasillaSietePresionado1
	
	jmp darErrorSietePresionado
sumar1siete:
	cmp jugadoractual,1
	jne sumar2siete
	
	add word ptr connectfour[si],1
	jmp terminarsietepresionadoP
sumar2siete:
	add word ptr connectfour[si],2
	jmp terminarsietepresionadoP
siguienteCasillaSietePresionado1:
	mov cl,indiceexterno
siguienteCasillaSietePresionado2:
	dec cl

	mov al,cl	; indice externo [1][4]<-   (columna * fila)
	mov bl,7		; 7 columnas (meto el numero de columnas porque es 7 x 6 y las columnas van a ir primero en mi caso)
	mul bl			; 
	add ax,indiceinterno		; indice interno ->[1][4]
	shl ax,1		; multiplicamos por el tamano del dato de cada celda
	mov si,ax
	
	cmp word ptr connectfour[si],0
	je sumar1siete
	
	cmp cl,0
	je darErrorSietePresionado
	
	jmp siguienteCasillaSietePresionado2
darErrorSietePresionado:
	mov dl, filas
	mov dh, columnas
	lea si, connectfour
	lea di, clonconnectfour
	call printMat2D
	call println
	call println

	lea dx,errorlleno
	mov ah,9h
	int 21h
	
	cmp jugadoractual,1
	jne saltoConejojugador2Ciclo7
	jmp jugador1Ciclo
saltoConejojugador2Ciclo7:
	jmp jugador2Ciclo
terminarsietepresionadoP:
	mov dl, filas
	mov dh, columnas
	lea si, connectfour
	lea di, clonconnectfour
	call printMat2D
	call println
	call println
	
	call revisarGanadorVertical
	mov indiceexterno,5 ; casilla externa inicial   [0][5]<- Le resto uno para subir una casilla verticalmente (COLUMNA)
	mov indiceinterno,0 ; casilla interna inicial ->[0][5] Le sumo uno para ir a la casila de la derecha (FILA)
	call revisarGanadorHorizontal
	mov indiceexterno,4 ; casilla externa inicial   [0][5]<- Le resto uno para subir una casilla verticalmente (COLUMNA)
	mov indiceinterno,0 ; casilla interna inicial ->[0][5] Le sumo uno para ir a la casila de la derecha (FILA)
	call revisarGanadorHorizontal
	mov indiceexterno,3 ; casilla externa inicial   [0][5]<- Le resto uno para subir una casilla verticalmente (COLUMNA)
	mov indiceinterno,0 ; casilla interna inicial ->[0][5] Le sumo uno para ir a la casila de la derecha (FILA)
	call revisarGanadorHorizontal
	mov indiceexterno,2 ; casilla externa inicial   [0][5]<- Le resto uno para subir una casilla verticalmente (COLUMNA)
	mov indiceinterno,0 ; casilla interna inicial ->[0][5] Le sumo uno para ir a la casila de la derecha (FILA)
	call revisarGanadorHorizontal
	mov indiceexterno,1 ; casilla externa inicial   [0][5]<- Le resto uno para subir una casilla verticalmente (COLUMNA)
	mov indiceinterno,0 ; casilla interna inicial ->[0][5] Le sumo uno para ir a la casila de la derecha (FILA)
	call revisarGanadorHorizontal
	mov indiceexterno,0 ; casilla externa inicial   [0][5]<- Le resto uno para subir una casilla verticalmente (COLUMNA)
	mov indiceinterno,0 ; casilla interna inicial ->[0][5] Le sumo uno para ir a la casila de la derecha (FILA)
	call revisarGanadorHorizontal
	mov indiceexterno,2 ; casilla externa inicial   [0][5]<- Le resto uno para subir una casilla verticalmente (COLUMNA)
	mov indiceinterno,0 ; casilla interna inicial ->[0][5] Le sumo uno para ir a la casila de la derecha (FILA)
	call revisarGanadorDiagonal
	mov indiceexterno,1 ; casilla externa inicial   [0][5]<- Le resto uno para subir una casilla verticalmente (COLUMNA)
	mov indiceinterno,0 ; casilla interna inicial ->[0][5] Le sumo uno para ir a la casila de la derecha (FILA)
	call revisarGanadorDiagonal
	mov indiceexterno,0 ; casilla externa inicial   [0][5]<- Le resto uno para subir una casilla verticalmente (COLUMNA)
	mov indiceinterno,0 ; casilla interna inicial ->[0][5] Le sumo uno para ir a la casila de la derecha (FILA)
	call revisarGanadorDiagonal
	mov indiceexterno,0 ; casilla externa inicial   [0][5]<- Le resto uno para subir una casilla verticalmente (COLUMNA)
	mov indiceinterno,1 ; casilla interna inicial ->[0][5] Le sumo uno para ir a la casila de la derecha (FILA)
	call revisarGanadorDiagonal
	mov indiceexterno,0 ; casilla externa inicial   [0][5]<- Le resto uno para subir una casilla verticalmente (COLUMNA)
	mov indiceinterno,2 ; casilla interna inicial ->[0][5] Le sumo uno para ir a la casila de la derecha (FILA)
	call revisarGanadorDiagonal
	
	mov indiceexterno,2 ; casilla externa inicial   [0][5]<- Le resto uno para subir una casilla verticalmente (COLUMNA)
	mov indiceinterno,0 ; casilla interna inicial ->[0][5] Le sumo uno para ir a la casila de la derecha (FILA)
	call revisarGanadorDiagonalJ2
	mov indiceexterno,1 ; casilla externa inicial   [0][5]<- Le resto uno para subir una casilla verticalmente (COLUMNA)
	mov indiceinterno,0 ; casilla interna inicial ->[0][5] Le sumo uno para ir a la casila de la derecha (FILA)
	call revisarGanadorDiagonalJ2
	mov indiceexterno,0 ; casilla externa inicial   [0][5]<- Le resto uno para subir una casilla verticalmente (COLUMNA)
	mov indiceinterno,0 ; casilla interna inicial ->[0][5] Le sumo uno para ir a la casila de la derecha (FILA)
	call revisarGanadorDiagonalJ2
	mov indiceexterno,0 ; casilla externa inicial   [0][5]<- Le resto uno para subir una casilla verticalmente (COLUMNA)
	mov indiceinterno,1 ; casilla interna inicial ->[0][5] Le sumo uno para ir a la casila de la derecha (FILA)
	call revisarGanadorDiagonalJ2
	mov indiceexterno,0 ; casilla externa inicial   [0][5]<- Le resto uno para subir una casilla verticalmente (COLUMNA)
	mov indiceinterno,2 ; casilla interna inicial ->[0][5] Le sumo uno para ir a la casila de la derecha (FILA)
	call revisarGanadorDiagonalJ2
	
	mov indiceexterno,2 ; casilla externa inicial   [0][5]<- Le resto uno para subir una casilla verticalmente (COLUMNA)
	mov indiceinterno,6 ; casilla interna inicial ->[0][5] Le sumo uno para ir a la casila de la derecha (FILA)
	call revisarGanadorDiagonalMirror
	mov indiceexterno,1 ; casilla externa inicial   [0][5]<- Le resto uno para subir una casilla verticalmente (COLUMNA)
	mov indiceinterno,6 ; casilla interna inicial ->[0][5] Le sumo uno para ir a la casila de la derecha (FILA)
	call revisarGanadorDiagonalMirror
	mov indiceexterno,0 ; casilla externa inicial   [0][5]<- Le resto uno para subir una casilla verticalmente (COLUMNA)
	mov indiceinterno,6 ; casilla interna inicial ->[0][5] Le sumo uno para ir a la casila de la derecha (FILA)
	call revisarGanadorDiagonalMirror
	mov indiceexterno,0 ; casilla externa inicial   [0][5]<- Le resto uno para subir una casilla verticalmente (COLUMNA)
	mov indiceinterno,5 ; casilla interna inicial ->[0][5] Le sumo uno para ir a la casila de la derecha (FILA)
	call revisarGanadorDiagonalMirror
	mov indiceexterno,0 ; casilla externa inicial   [0][5]<- Le resto uno para subir una casilla verticalmente (COLUMNA)
	mov indiceinterno,4 ; casilla interna inicial ->[0][5] Le sumo uno para ir a la casila de la derecha (FILA)
	call revisarGanadorDiagonalMirror
	mov indiceexterno,0 ; casilla externa inicial   [0][5]<- Le resto uno para subir una casilla verticalmente (COLUMNA)
	mov indiceinterno,3 ; casilla interna inicial ->[0][5] Le sumo uno para ir a la casila de la derecha (FILA)
	call revisarGanadorDiagonalMirror
	
	mov indiceexterno,2 ; casilla externa inicial   [0][5]<- Le resto uno para subir una casilla verticalmente (COLUMNA)
	mov indiceinterno,6 ; casilla interna inicial ->[0][5] Le sumo uno para ir a la casila de la derecha (FILA)
	call revisarGanadorDiagonalJ2Mirror
	mov indiceexterno,1 ; casilla externa inicial   [0][5]<- Le resto uno para subir una casilla verticalmente (COLUMNA)
	mov indiceinterno,6 ; casilla interna inicial ->[0][5] Le sumo uno para ir a la casila de la derecha (FILA)
	call revisarGanadorDiagonalJ2Mirror
	mov indiceexterno,0 ; casilla externa inicial   [0][5]<- Le resto uno para subir una casilla verticalmente (COLUMNA)
	mov indiceinterno,6 ; casilla interna inicial ->[0][5] Le sumo uno para ir a la casila de la derecha (FILA)
	call revisarGanadorDiagonalJ2Mirror
	mov indiceexterno,0 ; casilla externa inicial   [0][5]<- Le resto uno para subir una casilla verticalmente (COLUMNA)
	mov indiceinterno,5 ; casilla interna inicial ->[0][5] Le sumo uno para ir a la casila de la derecha (FILA)
	call revisarGanadorDiagonalJ2Mirror
	mov indiceexterno,0 ; casilla externa inicial   [0][5]<- Le resto uno para subir una casilla verticalmente (COLUMNA)
	mov indiceinterno,4 ; casilla interna inicial ->[0][5] Le sumo uno para ir a la casila de la derecha (FILA)
	call revisarGanadorDiagonalJ2Mirror
	mov indiceexterno,0 ; casilla externa inicial   [0][5]<- Le resto uno para subir una casilla verticalmente (COLUMNA)
	mov indiceinterno,3 ; casilla interna inicial ->[0][5] Le sumo uno para ir a la casila de la derecha (FILA)
	call revisarGanadorDiagonalJ2Mirror
	ret
sietepresionadoP endp

revisarGanadorVertical proc ; Este procedimiento revisa todas las columnas que hay en el tablero para ver si hay un ganador
	mov cl,indiceexterno
	mov contador1,0
	mov contador2,0
	
	mov al,cl		; indice interno [0][0]<-   (columna * fila) ; FILA
	mov bl,7		; 7 columnas (meto el numero de columnas porque es 7 x 6 y las columnas van a ir primero en mi caso)
	mul bl			; 
	add ax,indiceinterno		; indice externo ->[0][0] ; COLUMNA
	shl ax,1		; multiplicamos por el tamano del dato de cada celda
	mov si,ax
cicloRevisarVertical1:
	cmp word ptr connectfour[si],0 ; si la primera celda de esta columna es un cero entonces esta vacia y no hay ganador en esa columna
	jne saltoConejoVertical
	jmp terminarRevisarGanadorVertical
saltoConejoVertical:
	mov al,cl		; indice interno [0][0]<-   (columna * fila)
	mov bl,7		; 7 columnas (meto el numero de columnas porque es 7 x 6 y las columnas van a ir primero en mi caso)
	mul bl			; 
	add ax,indiceinterno		; indice externo ->[0][0]
	shl ax,1		; multiplicamos por el tamano del dato de cada celda
	mov si,ax
	
	cmp word ptr connectfour[si],1 ; se revisa si hay un 1 en la celda de arriba y se sigue el ciclo hasta que ya no haya un 1
	jne revisarElDeArribaVertical
	inc contador1
	dec cl
	jmp continuarRevisarVertical1
revisarElDeArribaVertical:
	push cx 
	dec cl ; revisa las celdas de arriba para ver si hay 1
	mov al,cl		; indice interno [0][0]<-   (columna * fila)
	mov bl,7		; 7 columnas (meto el numero de columnas porque es 7 x 6 y las columnas van a ir primero en mi caso)
	mul bl			; 
	add ax,indiceinterno		; indice externo ->[0][0]
	shl ax,1		; multiplicamos por el tamano del dato de cada celda
	mov si,ax
	
	cmp word ptr connectfour[si],1 
	jne cicloRevisarVertical2p1
	dec contador1
	pop cx
	dec cl
	jmp continuarRevisarVertical1
continuarRevisarVertical1:
	cmp contador1,4 ; si se encontro 4 1s entonces se encontro el ganador
	je ganadorEncontradoJ1Vertical
	
	jmp cicloRevisarVertical1
cicloRevisarVertical2p1:
	pop cx ; si encontro un 2 entonces se repite el proceso pero para el jugador 2
	inc contador2
cicloRevisarVertical2p2:
	cmp word ptr connectfour[si],0
	je terminarRevisarGanadorVertical
	
	mov al,cl		; indice externo [0][0]<-   (columna * fila)
	mov bl,7		; 7 columnas (meto el numero de columnas porque es 7 x 6 y las columnas van a ir primero en mi caso)
	mul bl			; 
	add ax,indiceinterno		; indice interno ->[0][0]
	shl ax,1		; multiplicamos por el tamano del dato de cada celda
	mov si,ax
	
	cmp word ptr connectfour[si],2
	jne terminarRevisarGanadorVertical
	
	cmp contador2,4
	je ganadorEncontradoJ2Vertical
	inc contador2
	dec cl
	jmp cicloRevisarVertical2p2
ganadorEncontradoJ1Vertical: ; despliega que el ganador fue el 1
	lea dx,msgganador1
	mov ah,9h
	int 21h
	
	mov ah,4ch ; Hace interrupt para hacer exit hacia DOS para terminar el programa
	int 21h
ganadorEncontradoJ2Vertical:
	lea dx,msgganador2 ; despliega que el ganador fue el 2
	mov ah,9h
	int 21h
	
	mov ah,4ch ; Hace interrupt para hacer exit hacia DOS para terminar el programa
	int 21h
terminarRevisarGanadorVertical:
	ret
revisarGanadorVertical endP

revisarGanadorHorizontal proc ; este procedimiento revisa todas las filas para ver si hay un ganador
inicioRevisarHorizontal:
	mov cl,indiceexterno
	mov dx,indiceinterno
	mov contador1,0
	mov contador2,0
	
	mov al,cl		; indice interno [0][5]<-   (columna * fila) ; FILA
	mov bl,7		; 7 columnas (meto el numero de columnas porque es 7 x 6 y las columnas van a ir primero en mi caso)
	mul bl			; 
	add ax,dx		; indice externo ->[0][5] ; COLUMNA
	shl ax,1		; multiplicamos por el tamano del dato de cada celda
	mov si,ax
cicloRevisarHorizontal1:
	cmp word ptr connectfour[si],0 ; valida si el primer elemento es un cero
	jne saltoConejoHorizontal2
	jmp primeroEsCeroRevisar
primeroEsCeroRevisar:
	dec contador3
	cmp contador3,0
	jne primeroEsCeroRevisar2
	jmp terminarRevisarGanadorHorizontal
primeroEsCeroRevisar2:
	inc dx
	mov al,cl		; indice interno [0][0]<-   (columna * fila)
	mov bl,7		; 7 columnas (meto el numero de columnas porque es 7 x 6 y las columnas van a ir primero en mi caso)
	mul bl			; 
	add ax,dx		; indice externo ->[0][0]
	shl ax,1		; multiplicamos por el tamano del dato de cada celda
	mov si,ax
	
	cmp word ptr connectfour[si],0
	je primeroEsCeroRevisar
	jmp saltoConejoHorizontal2
saltoConejoHorizontal2:
	mov al,cl		; indice interno [0][5]<-   (columna * fila)
	mov bl,7		; 7 columnas (meto el numero de columnas porque es 7 x 6 y las columnas van a ir primero en mi caso)
	mul bl			; 
	add ax,dx		; indice externo ->[0][5]
	shl ax,1		; multiplicamos por el tamano del dato de cada celda
	mov si,ax
	
	cmp word ptr connectfour[si],1
	jne revisarElDeDerechaHorizontal
	inc contador1
	inc dx
	jmp continuarRevisarHorizontal1
revisarElDeDerechaHorizontal:
	push dx ; se revisa el elemento de la derecha incrementando la columna para ir a la derecha
	inc dx
	mov al,cl		; indice interno [0][0]<-   (columna * fila)
	mov bl,7		; 7 columnas (meto el numero de columnas porque es 7 x 6 y las columnas van a ir primero en mi caso)
	mul bl			; 
	add ax,dx		; indice externo ->[0][0]
	shl ax,1		; multiplicamos por el tamano del dato de cada celda
	mov si,ax
	
	cmp word ptr connectfour[si],1
	jne cicloRevisarHorizontal2p1
	dec contador1
	pop dx
	inc dx
	jmp continuarRevisarHorizontal1
continuarRevisarHorizontal1:
	cmp contador1,4
	je ganadorEncontradoJ1Horizontal
	
	jmp cicloRevisarHorizontal1
cicloRevisarHorizontal2p1:
	pop dx
	inc contador2
cicloRevisarHorizontal2p2: ; revisa las columnas de la derecha para ir horizontalmente pero para el jugador 2
	cmp word ptr connectfour[si],0
	je terminarRevisarGanadorHorizontal
	
	mov al,cl		; indice externo [0][0]<-   (columna * fila)
	mov bl,7		; 7 columnas (meto el numero de columnas porque es 7 x 6 y las columnas van a ir primero en mi caso)
	mul bl			; 
	add ax,dx		; indice interno ->[0][0]
	shl ax,1		; multiplicamos por el tamano del dato de cada celda
	mov si,ax
	
	cmp word ptr connectfour[si],2
	jne terminarRevisarGanadorHorizontal
	
	cmp contador2,4
	je ganadorEncontradoJ2Horizontal
	inc contador2
	inc dx
	jmp cicloRevisarHorizontal2p2
ganadorEncontradoJ1Horizontal: ; despliega mensajes de ganador y perdedor
	lea dx,msgganador1
	mov ah,9h
	int 21h
	
	mov ah,4ch ; Hace interrupt para hacer exit hacia DOS para terminar el programa
	int 21h
ganadorEncontradoJ2Horizontal:
	lea dx,msgganador2
	mov ah,9h
	int 21h
	
	mov ah,4ch ; Hace interrupt para hacer exit hacia DOS para terminar el programa
	int 21h
terminarRevisarGanadorHorizontal:
	ret
revisarGanadorHorizontal endP

revisarGanadorDiagonal proc ; se revisan las diagonales de la primera mitad del tablero para determinar si hubo un ganador, lo hace para el jugador 1
inicioRevisarDiagonal:
	mov cl,indiceexterno
	mov dx,indiceinterno
	mov contador1,0
	mov contador2,0
	
	mov al,cl		; indice interno [0][5]<-   (columna * fila) ; FILA
	mov bl,7		; 7 columnas (meto el numero de columnas porque es 7 x 6 y las columnas van a ir primero en mi caso)
	mul bl			; 
	add ax,dx		; indice externo ->[0][5] ; COLUMNA
	shl ax,1		; multiplicamos por el tamano del dato de cada celda
	mov si,ax
cicloRevisarDiagonal1:
	cmp word ptr connectfour[si],0 ; se valida si hay un cero de primero
	jne saltoConejoDiagonal2
	jmp primeroEsCeroRevisarDiagonal
primeroEsCeroRevisarDiagonal:
	dec contador3
	cmp contador3,0
	jne primeroEsCeroRevisarDiagonal2
	jmp terminarRevisarGanadorDiagonal
primeroEsCeroRevisarDiagonal2:
	inc dx	; indice interno [0][2] pasa a ser [1][3] luego [2][4] etc
	inc cl	; ; indice externo [0][2] pasa a ser [1][3] luego [2][4] etc
	
	mov al,cl		; indice interno [0][0]<-   (columna * fila)
	mov bl,7		; 7 columnas (meto el numero de columnas porque es 7 x 6 y las columnas van a ir primero en mi caso)
	mul bl			; 
	add ax,dx		; indice externo ->[0][0]
	shl ax,1		; multiplicamos por el tamano del dato de cada celda
	mov si,ax
	
	cmp word ptr connectfour[si],0
	je primeroEsCeroRevisarDiagonal
	jmp saltoConejoDiagonal2
saltoConejoDiagonal2:
	mov al,cl		; indice interno [0][5]<-   (columna * fila)
	mov bl,7		; 7 columnas (meto el numero de columnas porque es 7 x 6 y las columnas van a ir primero en mi caso)
	mul bl			; 
	add ax,dx		; indice externo ->[0][5]
	shl ax,1		; multiplicamos por el tamano del dato de cada celda
	mov si,ax
	
	cmp word ptr connectfour[si],1
	jne revisarElDeDerechaDiagonal ; empieza el proceso de revisar la digonal
	inc contador1
	inc dx
	inc cl
	jmp continuarRevisarDiagonal
revisarElDeDerechaDiagonal:
	push dx
	push cx ; va revisando las diagonales sumandole a cx y dx
	
	inc dx
	inc cx
	mov al,cl		; indice interno [0][0]<-   (columna * fila)
	mov bl,7		; 7 columnas (meto el numero de columnas porque es 7 x 6 y las columnas van a ir primero en mi caso)
	mul bl			; 
	add ax,dx		; indice externo ->[0][0]
	shl ax,1		; multiplicamos por el tamano del dato de cada celda
	mov si,ax
	
	cmp word ptr connectfour[si],1
	jne cicloRevisarDiagonal2p1
	dec contador1
	pop dx
	pop cx
	inc dx
	inc cx
	jmp continuarRevisarDiagonal
continuarRevisarDiagonal:
	cmp contador1,4 ; si encontro 4 seguidas del mismo numero entonces hay ganador
	je ganadorEncontradoJ1Diagonal
	
	jmp cicloRevisarDiagonal1
cicloRevisarDiagonal2p1:
	pop dx
	pop cx ; se resetean el cx y el dx
	inc contador2
cicloRevisarDiagonal2p2:
	cmp word ptr connectfour[si],0
	je terminarRevisarGanadorDiagonal
	
	mov al,cl		; indice externo [0][0]<-   (columna * fila)
	mov bl,7		; 7 columnas (meto el numero de columnas porque es 7 x 6 y las columnas van a ir primero en mi caso)
	mul bl			; 
	add ax,dx		; indice interno ->[0][0]
	shl ax,1		; multiplicamos por el tamano del dato de cada celda
	mov si,ax
	
	cmp word ptr connectfour[si],1 ; revisa las diagonales pero para el segundo jugador
	jne terminarRevisarGanadorDiagonal
	
	cmp contador1,4
	je ganadorEncontradoJ1Diagonal
	inc contador2
	inc dx
	inc cx
	jmp cicloRevisarDiagonal2p2 ; se mete en un ciclo para revisar las diagonales 
ganadorEncontradoJ1Diagonal:
	lea dx,msgganador1
	mov ah,9h
	int 21h
	
	mov ah,4ch ; Hace interrupt para hacer exit hacia DOS para terminar el programa
	int 21h
ganadorEncontradoJ2Diagonal:
	lea dx,msgganador2
	mov ah,9h
	int 21h
	
	mov ah,4ch ; Hace interrupt para hacer exit hacia DOS para terminar el programa
	int 21h
terminarRevisarGanadorDiagonal:
	ret
revisarGanadorDiagonal endP

revisarGanadorDiagonalJ2 proc ; hace lo mismo que el procedimiento de revisar las diagonales J1 pero lo hace para el segundo jugador
inicioRevisarDiagonalJ2:
	mov cl,indiceexterno
	mov dx,indiceinterno
	mov contador1,0
	mov contador2,0
	
	mov al,cl		; indice interno [0][5]<-   (columna * fila) ; FILA
	mov bl,7		; 7 columnas (meto el numero de columnas porque es 7 x 6 y las columnas van a ir primero en mi caso)
	mul bl			; 
	add ax,dx		; indice externo ->[0][5] ; COLUMNA
	shl ax,1		; multiplicamos por el tamano del dato de cada celda
	mov si,ax
cicloRevisarDiagonal1J2:
	cmp word ptr connectfour[si],0
	jne saltoConejoDiagonal2J2
	jmp primeroEsCeroRevisarDiagonalJ2
primeroEsCeroRevisarDiagonalJ2:
	dec contador3
	cmp contador3,0
	jne primeroEsCeroRevisarDiagonal2J2
	jmp terminarRevisarGanadorDiagonalJ2
primeroEsCeroRevisarDiagonal2J2:
	inc dx	; indice interno [0][2] pasa a ser [1][3] luego [2][4] etc
	inc cl	; ; indice externo [0][2] pasa a ser [1][3] luego [2][4] etc
	
	mov al,cl		; indice interno [0][0]<-   (columna * fila)
	mov bl,7		; 7 columnas (meto el numero de columnas porque es 7 x 6 y las columnas van a ir primero en mi caso)
	mul bl			; 
	add ax,dx		; indice externo ->[0][0]
	shl ax,1		; multiplicamos por el tamano del dato de cada celda
	mov si,ax
	
	cmp word ptr connectfour[si],0
	je primeroEsCeroRevisarDiagonalJ2
	jmp saltoConejoDiagonal2J2
saltoConejoDiagonal2J2:
	mov al,cl		; indice interno [0][5]<-   (columna * fila)
	mov bl,7		; 7 columnas (meto el numero de columnas porque es 7 x 6 y las columnas van a ir primero en mi caso)
	mul bl			; 
	add ax,dx		; indice externo ->[0][5]
	shl ax,1		; multiplicamos por el tamano del dato de cada celda
	mov si,ax
	
	cmp word ptr connectfour[si],2
	jne revisarElDeDerechaDiagonalJ2
	inc contador1
	inc dx
	inc cl
	jmp continuarRevisarDiagonalJ2
revisarElDeDerechaDiagonalJ2:
	push dx
	push cx
	
	inc dx
	inc cx
	mov al,cl		; indice interno [0][0]<-   (columna * fila)
	mov bl,7		; 7 columnas (meto el numero de columnas porque es 7 x 6 y las columnas van a ir primero en mi caso)
	mul bl			; 
	add ax,dx		; indice externo ->[0][0]
	shl ax,1		; multiplicamos por el tamano del dato de cada celda
	mov si,ax
	
	cmp word ptr connectfour[si],2
	jne cicloRevisarDiagonal2p1J2
	dec contador1
	pop dx
	pop cx
	inc dx
	inc cx
	jmp continuarRevisarDiagonal
continuarRevisarDiagonalJ2:
	cmp contador1,4
	je ganadorEncontradoJ2DiagonalJ2
	
	jmp cicloRevisarDiagonal1J2
cicloRevisarDiagonal2p1J2:
	pop dx
	pop cx
	inc contador2
cicloRevisarDiagonal2p2J2:
	cmp word ptr connectfour[si],0
	je terminarRevisarGanadorDiagonalJ2
	
	mov al,cl		; indice externo [0][0]<-   (columna * fila)
	mov bl,7		; 7 columnas (meto el numero de columnas porque es 7 x 6 y las columnas van a ir primero en mi caso)
	mul bl			; 
	add ax,dx		; indice interno ->[0][0]
	shl ax,1		; multiplicamos por el tamano del dato de cada celda
	mov si,ax
	
	cmp word ptr connectfour[si],2
	jne terminarRevisarGanadorDiagonalJ2
	
	cmp contador1,4
	je ganadorEncontradoJ2DiagonalJ2
	inc contador2
	inc dx
	inc cx
	jmp cicloRevisarDiagonal2p2J2
ganadorEncontradoJ2DiagonalJ2:
	lea dx,msgganador2
	mov ah,9h
	int 21h
	
	mov ah,4ch ; Hace interrupt para hacer exit hacia DOS para terminar el programa
	int 21h
terminarRevisarGanadorDiagonalJ2:
	ret
revisarGanadorDiagonalJ2 endP

revisarGanadorDiagonalMirror proc ; este procedimiento revisa la otra mitad de las diagonales para terminar de revisar todo el tablero, lo hace para el primer jugador
inicioRevisarDiagonalMirror:
	mov cl,indiceexterno
	mov dx,indiceinterno
	mov contador1,0
	mov contador2,0
	
	mov al,cl		; indice interno [0][5]<-   (columna * fila) ; FILA
	mov bl,7		; 7 columnas (meto el numero de columnas porque es 7 x 6 y las columnas van a ir primero en mi caso)
	mul bl			; 
	add ax,dx		; indice externo ->[0][5] ; COLUMNA
	shl ax,1		; multiplicamos por el tamano del dato de cada celda
	mov si,ax
cicloRevisarDiagonal1Mirror:
	cmp word ptr connectfour[si],0
	jne saltoConejoDiagonal2Mirror
	jmp primeroEsCeroRevisarDiagonalMirror
primeroEsCeroRevisarDiagonalMirror:
	dec contador3
	cmp contador3,0
	jne primeroEsCeroRevisarDiagonal2Mirror
	jmp terminarRevisarGanadorDiagonalMirror
primeroEsCeroRevisarDiagonal2Mirror:
	dec dx	; indice interno [5][2] pasa a ser [4][3] luego [3][4] etc
	inc cl	; ; indice externo [5][2] pasa a ser [4][3] luego [3][4] etc
	
	mov al,cl		; indice interno [0][0]<-   (columna * fila)
	mov bl,7		; 7 columnas (meto el numero de columnas porque es 7 x 6 y las columnas van a ir primero en mi caso)
	mul bl			; 
	add ax,dx		; indice externo ->[0][0]
	shl ax,1		; multiplicamos por el tamano del dato de cada celda
	mov si,ax
	
	cmp word ptr connectfour[si],0
	je primeroEsCeroRevisarDiagonalMirror
	jmp saltoConejoDiagonal2Mirror
saltoConejoDiagonal2Mirror:
	mov al,cl		; indice interno [0][5]<-   (columna * fila)
	mov bl,7		; 7 columnas (meto el numero de columnas porque es 7 x 6 y las columnas van a ir primero en mi caso)
	mul bl			; 
	add ax,dx		; indice externo ->[0][5]
	shl ax,1		; multiplicamos por el tamano del dato de cada celda
	mov si,ax
	
	cmp word ptr connectfour[si],1
	jne revisarElDeDerechaDiagonalMirror
	inc contador1
	dec dx
	inc cl
	jmp continuarRevisarDiagonalMirror
revisarElDeDerechaDiagonalMirror:
	push dx
	push cx
	
	dec dx
	inc cx
	mov al,cl		; indice interno [0][0]<-   (columna * fila)
	mov bl,7		; 7 columnas (meto el numero de columnas porque es 7 x 6 y las columnas van a ir primero en mi caso)
	mul bl			; 
	add ax,dx		; indice externo ->[0][0]
	shl ax,1		; multiplicamos por el tamano del dato de cada celda
	mov si,ax
	
	cmp word ptr connectfour[si],1
	jne cicloRevisarDiagonal2p1Mirror
	dec contador1
	pop dx
	pop cx
	dec dx
	inc cx
	jmp continuarRevisarDiagonalMirror
continuarRevisarDiagonalMirror:
	cmp contador1,4
	je ganadorEncontradoJ1DiagonalMirror
	
	jmp cicloRevisarDiagonal1Mirror
cicloRevisarDiagonal2p1Mirror:
	pop dx
	pop cx
	inc contador2
cicloRevisarDiagonal2p2Mirror:
	cmp word ptr connectfour[si],0
	je terminarRevisarGanadorDiagonalMirror
	
	mov al,cl		; indice externo [0][0]<-   (columna * fila)
	mov bl,7		; 7 columnas (meto el numero de columnas porque es 7 x 6 y las columnas van a ir primero en mi caso)
	mul bl			; 
	add ax,dx		; indice interno ->[0][0]
	shl ax,1		; multiplicamos por el tamano del dato de cada celda
	mov si,ax
	
	cmp word ptr connectfour[si],1
	jne terminarRevisarGanadorDiagonalMirror
	
	cmp contador1,4
	je ganadorEncontradoJ1DiagonalMirror
	inc contador2
	dec dx
	inc cx
	jmp cicloRevisarDiagonal2p2Mirror
ganadorEncontradoJ1DiagonalMirror:
	lea dx,msgganador1
	mov ah,9h
	int 21h
	
	mov ah,4ch ; Hace interrupt para hacer exit hacia DOS para terminar el programa
	int 21h
ganadorEncontradoJ2DiagonalMirror:
	lea dx,msgganador2
	mov ah,9h
	int 21h
	
	mov ah,4ch ; Hace interrupt para hacer exit hacia DOS para terminar el programa
	int 21h
terminarRevisarGanadorDiagonalMirror:
	ret
revisarGanadorDiagonalMirror endP

revisarGanadorDiagonalJ2Mirror proc ; revisa la segunda mitad de las diagonales pero para el segundo jugador
inicioRevisarDiagonalJ2Mirror:
	mov cl,indiceexterno
	mov dx,indiceinterno
	mov contador1,0
	mov contador2,0
	
	mov al,cl		; indice interno [0][5]<-   (columna * fila) ; FILA
	mov bl,7		; 7 columnas (meto el numero de columnas porque es 7 x 6 y las columnas van a ir primero en mi caso)
	mul bl			; 
	add ax,dx		; indice externo ->[0][5] ; COLUMNA
	shl ax,1		; multiplicamos por el tamano del dato de cada celda
	mov si,ax
cicloRevisarDiagonal1J2Mirror:
	cmp word ptr connectfour[si],0
	jne saltoConejoDiagonal2J2Mirror
	jmp primeroEsCeroRevisarDiagonalJ2Mirror
primeroEsCeroRevisarDiagonalJ2Mirror:
	dec contador3
	cmp contador3,0
	jne primeroEsCeroRevisarDiagonal2J2Mirror
	jmp terminarRevisarGanadorDiagonalJ2Mirror
primeroEsCeroRevisarDiagonal2J2Mirror:
	dec dx	; indice interno [0][2] pasa a ser [1][3] luego [2][4] etc
	inc cl	; ; indice externo [0][2] pasa a ser [1][3] luego [2][4] etc
	
	mov al,cl		; indice interno [0][0]<-   (columna * fila)
	mov bl,7		; 7 columnas (meto el numero de columnas porque es 7 x 6 y las columnas van a ir primero en mi caso)
	mul bl			; 
	add ax,dx		; indice externo ->[0][0]
	shl ax,1		; multiplicamos por el tamano del dato de cada celda
	mov si,ax
	
	cmp word ptr connectfour[si],0
	je primeroEsCeroRevisarDiagonalJ2Mirror
	jmp saltoConejoDiagonal2J2Mirror
saltoConejoDiagonal2J2Mirror:
	mov al,cl		; indice interno [0][5]<-   (columna * fila)
	mov bl,7		; 7 columnas (meto el numero de columnas porque es 7 x 6 y las columnas van a ir primero en mi caso)
	mul bl			; 
	add ax,dx		; indice externo ->[0][5]
	shl ax,1		; multiplicamos por el tamano del dato de cada celda
	mov si,ax
	
	cmp word ptr connectfour[si],2
	jne revisarElDeDerechaDiagonalJ2Mirror
	inc contador1
	dec dx
	inc cl
	jmp continuarRevisarDiagonalJ2Mirror
revisarElDeDerechaDiagonalJ2Mirror:
	push dx
	push cx
	
	dec dx
	inc cx
	mov al,cl		; indice interno [0][0]<-   (columna * fila)
	mov bl,7		; 7 columnas (meto el numero de columnas porque es 7 x 6 y las columnas van a ir primero en mi caso)
	mul bl			; 
	add ax,dx		; indice externo ->[0][0]
	shl ax,1		; multiplicamos por el tamano del dato de cada celda
	mov si,ax
	
	cmp word ptr connectfour[si],2
	jne cicloRevisarDiagonal2p1J2Mirror
	dec contador1
	pop dx
	pop cx
	dec dx
	inc cx
	jmp continuarRevisarDiagonalMirror
continuarRevisarDiagonalJ2Mirror:
	cmp contador1,4
	je ganadorEncontradoJ2DiagonalJ2Mirror
	
	jmp cicloRevisarDiagonal1J2Mirror
cicloRevisarDiagonal2p1J2Mirror:
	pop dx
	pop cx
	inc contador2
cicloRevisarDiagonal2p2J2Mirror:
	cmp word ptr connectfour[si],0
	je terminarRevisarGanadorDiagonalJ2Mirror
	
	mov al,cl		; indice externo [0][0]<-   (columna * fila)
	mov bl,7		; 7 columnas (meto el numero de columnas porque es 7 x 6 y las columnas van a ir primero en mi caso)
	mul bl			; 
	add ax,dx		; indice interno ->[0][0]
	shl ax,1		; multiplicamos por el tamano del dato de cada celda
	mov si,ax
	
	cmp word ptr connectfour[si],2
	jne terminarRevisarGanadorDiagonalJ2Mirror
	
	cmp contador1,4
	je ganadorEncontradoJ2DiagonalJ2Mirror
	inc contador2
	dec dx
	inc cx
	jmp cicloRevisarDiagonal2p2J2Mirror
ganadorEncontradoJ2DiagonalJ2Mirror:
	lea dx,msgganador2
	mov ah,9h
	int 21h
	
	mov ah,4ch ; Hace interrupt para hacer exit hacia DOS para terminar el programa
	int 21h
terminarRevisarGanadorDiagonalJ2Mirror:
	ret
revisarGanadorDiagonalJ2Mirror endP

F5presionadoP proc ; en este procedimiento se genera un movimiento aleatorio al presionar el boton F5, lo hace mediante con un interrupt conseguir la cantidad de tics y lo usa como semilla para generar la jugada
generarNumero:
   mov ah, 00h  ; hace un interrupt para conseguir el tiempo del sistema    
   int 1ah      ; CX:DX ahora tiene la cantidad de tics de reloj desde la medianoche   

   mov  ax, dx
   xor  dx, dx
   mov  cx, 10    
   div  cx       ; dx contiene el residuo de la division de 0 a 9
   
   cmp dl,7 ; como solo hay 7 columnas, si salio 8 o 9 entonces se repite el proceso hasta que salga un numero que sea 7 o menos
   jg generarNumero
   cmp dl,0 ; como no hay columna 0 ya que a la primera se le llamo columna 1, si sale un 0 entonces se repite hasta que salga un numero entre 1 y 7 incluyendolos
   je generarNumero

   add  dl, '0'  ; convierte el numero generado a ascii y lo mueve a la variable moverandom para usarla para seleccionar una columna en la que poner esta variable
   mov moverandom,dl
terminarF5presionadoP:
	ret
F5presionadoP endP

VpresionadoP Proc ; Este procedimiento voltea el tablero, va cambiando los numeros de posicion de afuera hacia adentro
	mov cl,indiceexterno
	mov contadorvoltear,0
inicioCicloVoltear:
	mov al,cl		; indice interno [0][0]<-   (columna * fila) ; FILA
	mov bl,7		; 7 columnas (meto el numero de columnas porque es 7 x 6 y las columnas van a ir primero en mi caso)
	mul bl			; 
	add ax,indiceinterno		; indice externo ->[0][0] ; COLUMNA
	shl ax,1		; multiplicamos por el tamano del dato de cada celda
	mov si,ax
	
	push si ; se guarda el contenido del registro si
	
	mov dx,word ptr connectfour[si] ; se mueve el contenido del si a dx y se guarda en voltear 1
	mov voltear1,dx
getNumerosVoltear:
	mov al,cl		; indice externo ->[5][0]   (fila*columna)
	mov bl,7		; 7 columnas (meto el numero de columnas porque es 7 x 6 y las columnas van a ir primero en mi caso)
	mul bl			; 
	add ax,indiceinterno		; indice interno [5][0]<-
	shl ax,1		; multiplicamos por el tamano del dato de cada celda
	mov si,ax
	
	cmp word ptr connectfour[si],0 ; si hay un cero entonces ya se encontraron todos los numeros de la columna
	je empezarVoltear
	cmp contadorvoltear,6 ; si hay 6 numeros como no hay un cero que revisar arriba del sexto numero se empieza a voltearlos sin revisar si hay un cero
	je empezarVoltear
	inc contadorvoltear
	dec cl ; se va subiendo en la columna
	jmp getNumerosVoltear
empezarVoltear:
	cmp contadorvoltear,0 ; si no habia numeros entonces se termina el procedimiento
	jne saltoConejoVoltear
	jmp mostrarTableroVolteado
saltoConejoVoltear:
	inc cl ; se devuelve una celda en la columna para pasar de donde esta el cero a donde esta el ultimo numero de la columna
	
	mov al,cl		; indice interno [0][0]<-   (columna * fila) ; FILA
	mov bl,7		; 7 columnas (meto el numero de columnas porque es 7 x 6 y las columnas van a ir primero en mi caso)
	mul bl			; 
	add ax,indiceinterno		; indice externo ->[0][0] ; COLUMNA
	shl ax,1		; multiplicamos por el tamano del dato de cada celda
	mov si,ax
	
	mov dx,word ptr connectfour[si] ; se cambia la posicion del numero de mas arriba con el de mas abajo
	mov voltear2,dx
	
	mov dx,voltear1
	mov word ptr connectfour[si],dx
	dec contadorvoltear ; se va decrementando el contador de voltear hasta que sea cero
	
	cmp contadorvoltear,0
	je mostrarTableroVolteado
	
	pop si ; se devuelve al primer numero de la columna
	
	mov dx,voltear2
	mov word ptr connectfour[si],dx ; se voltea el primero de la columna
	dec contadorvoltear
	
	cmp contadorvoltear,1 ; si habia solo 3 numeros ya se termina porque el de en medio no se voltea
	je mostrarTableroVolteado
	
	jmp empezarVoltear2
empezarVoltear2:
	cmp contadorvoltear,0 ; si ya no hay mas numeros que voltear termina
	je mostrarTableroVolteado
	
	inc cl ; baja un numero en la columna
	
	mov al,cl		; indice interno [0][0]<-   (columna * fila) ; FILA
	mov bl,7		; 7 columnas (meto el numero de columnas porque es 7 x 6 y las columnas van a ir primero en mi caso)
	mul bl			; 
	add ax,indiceinterno		; indice externo ->[0][0] ; COLUMNA
	shl ax,1		; multiplicamos por el tamano del dato de cada celda
	mov si,ax
	
	push si ; se guarda la posicion de si actual
	
	mov dx,word ptr connectfour[si] ; se guarda el primer numero a voltear
	mov voltear1,dx
	
	inc cl ; se baja una celda en la columna
	
	mov al,cl		; indice interno [0][0]<-   (columna * fila) ; FILA
	mov bl,7		; 7 columnas (meto el numero de columnas porque es 7 x 6 y las columnas van a ir primero en mi caso)
	mul bl			; 
	add ax,indiceinterno		; indice externo ->[0][0] ; COLUMNA
	shl ax,1		; multiplicamos por el tamano del dato de cada celda
	mov si,ax
	
	mov dx,word ptr connectfour[si]
	mov voltear2,dx
	
	mov dx,voltear1
	mov word ptr connectfour[si],dx ; se voltean los dos numeros de nuevo
	dec contadorvoltear
	
	pop si ; se devuelve el si
	
	mov dx,voltear2
	mov word ptr connectfour[si],dx
	dec contadorvoltear
	
	cmp contadorvoltear,1 ; si habian cinco numeros entonces ya termina porque uno sobra y no se voltea
	je mostrarTableroVolteado
	
	jmp empezarVoltear2
mostrarTableroVolteado: ; se revisa el tablero volteado vertical, horizontal y diagonalmente para ver si al voltear el tablero hay un ganador
	mov indiceexterno,5 ; casilla externa inicial   [0][5]<-
	mov indiceinterno,0 ; casilla interna inicial ->[0][5]
	call revisarGanadorVertical
	mov indiceexterno,5 ; casilla externa inicial   [0][5]<-
	mov indiceinterno,1 ; casilla interna inicial ->[0][5]
	call revisarGanadorVertical
	mov indiceexterno,5 ; casilla externa inicial   [0][5]<-
	mov indiceinterno,2 ; casilla interna inicial ->[0][5]
	call revisarGanadorVertical
	mov indiceexterno,5 ; casilla externa inicial   [0][5]<-
	mov indiceinterno,3 ; casilla interna inicial ->[0][5]
	call revisarGanadorVertical
	mov indiceexterno,5 ; casilla externa inicial   [0][5]<-
	mov indiceinterno,4 ; casilla interna inicial ->[0][5]
	call revisarGanadorVertical
	mov indiceexterno,5 ; casilla externa inicial   [0][5]<-
	mov indiceinterno,5 ; casilla interna inicial ->[0][5]
	call revisarGanadorVertical
	mov indiceexterno,5 ; casilla externa inicial   [0][5]<-
	mov indiceinterno,6 ; casilla interna inicial ->[0][5]
	call revisarGanadorVertical
	mov indiceexterno,5 ; casilla externa inicial   [0][5]<- Le resto uno para subir una casilla verticalmente (FILA)
	mov indiceinterno,0 ; casilla interna inicial ->[0][5] Le sumo uno para ir a la casila de la derecha (COLUMNA)
	call revisarGanadorHorizontal
	mov indiceexterno,4 ; casilla externa inicial   [0][5]<- Le resto uno para subir una casilla verticalmente (COLUMNA)
	mov indiceinterno,0 ; casilla interna inicial ->[0][5] Le sumo uno para ir a la casila de la derecha (FILA)
	call revisarGanadorHorizontal
	mov indiceexterno,3 ; casilla externa inicial   [0][5]<- Le resto uno para subir una casilla verticalmente (COLUMNA)
	mov indiceinterno,0 ; casilla interna inicial ->[0][5] Le sumo uno para ir a la casila de la derecha (FILA)
	call revisarGanadorHorizontal
	mov indiceexterno,2 ; casilla externa inicial   [0][5]<- Le resto uno para subir una casilla verticalmente (COLUMNA)
	mov indiceinterno,0 ; casilla interna inicial ->[0][5] Le sumo uno para ir a la casila de la derecha (FILA)
	call revisarGanadorHorizontal
	mov indiceexterno,1 ; casilla externa inicial   [0][5]<- Le resto uno para subir una casilla verticalmente (COLUMNA)
	mov indiceinterno,0 ; casilla interna inicial ->[0][5] Le sumo uno para ir a la casila de la derecha (FILA)
	call revisarGanadorHorizontal
	mov indiceexterno,0 ; casilla externa inicial   [0][5]<- Le resto uno para subir una casilla verticalmente (COLUMNA)
	mov indiceinterno,0 ; casilla interna inicial ->[0][5] Le sumo uno para ir a la casila de la derecha (FILA)
	call revisarGanadorHorizontal
	mov indiceexterno,2 ; casilla externa inicial   [0][5]<- Le resto uno para subir una casilla verticalmente (COLUMNA)
	mov indiceinterno,0 ; casilla interna inicial ->[0][5] Le sumo uno para ir a la casila de la derecha (FILA)
	call revisarGanadorDiagonal
	mov indiceexterno,1 ; casilla externa inicial   [0][5]<- Le resto uno para subir una casilla verticalmente (COLUMNA)
	mov indiceinterno,0 ; casilla interna inicial ->[0][5] Le sumo uno para ir a la casila de la derecha (FILA)
	call revisarGanadorDiagonal
	mov indiceexterno,0 ; casilla externa inicial   [0][5]<- Le resto uno para subir una casilla verticalmente (COLUMNA)
	mov indiceinterno,0 ; casilla interna inicial ->[0][5] Le sumo uno para ir a la casila de la derecha (FILA)
	call revisarGanadorDiagonal
	mov indiceexterno,0 ; casilla externa inicial   [0][5]<- Le resto uno para subir una casilla verticalmente (COLUMNA)
	mov indiceinterno,1 ; casilla interna inicial ->[0][5] Le sumo uno para ir a la casila de la derecha (FILA)
	call revisarGanadorDiagonal
	mov indiceexterno,0 ; casilla externa inicial   [0][5]<- Le resto uno para subir una casilla verticalmente (COLUMNA)
	mov indiceinterno,2 ; casilla interna inicial ->[0][5] Le sumo uno para ir a la casila de la derecha (FILA)
	call revisarGanadorDiagonal
	mov indiceexterno,2 ; casilla externa inicial   [0][5]<- Le resto uno para subir una casilla verticalmente (COLUMNA)
	mov indiceinterno,0 ; casilla interna inicial ->[0][5] Le sumo uno para ir a la casila de la derecha (FILA)
	call revisarGanadorDiagonalJ2
	mov indiceexterno,1 ; casilla externa inicial   [0][5]<- Le resto uno para subir una casilla verticalmente (COLUMNA)
	mov indiceinterno,0 ; casilla interna inicial ->[0][5] Le sumo uno para ir a la casila de la derecha (FILA)
	call revisarGanadorDiagonalJ2
	mov indiceexterno,0 ; casilla externa inicial   [0][5]<- Le resto uno para subir una casilla verticalmente (COLUMNA)
	mov indiceinterno,0 ; casilla interna inicial ->[0][5] Le sumo uno para ir a la casila de la derecha (FILA)
	call revisarGanadorDiagonalJ2
	mov indiceexterno,0 ; casilla externa inicial   [0][5]<- Le resto uno para subir una casilla verticalmente (COLUMNA)
	mov indiceinterno,1 ; casilla interna inicial ->[0][5] Le sumo uno para ir a la casila de la derecha (FILA)
	call revisarGanadorDiagonalJ2
	mov indiceexterno,0 ; casilla externa inicial   [0][5]<- Le resto uno para subir una casilla verticalmente (COLUMNA)
	mov indiceinterno,2 ; casilla interna inicial ->[0][5] Le sumo uno para ir a la casila de la derecha (FILA)
	call revisarGanadorDiagonalJ2
terminarVpresionadoP:
	ret
VpresionadoP endp
	
main:

	mov ax,ds ; Se mueve ds a es
	mov es,ax

	mov ax,data ; Inicializa el data segment mandandolo al ds register
	mov ds,ax

	mov ax,pila ; Inicializa la pila mandandola al ss register
	mov ss,ax
	
	mov dl, filas				; se mueven las filas y columnas a dl y dh, se hace load address al si a la matriz connectfour y su clon al di
	mov dh, columnas		
	lea si, connectfour
	lea di, clonconnectfour
	
	call printMat2D			; Muestra el tablero inicial	
	call println
	call println
	
jugador1Ciclo: ; Empieza el turno del jugador 1
	mov jugadoractual,1 ; El jugador actual es el 1
	
	lea dx, msgjugador1 ; Se despliega "Jugador 1?"
	mov ah,9h
	int 21h

	mov ah,00h ; Se le pide un movimiento al jugador 1
    int 16h
	
	cmp al,'V' ; Si se presiona v o V se voltea el tablero
	je VpresionadoJ1
	
	cmp al,'v'
	je VpresionadoJ1
	
	jmp seguirMainMenuJ1
VpresionadoJ1: ; Se voltea cada columna del tablero usando el procedimiento de voltear
	mov indiceexterno,5 ; casilla externa inicial   ->[5][0] Le resto uno para subir una casilla verticalmente (FILA) (fila x columna)
	mov indiceinterno,0 ; casilla interna inicial [5][0]<- Le sumo uno para ir a la casila de la derecha (COLUMNA)
	call VpresionadoP
	mov indiceexterno,5 ; casilla externa inicial   ->[5][0] Le resto uno para subir una casilla verticalmente (FILA) (fila x columna)
	mov indiceinterno,1 ; casilla interna inicial [5][0]<- Le sumo uno para ir a la casila de la derecha (COLUMNA)
	call VpresionadoP
	mov indiceexterno,5 ; casilla externa inicial   ->[5][0] Le resto uno para subir una casilla verticalmente (FILA) (fila x columna)
	mov indiceinterno,2 ; casilla interna inicial [5][0]<- Le sumo uno para ir a la casila de la derecha (COLUMNA)
	call VpresionadoP
	mov indiceexterno,5 ; casilla externa inicial   ->[5][0] Le resto uno para subir una casilla verticalmente (FILA) (fila x columna)
	mov indiceinterno,3 ; casilla interna inicial [5][0]<- Le sumo uno para ir a la casila de la derecha (COLUMNA)
	call VpresionadoP
	mov indiceexterno,5 ; casilla externa inicial   ->[5][0] Le resto uno para subir una casilla verticalmente (FILA) (fila x columna)
	mov indiceinterno,4 ; casilla interna inicial [5][0]<- Le sumo uno para ir a la casila de la derecha (COLUMNA)
	call VpresionadoP
	
	mov dl, filas			
	mov dh, columnas		
	lea si, connectfour
	lea di, clonconnectfour ; Se despliega el tablero volteado
	
	call printMat2D			
	call println
	call println
	jmp jugador2Ciclo
seguirMainMenuJ1:
	cmp al, 27 ; Si se presiona ESC, va al procedimiento de salirse
    je ESCpresionado
	
	cmp al,'1' ; Si se presiono una tecla del 1-7, va al procedimiento de meter una ficha en una de estas columnas respectivamente
	je unopresionadoJ1
	
	cmp al,'2'
	je dospresionadoJ1
	
	cmp al,'3'
	je trespresionadoJ1
	
	cmp al,'4'
	je cuatropresionadoJ1
	
	cmp al,'5'
	je cincopresionadoJ1
	
	cmp al,'6'
	je seispresionadoJ1
	
	cmp al,'7'
	je sietepresionadoJ1
	
	cmp ah, 3bh ; Si se presiono F1, se despliega la ayuda del programa
    je F1presionado
	
	cmp al,'A' ; Si se presiono A o a, se despliega el acerca de del programa
	je Apresionado
	
	cmp al,'a'
	je Apresionado
	
	cmp ah, 3fh ; Si no se presiono F5, entonces no se presiono ninguna tecla valida y da error. Si si se presiono F5 va al procedimiento de generar un movimiento aleatorio.
	jne darError
	
	jmp F5presionado

ESCpresionado: ; Despliega el mensaje de ESC presionado y termina el programa.
	call ESCpresionadoP
	mov ah,4ch ; Hace interrupt para hacer exit hacia DOS para terminar el programa
	int 21h

F1presionado: ; Despliega el mensaje de F1 presionado y empieza el turno del jugador 2
	call F1presionadoP
	jmp jugador2Ciclo

Apresionado: ; Despliega el mensaje de A presionado y empieza el turno del jugador 2
	call ApresionadoP
	jmp jugador2Ciclo

darError: ; Despliega un mensaje de error ya que no se presiono una tecla valida y da error
	call darErrorP
	mov ah,4ch ; Hace interrupt para hacer exit hacia DOS para terminar el programa
	int 21h

unopresionadoJ1: ; Llama el procedimiento de insertar una ficha en la columna pedida, lo hace para el jugador 1
	call unopresionadoP
	jmp jugador2Ciclo
dospresionadoJ1:
	call dospresionadoP
	jmp jugador2Ciclo
trespresionadoJ1:
	call trespresionadoP
	jmp jugador2Ciclo
cuatropresionadoJ1:
	call cuatropresionadoP
	jmp jugador2Ciclo
cincopresionadoJ1:
	call cincopresionadoP
	jmp jugador2Ciclo
seispresionadoJ1:
	call seispresionadoP
	jmp jugador2Ciclo
sietepresionadoJ1:
	call sietepresionadoP
	jmp jugador2Ciclo
	
jugador2Ciclo: ; Empieza el turno del jugador 2
	mov jugadoractual,2 ; El jugador actual es el 2
	
	lea dx, msgjugador2 ; Despliega "Jugador 2?"
	mov ah,9h
	int 21h

	mov ah,00h ; Pide la tecla al jugador 2
    int 16h
	
	cmp al,'V' ; Hace todo lo mismo que en el turno del juador 1 pero lo hace para el jugador 2
	je VpresionadoJ2
	
	cmp al,'v'
	je VpresionadoJ2
	
	jmp seguirMainMenuJ2
VpresionadoJ2:
	mov indiceexterno,5 ; casilla externa inicial   ->[5][0] Le resto uno para subir una casilla verticalmente (FILA) (fila x columna)
	mov indiceinterno,0 ; casilla interna inicial [5][0]<- Le sumo uno para ir a la casila de la derecha (COLUMNA)
	call VpresionadoP
	mov indiceexterno,5 ; casilla externa inicial   ->[5][0] Le resto uno para subir una casilla verticalmente (FILA) (fila x columna)
	mov indiceinterno,1 ; casilla interna inicial [5][0]<- Le sumo uno para ir a la casila de la derecha (COLUMNA)
	call VpresionadoP
	mov indiceexterno,5 ; casilla externa inicial   ->[5][0] Le resto uno para subir una casilla verticalmente (FILA) (fila x columna)
	mov indiceinterno,2 ; casilla interna inicial [5][0]<- Le sumo uno para ir a la casila de la derecha (COLUMNA)
	call VpresionadoP
	mov indiceexterno,5 ; casilla externa inicial   ->[5][0] Le resto uno para subir una casilla verticalmente (FILA) (fila x columna)
	mov indiceinterno,3 ; casilla interna inicial [5][0]<- Le sumo uno para ir a la casila de la derecha (COLUMNA)
	call VpresionadoP
	mov indiceexterno,5 ; casilla externa inicial   ->[5][0] Le resto uno para subir una casilla verticalmente (FILA) (fila x columna)
	mov indiceinterno,4 ; casilla interna inicial [5][0]<- Le sumo uno para ir a la casila de la derecha (COLUMNA)
	call VpresionadoP
	
	mov dl, filas			
	mov dh, columnas		
	lea si, connectfour
	lea di, clonconnectfour
	
	call printMat2D			; Tablero inicial	
	call println
	call println
	
	jmp jugador1Ciclo
seguirMainMenuJ2:
	
	cmp al, 27
    je ESCpresionadoJ2
	
	cmp ah, 3bh
    je F1presionadoJ2
	
	cmp ah, 3fh
	je F5presionado
	
	cmp al,'A'
	je ApresionadoJ2
	
	cmp al,'a'
	je ApresionadoJ2

	
	cmp al,'1'
	je unopresionadoJ2
	
	cmp al,'2'
	je dospresionadoJ2
	
	cmp al,'3'
	je trespresionadoJ2
	
	cmp al,'4'
	je cuatropresionadoJ2
	
	cmp al,'5'
	je cincopresionadoJ2
	
	cmp al,'6'
	je seispresionadoJ2
	
	cmp al,'7'
	je sietepresionadoJ2
	
	jmp darError

ESCpresionadoJ2:
	call ESCpresionadoP
	mov ah,4ch ; Hace interrupt para hacer exit hacia DOS para terminar el programa
	int 21h

F1presionadoJ2:
	call F1presionadoP
	jmp jugador1Ciclo

ApresionadoJ2:
	call ApresionadoP
	jmp jugador1Ciclo

unopresionadoJ2:
	call unopresionadoP
	jmp jugador1Ciclo
dospresionadoJ2:
	call dospresionadoP
	jmp jugador1Ciclo
trespresionadoJ2:
	call trespresionadoP
	jmp jugador1Ciclo
cuatropresionadoJ2:
	call cuatropresionadoP
	jmp jugador1Ciclo
cincopresionadoJ2:
	call cincopresionadoP
	jmp jugador1Ciclo
seispresionadoJ2:
	call seispresionadoP
	jmp jugador1Ciclo
sietepresionadoJ2:
	call sietepresionadoP
	jmp jugador1Ciclo

F5presionado: ; Si se presiono F5, entonces llama el procedimiento de generar un movimiento aleatorio, y esto genera un numero del 1 al 7. Luego dependiendo de que numero fue, inserta una ficha en la columna respectiva
	call F5presionadoP
	cmp jugadoractual,1
	je jugador1F5
	jmp jugador2F5
jugador1F5: ; Lo hace para el jugador 1 si este presiono F5
	cmp moverandom,'1'
	je unopresionadoJ1F5
	
	cmp moverandom,'2'
	je dospresionadoJ1F5
	
	cmp moverandom,'3'
	je trespresionadoJ1F5
	
	cmp moverandom,'4'
	je cuatropresionadoJ1F5
	
	cmp moverandom,'5'
	je cincopresionadoJ1F5
	
	cmp moverandom,'6'
	je seispresionadoJ1F5
	
	cmp moverandom,'7'
	je sietepresionadoJ1F5
unopresionadoJ1F5:
	call unopresionadoP
	jmp jugador2Ciclo
dospresionadoJ1F5:
	call dospresionadoP
	jmp jugador2Ciclo
trespresionadoJ1F5:
	call trespresionadoP
	jmp jugador2Ciclo
cuatropresionadoJ1F5:
	call cuatropresionadoP
	jmp jugador2Ciclo
cincopresionadoJ1F5:
	call cincopresionadoP
	jmp jugador2Ciclo
seispresionadoJ1F5:
	call seispresionadoP
	jmp jugador2Ciclo
sietepresionadoJ1F5:
	call sietepresionadoP
	jmp jugador2Ciclo
	
jugador2F5: ; Lo hace para el jugador 2 si este presiono F5.
	cmp moverandom,'1'
	je unopresionadoJ2F5
	
	cmp moverandom,'2'
	je dospresionadoJ2F5
	
	cmp moverandom,'3'
	je trespresionadoJ2F5
	
	cmp moverandom,'4'
	je cuatropresionadoJ2F5
	
	cmp moverandom,'5'
	je cincopresionadoJ2F5
	
	cmp moverandom,'6'
	je seispresionadoJ2F5
	
	cmp moverandom,'7'
	je sietepresionadoJ2F5
unopresionadoJ2F5:
	call unopresionadoP
	jmp jugador1Ciclo
dospresionadoJ2F5:
	call dospresionadoP
	jmp jugador1Ciclo
trespresionadoJ2F5:
	call trespresionadoP
	jmp jugador1Ciclo
cuatropresionadoJ2F5:
	call cuatropresionadoP
	jmp jugador1Ciclo
cincopresionadoJ2F5:
	call cincopresionadoP
	jmp jugador1Ciclo
seispresionadoJ2F5:
	call seispresionadoP
	jmp jugador1Ciclo
sietepresionadoJ2F5:
	call sietepresionadoP
	jmp jugador1Ciclo
	
terminarMain: ; Se termina el programa
	mov ah,4ch ; Hace interrupt para hacer exit hacia DOS para terminar el programa
	int 21h

code ends

end main