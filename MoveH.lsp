;;;
;;;    MoveH.lsp
;;;    Version 0.02
;;;    
;;;    Copyright 2003 by Pablo Saavedra.
;;;
;;;============================================================================
;;;
;;; Rutinas para manejar la posición Z de los objetos
;;;
;;;============================================================================
;;;
;;; Pendiente: Control de datos de entrada
;;;
;;;============================================================================


;;; -------------- FUNCION MOVEH ---------------------------
;;; Descripcion: Mueve objetos sin cambiar su coordinada Z.

(DEFUN C:MOVEH ()
  (SETVAR "cmdecho" 0)
  (COMMAND "_.ucs" "_world")

  (PROMPT "Selecciona objetos a desplazar: ")
  (SETQ ConjEntidad (SSGET)) (TERPRI)
  (SETQ PuntoBaseDespl (GETPOINT "Especifica punto base: ")) (TERPRI)
  (SETQ PuntoDestDespl (GETPOINT PuntoBaseDespl "Especifica punto destino: ")) (TERPRI)

  (SETQ PuntoDestDespl (LIST (CAR PuntoDestDespl) (CADR PuntoDestDespl) 0))             ;Ponemos la coordZ de punto destino = 0
  (SETQ PuntoBaseDespl (LIST (CAR PuntoBaseDespl) (CADR PuntoBaseDespl) 0))             ;Ponemos la coordZ de punto base    = 0

  (SETQ refnt0 (GETVAR "osmode")) (SETVAR "osmode" 0) 	                                ;Guardamos el valor de OSNAP y lo desactivamos

  (COMMAND "_.move" ConjEntidad "" PuntoBaseDespl PuntoDestDespl)                       ;Movemos los objetos seleccionados

  (SETVAR "osmode" refnt0)(SETVAR "cmdecho" 1)(PRIN1) 	                                ;recuperamos el valor de OSNAP y Echo On
                                                      	                                ;PRIN1 evita el "nil" de final de ejecucion
  (SETVAR "cmdecho" 1)
)

;;; -------------- FUNCION ADDH ------------------------------------------
;;; Descripcion: Cambia la coordinada Z de los objetos seleccionados
;;;              incrementando o decrementando el valor introducido.

(DEFUN C:ADDH ()
  (SETVAR "cmdecho" 0)
  (COMMAND "_.ucs" "_world")

  (PROMPT "Selecciona objetos a desplazar: ")
  (SETQ ConjEntidad (SSGET)) (TERPRI)
  (SETQ PuntoBaseDespl (LIST 0 0 0)) (TERPRI)
  (SETQ CantDespl (GETREAL "Introduce cambio de altura: ")) (TERPRI)
  (SETQ PuntoDestDespl (LIST 0 0 CantDespl))

  (SETQ refnt0 (GETVAR "osmode")) (SETVAR "osmode" 0) 	                                ;Guardamos el valor de OSNAP y lo desactivamos

  (COMMAND "_.move" ConjEntidad "" PuntoBaseDespl PuntoDestDespl)                       ;Movemos los objetos seleccionados

  (SETVAR "osmode" refnt0)(SETVAR "cmdecho" 1)(PRIN1) 	                                ;recuperamos el valor de OSNAP y Echo On
                                                      	                                ;PRIN1 evita el "nil" de final de ejecucion
  (SETVAR "cmdecho" 1)
)

;;; -------------- FUNCION SETH ----------------------------------------
;;; Descripcion: Cambia la coordinada Z de los objetos seleccionados
;;;              basandose en un punto de referencia.

(DEFUN C:SETH()
  (SETVAR "cmdecho" 0)
  (COMMAND "_.ucs" "_world")
  (PROMPT "Selecciona objetos a desplazar: ")
  (SETQ ConjEntidad (SSGET)) (TERPRI)
  (SETQ PuntoBaseDespl (GETPOINT "Especifica punto de referencia: ")) (TERPRI)

  ;Ahora damos la posibilidad de cambiar la altura del punto
  (SETQ refnt0 (GETVAR "osmode")) (SETVAR "osmode" 0) 	;Guardamos el valor de OSNAP y lo desactivamos
  (INITGET 128)
  (SETQ ValorAnterior (CADDR PuntoBaseDespl))
  (SETQ NuevaCoordZ (GETREAL (STRCAT "Introduce la coordenada Z del punto <"(RTOS ValorAnterior)">:"))) (TERPRI)
  (IF (= NuevaCoordZ nil) (SETQ NuevaCoordZ ValorAnterior))

  (SETQ PuntoDestinoDespl (LIST (CAR PuntoBaseDespl) (CADR PuntoBaseDespl) NuevaCoordZ))
  (COMMAND "_.move" ConjEntidad "" PuntoBaseDespl PuntoDestinoDespl)

  (SETVAR "osmode" refnt0)(SETVAR "cmdecho" 1)(PRIN1) 	;recuperamos el valor de OSNAP y Echo On
                                                      	;PRIN1 evita el "nil" de final de ejecucion
  (SETVAR "cmdecho" 1)
)


;;; -------------- FUNCION EQH ----------------------------------------
;;; Descripcion: Cambia la coordinada Z de los objetos seleccionados basandose en otro punto
;;;              basandose en un punto de referencia.

(DEFUN C:EQH()
  (SETVAR "cmdecho" 0)
  (COMMAND "_.ucs" "_world")
  (PROMPT "Selecciona objetos a desplazar: ")
  (SETQ ConjEntidad (SSGET)) (TERPRI)
  (SETQ PuntoBaseDespl (GETPOINT "Especifica punto base de referencia: ")) (TERPRI)
  (SETQ PuntoDestDespl (GETPOINT PuntoBaseDespl "Especifica punto destino de referencia: ")) (TERPRI)
  
  ;Ahora damos la posibilidad de cambiar la altura del punto
  (SETQ refnt0 (GETVAR "osmode")) (SETVAR "osmode" 0) 	;Guardamos el valor de OSNAP y lo desactivamos
  (SETQ PuntoDestinoDespl (LIST (CAR PuntoBaseDespl) (CADR PuntoBaseDespl) (CADDR PuntoDestDespl)))
  (COMMAND "_.move" ConjEntidad "" PuntoBaseDespl PuntoDestinoDespl)

  (SETVAR "osmode" refnt0)(SETVAR "cmdecho" 1)(PRIN1) 	;recuperamos el valor de OSNAP y Echo On
                                                      	;PRIN1 evita el "nil" de final de ejecucion
  (SETVAR "cmdecho" 1)
)



