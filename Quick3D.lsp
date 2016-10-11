;;;
;;;    Quick3D.lsp
;;;    Version 0.04
;;;    
;;;    ©2006 Pablo Saavedra López
;;;
;;;============================================================================
;;;
;;; Librería de instrucciones para el modelado rápido de edificios en 3D
;;; 
;;;============================================================================
;;;
;;; Pendiente: Mapeado, control de datos.
;;;
;;;============================================================================

;;;============================================================================
;;;========================= MARCO (INSTRUCCION INTERNA) ======================
;;;============================================================================

(DEFUN Marco (PtoInsercion MAncho MAlto MFondo MGrosor / refnt0 PtoOrigen MarcoVentana RecorteMarco OldUCSOrigin)

  (SETVAR "cmdecho" 0) 					;Quitar eco de mensajes a la pantalla
  (SETQ refnt0 (GETVAR "osmode")) (SETVAR "osmode" 0) 	;Guardamos el valor de OSNAP y lo quitamos

  (SETQ OldUCSOrigin (GETVAR "UCSORG")) 		;Guardamos el valor del UCS actual
  (COMMAND "_UCS" "_World")             
  (COMMAND "_UCS" "_MOVE" PtoInsercion) 		;y ponemos el origen en el punto de insercion
  
  (COMMAND "_.box" "0,0,0"  (LIST MAncho MAlto MFondo)) ;generamos la caja exterior
  (setq MarcoVentana (entlast))                         ;y guardamos su nombre
  
  (COMMAND "_.box" (List MGrosor MGrosor 0) (LIST (- MAncho MGrosor) (- MAlto MGrosor) MFondo)) ;generamos la caja interior
  (setq RecorteMarco (entlast))                                                                 ;y guardamos su nombre

  (command "_.subtract" MarcoVentana "" RecorteMarco "");A la caja exterior le recortamos la caja interior

  (COMMAND "_UCS" "_World")              		;Ponemos el UCS en su valor inicial
  (COMMAND "_UCS" "_MOVE" OldUCSOrigin)
  (SETVAR "osmode" refnt0)(SETVAR "cmdecho" 1)(PRIN1) 	;recuperamos el valor de OSNAP y Echo On
                                                      	;PRIN1 evita el "nil" de final de ejecucion
)

;;;============================================================================
;;;========================= MARCO (INSTRUCCION EXTERNA) ======================
;;;============================================================================

(DEFUN c:Marco (/ PtoInsercion MAncho MAlto MFondo MGrosor)
  (SETQ PtoInsercion (GETPOINT "Punto de insercion: ")) (TERPRI);TERPRI hace un INTRO despues de introducir
  (SETQ MAncho  (GETREAL "Ancho del marco: ")) (TERPRI);Ancho de la ventana
  (SETQ MAlto   (GETREAL "Alto del marco: ")) (TERPRI);Alto de la ventana
  (SETQ MFondo  (GETREAL "Fondo del marco: ")) (TERPRI);Fondo de la ventana
  (SETQ MGrosor (GETREAL "Grosor del marco: ")) (TERPRI);Grosor del marco
  (MARCO PtoInsercion Mancho MAlto MFondo MGrosor)
  (PRIN1)
)

;;;============================================================================
;;;========================= VENTANA (INSTRUCCION INTERNA) ====================
;;;============================================================================

(DEFUN Ventana (/ OldUCSorigin)

  (SETVAR "cmdecho" 0) ;Quitar eco de mensajes a la pantalla
  (SETQ refnt0 (GETVAR "osmode")) (SETVAR "osmode" 0) ;Guardamos el valor de OSNAP y lo quitamos
  (SETQ CapaActual (GETVAR "clayer"))                 ;Guardamos el valor de la capa actual
  (SETQ OldAngDir  (GETVAR "angdir"))                 ;Guardamos el valor de ANGDIR para usar ROTATE3D
  (SETQ OldAngBase (GETVAR "angbase"))                ;Guardamos el valor de ANGBASE para usar ROTATE3D

  (SETQ OldUCSOrigin (GETVAR "UCSORG")) 	      ;Guardamos el valor del UCS actual
  (COMMAND "_UCS" "_World")
  
  ;Peticion de los datos de la ventana
  (INITGET 1)
  (SETQ PtoInsercion (GETPOINT "Punto de insercion: "))                ;TERPRI hace un INTRO despues de introducir
  (PRIN1 PtoInsercion)(TERPRI)
  ;(COMMAND "_UCS" "_MOVE" PtoInsercion) 		               ;y ponemos el origen en el punto de insercion

  (INITGET 6)
  (SETQ ValorAnterior VAncho)
  (SETQ VAncho  (GETREAL (STRCAT "Ancho de la ventana <"(RTOS ValorAnterior)">:"))) (TERPRI)
  (IF (= VAncho nil) (SETQ VAncho ValorAnterior))
  
  (INITGET 6)
  (SETQ ValorAnterior VAlto)
  (SETQ VAlto  (GETREAL (STRCAT "Alto de la ventana <"(RTOS ValorAnterior)">:"))) (TERPRI)
  (IF (= VAlto nil) (SETQ VAlto ValorAnterior))

  (INITGET 6)
  (SETQ ValorAnterior VFondo)
  (SETQ VFondo  (GETREAL (STRCAT "Fondo de la ventana <"(RTOS ValorAnterior)">:"))) (TERPRI)
  (IF (= VFondo nil) (SETQ VFondo ValorAnterior))

  (INITGET 6)
  (SETQ ValorAnterior VGrosor)
  (SETQ VGrosor  (GETREAL (STRCAT "Grosor del marco <"(RTOS ValorAnterior)">:"))) (TERPRI)
  (IF (= VGrosor nil) (SETQ VGrosor ValorAnterior))
    
  (INITGET 6)
  (SETQ ValorAnterior NumHojas)
  (SETQ NumHojas  (GETINT (STRCAT "Numero de hojas de la ventana <"(ITOA ValorAnterior)">:"))) (TERPRI)
  (IF (= NumHojas nil) (SETQ NumHojas ValorAnterior))
  
  (INITGET 4)
  (SETQ ValorAnterior VRetranq)
  (SETQ VRetranq  (GETREAL (STRCAT "Retranqueo de la ventana <"(RTOS ValorAnterior)">:"))) (TERPRI) ;Distancia que se mete la ventana en el tabique
  (IF (= VRetranq nil) (SETQ VRetranq ValorAnterior))

  (SETQ ValorAnterior VierteaguasON)
  (INITGET "Si No")
  (SETQ VierteaguasON (GETKWORD (STRCAT "Colocar vierteaguas (Si/No)? <" ValorAnterior ">: ")))
  (IF (= VierteAguasON nil) (SETQ VierteaguasON ValorAnterior))

  (INITGET 4)
  (SETQ ValorAnterior AnchoGuarnicionExterior)
  (SETQ AnchoGuarnicionExterior  (GETREAL (STRCAT "Ancho de la guarnicion exterior (0 = sin guarnicion)<"(RTOS ValorAnterior)">:"))) (TERPRI)
  (IF (= AnchoGuarnicionExterior nil) (SETQ AnchoGuarnicionExterior ValorAnterior))

  

  ;calculo de medidas
  (SETQ CAncho (/ (- (- VAncho (* VGrosor 2)) (* VGrosor (+ NumHojas 1))) NumHojas));Ancho del cristal de cada hoja
  (SETQ CAlto  (- VAlto (* VGrosor 4)));Alto del cristal de cada hoja
  (SETQ HAncho (+ CAncho (* VGrosor 2)));Ancho de cada hoja
  (SETQ HAlto  (+ CAlto (* VGrosor 2)));Alto de cada hoja
  (SETQ HFondo (/ (- VFondo 2) 2));Fondo de cada hoja

  (COMMAND "_UCS" "_MOVE" PtoInsercion) 		               ;y ponemos el origen en el punto de insercion

  ;creacion de las entidades
  (SETVAR "CLAYER" "Q3D_VentanaMarco")
  (MARCO PtoInsercion VAncho VAlto VFondo VGrosor)             ;creacion del marco exterior
  (SETQ EntMarco (ENTLAST))				       ;Nos quedamos con el nombre de la entidad del Marco exterior
  (SETQ ListaEntidad (ENTGET EntMarco))
  (SETQ NombreBloque (CDR(ASSOC 5 ListaEntidad)))              ;Guardamos el nombre FIJO del bloque
  (SETQ NombreBloque (STRCAT "VENTANA-" NombreBloque))         ;Y le añadimos "VENTANA-"
  (SETQ ObjetosDelBloque (SSADD))                              ;Creamos un conjunto de seleccion vacio
  (SSADD EntMarco ObjetosDelBloque)			       ;Añadimos el marco al conjunto

  (SETVAR "CLAYER" "Q3D_VentanaHoja")
  (MARCO PtoInsercion HAncho HAlto HFondo VGrosor)       ;creamos la hoja
  (SETQ EntHoja (ENTLAST))
  
  (SETVAR "CLAYER" "Q3D_VentanaCristal")
  (COMMAND "_.BOX" "0,0,0" (LIST CAncho CAlto CristalGrosor))   ;creamos el cristal
  (SETQ EntCristal (ENTLAST))
  (COMMAND "_.move" EntCristal "" "0,0,0" (LIST VGrosor VGrosor (-(+ 1 (/ HFondo 2)) (/ CristalGrosor 2))));Lo movemos a su posicion
  (COMMAND "_.move" EntHoja EntCristal "" "0,0,0" (LIST VGrosor VGrosor 0));Lo movemos todo a su posicion

  ;(SUBIR EntHoja 1)
;(DEFUN Subir (Objeto Cantidad) ;Sube la cota de un Objeto la Cantidad especificada
   (COMMAND "_.move" EntHoja "" "0,0,0" (LIST 0 0 1))
;)
  (SSADD EntHoja ObjetosDelBloque)    ;Añadimos la hoja al conjunto del bloque
  (SSADD EntCristal ObjetosDelBloque) ;Añadimos el cristal al conjunto del bloque
    
  ;calculamos la nueva posicion de cada hoja
  (SETQ ContadorHojas 1)
  (SETQ IncrZ 0)
  (while (< ContadorHojas NumHojas)
         (IF (= IncrZ 0)
	   (SETQ IncrZ HFondo)
	   (SETQ IncrZ 0)
	 )
         ;...y la copiamos a su sitio
         (COMMAND "_.copy" EntHoja   "" "0,0,0" (LIST (- (* HAncho ContadorHojas) (* ContadorHojas VGrosor)) 0 IncrZ))
         (SETQ EntHojaNueva (ENTLAST))
         (COMMAND "_.copy" EntCristal "" "0,0,0" (LIST (- (* HAncho ContadorHojas) (* ContadorHojas VGrosor)) 0 IncrZ))
         (SETQ EntCristalNuevo (ENTLAST))
         ;Añadimos la hoja y el cristal creados al conjunto de seleccion
         (SSADD EntHojaNueva ObjetosDelBloque)
         (SSADD EntCristalNuevo ObjetosDelBloque)
         ;...y vamos con la siguiente ventana...
         (SETQ ContadorHojas (+ ContadorHojas 1))
  );end {WHILE}

  ;Creamos el vierteaguas,...
  (IF (= VierteaguasON "Si")
      (PROGN
         (SETVAR "CLAYER" "Q3D_VentanaVierteaguas");Activamos la capa del vienteaguas
         (COMMAND "_.BOX" "0,0,0" (LIST (+ VAncho (* VierteaguasVuelo 2)) VierteaguasGrosor (+ VRetranq VierteaguasVuelo)))
  	 (SETQ EntVierteaguas (ENTLAST))
  
         ;...lo giramos,...  
         (SETVAR "angdir"  1)
         (SETVAR "angbase" 90)
         (ROTATE3D EntVierteAguas (LIST 0 0 0) (LIST VAncho 0 0) VierteaguasAngRot)
  
         ;...y lo llevamos a su sitio.
         (COMMAND "_.move" EntVierteAguas "" "0,0,0" (LIST (* VierteaguasVuelo -1) 0 VGrosor))
         (SSADD EntVierteaguas ObjetosDelBloque)
      )
  )
  ;Si el ancho de la guarnicion exterior es mayor de 0 lo creamos
  (IF (> AnchoGuarnicionExterior 0)
      (PROGN
	  (SETQ FondoGuarnicion (+ 1 VRetranq))
	  (SETQ DesplazamientoZ VFondo)
	  (IF (<= GrosorGuarnicionInterior 0)
	      (PROGN
		(SETQ FondoGuarnicion 1)
	        (SETQ DesplazamientoZ (+ VRetranq VFondo))
	      )
          )
	  (SETVAR "CLAYER" "Q3D_VentanaGuarnicion")
          (MARCO PtoInsercion (+ VAncho (* 2 AnchoGuarnicionExterior))
		              (+ VAlto (* 2 AnchoGuarnicionExterior))
		              FondoGuarnicion (+ GrosorGuarnicionInterior AnchoGuarnicionExterior))
	  ;Lo movemos a su sitio...
	  (COMMAND "_.move" (ENTLAST) "" "0,0,0" (LIST (* -1 AnchoGuarnicionExterior) (* -1 AnchoGuarnicionExterior) DesplazamientoZ))
	  (SSADD (ENTLAST) ObjetosDelBloque)
      )
  )
  
  ;Creamos el bloque, lo insertamos y lo giramos
  (COMMAND "_.-BLOCK" NombreBloque "0,0,0" ObjetosDelBloque "")
  (SETVAR "CLAYER" "0")
  (COMMAND "_.-INSERT" NombreBloque "0,0,0" "" "" "")
  (SETQ Bloque (ENTLAST))
  (ROTATE3D Bloque (LIST 0 0 0) (LIST VAncho 0 0) 270)
  (COMMAND "_.move" Bloque "" "0,0,0" (LIST 0 (+ VRetranq VFondo) 0))

  ;Creamos el solido a substraer de la pared = Retranqueo + FondoVentana
  (SETVAR "CLAYER" "Q3Daux_Recortes");Activamos la capa auxiliar
  (COMMAND "_.BOX" "0,0,0" (LIST VAncho (+ VRetranq VFondo) VAlto))

  ;Liberamos la memoria de los conjuntos de seleccion
  (SETQ ObjetosDelBloque nil)
  
  ;Dejamos todo como estaba
  (COMMAND "_UCS" "_World")              		;Ponemos el UCS en su valor inicial
  (COMMAND "_UCS" "_MOVE" OldUCSOrigin)
  
  (SETVAR "angdir"  OldAngDir)                 ;Guardamos el valor de ANGDIR para usar ROTATE3D
  (SETVAR "angbase" OldAngBase)                ;Guardamos el valor de ANGBASE para usar ROTATE3D
  (SETVAR "CLAYER"  CapaActual)                        ;Activamos la capa que estaba activa al principio
  (SETVAR "osmode"  refnt0)(SETVAR "cmdecho" 1)(PRIN1) ;recuperamos el valos de OSNAP y Echo On
                                                      ;PRIN1 evita el "nil" de final de ejecucion
)

;;;============================================================================
;;;========================= VENTANA (INSTRUCCION EXTERNA) ====================
;;;============================================================================

(DEFUN C:Ventana ()
   (Ventana)
)

;==================================================================================
;=========================== HOJAVENTANA (INSTRUCCION INTERNA) ====================
;==================================================================================
(DEFUN HojaVentana (/ refnt0 CapaActual OldAngDir OldAngBase OldUCSorigin
		      PtoInsercion ValorAnterior CAncho CAlto ListaEntidad
		      NombreBloque ObjetosDelBloque EntHoja EntCristal Bloque)

  (SETVAR "cmdecho" 0) ;Quitar eco de mensajes a la pantalla
  (SETQ refnt0 (GETVAR "osmode")) (SETVAR "osmode" 0) ;Guardamos el valor de OSNAP y lo quitamos
  (SETQ CapaActual (GETVAR "clayer"))                 ;Guardamos el valor de la capa actual
  (SETQ OldAngDir  (GETVAR "angdir"))                 ;Guardamos el valor de ANGDIR para usar ROTATE3D
  (SETQ OldAngBase (GETVAR "angbase"))                ;Guardamos el valor de ANGBASE para usar ROTATE3D

  (SETQ OldUCSOrigin (GETVAR "UCSORG")) 	      ;Guardamos el valor del UCS actual
  (COMMAND "_UCS" "_World")
  
  ;Peticion de los datos de la ventana
  (INITGET 1)
  (SETQ PtoInsercion (GETPOINT "Punto de insercion: "))                ;TERPRI hace un INTRO despues de introducir
  (PRIN1 PtoInsercion)(TERPRI)

  (INITGET 6)
  (SETQ ValorAnterior HVAncho)
  (SETQ HVAncho  (GETREAL (STRCAT "Ancho de la hoja <"(RTOS ValorAnterior)">:"))) (TERPRI)
  (IF (= HVAncho nil) (SETQ HVAncho ValorAnterior))
  
  (INITGET 6)
  (SETQ ValorAnterior HVAlto)
  (SETQ HVAlto  (GETREAL (STRCAT "Alto de la hoja <"(RTOS ValorAnterior)">:"))) (TERPRI)
  (IF (= HVAlto nil) (SETQ HVAlto ValorAnterior))

  (INITGET 6)
  (SETQ ValorAnterior HVFondo)
  (SETQ HVFondo  (GETREAL (STRCAT "Fondo de la hoja <"(RTOS ValorAnterior)">:"))) (TERPRI)
  (IF (= HVFondo nil) (SETQ HVFondo ValorAnterior))

  (INITGET 6)
  (SETQ ValorAnterior HVGrosor)
  (SETQ HVGrosor  (GETREAL (STRCAT "Grosor del marco <"(RTOS ValorAnterior)">:"))) (TERPRI)
  (IF (= HVGrosor nil) (SETQ HVGrosor ValorAnterior))

  ;calculo de medidas
  (SETQ CAncho (- HVAncho (* HVGrosor 2)))
  (SETQ CAlto  (- HVAlto (* HVGrosor 2)));Alto del cristal de cada hoja

  (COMMAND "_UCS" "_MOVE" PtoInsercion) 		               ;y ponemos el origen en el punto de insercion

  ;creacion de las entidades
  (SETVAR "CLAYER" "Q3D_VentanaHoja")
  (MARCO PtoInsercion HVAncho HVAlto HVFondo HVGrosor)             ;creamos la hoja
  (SETQ EntHoja (ENTLAST))
  (SETQ ListaEntidad (ENTGET EntHoja))
  (SETQ NombreBloque (CDR(ASSOC 5 ListaEntidad)))              ;Guardamos el nombre FIJO del bloque
  (SETQ NombreBloque (STRCAT "HOJAVENTANA-" NombreBloque))     ;Y le añadimos "HOJAVENTANA-"
  (SETQ ObjetosDelBloque (SSADD))                              ;Creamos un conjunto de seleccion vacio
  (SSADD EntHoja ObjetosDelBloque)			       ;Añadimos el marco al conjunto
  
  (SETVAR "CLAYER" "Q3D_VentanaCristal")
  (COMMAND "_.BOX" "0,0,0" (LIST CAncho CAlto CristalGrosor))   ;creamos del cristal
  (SETQ EntCristal (ENTLAST))
  (COMMAND "_.move" EntCristal "" "0,0,0" (LIST HVGrosor HVGrosor (-(/ HVFondo 2) (/ CristalGrosor 2))));Lo movemos a su posicion

  (SSADD EntCristal ObjetosDelBloque) ;Añadimos el cristal al conjunto del bloque
    
  ;calculamos la nueva posicion de cada hoja

  ;Creamos el bloque, lo insertamos y lo giramos
  (COMMAND "_.-BLOCK" NombreBloque "0,0,0" ObjetosDelBloque "")
  (SETVAR "CLAYER" "0")
  (COMMAND "_.-INSERT" NombreBloque "0,0,0" "" "" "")
  (SETQ Bloque (ENTLAST))
  ;(ROTATE3D Bloque (LIST 0 0 0) (LIST HVAncho 0 0) 270)
  (ROTATE3D Bloque (LIST 0 0 0) (LIST HVAncho 0 0) 90)
  (COMMAND "_.move" Bloque "" "0,0,0" (LIST 0 HVFondo 0))

  ;Creamos el solido a substraer de la pared 
  (SETVAR "CLAYER" "Q3Daux_Recortes");Activamos la capa auxiliar
  (COMMAND "_.BOX" "0,0,0" (LIST HVAncho HVFondo HVAlto))

  ;Liberamos la memoria de los conjuntos de seleccion
  (SETQ ObjetosDelBloque nil)
  
  ;Dejamos todo como estaba
  (COMMAND "_UCS" "_World")              		;Ponemos el UCS en su valor inicial
  (COMMAND "_UCS" "_MOVE" OldUCSOrigin)
  
  (SETVAR "angdir"  OldAngDir)                 ;Guardamos el valor de ANGDIR para usar ROTATE3D
  (SETVAR "angbase" OldAngBase)                ;Guardamos el valor de ANGBASE para usar ROTATE3D
  (SETVAR "CLAYER"  CapaActual)                        ;Activamos la capa que estaba activa al principio
  (SETVAR "osmode"  refnt0)(SETVAR "cmdecho" 1)(PRIN1) ;recuperamos el valos de OSNAP y Echo On
                                                       ;PRIN1 evita el "nil" de final de ejecucion
)

;==================================================================================
;=========================== HOJAVENTANA (INSTRUCCION EXTERNA) ====================
;==================================================================================

(DEFUN C:HojaVentana ()
   (HojaVentana)
)
;==================================================================================
;=========================== ZonaVerde (INSTRUCCION INTERNA) ======================
;==================================================================================
(DEFUN ZonaVerde ()

  (SETVAR "cmdecho" 0) ;Quitar eco de mensajes a la pantalla
  (SETQ refnt0 (GETVAR "osmode")) (SETVAR "osmode" 0) ;Guardamos el valor de OSNAP y lo quitamos
  (SETQ CapaActual (GETVAR "clayer"))                 ;Guardamos el valor de la capa actual

  (SETQ OldUCSOrigin (GETVAR "UCSORG")) 	      ;Guardamos el valor del UCS actual
  (COMMAND "_UCS" "_World")
  
  ;Peticion de los datos de la ventana
  (INITGET 1)
  (SETQ PtoInsercion (GETPOINT "Punto de insercion: "))                ;TERPRI hace un INTRO despues de introducir
  (PRIN1 PtoInsercion)(TERPRI)

  (INITGET 6)
  (SETQ ValorAnterior ZVAncho)
  (SETQ ZVAncho  (GETREAL (STRCAT "Ancho (X) de la zona verde <"(RTOS ValorAnterior)">:"))) (TERPRI)
  (IF (= ZVAncho nil) (SETQ ZVAncho ValorAnterior))
  
  (INITGET 6)
  (SETQ ValorAnterior ZVLargo)
  (SETQ ZVLargo  (GETREAL (STRCAT "Largo (Y) de la zona verde <"(RTOS ValorAnterior)">:"))) (TERPRI)
  (IF (= ZVLargo nil) (SETQ ZVLargo ValorAnterior))

  (INITGET 6)
  (SETQ ValorAnterior ZVAlto)
  (SETQ ZVAlto  (GETREAL (STRCAT "Largo (Z) de la zona verde <"(RTOS ValorAnterior)">:"))) (TERPRI)
  (IF (= ZVAlto nil) (SETQ ZVAlto ValorAnterior))

  (INITGET 6)
  (SETQ ValorAnterior ZVRetHierba)
  (SETQ ZVRetHierba  (GETREAL (STRCAT "Retranqueo interior de la hierba <"(RTOS ValorAnterior)">:"))) (TERPRI)
  (IF (= ZVRetHierba nil) (SETQ ZVRetHierba ValorAnterior))
  
  (COMMAND "_UCS" "_MOVE" PtoInsercion) 		               ;y ponemos el origen en el punto de insercion

  ;creacion de las entidades
  (SETVAR "CLAYER" "3Dext_ZonaVerdeLateral")
  (COMMAND "._box" "0,0,0" (LIST ZVAncho ZVLargo ZVAlto))
  (SETQ EntLateral (ENTLAST))
  
  (SETVAR "CLAYER" "3Dext_ZonaVerdeHierba")
  (COMMAND "_.box" (LIST ZVRetHierba ZVRetHierba 0) (LIST (- ZVAncho ZVRetHierba) (- ZVLargo ZVRetHierba) ZVAlto))   ;creamos del cristal
  (COMMAND "_.copy" (entlast) "" "0,0,0" "0,0,0")
  (COMMAND "_.subtract" EntLateral "" (entlast) "")
  
  ;Dejamos todo como estaba
  (COMMAND "_UCS" "_World")              		;Ponemos el UCS en su valor inicial
  (COMMAND "_UCS" "_MOVE" OldUCSOrigin)
  
  (SETVAR "CLAYER"  CapaActual)                        ;Activamos la capa que estaba activa al principio
  (SETVAR "osmode"  refnt0)(SETVAR "cmdecho" 1)(PRIN1) ;recuperamos el valos de OSNAP y Echo On
                                                      ;PRIN1 evita el "nil" de final de ejecucion
)
;==================================================================================
;=========================== ZonaVerde (INSTRUCCION EXTERNA) ======================
;==================================================================================

(DEFUN C:ZonaVerde ()
   (ZonaVerde)
)

;===============================================================
;================== QLayer =====================================
;===============================================================
; Nombre de la funcion: Qlayer
;                  Uso: (QLAYER "LAYERNAME" "COLOR")
;          Descripcion: Crea una capa a partir de los argumentos especificados de Nombre y Color
;     Especificaciones:

(DEFUN QLayer (NombreCapa ColorCapa / refnt0 CapaActual)

  (COMMAND "_.-LAYER" "N" NombreCapa "")
  (COMMAND "_.-LAYER" "C" ColorCapa NombreCapa "")

)

;===============================================================
;================== Espacio3D ==================================
;===============================================================
; Nombre de la funcion: Espacio3D
;          Descripcion: Crea un espacio tridimensional a partir de una polilinea.
;     Especificaciones: La polilinea debe estar a cota 0.
(DEFUN Espacio3D (Entidad / refnt0)
  
  (SETVAR "cmdecho" 0)
  (SETQ CapaActual (GETVAR "clayer"))
  (SETQ refnt0 (GETVAR "osmode"))
  (SETVAR "osmode" 0)

  (COMMAND "_.copy" Entidad "" "0,0,0" "0,0,0")
  (SETQ EntMurosInt (ENTLAST))
  ; Creamos la parte exterior del tabique
  (COMMAND "_.offset" EspesorTabique EntMurosInt "100000,100000" "")
  (SETQ EntMurosExt (ENTLAST))
  ;Y nos quedamos con la polilinea exterior para crear el techo y el suelo
  (COMMAND "_.copy" EntMurosExt "" "0,0,0" "0,0,0")
  (SETQ EntSuelo (ENTLAST))
  (COMMAND "_.copy" EntMurosExt "" "0,0,0" "0,0,0")
  (SETQ EntTecho (ENTLAST))
  ;Extrusionamos las paredes
  (SETVAR "CLAYER" "Q3Di_Muros")
  (COMMAND "_.extrude" EntMurosInt "" AlturaTecho "0")
  (SETQ EntMurosInt (ENTLAST))
  (COMMAND "_.extrude" EntMurosExt "" AlturaTecho "0")
  (SETQ EntMurosExt (ENTLAST))
  (COMMAND "_.subtract" EntMurosExt "" EntMurosInt "")
  ;Creamos el suelo y el techo
  (SETVAR "CLAYER" "Q3Di_Suelo")
  (COMMAND "_.extrude" EntSuelo "" EspesorSuelo "0")
  (SETVAR "CLAYER" "Q3Di_Techo")
  (COMMAND "_.move" EntTecho "" "0,0,0" (LIST 0 0 AlturaTecho))
  (COMMAND "_.extrude" EntTecho "" EspesorTecho "0")
  
  (SETVAR "CLAYER" CapaActual)
  (SETVAR "osmode" refnt0)
  (SETVAR "cmdecho" 1)
)

(DEFUN C:Espacio3D ()
  (SETVAR "cmdecho" 0)
  (COMMAND "_.ucs" "_world")
  (PROMPT "Señala el tabique (Polilinea 2D):")
  (SETQ Entidad (SSNAME (SSGET) 0)) (TERPRI)
  
  (SETQ ValorAnterior AlturaTecho)
  (INITGET 6)
  (SETQ AlturaTecho (GETREAL (STRCAT "Introduzca altura de los tabiques <" (RTOS ValorAnterior) ">: ")))(TERPRI)
  (IF (= AlturaTecho nil) (SETQ AlturaTecho ValorAnterior))

  (SETQ ValorAnterior EspesorTabique)
  (INITGET 6)
  (SETQ EspesorTabique (GETREAL (STRCAT "Introduzca grosor de los tabiques <" (RTOS ValorAnterior) ">: ")))(TERPRI)
  (IF (= EspesorTabique nil) (SETQ EspesorTabique ValorAnterior))

  (Espacio3D Entidad)
  (SETVAR "cmdecho" 1)
)

;===============================================================
;================== LevantarTabiques ===========================
;===============================================================
; Nombre de la funcion: LevantarTabiques
;          Descripcion: Crea un tabique a partir de una polilinea.
;     Especificaciones: La polilinea debe estar a cota 0.
(DEFUN LevantarTabiques (Entidad / refnt0)
  (SETVAR "cmdecho" 0)
  (SETQ CapaActual (GETVAR "clayer"))
  (SETQ refnt0 (GETVAR "osmode"))
  (SETVAR "osmode" 0)

  (COMMAND "_.copy" Entidad "" "0,0,0" "0,0,0")
  (SETQ EntTabique (ENTLAST))
  (SETVAR "CLAYER" "Q3Di_Muros")
  (COMMAND "_.extrude" EntTabique "" AlturaTecho "0")
  (SETVAR "osmode" refnt0)
  (SETVAR "cmdecho" 1)
)

(DEFUN C:LevantarTabiques ()
  (SETVAR "cmdecho" 0)
  (COMMAND "_.ucs" "_world")
  (PROMPT "Señale los tabiques a crear (2Dpolys):")
  (SETQ ConjEntidad (SSGET)) (TERPRI)

  (SETQ AlturaAnterior AlturaTecho)
  (INITGET 6)
  (SETQ AlturaTecho (GETREAL (STRCAT "Introduzca altura de los tabiques <" (RTOS AlturaAnterior) ">: ")))(TERPRI)
  (IF (= AlturaTecho nil) (SETQ AlturaTecho AlturaAnterior))

  (SETQ ContadorEntidades 1)
  (WHILE (<= ContadorEntidades (SSLENGTH ConjEntidad))
    (SETQ Entidad (SSNAME ConjEntidad (- ContadorEntidades 1)))
    (LevantarTabiques Entidad)
    (SETQ ContadorEntidades (+ 1 ContadorEntidades))
  );_end of while

  (SETVAR "CLAYER" CapaActual)
  (SETVAR "cmdecho" 1)
  (setq ConjEntidad nil)
);_end of defun

;===============================================================
;================== HuecoPuertas ===============================
;===============================================================
; Nombre de la funcion: HuecoPuertas
;          Descripcion: Crea un hueco en un solido a partir de una polilinea cerrada.
;     Especificaciones: 
(DEFUN C:HuecoPuertas ()
  (SETVAR "cmdecho" 0)
  (SETQ refnt0 (GETVAR "osmode"))
  (SETVAR "osmode" 0)
  (COMMAND "_.ucs" "_world")
  (PROMPT "Señale los tabiques 3D donde se van a hacer los cortes:")
  (SETQ ConjTabiques (SSGET)) (TERPRI)
  (PROMPT "Señale las polilineas 2D (plantillas de corte):")
  (SETQ ConjPolilineas (SSGET)) (TERPRI)

  (SETQ ValorAnterior AlturaHuecoPuerta)
  (INITGET 6)
  (SETQ AlturaHuecoPuerta (GETREAL (STRCAT "Introduzca altura de los huecos <" (RTOS ValorAnterior) ">: ")))(TERPRI)
  (IF (= AlturaHuecoPuerta nil) (SETQ AlturaHuecoPuerta ValorAnterior))
  ;Tomamos una entidad de referencia para crear el conjunto de elementos extrusionados
  (COMMAND "_.point" "0,0,0")
  (SETQ PuntoAux (ENTLAST))
  ;Extrusionamos todas las polilineas.
  (COMMAND "_.extrude" ConjPolilineas "" AlturaHuecoPuerta "0")
  ;Nos quedamos con el conjunto de elementos extrusionados
  (SETQ PrimerCorte (ENTNEXT PuntoAux))
  (SETQ UltimoCorte (ENTLAST))
  (SETQ ConjCortes (SSSUBSET PrimerCorte UltimoCorte))
  ;Realizamos el corte
  (COMMAND "_.subtract" ConjTabiques "" ConjCortes "")
  ;Liberamos memoria
  (SETQ ConjTabiques nil)
  (SETQ ConjPolilineas nil)
  (SETQ ConjCortes nil)
  (ENTDEL PuntoAux)

  (SETVAR "osmode" refnt0)
  (SETVAR "cmdecho" 1)
)

;===============================================================
;================== InitQuick3D ================================
;===============================================================
; Nombre de la funcion: InitQuick3D
;          Descripcion: Inicializa las capas para creación en 3D
;     Especificaciones: 
(DEFUN C:InitQuick3D ()

  (SETVAR "cmdecho" 0)
  
  ;Inicializamos las capas de interior...
  (QLayer "Q3Di_Muros"               "041")
  (QLayer "Q3Di_Techo"               "051")
  (QLayer "Q3Di_Suelo"               "140")
    
  ;... y las de ventanas...
  (QLayer "Q3D_VentanaMarco"         "095")
  (QLayer "Q3D_VentanaHoja"          "054")
  (QLayer "Q3D_VentanaCristal"       "141")
  (QLayer "Q3D_VentanaVierteaguas"   "253")
  (QLayer "Q3D_VentanaGuarnicion"    "002")
  (QLayer "Q3D_VentanaJunquillo"     "033")
    
  ;...y de exterior (edificio)...
  (QLayer "Q3D_CubiertaBase"         "001")
  (QLayer "Q3D_CubiertaTeja"         "022")
  (QLayer "Q3D_ChimeneaCuerpo"       "032")
  (QLayer "Q3D_ChimeneaCaperuza"     "252")
  (QLayer "Q3D_BalconBase"           "253")
  (QLayer "Q3D_BalconBalaustre"      "251")
  (QLayer "Q3D_BalconJunta"          "252")
  (QLayer "Q3D_Cornisa"              "007")
  (QLayer "Q3D_Pilares"              "253")

  ;...y de exterior (edificio)...
  (QLayer "Q3D_MurosExtPlaca"        "253")
  (QLayer "Q3D_MurosExtJunta"        "007")
  (QLayer "Q3D_MurosExt00"           "041")
  (QLayer "Q3D_MurosExt01"           "051")
  (QLayer "Q3D_MurosExt02"           "031")
  (QLayer "Q3D_MurosExt03"           "021")
  (QLayer "Q3D_MurosExt04"           "061")

  ;...y de exterior (entorno)...
  (QLayer "Q3Dx_LineasTrafico"       "007")
  (QLayer "Q3Dx_Asfalto"             "252")
  (QLayer "Q3Dx_AceraBase"           "125")
  (QLayer "Q3Dx_AceraBorde"          "250")
  (QLayer "Q3Dx_ZonaVerdeHierba"     "106")
  (QLayer "Q3Dx_ZonaVerdeBorde"      "042")

  ;...y capas auxiliares...
  (QLayer "Q3Daux_Recortes"          "003")
  (QLayer "Q3Daux_JuntasRecorte"     "004")
  (QLayer "Q3Daux_01"                "001")
  (QLayer "Q3Daux_02"                "002")
  (QLayer "Q3Daux_03"                "003")

  (SETVAR "cmdecho" 1)

)

;===========================================================================
;===================== carga de aplicaciones al inicio =====================
;===========================================================================

(PROMPT "Inicializando Quick3D...")(TERPRI)

;Declaracion de constantes
(SETQ CristalGrosor 0.6)
(SETQ VierteaguasVuelo 3)
(SETQ VierteaguasGrosor 2)
(SETQ VierteaguasAngRot -5) ;Angulo de rotacion del vierteaguas

;inicializacion de variables globales *** FUNCION VENTANA ***
(SETQ VAncho         160)
(SETQ VAlto          120)
(SETQ VFondo           6)
(SETQ VGrosor          6)
(SETQ NumHojas         2)
(SETQ VRetranq        20)
(SETQ VierteaguasON "Si")
(SETQ AnchoGuarnicionExterior 15)
(SETQ GrosorGuarnicionInterior 0.1) ; 0.1 = Guarnicion dentro de ventana, 0 = Sin guarnicion interior

;inicializacion de variables globales  *** FUNCION HOJAVENTANA ***
(SETQ HVAncho         200)
(SETQ HVAlto          130)
(SETQ HVFondo           6)
(SETQ HVGrosor          6)

;inicializacion de variables globales  *** FUNCION ZONAVERDE ***
(SETQ ZVAncho     600)
(SETQ ZVLargo     600)
(SETQ ZVAlto       40)
(SETQ ZVRetHierba  20)

;inicializacion de variables globales  *** FUNCIONES DE INTERIOR ***
(SETQ EspesorTabique 25)
(SETQ AlturaTecho 300)
(SETQ EspesorSuelo 1)
(SETQ EspesorTecho 20)
(SETQ AlturaHuecoPuerta 210)

;Carga de aplicaciones para la ejecucion del programa
(SETQ AplicCargadas (ARX))
(SETQ geom3Dcargado (MEMBER '"geom3d.arx" AplicCargadas))
(IF (= geom3Dcargado nil)
    (ARXLOAD "GEOM3D.ARX" (PROMPT "Error al cargar la aplicacion GEOM3D.ARX"))
)
