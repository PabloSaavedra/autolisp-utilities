;;;
;;;    BlockAlign.lsp
;;;    Version 0.02
;;;    
;;;    ©2006 Pablo Saavedra López
;;;
;;;============================================================================
;;;
;;; Rutina para alinear la rotación un bloque fuente con otro destino
;;;
;;;============================================================================
;;;
;;; Pendiente:
;;;
;;;============================================================================

; Convierte valor en radianes a grados
(defun rad2deg (nbrOfRadians)
  (* 180.0 (/ nbrOfRadians pi))
)

(DEFUN C:BlockAlign ()
  (PROMPT "BlockAlign. Señale el bloque a girar...")
  (SETQ ConjEntidad (SSGET)) (TERPRI)

  ; Tomamos el objeto a alinear
  (SETQ MiEntidad1 (SSNAME ConjEntidad 0))          ;Miramos cual es el nombre de la entidad y lo metemos en "MiEntidad"
  (SETQ ListaMiEntidad1 (ENTGET MiEntidad1))        ;y extraemos su lista de pares y la metemos en "ListaMiEntidad"
  
  ; Tomamos el objeto base de alineado
  (PROMPT "Señale el bloque base de alineado...")
  (SETQ ConjEntidad (SSGET)) (TERPRI)
  (SETQ MiEntidad2 (SSNAME ConjEntidad 0))          ;Miramos cual es el nombre de la entidad y lo metemos en "MiEntidad2"
  (SETQ ListaMiEntidad2 (ENTGET MiEntidad2))        ;y extraemos su lista de pares y la metemos en "ListaMiEntidad2"
  (SETQ Rotacion (CDR(ASSOC 50 ListaMiEntidad2)))   ;y metemos en Rotacion el par 50 de la lista (Rotacion en radianes)

  ; Modificamos la lista de entidades con la nueva rotación
  (setq ListaMiEntidad1 (subst                      ; Vamos a sustituir...
			  (cons 50 Rotacion)        ; crea un par punteado que es (50. rotacion)
			  (assoc 50 ListaMiEntidad1); y lo mete en la posicion donde encuentra el valor 50 la funcion assoc, que seria (50. RotacionActual)
			  ListaMiEntidad1))         ; en ListaMiEntidad1
  
  (entmod ListaMiEntidad1)                          ; y metemos la entidad de nuevo en la lista de entidades
)

