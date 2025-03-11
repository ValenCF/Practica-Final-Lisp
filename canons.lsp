;-------------------------------------------------------------
;------------------------PRACTICA-----------------------------
;-------------------------------------------------------------

; Valentino Coppola y Blerim Sinani

;Método para crear los bordes del campo de juego
(defun creaBordes ()
(move 0 0)
(draw 0 340)
(draw 640 340)
(draw 640 0)
(draw 0 0)
)

;Crea ambos campos de los jugadores, y el muro central
(defun crearCampos() 
;muro central
(setq rs (make-random-state t))
(putprop 'camp (+ 20 (random 20 rs)) 'amp)
(putprop 'camp (+ 100 (random 50 rs)) 'alt)
(putprop 'camp (truncate (- 320 (/ (get 'camp 'amp) 2))) 'x)
(move (get 'camp 'x) 0)
(draw (get 'camp 'x) (get 'camp 'alt))
(draw (+ (get 'camp 'x) (get 'camp 'amp)) (get 'camp 'alt))
(draw (+ (get 'camp 'x) (get 'camp 'amp)) 0)
;declaración random de las alturas de los campos
(putprop 'camp (+ 15(random 30 rs)) 'altcampE)
(putprop 'camp (+ 15(random 30 rs))'altcampD)

;Campo izquierda
(move 0 (get 'camp 'altcampE))
(draw (get 'camp 'x) (get 'camp 'altcampE))
;Campo derecha
(move (+ (get 'camp 'x) (get 'camp 'amp)) (get 'camp 'altcampD))
(draw 640 (get 'camp 'altcampD))

;Se crea el palo de la bandera
(putprop 'camp (truncate (+ (get 'camp 'x) (/ (get 'camp 'amp) 2))) 'centre)
(move (get 'camp 'centre) (get 'camp 'alt))
(draw (get 'camp 'centre) (+ (get 'camp 'alt) 20))

;se define el valor del viento
(putprop 'camp (- (random 10 rs) 5) 'vent)
(pintaBandera (get 'camp 'centre) (+ (get 'camp 'alt) 20))
)

;Método que repite la creación de la pantalla de juego para ir actualizando la.
(defun repiteCampo()
(creaBordes)
(move (get 'camp 'x) 0)
(draw (get 'camp 'x) (get 'camp 'alt))
(draw (+ (get 'camp 'x) (get 'camp 'amp)) (get 'camp 'alt))
(draw (+ (get 'camp 'x) (get 'camp 'amp)) 0)

;Campo izquierda
(move 0 (get 'camp 'altcampE))
(draw (get 'camp 'x) (get 'camp 'altcampE))
;Campo derecha
(move (+ (get 'camp 'x) (get 'camp 'amp)) (get 'camp 'altcampD))
(draw 640 (get 'camp 'altcampD))

(move (get 'camp 'centre) (get 'camp 'alt))
(draw (get 'camp 'centre) (+ (get 'camp 'alt) 20))
(pintaBandera (get 'camp 'centre) (+ (get 'camp 'alt) 20))
)

;Método que comprueba el valor del viento y dependiendo de este, dibuja la 
;bandera hacia un lado o hacia el otro.
(defun pintaBandera (x y)
  (cond ((= (get 'camp 'vent) 0) nil)
        ((< (get 'camp 'vent) 0) (pintaRectanguloEsq x y))
        ((> (get 'camp 'vent) 0) (pintaRectanguloDre x y))
        (t)))

;Método que pinta un rectangulo hacia la izquierda para dibujar la bandera
(defun pintaRectanguloEsq (x y) 

  (move x y) 
  (draw (- x 3) y)
  (draw (- x 3) (- y 10))
  (draw x (- y 10)))

;Método que pinta un rectangulo hacia la derecha para dibujar la bandera
(defun pintaRectanguloDre (x y)
  (move x y) 
  (draw (+ x 3) y)
  (draw (+ x 3) (- y 10))
  (draw x (- y 10)))


;MODIFICACIÓN DE PROPIEDADES
 
;Incrementa el ángulo del cañón especificado, sin exceder 180 grados.
(defun inc-angle (cano)
  (cond
    ((<= (+ (get cano 'angle) 5) 180)
     (putprop cano (+ (get cano 'angle) 5) 'angle))
    (t
     (putprop cano 180 'angle))))

;Decrementa el ángulo del cañón especificado, sin bajar de 0 grados.
(defun dec-angle (cano)
  (cond
    ((>= (- (get cano 'angle) 5) 0)
     (putprop cano (- (get cano 'angle) 5) 'angle))
    (t
     (putprop cano 0 'angle))))

;Incrementa la velocidad del cañon indicado
(defun inc-velocitat (cano)

    (putprop cano (+ (get cano 'vel)5) 'vel)
)
;Decrementa la velocidad del cañon indicado

(defun dec-velocitat (cano)
  (putprop cano (- (get cano 'vel)5) 'vel)
)

;función que permite mover los cañones hacia la izquierda
(defun moveEsq (cano valor)
  (cond
    ((and (eq cano 'cano1)
          (> (get cano 'posx) 5))
     (putprop cano (+ (get cano 'posx) valor) 'posx))
    ((and (eq cano 'cano2)
          (> (get cano 'posx)
             (+ (get 'camp 'x) (get 'camp 'amp) 5)))
     (putprop cano (+ (get cano 'posx) valor) 'posx))))

;Función que permite mover los cañones hacia la derecha
(defun moveDreta (cano valor)
  (cond
    ((and (eq cano 'cano1)
          (< (get cano 'posx)
             (- (get 'camp 'x) (truncate (/ (get 'camp 'amp) 2)) 10)))
     (putprop cano (+ (get cano 'posx) valor) 'posx))
    ((and (eq cano 'cano2)
          (< (get cano 'posx) 615))
     (putprop cano (+ (get cano 'posx) valor) 'posx))))

;Método juego que consiste en un bucle hasta que se pressione la tecla esc
;o se impacte a uno de los cañones y permite controlar las acciones de 
;las teclas
(defun juego ()
    "Bucle principal del juego"
    (cls)
    (repiteCampo)
    (dibuixaCanons)
    (princ "Presiona una tecla o ESC para salir.")
    (terpri)
    (setq tecla (get-key))
    (cond ((equal tecla 97) ; A
           (moveEsq 'cano1 -5) (juego))
          ((equal tecla 100) ; D
           (moveDreta 'cano1 5) (juego))
          ((equal tecla 106) ; J
           (moveEsq 'cano2 -5) (juego))
          ((equal tecla 108) ; L
           (moveDreta 'cano2 5) (juego))
          ((equal tecla 119) ; W
           (inc-angle 'cano1) (juego))
          ((equal tecla 105) ; I
           (inc-angle 'cano2) (juego))
          ((equal tecla 115) ; S
           (dec-angle 'cano1) (juego))
          ((equal tecla 107) ; K
           (dec-angle 'cano2) (juego))
          ((equal tecla 113) ; Q
           (dec-velocitat 'cano1) (juego))
          ((equal tecla 111) ; O
           (dec-velocitat 'cano2) (juego))
          ((equal tecla 101) ; E
           (inc-velocitat 'cano1) (juego))
          ((equal tecla 117) ; U
           (inc-velocitat 'cano2) (juego))
          ((equal tecla 102) ; F
           (disparar1) (juego))
          ((equal tecla 104) ; H
           (disparar2) (juego))
          ((equal tecla 27)  ; ESC
           t)                      ; acaba recursión
          (t                 ; altrament
           (juego))))           ; repite

(defun rectangle (x y w h)
    (move x y)
    ;(color 255 255 255 0 255 0)
    (drawrel w 0)
    (drawrel 0 h)
    (drawrel (- w) 0)
    (drawrel 0 (- h))
    ;(color 255 255 255 0 0 0)
    )

;Método que dibuja los cañones
(defun dibuixaCanons ()
;cano1
(rectangle (get 'cano1 'posx) (get 'cano1 'posy) 20 10)
(linia (+ (get 'cano1 'posx) 10) 
(+ (get 'cano1 'posy) 10) 20 (get 'cano1 'angle))
;cano2
(rectangle (get 'cano2 'posx) (get 'cano2 'posy) 20 10)
(linia (+ (get 'cano2 'posx) 10) 
(+ (get 'cano2 'posy) 10) 20 (get 'cano2 'angle)))

;Métodos para crear los cañones
(defun linia (x y delta angle)
  (mover x y)
  (linia-aux x y delta angle delta))

(defun linia-aux (x y delta angle remaining)
  (cond ((> remaining 0)
         (drawr (+ x (* (- delta remaining) (cos (radians angle))))
                (+ y (* (- delta remaining) (sin (radians angle)))))
         (linia-aux x y delta angle (- remaining 1)))
        (t t)))

(defun mover (x y)
  (move (round x) (round y)))

(defun drawr (x y)
  (draw (round x) (round y)))

(defun radians (graus)
  (/ (* graus (* 2 pi)) 360))


;Método main que ejecuta el programa
(defun inicia ()

  ; Crear bordes y campos
  (creaBordes)
  (crearCampos)
  (set-canons)
  (dibuixaCanons)
  (juego)
  )

;Método que define las propiedades de los cañones
(defun set-canons ()
  ;cano1
  (putprop 'cano1 (- 160 (truncate(/ (get 'camp 'amp) 2)) 10) 'posx)
  (putprop 'cano1 (get 'camp 'altcampE) 'posy)
  (putprop 'cano1 45 'angle)
  (putprop 'cano1 20 'vel)

  ;cano2
  (putprop 'cano2 (+ 480 (truncate(/ (get 'camp 'amp) 2)) 10) 'posx)
  (putprop 'cano2 (get 'camp 'altcampD) 'posy)
  (putprop 'cano2 135 'angle)
  (putprop 'cano2 20 'vel)

)

;Es lo mismo que el dibujado de linia normal solo que con un sleep.
(defun linia-aux2 (x y delta angle remaining)
  (sleep 0.000000001)
  (cond ((> remaining 0)
         (drawr (+ x (* (- delta remaining) (cos (radians angle))))
                (+ y (* (- delta remaining) (sin (radians angle)))))
         (linia-aux2 x y delta angle (- remaining 1)))
        (t t)))

(defun linia2 (x y delta angle)
  (mover x y)
  (linia-aux2 x y delta angle delta))

;Método para disparar el cañon 1
(defun disparar1 ()
  (calc-trayectoria 
   (+(get 'cano1 'posx) 10)
   (+(get 'cano1 'posy) 10)
   (* (get 'cano1 'vel) (cos (radians (get 'cano1 'angle))))
   (* (get 'cano1 'vel) (sin (radians (get 'cano1 'angle))))
   -9.8
   0.1 'cano1))

;Método para disparar el cañon 2   
(defun disparar2 ()
  (calc-trayectoria
   (+(get 'cano2 'posx) 10)
   (+(get 'cano2 'posy) 10)
   (* (get 'cano2 'vel) (cos (radians (get 'cano2 'angle))))
   (* (get 'cano2 'vel) (sin (radians (get 'cano2 'angle))))
   -9.8
   0.1 'cano2))

;Método que calcula la trayectoria del disparo
(defun calc-trayectoria (x y vix viy a delta-t cano)
  (cond
    ((< y 0) nil) ; cuando la bala cae fuera de la pantalla
    ;comprova que la bala no impacti amb el mur
    ((and (> x (get 'camp 'x)) 
    (< x (+ (get 'camp 'amp) (get 'camp 'x))) 
    (< y (get 'camp 'alt))) nil)
    ;Comprova que la bala no vagi davall terra
    ((or (and (< y (get 'cano1 'posy)) (< x (get 'camp 'x))) 
    (and (< y (get 'cano2 'posy)) (> x (+ (get 'camp 'amp) 
    (get 'camp 'x))))) nil)
    ;comprova que si la bala impacta amb un cano
    ((and (eq cano 'cano2) (> x (get 'cano1 'posx))
    (< x (+ (get 'cano1 'posx) 20))
    (< y (+ (get 'cano1 'posy) 10))
             (error "Impacte")))
        ; IMPACTO CAÑON DERECHO
    ((and (eq cano 'cano1) (> x  (get 'cano2 'posx) )
     (< x (+(get 'cano2 'posx) 20))
     (< y (+ (get 'cano2 'posy) 10))
         (error "Impacte")))

    (t (drawr x y)
       (linia2 x y (sqrt (+ (expt (* vix delta-t) 2) 
    (expt (* viy delta-t) 2))) (atan viy vix))
       (calc-trayectoria
        (+ x (* vix delta-t))
        (+ y (* viy delta-t) (* 0.5 a (* delta-t delta-t)))
        vix
        (+ viy (* a delta-t))
        a
        delta-t cano)
        )))
        




(defun sleep (seconds)
 "Espera la quantitat indicada de segons"
 ; Això és un bucle iteratiu. NO PODEU FER-NE SERVIR ENLLOC MÉS
 (do ((endtime (+ (get-internal-real-time)
 (* seconds internal-time-units-per-second))))
 ((> (get-internal-real-time) endtime))))