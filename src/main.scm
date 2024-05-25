(use-modules (raylib)
	     (srfi srfi-27)
	     (ice-9 format))

(define CELL-MUSHROOM 1)
(define CELL-EMPTY 0)
(define DIRECTION-U 0)
(define DIRECTION-D 1)
(define DIRECTION-L 2)
(define DIRECTION-R 3)

;; Initialization
(set! *random-state* (random-state-from-platform)) ;; Change random seed

(define grid-width 32)
(define grid-height 45)
(define sprite-size 16)
(define screen-width (* sprite-size grid-width))
(define screen-height (* sprite-size grid-height))
(define player-movement-area (* 5 sprite-size))

(InitWindow screen-width screen-height "Basic Game Loop with Raylib")
(SetTargetFPS 60) ; Set our game to run at 60 frames-per-second

;; Define a player object
(define player-position (list (/ screen-width 2) 650))
(define player-speed 5)

(define player-fire-position '(0 600))
(define player-fire-active #f)

;; Function to update player position
(define (clamp val val-min val-max)
  (min (max val val-min) val-max))

(define (update-player-position pos dx dy)
  (list
   (clamp (+ (car pos) dx) 0 (- screen-width sprite-size))
   (clamp (+ (cadr pos) dy) (- screen-height player-movement-area) (- screen-height sprite-size))))

(define (update-player-fire-position pos)
  (list (car pos) (- (cadr pos) 15)))

(define shiptex (LoadTexture "./assets/ship16.png"))
(define firetex (LoadTexture "./assets/fire16.png"))
(define enemytex (LoadTexture "./assets/enemy16.png"))
(define mushtex (LoadTexture "./assets/mushroom16.png"))


(define grid-data (make-array CELL-EMPTY grid-height grid-width))

(define score 0)

;; Utilities
(define (pos-to-grid pos)
  (list
   (min (floor/ (car pos) sprite-size) (- grid-width 1))
   (min (floor/ (cadr pos) sprite-size) (- grid-height 1))))

(define (opposite-direction dir)
  (cond
   ((= dir DIRECTION-L) DIRECTION-R)
   ((= dir DIRECTION-R) DIRECTION-L)))

(define (rectangles-collide? rect1 rect2 width)
  (let* ((x1 (car rect1))
         (y1 (cadr rect1))
         (x2 (car rect2))
         (y2 (cadr rect2))
         (x1-max (+ x1 width))
         (y1-max (+ y1 width))
         (x2-max (+ x2 width))
         (y2-max (+ y2 width)))
    (not (or (< x1-max x2)    ;; rect1 is to the left of rect2
             (< x2-max x1)    ;; rect2 is to the left of rect1
             (< y1-max y2)    ;; rect1 is below rect2
             (< y2-max y1))))) ;; rect2 is below rect1

(define (add-score points)
  (set! score (+ score points)))

;; Functions for create enemies and mushrooms
(define (make-mushrooms count)
  (for-each (lambda (idx)
	      (let ((grid-x (random grid-width))
		    (grid-y (random (- grid-height 5))))
		(array-set! grid-data CELL-MUSHROOM grid-y grid-x)))
	    (iota count)))

(define (make-enemies blocks)
  (apply append (map (lambda (block-index)
		       (let ((number-of-enemies (random 5))
			     (start-x (random screen-width))
			     (start-y (* sprite-size (random 2)))
			     (direction (+ 2 (random 1))))
			 (map (lambda (index)
				(vector direction (+ (* sprite-size index) start-x) start-y))
			      (iota number-of-enemies))))
		     (iota blocks))))

;; Initial state
(make-mushrooms 40)
(define enemies (make-enemies 10))
(define (enemy-get-x enemy) (vector-ref enemy 1))
(define (enemy-get-y enemy) (vector-ref enemy 2))
(define level-completed #f)
(define level 1)
(define lives 4)
(define enemy-speed 3)

;; Main game loop
(define (main-loop)
  (if (not (WindowShouldClose)) ; Detect window close button or ESC key
      (begin
        ;; Update
	(if (= (length enemies) 0)
	    (begin
	      (set! level (+ 1 level))
	      (set! enemy-speed (+ 1 enemy-speed))
	      (set! enemies (make-enemies (* 3 level)))
	      (make-mushrooms (* 5 level))))
	
	;; Player pos
        (let ((dx (cond ((IsKeyDown KEY_RIGHT) player-speed)
                        ((IsKeyDown KEY_LEFT) (- player-speed))
                        (else 0)))
              (dy (cond ((IsKeyDown KEY_UP) (- player-speed))
                        ((IsKeyDown KEY_DOWN) player-speed)
                        (else 0))))
          (set! player-position (update-player-position player-position dx dy)))

	;; is player firing?
	(let ((firing (and (IsKeyDown KEY_Z) (not player-fire-active))))
	  (if firing (begin
		       (set! player-fire-active #t)
		       (set! player-fire-position (list (car player-position) (cadr player-position))))))

	;; player fire
	(if player-fire-active
	    (begin
	      (set! player-fire-position (update-player-fire-position player-fire-position))
	      (let* ((new-pos-grid (pos-to-grid player-fire-position))
		     (grid-x (car new-pos-grid))
		     (grid-y (cadr new-pos-grid)))
		(cond
		 ((< (cadr player-fire-position) 0)
		  (set! player-fire-active #f)) ; Remove fire when goes out of screen
		 ((= CELL-MUSHROOM (array-ref grid-data grid-y grid-x))
		  (array-set! grid-data CELL-EMPTY grid-y grid-x)
		  (set! player-fire-active #f)
		  (add-score 50)) ; Check collision with mushrooms		 
		 ))))


	;; enemy position
	(for-each (lambda (enemy)
		    (let ((direction (vector-ref enemy 0))
			  (e-x (vector-ref enemy 1))
			  (e-y (vector-ref enemy 2)))
		      (cond
		       ((= DIRECTION-L direction) (vector-set! enemy 1 (- e-x enemy-speed)))
		       ((= DIRECTION-R direction) (vector-set! enemy 1 (+ e-x enemy-speed))))
		      (let* ((new-pos (list (vector-ref enemy 1) (vector-ref enemy 2)))
			     (new-pos-grid (pos-to-grid new-pos))
			     (grid-x (car new-pos-grid))
			     (grid-y (cadr new-pos-grid)))
			(cond
			 ((> grid-x (- grid-width 2))
			  (begin
			      (vector-set! enemy 2 (+ sprite-size e-y))
			      (vector-set! enemy 0 DIRECTION-L)))
			 ((< grid-x 0)
			  (begin
			    (vector-set! enemy 2 (+ sprite-size e-y))
			    (vector-set! enemy 0 DIRECTION-R)))
			 ((= CELL-MUSHROOM (array-ref grid-data grid-y grid-x))
			  (begin
			    (vector-set! enemy 2 (+ sprite-size e-y))
			    (vector-set! enemy 0 (opposite-direction direction))))))))
			
		  enemies)
	
	;; Fire collision with enemies and player
	(set!
	 enemies
	 (filter
	  (lambda (enemy)
	    (cond
	     ((rectangles-collide? (list (enemy-get-x enemy) (enemy-get-y enemy)) player-fire-position sprite-size) 
	      (add-score 100)
	      ;; Dead enemies convert into a mushroom
	      (let ((dead-enemy-grid (pos-to-grid (list (enemy-get-x enemy) (enemy-get-y enemy)))))
		(array-set! grid-data CELL-MUSHROOM (cadr dead-enemy-grid) (car dead-enemy-grid)))
	      #f)
	     ((rectangles-collide? (list (enemy-get-x enemy) (enemy-get-y enemy)) player-position sprite-size)
	      (set! lives (- lives 1))
	      (set! player-position '(0 600))
	      #t)
	     (else #t)))
	  enemies))
	
        ;; Draw
        (BeginDrawing)
	
	;; Draw background
        (ClearBackground (make-Color 30 30 30 255))
	(DrawRectangle 0 (- screen-height player-movement-area)
		       screen-width player-movement-area
		       (make-Color 50 40 50 255))

	;; player
	(DrawTexture shiptex (car player-position) (cadr player-position) WHITE)

	;; fire
	(if player-fire-active
	    (DrawTexture firetex (car player-fire-position) (cadr player-fire-position) WHITE))

	;; draw grid - mushrooms
	(for-each (lambda (row)
		    (for-each (lambda (col)
				(let ((x (* col sprite-size))
				      (y (* row sprite-size)))
				  (if (= (array-ref grid-data row col) CELL-MUSHROOM)
					(DrawTexture mushtex x y WHITE))))
			      (iota grid-width)))
		  (iota grid-height))

	;; enemies
	(for-each (lambda (enemy)
		    (let ((e-x (vector-ref enemy 1))
			  (e-y (vector-ref enemy 2)))
		      (DrawTexture enemytex e-x e-y WHITE)))
		  enemies)

	;; Hud
	(DrawText (format #f "~6,'0d" score) 0 0 28 WHITE)
	(DrawText (format #f "Level: ~d" level) (- (/ screen-width 2) 50) 0 28 WHITE)
	(DrawText (format #f "~d" lives) (- screen-width 30) 0 28 WHITE)
	(EndDrawing)
	
        ;; Recursively call the main loop
        (main-loop))))

;; Start the main game loop
(main-loop)

;; De-Initialization
(CloseWindow) ; Close window and OpenGL context
