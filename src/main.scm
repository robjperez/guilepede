(use-modules (raylib)
	     (srfi srfi-27)) ; For array procedures

(define CELL-MUSHROOM 1)
(define CELL-EMPTY 0)
(define DIRECTION-U 0)
(define DIRECTION-D 1)
(define DIRECTION-L 2)
(define DIRECTION-R 3)

;; Initialization
(define screen-width 512)
(define screen-height 720)

(InitWindow screen-width screen-height "Basic Game Loop with Raylib")

;; Define a player object
(define player-position (list (/ screen-width 2) 650))
(define player-speed 5)

(define player-fire-position '(0 600))
(define player-fire-active #f)

(SetTargetFPS 60) ; Set our game to run at 60 frames-per-second

;; Function to update player position
(define (update-player-position pos dx dy)
  (list (+ (car pos) dx) (+ (cadr pos) dy)))

(define (update-player-fire-position pos)
  (list (car pos) (- (cadr pos) 15)))

(define shiptex (LoadTexture "./assets/ship16.png"))
(define firetex (LoadTexture "./assets/fire16.png"))
(define enemytex (LoadTexture "./assets/enemy16.png"))
(define mushtex (LoadTexture "./assets/mushroom16.png"))

(define grid-width 32)
(define grid-height 45)
(define grid-data (make-array CELL-EMPTY grid-height grid-width))

;; Utilities
(define (pos-to-grid pos) (list (floor/ (car pos) 16) (floor/ (cadr pos) 16)))
(define (opposite-direction dir)
  (cond
   ((= dir DIRECTION-L) DIRECTION-R)
   ((= dir DIRECTION-R) DIRECTION-L)))

;; Fill the grid with random mushrooms
(define (generate-random-positions total-cells num-positions)
  (let loop ((positions '()))
    (if (= (length positions) num-positions)
        positions
        (let ((new-pos (random-integer total-cells)))
          (if (member new-pos positions)
              (loop positions)
              (loop (cons new-pos positions)))))))
;; Generate 20 unique random positions
(define random-positions (generate-random-positions (* grid-width grid-height) 10))

;; Fill the array with #t in the random positions
(for-each (lambda (pos)
            (let ((row (quotient pos grid-height))
                  (col (remainder pos grid-width)))
              (array-set! grid-data CELL-MUSHROOM row col)))
          random-positions)

;; Define enemies
(define enemies
  (list
   (vector DIRECTION-R 0 0)
   (vector DIRECTION-R 16 0)
   (vector DIRECTION-R 32 0)
   (vector DIRECTION-R 48 0)
   (vector DIRECTION-R 64 0)))

(define (enemy-get-x enemy) (vector-ref enemy 1))
(define (enemy-get-y enemy) (vector-ref enemy 2))

;; Main game loop
(define (main-loop)
  (if (not (WindowShouldClose)) ; Detect window close button or ESC key
      (begin
        ;; Update
	;; Player pos
        (let ((dx (cond ((IsKeyDown KEY_RIGHT) player-speed)
                        ((IsKeyDown KEY_LEFT) (- player-speed))
                        (else 0)))
              (dy (cond ((IsKeyDown KEY_UP) (- player-speed))
                        ((IsKeyDown KEY_DOWN) player-speed)
                        (else 0)))
	      (firing (and (IsKeyDown KEY_Z) (not player-fire-active))))
          (set! player-position (update-player-position player-position dx dy))
	  (if firing (begin
		       (set! player-fire-active #t)
		       (set! player-fire-position (list (car player-position) (cadr player-position))))))

	;; player fire
	(if player-fire-active
	    (begin
	      (set! player-fire-position (update-player-fire-position player-fire-position))
	      (if (< (cadr player-fire-position) 0)
					; (set! player-fire-active 0)
		  (set! player-fire-active #f))))


	;; enemy position
	(for-each (lambda (enemy)
		    (let ((direction (vector-ref enemy 0))
			  (e-x (vector-ref enemy 1))
			  (e-y (vector-ref enemy 2)))
		      (cond
		       ((= DIRECTION-L direction) (vector-set! enemy 1 (- e-x 5)))
		       ((= DIRECTION-R direction) (vector-set! enemy 1 (+ e-x 5))))
		      (let* ((new-pos (list (vector-ref enemy 1) (vector-ref enemy 2)))
			     (new-pos-grid (pos-to-grid new-pos))
			     (grid-x (car new-pos-grid))
			     (grid-y (cadr new-pos-grid)))
			(cond
		
			 ((> grid-x grid-width)
			  (begin
			      (display "R BOUNDS ")
			      (vector-set! enemy 2 (+ 16 e-y))
			      (vector-set! enemy 0 DIRECTION-L)))
			 ((< grid-x 0)
			  (begin
			    (display "L BOUNDS ")
			    (vector-set! enemy 2 (+ 16 e-y))
			    (vector-set! enemy 0 DIRECTION-R)))
			 ((= CELL-MUSHROOM (array-ref grid-data grid-x grid-y))
			    (begin
			      (display "HIT! ")
			      (vector-set! enemy 2 (+ 16 e-y))
			      (vector-set! enemy 0 (opposite-direction direction))))))))
			
		  enemies)
			
        ;; Draw
        (BeginDrawing)
        (ClearBackground BLACK)
	;; player
	(DrawTexture shiptex (car player-position) (cadr player-position) WHITE)
	;; fire
	(if player-fire-active
	    (DrawTexture firetex (car player-fire-position) (cadr player-fire-position) WHITE))

	;; draw grid - mushrooms
	(for-each (lambda (row)
		    (for-each (lambda (col)
				(let ((x (* col 16))
				      (y (* row 16)))
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
	(EndDrawing)

        ;; Recursively call the main loop
        (main-loop))))

;; Start the main game loop
(main-loop)

;; De-Initialization
(CloseWindow) ; Close window and OpenGL context
