(use-modules (raylib)
	     (srfi srfi-27)
	     (ice-9 format)) ; For array procedures

(define CELL-MUSHROOM 1)
(define CELL-EMPTY 0)
(define DIRECTION-U 0)
(define DIRECTION-D 1)
(define DIRECTION-L 2)
(define DIRECTION-R 3)

;; Initialization
(set! *random-state* (random-state-from-platform))

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

(define score 0)

;; Utilities
(define (pos-to-grid pos)
  (list
   (min (floor/ (car pos) 16) (- grid-width 1))
   (min (floor/ (cadr pos) 16) (- grid-height 1))))

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

;; Fill the grid with random mushrooms
(define (generate-random-positions total-cells num-positions)
  (let loop ((positions '()))
    (if (= (length positions) num-positions)
        positions
        (let ((new-pos (random total-cells)))
          (if (member new-pos positions)
              (loop positions)
              (loop (cons new-pos positions)))))))
;; Generate 20 unique random positions
(define random-positions (generate-random-positions (* grid-width grid-height) 60))

;; Fill the array with #t in the random positions
(for-each (lambda (pos)
            (let ((row (quotient pos grid-width))
                  (col (remainder pos grid-width)))
              (array-set! grid-data CELL-MUSHROOM row col)))
          random-positions)

;; Define enemies
(define (make-enemies2 blocks)
  (let ((enemies (list)))
    (for-each (lambda (block-index)
		(format #t "bi: ~d\n" block-index)
		(let ((number-of-enemies (random 8))
		      (direction (+ 2 (random 1)))
		      (start-x (random screen-width)))
		  (format #t "ne: ~d, dir: ~d, x: ~d\n" number-of-enemies direction start-x)
		  (for-each (lambda (index)
			      (append enemies (list (vector direction start-x 0)))
			      (display enemies)
			      (newline))
			    (iota number-of-enemies))		  
		  ))
	      (iota blocks))
    enemies
    ))

(define (make-enemies blocks)
  (apply append (map (lambda (block-index)
		       (let ((number-of-enemies (random 5))
			     (start-x (random screen-width))
			     (start-y (* 16 (random 2)))
			     (direction (+ 2 (random 1))))
			 (map (lambda (index)
				(vector direction (+ (* 16 index) start-x) start-y))
			      (iota number-of-enemies))))
		     (iota blocks))))

(define enemies (make-enemies 10))
(define (enemy-get-x enemy) (vector-ref enemy 1))
(define (enemy-get-y enemy) (vector-ref enemy 2))
(define level-completed #f)
(define level 1)
(define lives 4)
;; Main game loop
(define (main-loop)
  (if (not (WindowShouldClose)) ; Detect window close button or ESC key
      (begin
        ;; Update
	(if (= (length enemies) 0)
	    (begin
	      (set! level (+ 1 level))
	      (set! enemies (make-enemies (* 3 level)))))
	
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
	      (let* ((new-pos-grid (pos-to-grid player-fire-position))
		     (grid-x (car new-pos-grid))
		     (grid-y (cadr new-pos-grid)))
		(cond
		 ((< (cadr player-fire-position) 0)
		  (set! player-fire-active #f)) ;Remove fire when goes out of screen
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
		       ((= DIRECTION-L direction) (vector-set! enemy 1 (- e-x 3)))
		       ((= DIRECTION-R direction) (vector-set! enemy 1 (+ e-x 3))))
		      (let* ((new-pos (list (vector-ref enemy 1) (vector-ref enemy 2)))
			     (new-pos-grid (pos-to-grid new-pos))
			     (grid-x (car new-pos-grid))
			     (grid-y (cadr new-pos-grid)))
			(cond
			 ((> grid-x (- grid-width 2))
			  (begin
			      ;(display "R BOUNDS\n")
			      (vector-set! enemy 2 (+ 16 e-y))
			      (vector-set! enemy 0 DIRECTION-L)))
			 ((< grid-x 0)
			  (begin
			    ;(format #t "L BOUNDS\n")
			    (vector-set! enemy 2 (+ 16 e-y))
			    (vector-set! enemy 0 DIRECTION-R)))
			 ((= CELL-MUSHROOM (array-ref grid-data grid-y grid-x))
			  (begin
			    ;(format #t "HIT! ~d, ~d\n" grid-x grid-y)
			    (vector-set! enemy 2 (+ 16 e-y))
			    (vector-set! enemy 0 (opposite-direction direction))))))))
			
		  enemies)
	
	;; Fire collision with enemies and player
	(set! enemies (filter (lambda (enemy)
				(cond
				 ((rectangles-collide? (list (enemy-get-x enemy) (enemy-get-y enemy)) player-fire-position 16) 
				  (add-score 100)
				  ; Dead enemies convert into a mushroom
				  (let ((dead-enemy-grid (pos-to-grid (list (enemy-get-x enemy) (enemy-get-y enemy)))))
				    (array-set! grid-data CELL-MUSHROOM (cadr dead-enemy-grid) (car dead-enemy-grid)))
				  #f)
				 ((rectangles-collide? (list (enemy-get-x enemy) (enemy-get-y enemy)) player-position 16)
				  (set! lives (- lives 1))
				  (set! player-position '(0 600))
				  #t)
				 (else #t)))
			      enemies))
	
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
