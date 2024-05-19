(use-modules (raylib)
	     (srfi srfi-27)) ; For array procedures


;; Initialization
(define screen-width 800)
(define screen-height 600)

(InitWindow screen-width screen-height "Basic Game Loop with Raylib")

;; Define a player object
(define player-position (list (/ screen-width 2) 550))
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
(define grid-data (make-array 0 grid-height grid-width))

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
(define random-positions (generate-random-positions (* grid-width grid-height) 40))

;; Fill the array with #t in the random positions
(for-each (lambda (pos)
            (let ((row (quotient pos grid-width))
                  (col (remainder pos grid-width)))
              (array-set! grid-data 1 row col)))
          random-positions)
(display grid-data)
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
				(if (= (array-ref grid-data row col) 1)
				    (DrawTexture mushtex (* row 16) (* col 16) WHITE)))
                      (iota grid-width)))
        (iota grid-height))

	;; enemies
	(EndDrawing)

        ;; Recursively call the main loop
        (main-loop))))

;; Start the main game loop
(main-loop)

;; De-Initialization
(CloseWindow) ; Close window and OpenGL context
