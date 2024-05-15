(use-modules (raylib))

;; Initialization
(define screen-width 800)
(define screen-height 450)
(InitWindow screen-width screen-height "Basic Game Loop with Raylib")

;; Define a player object
(define player-position (list (/ screen-width 2) (/ screen-height 2)))
(define player-speed 5)

(SetTargetFPS 60) ; Set our game to run at 60 frames-per-second

;; Function to update player position
(define (update-player-position pos dx dy)
  (list (+ (car pos) dx) (+ (cadr pos) dy)))

;; Main game loop
(define (main-loop)
  (if (not (WindowShouldClose)) ; Detect window close button or ESC key
      (begin
        ;; Update
        (let ((dx (cond ((IsKeyDown KEY_RIGHT) player-speed)
                        ((IsKeyDown KEY_LEFT) (- player-speed))
                        (else 0)))
              (dy (cond ((IsKeyDown KEY_UP) (- player-speed))
                        ((IsKeyDown KEY_DOWN) player-speed)
                        (else 0))))
          (set! player-position (update-player-position player-position dx dy)))


        ;; Draw
        (BeginDrawing)
        (ClearBackground RAYWHITE)
        (DrawText "Move the player with arrow keys" 10 10 20 DARKGRAY)
        (DrawCircleV (make-Vector2 (car player-position) (cadr player-position)) 50 MAROON)
        (EndDrawing)

        ;; Recursively call the main loop
        (main-loop))))

;; Start the main game loop
(main-loop)

;; De-Initialization
(CloseWindow) ; Close window and OpenGL context
