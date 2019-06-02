#lang racket/base

;; NOTE: this is mere translation of https://bitbucket.org/dandago/gigilabs/src/master/Sdl2KeyboardMouse/Sdl2KeyboardMouse/main.cpp

(require
 sdl2
 ffi/unsafe)

(define (main)
    ;; variables

    (define quit #f)
    (define event-ptr (cast (malloc (ctype-sizeof _SDL_Event)) _pointer _SDL_Event*))
    (define x 288)
    (define y 208)

    ;; init SDL

    (SDL_Init 'SDL_INIT_VIDEO)
    (define window (SDL_CreateWindow "SDL2 Keyboard/Mouse events"
                                     SDL_WINDOWPOS_UNDEFINED
                                     SDL_WINDOWPOS_UNDEFINED
                                     640 480 '()))
    (define renderer (SDL_CreateRenderer window -1 '()))

    (define image (SDL_LoadBMP "spaceship.bmp"))
    (define texture (SDL_CreateTextureFromSurface renderer image))
    (SDL_FreeSurface image)

    (SDL_SetRenderDrawColor renderer 255 255 255 255)

    ;; handle events

    (for ([dummy (in-naturals)]
          #:break quit)
        (SDL_Delay 20)
        (SDL_PollEvent event-ptr)
        (define event (ptr-ref event-ptr _SDL_Event))

        (case (union-ref event 0)
            [(SDL_QUIT)
             (set! quit #t)]
            [(SDL_KEYDOWN)
             (case (SDL_Keysym-sym
                    (SDL_KeyboardEvent-keysym
                     (union-ref event 4)))
                 [(SDLK_LEFT) (set! x (sub1 x))]
                 [(SDLK_RIGHT) (set! x (add1 x))]
                 [(SDLK_UP) (set! y (sub1 y))]
                 [(SDLK_DOWN) (set! y (add1 y))])]
            [(SDL_MOUSEBUTTONDOWN)
             (case (SDL_MouseButtonEvent-button (union-ref event 8))
                 [(SDL_BUTTON_LEFT)
                  (SDL_ShowSimpleMessageBox 0 "Mouse" "Left button was pressed!" window)]
                 [(SDL_BUTTON_RIGHT)
                  (SDL_ShowSimpleMessageBox 0 "Mouse" "Right button was pressed!" window)]
                 [else
                  (SDL_ShowSimpleMessageBox 0 "Mouse" "Some other button was pressed!" window)])]
            [(SDL_MOUSEMOTION)
             (define motion (union-ref event 7))
             (SDL_SetWindowTitle
              window
              (format "X: ~a Y: ~a"
                      (SDL_MouseMotionEvent-x motion)
                      (SDL_MouseMotionEvent-y motion)))])

        (SDL_RenderClear renderer)
        (SDL_RenderCopy renderer texture #f (make-SDL_Rect x y 64 64))
        (SDL_RenderPresent renderer))

    ;; cleanup SDL

    (SDL_DestroyTexture texture)
    (SDL_DestroyRenderer renderer)
    (SDL_DestroyWindow window)
    (SDL_Quit))

(main)
