#lang racket/base

;; NOTE: this is mere translation of https://git.io/fj0kd

(require sdl2)

(unless (zero? (SDL_Init 'SDL_INIT_VIDEO))
    (eprintf "could not initialize sdl2: ~a\n" (SDL_GetError)))

(define window (SDL_CreateWindow
                "hello_sdl2"
                SDL_WINDOWPOS_UNDEFINED
                SDL_WINDOWPOS_UNDEFINED
                640
                480
                'SDL_WINDOW_SHOWN))
(unless window
    (eprintf "could not create window: ~a\n" (SDL_GetError)))

(define screen-surface (SDL_GetWindowSurface window))
(SDL_FillRect screen-surface #f (SDL_MapRGB (SDL_Surface-format screen-surface) #xFF #xFF #xFF))
(SDL_UpdateWindowSurface window)
(SDL_Delay 2000)
(SDL_DestroyWindow window)
(SDL_Quit)
