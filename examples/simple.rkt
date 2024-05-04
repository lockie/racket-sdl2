#lang racket/base

;; NOTE: this is mere translation of https://git.io/fj0Uo

(require sdl2)

(define (main)
    (unless (zero? (SDL_Init 'SDL_INIT_EVERYTHING))
        (eprintf "SDL_Init Error: ~a\n" (SDL_GetError))
        (exit 1))
    (define win
        (SDL_CreateWindow "Hello World!" 100 100 620 387 'SDL_WINDOW_SHOWN))
    (unless win
        (eprintf "SDL_CreateWindow Error: ~a\n" (SDL_GetError))
        (exit 1))
    (define ren
        (SDL_CreateRenderer win -1 '(SDL_RENDERER_ACCELERATED SDL_RENDERER_PRESENTVSYNC)))
    (unless ren
        (eprintf "SDL_CreateRenderer Error: ~a\n" (SDL_GetError))
        (exit 1))
    (define bmp
        (SDL_LoadBMP (path->complete-path "grumpy-cat.bmp")))
    (unless bmp
        (eprintf "SDL_LoadBMP Error: ~a\n" (SDL_GetError)))
    (define tex
        (SDL_CreateTextureFromSurface ren bmp))
    (unless tex
        (eprintf "SDL_CreateTextureFromSurface Error: ~a\n" (SDL_GetError)))
    (SDL_FreeSurface bmp)

    (for ([i 20])
        (SDL_RenderClear ren)
        (SDL_RenderCopy ren tex #f #f)
        (SDL_RenderPresent ren)
        (SDL_Delay 100))

    (SDL_DestroyTexture tex)
    (SDL_DestroyRenderer ren)
    (SDL_DestroyWindow win)
    (SDL_Quit))


(main)
