#lang racket/base

(require
 ffi/unsafe
 ffi/unsafe/define
 sdl2
 sdl2/private/lib-path)

(provide
 (all-defined-out))


(define-ffi-definer define-sdl2-image (ffi-lib (lib-path "image") '("0" #f)))

(define-sdl2-image IMG_Linked_Version (_fun -> _SDL_version*))

(define _IMG_InitFlags
    (_bitmask
     '(IMG_INIT_JPG = #x00000001
       IMG_INIT_PNG = #x00000002
       IMG_INIT_TIF = #x00000004
       IMG_INIT_WEBP = #x00000008)))

(define-sdl2-image IMG_Init (_fun _IMG_InitFlags -> _IMG_InitFlags))

(define-sdl2-image IMG_Quit (_fun -> _void))

(define-sdl2-image IMG_LoadTyped_RW (_fun _SDL_RWops* _int _string -> _SDL_Surface*/null))
(define-sdl2-image IMG_Load (_fun _string -> _SDL_Surface*/null))
(define-sdl2-image IMG_Load_RW (_fun _SDL_RWops* _int -> _SDL_Surface*/null))

(define-sdl2-image IMG_LoadTexture (_fun _SDL_Renderer* _string -> _SDL_Texture*/null))
(define-sdl2-image IMG_LoadTexture_RW (_fun _SDL_Renderer* _SDL_RWops* _int -> _SDL_Texture*/null))
(define-sdl2-image IMG_LoadTextureTyped_RW (_fun _SDL_Renderer* _SDL_RWops* _int _string -> _SDL_Texture*/null))

(define-sdl2-image IMG_isICO  (_fun _SDL_RWops* -> _int))
(define-sdl2-image IMG_isCUR  (_fun _SDL_RWops* -> _int))
(define-sdl2-image IMG_isBMP  (_fun _SDL_RWops* -> _int))
(define-sdl2-image IMG_isGIF  (_fun _SDL_RWops* -> _int))
(define-sdl2-image IMG_isJPG  (_fun _SDL_RWops* -> _int))
(define-sdl2-image IMG_isLBM  (_fun _SDL_RWops* -> _int))
(define-sdl2-image IMG_isPCX  (_fun _SDL_RWops* -> _int))
(define-sdl2-image IMG_isPNG  (_fun _SDL_RWops* -> _int))
(define-sdl2-image IMG_isPNM  (_fun _SDL_RWops* -> _int))
(define-sdl2-image IMG_isSVG  (_fun _SDL_RWops* -> _int)
    #:make-fail make-not-available)
(define-sdl2-image IMG_isTIF  (_fun _SDL_RWops* -> _int))
(define-sdl2-image IMG_isXCF  (_fun _SDL_RWops* -> _int))
(define-sdl2-image IMG_isXPM  (_fun _SDL_RWops* -> _int))
(define-sdl2-image IMG_isXV   (_fun _SDL_RWops* -> _int))
(define-sdl2-image IMG_isWEBP (_fun _SDL_RWops* -> _int))

(define-sdl2-image IMG_LoadICO_RW  (_fun _SDL_RWops* -> _SDL_Surface*/null))
(define-sdl2-image IMG_LoadCUR_RW  (_fun _SDL_RWops* -> _SDL_Surface*/null))
(define-sdl2-image IMG_LoadBMP_RW  (_fun _SDL_RWops* -> _SDL_Surface*/null))
(define-sdl2-image IMG_LoadGIF_RW  (_fun _SDL_RWops* -> _SDL_Surface*/null))
(define-sdl2-image IMG_LoadJPG_RW  (_fun _SDL_RWops* -> _SDL_Surface*/null))
(define-sdl2-image IMG_LoadLBM_RW  (_fun _SDL_RWops* -> _SDL_Surface*/null))
(define-sdl2-image IMG_LoadPCX_RW  (_fun _SDL_RWops* -> _SDL_Surface*/null))
(define-sdl2-image IMG_LoadPNG_RW  (_fun _SDL_RWops* -> _SDL_Surface*/null))
(define-sdl2-image IMG_LoadPNM_RW  (_fun _SDL_RWops* -> _SDL_Surface*/null))
(define-sdl2-image IMG_LoadSVG_RW  (_fun _SDL_RWops* -> _SDL_Surface*/null)
    #:make-fail make-not-available)
(define-sdl2-image IMG_LoadTGA_RW  (_fun _SDL_RWops* -> _SDL_Surface*/null))
(define-sdl2-image IMG_LoadTIF_RW  (_fun _SDL_RWops* -> _SDL_Surface*/null))
(define-sdl2-image IMG_LoadXCF_RW  (_fun _SDL_RWops* -> _SDL_Surface*/null))
(define-sdl2-image IMG_LoadXPM_RW  (_fun _SDL_RWops* -> _SDL_Surface*/null))
(define-sdl2-image IMG_LoadXV_RW   (_fun _SDL_RWops* -> _SDL_Surface*/null))
(define-sdl2-image IMG_LoadWEBP_RW (_fun _SDL_RWops* -> _SDL_Surface*/null))

(define-sdl2-image IMG_ReadXPMFromArray (_fun (_ptr io _string) -> _SDL_Surface*/null))

(define-sdl2-image IMG_SavePNG (_fun _SDL_Surface* _string -> _int))
(define-sdl2-image IMG_SavePNG_RW (_fun _SDL_Surface* _SDL_RWops* _int -> _int))
(define-sdl2-image IMG_SaveJPG (_fun _SDL_Surface* _string _int -> _int)
    #:make-fail make-not-available)
(define-sdl2-image IMG_SaveJPG_RW (_fun _SDL_Surface* _SDL_RWops* _int _int -> _int)
    #:make-fail make-not-available)

(define IMG_SetError SDL_SetError)
(define IMG_GetError SDL_GetError)
