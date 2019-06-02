#lang racket/base

(require
 ffi/unsafe
 ffi/unsafe/define
 sdl2)

(provide
 (all-defined-out))


(define-ffi-definer define-sdl2-ttf (ffi-lib (sdl2-lib-path "ttf") '("0" #f)))

(define-sdl2-ttf TTF_Linked_Version (_fun -> _SDL_version*))

(define UNICODE_BOM_NATIVE  #xFEFF)
(define UNICODE_BOM_SWAPPED #xFFFE)

(define-sdl2-ttf TTF_ByteSwappedUNICODE (_fun _int -> _void))

(define-cpointer-type _TTF_Font*)

(define-sdl2-ttf TTF_Init (_fun -> _int))

(define-sdl2-ttf TTF_OpenFont (_fun _string _int -> _TTF_Font*/null))
(define-sdl2-ttf TTF_OpenFontIndex (_fun _string _int _long -> _TTF_Font*/null))
(define-sdl2-ttf TTF_OpenFontRW (_fun _SDL_RWops* _int _int -> _TTF_Font*/null))
(define-sdl2-ttf TTF_OpenFontIndexRW (_fun _SDL_RWops* _int _int _long -> _TTF_Font*/null))

(define TTF_STYLE_NORMAL        #x00)
(define TTF_STYLE_BOLD          #x01)
(define TTF_STYLE_ITALIC        #x02)
(define TTF_STYLE_UNDERLINE     #x04)
(define TTF_STYLE_STRIKETHROUGH #x08)

(define-sdl2-ttf TTF_GetFontStyle (_fun _TTF_Font* -> _int))
(define-sdl2-ttf TTF_SetFontStyle (_fun _TTF_Font* _int -> _void))
(define-sdl2-ttf TTF_GetFontOutline (_fun _TTF_Font* -> _int))
(define-sdl2-ttf TTF_SetFontOutline (_fun _TTF_Font* _int -> _void))

(define TTF_HINTING_NORMAL    0)
(define TTF_HINTING_LIGHT     1)
(define TTF_HINTING_MONO      2)
(define TTF_HINTING_NONE      3)

(define-sdl2-ttf TTF_GetFontHinting (_fun _TTF_Font* -> _int))
(define-sdl2-ttf TTF_SetFontHinting (_fun _TTF_Font* _int -> _void))

(define-sdl2-ttf TTF_FontHeight (_fun _TTF_Font* -> _int))

(define-sdl2-ttf TTF_FontAscent (_fun _TTF_Font* -> _int))

(define-sdl2-ttf TTF_FontDescent (_fun _TTF_Font* -> _int))

(define-sdl2-ttf TTF_FontLineSkip (_fun _TTF_Font* -> _int))

(define-sdl2-ttf TTF_GetFontKerning (_fun _TTF_Font* -> _int))
(define-sdl2-ttf TTF_SetFontKerning (_fun _TTF_Font* _int -> _void))

(define-sdl2-ttf TTF_FontFaces (_fun _TTF_Font* -> _long))

(define-sdl2-ttf TTF_FontFaceIsFixedWidth (_fun _TTF_Font* -> _int))
(define-sdl2-ttf TTF_FontFaceFamilyName (_fun _TTF_Font* -> _string))
(define-sdl2-ttf TTF_FontFaceStyleName (_fun _TTF_Font* -> _string))

(define-sdl2-ttf TTF_GlyphIsProvided (_fun _TTF_Font* _uint16 -> _int))

(define-sdl2-ttf TTF_GlyphMetrics (_fun _TTF_Font* _uint16 _int* _int* _int* _int* _int* -> _int))

(define-sdl2-ttf TTF_SizeText (_fun _TTF_Font* _string _int* _int* -> _int))
(define-sdl2-ttf TTF_SizeUTF8 (_fun _TTF_Font* _string _int* _int* -> _int))
(define-sdl2-ttf TTF_SizeUNICODE (_fun _TTF_Font* _uint16* _int* _int* -> _int))

(define-sdl2-ttf TTF_RenderText_Solid (_fun _TTF_Font* _string _SDL_Color -> _SDL_Surface*/null))
(define-sdl2-ttf TTF_RenderUTF8_Solid (_fun _TTF_Font* _string _SDL_Color -> _SDL_Surface*/null))
(define-sdl2-ttf TTF_RenderUNICODE_Solid (_fun _TTF_Font* _uint16* _SDL_Color -> _SDL_Surface*/null))

(define-sdl2-ttf TTF_RenderGlyph_Solid (_fun _TTF_Font* _uint16 _SDL_Color -> _SDL_Surface*/null))

(define-sdl2-ttf TTF_RenderText_Shaded (_fun _TTF_Font* _string _SDL_Color _SDL_Color -> _SDL_Surface*/null))
(define-sdl2-ttf TTF_RenderUTF8_Shaded (_fun _TTF_Font* _string _SDL_Color _SDL_Color -> _SDL_Surface*/null))
(define-sdl2-ttf TTF_RenderUNICODE_Shaded (_fun _TTF_Font* _uint16* _SDL_Color _SDL_Color -> _SDL_Surface*/null))

(define-sdl2-ttf TTF_RenderGlyph_Shaded (_fun _TTF_Font* _uint16 _SDL_Color _SDL_Color -> _SDL_Surface*/null))

(define-sdl2-ttf TTF_RenderText_Blended (_fun _TTF_Font* _string _SDL_Color -> _SDL_Surface*/null))
(define-sdl2-ttf TTF_RenderUTF8_Blended (_fun _TTF_Font* _string _SDL_Color -> _SDL_Surface*/null))
(define-sdl2-ttf TTF_RenderUNICODE_Blended (_fun _TTF_Font* _uint16* _SDL_Color -> _SDL_Surface*/null))

(define-sdl2-ttf TTF_RenderText_Blended_Wrapped (_fun _TTF_Font* _string _SDL_Color _uint32 -> _SDL_Surface*/null))
(define-sdl2-ttf TTF_RenderUTF8_Blended_Wrapped (_fun _TTF_Font* _string _SDL_Color _uint32 -> _SDL_Surface*/null))
(define-sdl2-ttf TTF_RenderUNICODE_Blended_Wrapped (_fun _TTF_Font* _uint16* _SDL_Color _uint32 -> _SDL_Surface*/null))

(define-sdl2-ttf TTF_RenderGlyph_Blended (_fun _TTF_Font* _uint16 _SDL_Color -> _SDL_Surface*/null))
(define-sdl2-ttf TTF_CloseFont (_fun _TTF_Font* -> _void))

(define-sdl2-ttf TTF_Quit (_fun -> _void))

(define-sdl2-ttf TTF_WasInit (_fun -> _int))

(define-sdl2-ttf TTF_GetFontKerningSizeGlyphs (_fun _TTF_Font* _uint16 _uint16 -> _int)
    #:make-fail make-not-available)

(define TTF_SetError    SDL_SetError)
(define TTF_GetError    SDL_GetError)
