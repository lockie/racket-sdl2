#lang racket/base

(require
 (for-syntax racket/base
             racket/syntax
             racket/string)
 ffi/unsafe
 ffi/unsafe/define
 (rename-in sdl2/pretty
            [set-error! sdl-set-error!]
            [get-error sdl-get-error])
 sdl2/private/lib-path)

(provide
 (except-out
  (all-defined-out)
  define-sdl2-ttf
  convention:uglify))


(define-syntax (convention:uglify id)
    (define str
        (string-replace
         (string-replace
          (symbol->string (syntax-e id))
          "!" "")
         "?" ""))
    (when (string-suffix? str "-version")
        (set! str (string-replace str "-version" "-_-version")))
    (when (string-suffix? str "-solid")
        (set! str (string-replace str "-solid" "-_-solid")))
    (when (string-suffix? str "-shaded")
        (set! str (string-replace str "-shaded" "-_-shaded")))
    (when (string-suffix? str "-blended")
        (set! str (string-replace str "-blended" "-_-blended")))
    (when (string-suffix? str "-blended-wrapped")
        (set! str (string-replace str "-blended-wrapped" "-_-blended-wrapped")))
    (when (string-suffix? str "-wrapped")
        (set! str (string-replace str "-wrapped" "-_-wrapped")))
    (define uppercase-exceptions
        '("rw" "unicode" "utf8"))
    (format-id id
               (apply string-append
                      "TTF_"
                      (map
                       (lambda (s)
                           (if (member s uppercase-exceptions)
                               (string-upcase s)
                               (string-titlecase s)))
                       (string-split str "-")))))

(define-ffi-definer define-sdl2-ttf
    (ffi-lib (lib-path "ttf") '("0" #f))
    #:make-c-id convention:uglify)

(define-sdl2-ttf linked-version (_fun -> _version*))

(define unicode-bom-native  #xfeff)
(define unicode-bom-swapped #xfffe)

(define-sdl2-ttf byte-swapped-unicode (_fun _int -> _void))

(define-cpointer-type _font*)

(define-sdl2-ttf init! (_fun -> _int))

(define-sdl2-ttf open-font (_fun _string _int -> _font*/null))
(define-sdl2-ttf open-font-index (_fun _string _int _long -> _font*/null))
(define-sdl2-ttf open-font-rw (_fun _rw-ops* _int _int -> _font*/null))
(define-sdl2-ttf open-font-index-rw (_fun _rw-ops* _int _int _long -> _font*/null))

(define _style
    (_bitmask
     '(normal        = #x00
       bold          = #x01
       italic        = #x02
       underline     = #x04
       strikethrough = #x08)
     _int))

(define-sdl2-ttf get-font-style (_fun _font* -> _style))
(define-sdl2-ttf set-font-style! (_fun _font* _style -> _void))
(define-sdl2-ttf get-font-outline (_fun _font* -> _int))
(define-sdl2-ttf set-font-outline! (_fun _font* _int -> _void))

(define _hinting
    (_enum
     '(normal   = 0
       light    = 1
       mono     = 2
       none     = 3)))

(define-sdl2-ttf get-font-hinting (_fun _font* -> _hinting))
(define-sdl2-ttf set-font-hinting! (_fun _font* _hinting -> _void))

(define-sdl2-ttf font-height (_fun _font* -> _int))

(define-sdl2-ttf font-ascent (_fun _font* -> _int))

(define-sdl2-ttf font-descent (_fun _font* -> _int))

(define-sdl2-ttf font-line-skip (_fun _font* -> _int))

(define-sdl2-ttf get-font-kerning (_fun _font* -> _int))
(define-sdl2-ttf set-font-kerning! (_fun _font* _int -> _void))

(define-sdl2-ttf font-faces (_fun _font* -> _long))

(define-sdl2-ttf font-face-is-fixed-width (_fun _font* -> _int))
(define-sdl2-ttf font-face-family-name (_fun _font* -> _string))
(define-sdl2-ttf font-face-style-name (_fun _font* -> _string))

(define-sdl2-ttf glyph-is-provided (_fun _font* _uint16 -> _int))

(define-sdl2-ttf glyph-metrics (_fun _font* _uint16 _int* _int* _int* _int* _int* -> _int))

(define-sdl2-ttf size-text (_fun _font* _string _int* _int* -> _int))
(define-sdl2-ttf size-utf8 (_fun _font* _string _int* _int* -> _int))
(define-sdl2-ttf size-unicode (_fun _font* _uint16* _int* _int* -> _int))

(define-sdl2-ttf render-text-solid (_fun _font* _string _color -> _surface*/null))
(define-sdl2-ttf render-utf8-solid (_fun _font* _string _color -> _surface*/null))
(define-sdl2-ttf render-unicode-solid (_fun _font* _uint16* _color -> _surface*/null))

(define-sdl2-ttf render-glyph-solid (_fun _font* _uint16 _color -> _surface*/null))

(define-sdl2-ttf render-text-shaded (_fun _font* _string _color _color -> _surface*/null))
(define-sdl2-ttf render-utf8-shaded (_fun _font* _string _color _color -> _surface*/null))
(define-sdl2-ttf render-unicode-shaded (_fun _font* _uint16* _color _color -> _surface*/null))

(define-sdl2-ttf render-glyph-shaded (_fun _font* _uint16 _color _color -> _surface*/null))

(define-sdl2-ttf render-text-blended (_fun _font* _string _color -> _surface*/null))
(define-sdl2-ttf render-utf8-blended (_fun _font* _string _color -> _surface*/null))
(define-sdl2-ttf render-unicode-blended (_fun _font* _uint16* _color -> _surface*/null))

(define-sdl2-ttf render-text-blended-wrapped (_fun _font* _string _color _uint32 -> _surface*/null))
(define-sdl2-ttf render-utf8-blended-wrapped (_fun _font* _string _color _uint32 -> _surface*/null))
(define-sdl2-ttf render-unicode-blended-wrapped (_fun _font* _uint16* _color _uint32 -> _surface*/null))

(define-sdl2-ttf render-glyph-blended (_fun _font* _uint16 _color -> _surface*/null))
(define-sdl2-ttf close-font! (_fun _font* -> _void))

(define-sdl2-ttf quit! (_fun -> _void))

(define-sdl2-ttf was-init? (_fun -> _int))

(define-sdl2-ttf get-font-kerning-size-glyphs (_fun _font* _uint16 _uint16 -> _int)
    #:make-fail make-not-available)

(define set-error!    sdl-set-error!)
(define get-error    sdl-get-error)
