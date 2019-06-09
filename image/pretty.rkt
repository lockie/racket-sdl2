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
  define-sdl2-image
  convention:uglify))


(define-syntax (convention:uglify id)
    (define str (string-replace (symbol->string (syntax-e id)) "!" ""))
    (when (string-suffix? str "?")
        (set! str (string-append
                   "is-" (string-replace str "?" ""))))
    (when (string-suffix? str "-rw")
        (set! str (string-replace str "-rw" "-_-rw")))
    (when (string-suffix? str "-version")
        (set! str (string-replace str "-version" "-_-version")))
    (define uppercase-exceptions
        '("rw" "ico" "cur" "bmp" "gif" "jpg" "lbm" "pcx" "png" "pnm" "svg" "tga"
               "tif" "xcf" "xpm" "xv" "webp"))
    (format-id id
               (apply string-append
                      "IMG_"
                      (map
                       (lambda (s)
                           (cond
                               [(member s uppercase-exceptions)
                                (string-upcase s)]
                               [(string=? s "is") "is"]
                               [else (string-titlecase s)]))
                       (string-split str "-")))))

(define-ffi-definer define-sdl2-image
    (ffi-lib (lib-path "image") '("0" #f))
    #:make-c-id convention:uglify)

(define-sdl2-image linked-version (_fun -> _version*))

(define _init-flags
    (_bitmask
     '(jpg = #x00000001
       png = #x00000002
       tif = #x00000004
       webp = #x00000008)))

(define-sdl2-image init! (_fun _init-flags -> _init-flags))

(define-sdl2-image quit! (_fun -> _void))

(define-sdl2-image load-typed-rw (_fun _rw-ops* _int _string -> _surface*/null))
(define-sdl2-image load (_fun _string -> _surface*/null))
(define-sdl2-image load-rw (_fun _rw-ops* _int -> _surface*/null))

(define-sdl2-image load-texture (_fun _renderer* _string -> _texture*/null))
(define-sdl2-image load-texture-rw (_fun _renderer* _rw-ops* _int -> _texture*/null))
(define-sdl2-image load-texture-typed-rw (_fun _renderer* _rw-ops* _int _string -> _texture*/null))

(define-sdl2-image ico?  (_fun _rw-ops* -> _int))
(define-sdl2-image cur?  (_fun _rw-ops* -> _int))
(define-sdl2-image bmp?  (_fun _rw-ops* -> _int))
(define-sdl2-image gif?  (_fun _rw-ops* -> _int))
(define-sdl2-image jpg?  (_fun _rw-ops* -> _int))
(define-sdl2-image lbm?  (_fun _rw-ops* -> _int))
(define-sdl2-image pcx?  (_fun _rw-ops* -> _int))
(define-sdl2-image png?  (_fun _rw-ops* -> _int))
(define-sdl2-image pnm?  (_fun _rw-ops* -> _int))
(define-sdl2-image svg?  (_fun _rw-ops* -> _int)
    #:make-fail make-not-available)
(define-sdl2-image tif?  (_fun _rw-ops* -> _int))
(define-sdl2-image xcf?  (_fun _rw-ops* -> _int))
(define-sdl2-image xpm?  (_fun _rw-ops* -> _int))
(define-sdl2-image xv?   (_fun _rw-ops* -> _int))
(define-sdl2-image webp? (_fun _rw-ops* -> _int))

(define-sdl2-image load-ico-rw  (_fun _rw-ops* -> _surface*/null))
(define-sdl2-image load-cur-rw  (_fun _rw-ops* -> _surface*/null))
(define-sdl2-image load-bmp-rw  (_fun _rw-ops* -> _surface*/null))
(define-sdl2-image load-gif-rw  (_fun _rw-ops* -> _surface*/null))
(define-sdl2-image load-jpg-rw  (_fun _rw-ops* -> _surface*/null))
(define-sdl2-image load-lbm-rw  (_fun _rw-ops* -> _surface*/null))
(define-sdl2-image load-pcx-rw  (_fun _rw-ops* -> _surface*/null))
(define-sdl2-image load-png-rw  (_fun _rw-ops* -> _surface*/null))
(define-sdl2-image load-pnm-rw  (_fun _rw-ops* -> _surface*/null))
(define-sdl2-image load-svg-rw  (_fun _rw-ops* -> _surface*/null)
    #:make-fail make-not-available)
(define-sdl2-image load-tga-rw  (_fun _rw-ops* -> _surface*/null))
(define-sdl2-image load-tif-rw  (_fun _rw-ops* -> _surface*/null))
(define-sdl2-image load-xcf-rw  (_fun _rw-ops* -> _surface*/null))
(define-sdl2-image load-xpm-rw  (_fun _rw-ops* -> _surface*/null))
(define-sdl2-image load-xv-rw   (_fun _rw-ops* -> _surface*/null))
(define-sdl2-image load-webp-rw (_fun _rw-ops* -> _surface*/null))

(define-sdl2-image read-xpm-from-array (_fun (_ptr io _string) -> _surface*/null))

(define-sdl2-image save-png! (_fun _surface* _string -> _int))
(define-sdl2-image save-png-rw! (_fun _surface* _rw-ops* _int -> _int))
(define-sdl2-image save-jpg! (_fun _surface* _string _int -> _int)
    #:make-fail make-not-available)
(define-sdl2-image save-jpg-rw! (_fun _surface* _rw-ops* _int _int -> _int)
    #:make-fail make-not-available)

(define set-error! sdl-set-error!)
(define get-error sdl-get-error)
