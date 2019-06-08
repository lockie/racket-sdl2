#lang racket/base

(provide lib-path)


(define (lib-path [suffix #f])
    (string-append
     (case (system-type 'os)
         [(unix)     "libSDL2"]
         [(windows)  "SDL2"]
         [(macosx)   "libSDL2"]
         [else (error "Platform not supported")])
     (if suffix
         (string-append "_" suffix)
         "")
     (if (eq? (system-type 'os) 'windows)
         ""
         "-2.0")))
