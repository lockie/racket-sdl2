#lang racket/base

(require
 (for-syntax racket/base
             racket/syntax
             racket/string)
 ffi/unsafe
 ffi/unsafe/define
 (rename-in sdl2/pretty
            [set-error! sdl-set-error!]
            [clear-error! sdl-clear-error!]
            [get-error sdl-get-error])
 sdl2/private/lib-path)

(provide
 (except-out
  (all-defined-out)
  define-sdl2-mixer
  convention:uglify))


(define-syntax (convention:uglify id)
    (define str (string-replace (symbol->string (syntax-e id)) "!" ""))
    (when (string-suffix? str "-rw")
        (set! str (string-replace str "-rw" "-_-rw")))
    (when (string-suffix? str "-version")
        (set! str (string-replace str "-version" "-_-version")))
    (when (string-suffix? str "-wav")
        (set! str (string-replace str "-wav" "-_-wav")))
    (when (string-suffix? str "-raw")
        (set! str (string-replace str "-raw" "-_-raw")))
    (define uppercase-exceptions
        '("rw" "mus" "wav" "raw" "cmd"))
    (format-id id
               (apply string-append
                      "Mix_"
                      (map
                       (lambda (s)
                           (cond
                               [(member s uppercase-exceptions)
                                (string-upcase s)]
                               [(string=? s "is") "is"]
                               [else (string-titlecase s)]))
                       (string-split str "-")))))

(define-ffi-definer define-sdl2-mixer
    (ffi-lib (lib-path "mixer") '("0" #f))
    #:make-c-id convention:uglify)

(define-sdl2-mixer linked-version (_fun -> _version*))

(define _init-flags
    (_bitmask
     '(flac = #x00000001
       mod  = #x00000002
       mp3  = #x00000008
       ogg  = #x00000010
       mid  = #x00000020
       opus = #x00000040)))

(define-sdl2-mixer init! (_fun _init-flags -> _init-flags))

(define-sdl2-mixer quit! (_fun -> _void))

(define default-frequency 22050)

(define default-format
    (if (system-big-endian?)
        audio-s16-msb
        audio-s16-lsb))

(define default-channels 2)

(define-cstruct _chunk
    ([allocated _int]
     [abuf (_cpointer _uint8)]
     [alen _uint32]
     [volume _uint8]))

(define _chunk* _chunk-pointer)
(define _chunk*/null _chunk-pointer/null)

(define _fading
    (_enum
     '(no-fading
       fading-out
       fading-in)))

(define _music-type
    (_enum
     '(none
       cmd
       wav
       mod
       mid
       ogg
       mp3
       mp3-mad-unused
       flac
       modplug-unused
       opus)))

(define-cpointer-type _music*)

(define-sdl2-mixer open-audio! (_fun _int _uint16 _int _int -> _int))

(define-sdl2-mixer open-audio-device! (_fun _int _uint16 _int _int _string _int -> _int)
    #:make-fail make-not-available)

(define-sdl2-mixer allocate-channels! (_fun _int -> _int))

(define-sdl2-mixer query-spec (_fun _int* _uint16* _int* -> _int))

(define-sdl2-mixer load-wav-rw (_fun _rw-ops* _int -> _chunk*/null))
(define (load-wav file) (load-wav-rw (rw-from-file file "rb") 1))

(define-sdl2-mixer load-mus (_fun _string -> _music*/null))

(define-sdl2-mixer load-mus-rw (_fun _rw-ops* _int -> _music*/null))

(define-sdl2-mixer load-mus-type-rw (_fun _rw-ops* _music-type _int -> _music*/null))

(define-sdl2-mixer quick-load-wav (_fun _uint8* -> _chunk*/null))

(define-sdl2-mixer quick-load-raw (_fun _uint8* _uint32 -> _chunk*/null))

(define-sdl2-mixer free-chunk! (_fun _chunk* -> _void))

(define-sdl2-mixer free-music! (_fun _music* -> _void))

(define-sdl2-mixer get-num-chunk-decoders (_fun -> _int))
(define-sdl2-mixer get-chunk-decoder (_fun _int -> _string))
(define-sdl2-mixer get-num-music-decoders (_fun -> _int))
(define-sdl2-mixer get-music-decoder (_fun _int -> _string))
(define-sdl2-mixer has-music-decoder (_fun _string -> _bool)
    ;; see https://discourse.libsdl.org/t/missing-mix-hasmusicdecoder-definition/23392
    #:make-fail make-not-available)

(define-sdl2-mixer get-music-type (_fun _music* -> _music-type))

(define-sdl2-mixer set-post-mix! (_fun (_fun _pointer _uint8* _int -> _void) _pointer -> _void))

(define-sdl2-mixer hook-music (_fun (_fun _pointer _uint8* _int -> _void) _pointer -> _void))

(define-sdl2-mixer hook-music-finished (_fun (_fun -> _void) -> _void))

(define-sdl2-mixer get-music-hook-data (_fun -> _pointer))

(define-sdl2-mixer channel-finished (_fun (_fun _int -> _void) -> _void))

(define channel-post -2)

(define _effect-func* (_fun _int _pointer _int _pointer -> _void)) ;; XXX * ?

(define _effect-done* (_fun _int _pointer -> _void))

(define-sdl2-mixer register-effect! (_fun _int _effect-func* _effect-done* _pointer -> _int))

(define-sdl2-mixer unregister-effect! (_fun _int _effect-func* -> _int))

(define-sdl2-mixer unregister-all-effects! (_fun _int -> _int))

(define effects-max-speed "MIX_EFFECTSMAXSPEED")

(define-sdl2-mixer set-panning! (_fun _int _uint8 _uint8 -> _int))

(define-sdl2-mixer set-position! (_fun _int _sint16 _uint8 -> _int))

(define-sdl2-mixer set-distance! (_fun _int _uint8 -> _int))

(define-sdl2-mixer set-reverse-stereo! (_fun _int _int -> _int))

(define-sdl2-mixer reserve-channels (_fun _int -> _int))

(define-sdl2-mixer group-channel (_fun _int _int -> _int))
(define-sdl2-mixer group-channels (_fun _int _int _int -> _int))
(define-sdl2-mixer group-available (_fun _int -> _int))
(define-sdl2-mixer group-count (_fun _int -> _int))
(define-sdl2-mixer group-oldest (_fun _int -> _int))
(define-sdl2-mixer group-newer (_fun _int -> _int))

(define (play-channel channel chunk loops)
    (play-channel-timed channel chunk loops -1))
(define-sdl2-mixer play-channel-timed (_fun _int _chunk* _int _int -> _int))
(define-sdl2-mixer play-music (_fun _music* _int -> _int))

(define-sdl2-mixer fade-in-music (_fun _music* _int _int -> _int))
(define-sdl2-mixer fade-in-music-pos (_fun _music* _int _int _double -> _int))
(define (fade-in-channel channel chunk loops ms)
    (fade-in-channel-timed channel chunk loops ms -1))
(define-sdl2-mixer fade-in-channel-timed (_fun _int _chunk* _int _int _int -> _int))

(define-sdl2-mixer volume(_fun _int _int -> _int))
(define-sdl2-mixer volume-chunk(_fun _chunk* _int -> _int))
(define-sdl2-mixer volume-music(_fun _int -> _int))

(define-sdl2-mixer halt-channel(_fun _int -> _int))
(define-sdl2-mixer halt-group(_fun _int -> _int))
(define-sdl2-mixer halt-music(_fun -> _int))

(define-sdl2-mixer expire-channel(_fun _int _int -> _int))

(define-sdl2-mixer fade-out-channel(_fun _int _int -> _int))
(define-sdl2-mixer fade-out-group(_fun _int _int -> _int))
(define-sdl2-mixer fade-out-music(_fun _int -> _int))

(define-sdl2-mixer fading-music(_fun -> _fading))
(define-sdl2-mixer fading-channel(_fun _int -> _fading))

(define-sdl2-mixer pause(_fun _int -> _void))
(define-sdl2-mixer resume(_fun _int -> _void))
(define-sdl2-mixer paused(_fun _int -> _int))

(define-sdl2-mixer pause-music(_fun -> _void))
(define-sdl2-mixer resume-music(_fun -> _void))
(define-sdl2-mixer rewind-music(_fun -> _void))
(define-sdl2-mixer paused-music(_fun -> _int))

(define-sdl2-mixer set-music-position! (_fun _double -> _int))

(define-sdl2-mixer playing(_fun _int -> _int))
(define-sdl2-mixer playing-music(_fun -> _int))

(define-sdl2-mixer set-music-cmd!(_fun _string -> _int))

(define-sdl2-mixer set-synchro-value!(_fun _int -> _int))
(define-sdl2-mixer get-synchro-value(_fun -> _int))

(define-sdl2-mixer set-sound-fonts!(_fun _string -> _int))
(define-sdl2-mixer get-sound-fonts(_fun -> _string))

(define-sdl2-mixer each-sound-font(_fun (_fun _string _pointer -> _int) _pointer -> _int))

(define-sdl2-mixer get-chunk(_fun _int -> _chunk*/null))

(define-sdl2-mixer close-audio!(_fun -> _void))

(define set-error!    sdl-set-error!)
(define get-error    sdl-get-error)
(define clear-error!  sdl-clear-error!)
