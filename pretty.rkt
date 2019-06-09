#lang racket/base

(require
 (for-syntax racket/base
             racket/format
             racket/syntax
             racket/string)
 ffi/unsafe
 ffi/unsafe/define
 (submod racket/performance-hint begin-encourage-inline)
 (rename-in racket/contract
            [-> contract/->])
 sdl2/private/lib-path)

(provide
 (except-out
  (all-defined-out)
  convention:uglify
  convention:uglify-minor
  sdl2-lib
  define-sdl2
  define-sdl2-vararg))

(define-syntax (convention:uglify id)
    (define str
        (string-replace
         (string-replace
          (symbol->string (syntax-e id))
          "!" "")
         "*" ""))
    (when (string-suffix? str "?")
        (set! str (string-append
                   "is-" (string-replace str "?" ""))))
    (when (and (> (length (string-split str "-")) 2) (string-suffix? str "-rw"))
          (set! str (string-replace str "-rw" "-_-rw")))
    (when (string-prefix? str "gl-")
        (set! str (string-replace str "gl-" "gl-_-")))
    (when (string-prefix? str "vulkan-")
        (set! str (string-replace str "vulkan-" "vulkan-_-")))
    (define uppercase-exceptions
        '("gl" "rw" "le16" "be16" "le32" "be32" "le64" "be64" "id" "guid" "tls"
               "rgb" "rgba" "yuv" "bmp" "rle" "wm" "cvt" "rt" "cas" "wav" "fp"
               "cpu" "ram" "rdtsc" "mmx" "sse" "sse2" "sse3" "sse41" "sse42"
               "avx" "avx2" "avx512" "neon" "d3d9" "direct3d9" "dxgi" "jni"
               "tv" "fs" "utf8" "unicode"))
    (format-id id
               (apply string-append
                      "SDL_"
                      (map
                       (lambda (s)
                           (cond
                               [(member s uppercase-exceptions)
                                (string-upcase s)]
                               [(string=? s "iphone") "iPhone"]
                               [(string=? s "dex") "DeX"]
                               [else (string-titlecase s)]))
                       (string-split str "-")))))

(define-syntax (convention:uglify-minor id)
    (define str
        (string-replace
         (symbol->string (syntax-e id))
         "*" ""))
    (when (string-suffix? str "?")
        (set! str (string-append
                   "is" (string-replace str "?" ""))))
    (format-id id
               (string-append
                "SDL_"
                (string-replace str "-" "_"))))


(define sdl2-lib (ffi-lib (lib-path) '("0" #f)))
(define-ffi-definer define-sdl2 sdl2-lib #:make-c-id convention:uglify)
(define-ffi-definer define-sdl2-minor sdl2-lib #:make-c-id convention:uglify-minor)

(define-cpointer-type _uint8*)
(define-cpointer-type _int8*)
(define-cpointer-type _uint16*)
(define-cpointer-type _int16*)
(define-cpointer-type _uint32*)
(define-cpointer-type _int32*)
(define-cpointer-type _uint*)
(define-cpointer-type _int*)
(define-cpointer-type _size*)
(define-cpointer-type _float*)

(define-syntax (define-sdl2-vararg stx)
    (syntax-case stx ()
        [(_ name orig-name (arg-types ...) ret-type)
         (with-syntax*
             ([name-str (~a (syntax->datum #'orig-name))]
              [arglist
               (map
                (lambda (arg)
                    (gensym (syntax->datum arg)))
                (syntax->list #'(arg-types ...)))]
              [full-arglist (append (syntax->list #'arglist) 'args)]
              [final-call (append '(apply fun) (syntax->list #'arglist) '(args))])
             #'(define name
                   (let ([interfaces (make-hash)])
                       (lambda full-arglist
                           (define itypes
                               (append (list arg-types ...)
                                       (map (lambda (x)
                                                (cond
                                                    [(and (integer? x) (exact? x)) _int]
                                                    [(and (number? x) (real? x))   _double*]
                                                    [(string? x)  _string]
                                                    [(bytes? x)   _bytes]
                                                    [(symbol? x)  _symbol]
                                                    [else
                                                     (error
                                                      "don't know how to deal with ~e" x)]))
                                            args)))
                           (let ([fun (hash-ref
                                       interfaces itypes
                                       (lambda ()
                                           (let ([i (get-ffi-obj
                                                     name-str sdl2-lib
                                                     (_cprocedure itypes ret-type))])
                                               (hash-set! interfaces itypes i)
                                               i)))])
                               final-call)))))]))


;; SDL_stdinc.h

(define arraysize array-length)
(define tablesize arraysize)

(define-syntax-rule (stringify-arg arg)
    (~a arg))

(define (fourcc A B C D)
    (let ([a (if (char? A) (char->integer A) A)]
          [b (if (char? B) (char->integer B) B)]
          [c (if (char? C) (char->integer C) C)]
          [d (if (char? D) (char->integer D) D)])
        (bitwise-ior
         (bitwise-and #xffffffff (arithmetic-shift (bitwise-and #xff a) 0))
         (bitwise-and #xffffffff (arithmetic-shift (bitwise-and #xff b) 8))
         (bitwise-and #xffffffff (arithmetic-shift (bitwise-and #xff c) 16))
         (bitwise-and #xffffffff (arithmetic-shift (bitwise-and #xff d) 24)))))

(define false 0)
(define true 1)


(define max-sint8   #x7f)
(define min-sint8   (bitwise-not #x7f))

(define max-uint8   #xff)
(define min-uint8   #x00)

(define max-sint16  #x7fff)
(define min-sint16  (bitwise-not #x7fff))

(define max-uint16  #xffff)
(define min-uint16  #x0000)

(define max-sint32  #x7fffffff)
(define min-sint32  (bitwise-not #x7fffffff))

(define max-uint32  #xffffffff)
(define min-uint32  #x00000000)

(define max-sint64  #x7fffffffffffffff)
(define min-sint64  (bitwise-not #x7fffffffffffffff))

(define max-uint64  #xffffffffffffffff)
(define min-uint64  #x0000000000000000)

(define-sdl2-minor malloc* (_fun _size -> _pointer))
(define-sdl2-minor calloc* (_fun _size _size -> _pointer))
(define-sdl2-minor realloc* (_fun _pointer _size -> _pointer))
(define-sdl2-minor free* (_fun _pointer -> _void))

(define stack-alloc malloc)
(define stack-free free)

(define _malloc-func (_fun _size -> _pointer))
(define _calloc-func (_fun _size _size -> _pointer))
(define _realloc-func (_fun _pointer _size -> _pointer))
(define _free-func (_fun _pointer -> _void))

(define-sdl2 get-memory-functions
    (_fun _malloc-func _calloc-func _realloc-func _free-func -> _void)
    #:make-fail make-not-available)

(define-sdl2 set-memory-functions!
    (_fun _malloc-func _calloc-func _realloc-func _free-func -> _int)
    #:make-fail make-not-available)

(define-sdl2 get-num-allocations (_fun -> _int)
    #:make-fail make-not-available)

(define-sdl2-minor getenv (_fun _string -> _string))
(define-sdl2-minor setenv (_fun _string _string _int -> _int))

(define-sdl2-minor qsort (_fun _pointer _size _size (_fun _pointer _pointer -> _int) -> _void))

(define-sdl2-minor abs (_fun _int -> _int))

(define-syntax-rule (min x y)
    (if (< x y) x y))

(define-syntax-rule (max x y)
    (if (> x y) x y))

(define-sdl2-minor digit? (_fun _int -> _int))
(define-sdl2-minor space? (_fun _int -> _int))
(define-sdl2-minor toupper (_fun _int -> _int))
(define-sdl2-minor tolower (_fun _int -> _int))

(define-sdl2-minor memset* (_fun _pointer _int _size -> _pointer))

(define-sdl2-minor memcpy* (_fun _pointer _pointer _size -> _pointer))

(define-sdl2-minor memmove* (_fun _pointer _pointer _size -> _pointer))

(define-sdl2-minor memcmp (_fun _pointer _pointer _size -> _int))

(define-sdl2-minor wcsdup (_fun _string/ucs-4 -> _string/ucs-4)
    #:make-fail make-not-available)

(define-sdl2-minor wcslen (_fun _string/ucs-4 -> _size))
(define-sdl2-minor wcslcpy (_fun _string/ucs-4 _string/ucs-4 _size -> _size))
(define-sdl2-minor wcslcat (_fun _string/ucs-4 _string/ucs-4 _size -> _size))
(define-sdl2-minor wcscmp (_fun _string/ucs-4 _string/ucs-4 -> _int)
    #:make-fail make-not-available)

(define-sdl2-minor strlen (_fun _string -> _size))
(define-sdl2-minor strlcpy (_fun _string _string _size -> _size))
(define-sdl2-minor utf8strlcpy (_fun _string _string _size -> _size))
(define-sdl2-minor strlcat (_fun _string _string _size -> _size))
(define-sdl2-minor strdup (_fun _string -> _string))
(define-sdl2-minor strrev (_fun _string -> _string))
(define-sdl2-minor strupr (_fun _string -> _string))
(define-sdl2-minor strlwr (_fun _string -> _string))
(define-sdl2-minor strchr (_fun _string _int -> _string))
(define-sdl2-minor strrchr (_fun _string _int -> _string))
(define-sdl2-minor strstr (_fun _string _string -> _string))
(define-sdl2-minor utf8strlen (_fun _string -> _size)
    #:make-fail make-not-available)

(define-sdl2-minor itoa (_fun _int _string _int -> _string))
(define-sdl2-minor uitoa (_fun _uint _string _int -> _string))
(define-sdl2-minor ltoa (_fun _long _string _int -> _string))
(define-sdl2-minor ultoa (_fun _ulong _string _int -> _string))
(define-sdl2-minor lltoa (_fun _sint64 _string _int -> _string))
(define-sdl2-minor ulltoa (_fun _uint64 _string _int -> _string))

(define-sdl2-minor atoi (_fun _string -> _int))
(define-sdl2-minor atof (_fun _string -> _double))
(define-sdl2-minor strtol (_fun _string (_ptr i _string) _int -> _long))
(define-sdl2-minor strtoul (_fun _string (_ptr i _string) _int -> _ulong))
(define-sdl2-minor strtoll (_fun _string (_ptr i _string) _int -> _sint64))
(define-sdl2-minor strtoull (_fun _string (_ptr i _string) _int -> _uint64))
(define-sdl2-minor strtod (_fun _string (_ptr i _string) -> _double))

(define-sdl2-minor strcmp (_fun _string _string -> _int))
(define-sdl2-minor strncmp (_fun _string _string _size -> _int))
(define-sdl2-minor strcasecmp (_fun _string _string -> _int))
(define-sdl2-minor strncasecmp (_fun _string _string _size -> _int))

(define-sdl2-minor acos (_fun _double -> _double)
    #:make-fail make-not-available)
(define-sdl2-minor acosf (_fun _float -> _float)
    #:make-fail make-not-available)
(define-sdl2-minor asin (_fun _double -> _double)
    #:make-fail make-not-available)
(define-sdl2-minor asinf (_fun _float -> _float)
    #:make-fail make-not-available)
(define-sdl2-minor atan (_fun _double -> _double))
(define-sdl2-minor atanf (_fun _float -> _float)
    #:make-fail make-not-available)
(define-sdl2-minor atan2 (_fun _double _double -> _double))
(define-sdl2-minor atan2f (_fun _float _float -> _float)
    #:make-fail make-not-available)
(define-sdl2-minor ceil (_fun _double -> _double))
(define-sdl2-minor ceilf (_fun _float -> _float)
    #:make-fail make-not-available)
(define-sdl2-minor copysign (_fun _double _double -> _double))
(define-sdl2-minor copysignf (_fun _float _float -> _float)
    #:make-fail make-not-available)
(define-sdl2-minor cos (_fun _double -> _double))
(define-sdl2-minor cosf (_fun _float -> _float))
(define-sdl2-minor exp (_fun _double -> _double)
    #:make-fail make-not-available)
(define-sdl2-minor expf (_fun _float -> _float)
    #:make-fail make-not-available)
(define-sdl2-minor fabs (_fun _double -> _double))
(define-sdl2-minor fabsf (_fun _float -> _float)
    #:make-fail make-not-available)
(define-sdl2-minor floor (_fun _double -> _double))
(define-sdl2-minor floorf (_fun _float -> _float)
    #:make-fail make-not-available)
(define-sdl2-minor fmod (_fun _double _double -> _double)
    #:make-fail make-not-available)
(define-sdl2-minor fmodf (_fun _float _float -> _float)
    #:make-fail make-not-available)
(define-sdl2-minor log (_fun _double -> _double))
(define-sdl2-minor logf (_fun _float -> _float)
    #:make-fail make-not-available)
(define-sdl2-minor log10 (_fun _double -> _double)
    #:make-fail make-not-available)
(define-sdl2-minor log10f (_fun _float -> _float)
    #:make-fail make-not-available)
(define-sdl2-minor pow (_fun _double _double -> _double))
(define-sdl2-minor powf (_fun _float _float -> _float)
    #:make-fail make-not-available)
(define-sdl2-minor scalbn (_fun _double _int -> _double))
(define-sdl2-minor scalbnf (_fun _float _int -> _float)
    #:make-fail make-not-available)
(define-sdl2-minor sin (_fun _double -> _double))
(define-sdl2-minor sinf (_fun _float -> _float))
(define-sdl2-minor sqrt (_fun _double -> _double))
(define-sdl2-minor sqrtf (_fun _float -> _float)
    #:make-fail make-not-available)
(define-sdl2-minor tan (_fun _double -> _double)
    #:make-fail make-not-available)
(define-sdl2-minor tanf (_fun _float -> _float)
    #:make-fail make-not-available)

(define ERROR     (cast -1 _intptr _size))
(define E2BIG     (cast -2 _intptr _size))
(define EILSEQ    (cast -3 _intptr _size))
(define EINVAL    (cast -4 _intptr _size))

(define-cpointer-type _iconv_t*)

(define-sdl2-minor iconv-open (_fun _string _string -> _iconv_t*))
(define-sdl2-minor iconv-close (_fun _iconv_t* -> _int))
(define-sdl2-minor iconv (_fun _iconv_t* (_ptr i _string) _size* (_ptr o _string) _size* -> _size))
(define-sdl2-minor iconv-string (_fun _string _string _string _size -> _string))
(define (iconv-utf8-locale s)
    (iconv-string "" "UTF-8" s (+ (strlen s) 1)))
(define (iconv-utf8-ucs2 s)
    (cast
     (iconv-string "UCS-2-INTERNAL" "UTF-8" s (+ (strlen s) 1))
     _string
     _uint16*/null))
(define (iconv-utf8-ucs4 s)
    (cast
     (iconv-string "UCS-4-INTERNAL" "UTF-8" s (+ (strlen s) 1))
     _string
     _uint32*/null))

(define (memcpy4 dst src dwords)
    (memcpy dst src (* dwords 4)))


;; SDL_assert.h

(define _assert-state
    (_enum
     '(retry
       break
       abort
       ignore
       always-ignore)))

(define-cstruct _assert-data
    ([always-ignore _int]
     [trigger-count _uint]
     [condition _string]
     [filename _string]
     [linenum _int]
     [function _string]
     [next _pointer]))

(define _assert-data* _assert-data-pointer)

(define-sdl2 report-assertion (_fun _assert-data* _string _string _int -> _assert-state))

(define _assertion-handler (_fun _assert-data* _pointer -> _assert-state))

(define-sdl2 set-assertion-handler! (_fun _assertion-handler _pointer -> _void))

(define-sdl2 get-default-assertion-handler (_fun -> _assertion-handler)
    #:make-fail make-not-available)

(define-sdl2 get-assertion-handler (_fun (_ptr o _pointer) -> _assertion-handler)
    #:make-fail make-not-available)

(define-sdl2 get-assertion-report (_fun -> _assert-data*))

(define-sdl2 reset-assertion-report! (_fun -> _void))


;; SDL_atomic.h

(define _spin-lock _int)
(define _spin-lock* _int*)

(define-sdl2 atomic-try-lock! (_fun _spin-lock* -> _bool))

(define-sdl2 atomic-lock! (_fun _spin-lock* -> _void))

(define-sdl2 atomic-unlock! (_fun _spin-lock* -> _void))

(define (compiler-barrier)
    (define tmp (cast (malloc (ctype-sizeof _int)) _pointer _int*))
    (ptr-set! tmp _int 0)
    (atomic-lock! tmp)
    (atomic-unlock! tmp))

(define memory-barrier-release! compiler-barrier)

(define memory-barrier-acquire! compiler-barrier)

(define-cstruct _atomic-t
    ([value _int]))

(define _atomic-t* _atomic-t-pointer)

(define-sdl2 atomic-cas! (_fun _atomic-t* _int _int -> _bool))

(define-sdl2 atomic-get (_fun _atomic-t* -> _int)
    #:make-fail make-not-available)

(define-sdl2 atomic-add! (_fun _atomic-t* _int -> _int)
    #:make-fail make-not-available)

(define (atomic-inc-ref! a) (atomic-add! a 1))

(define (atomic-dec-ref! a) (= 1 (atomic-add! a -1)))

(define-sdl2 atomic-cas-ptr! (_fun (_ptr io _pointer) _pointer _pointer -> _bool))

(define-sdl2 atomic-set-ptr! (_fun (_ptr io _pointer) _pointer -> _pointer)
    #:make-fail make-not-available)

(define-sdl2 atomic-get-ptr (_fun (_ptr i _pointer) -> _pointer)
    #:make-fail make-not-available)


;; SDL_rwops.h

(define unknown   0)
(define winfile   1)
(define stdfile   2)
(define jnifile   3)
(define memory    4)
(define memory-ro 5)

(define-cstruct _rw-ops
    ([size (_fun _pointer -> _sint64)]
     [seek (_fun _pointer _sint64 _int -> _sint64)]
     [read (_fun _pointer _pointer _size _size -> _size)]
     [write (_fun _pointer _pointer _size _size -> _size)]
     [close (_fun _pointer -> _int)]
     [type _uint32]
     ;; XXX be on the safe side and occupy maximum space for the hidden union
     [hidden-ptr1 _pointer]
     [hidden-ptr2 _pointer]
     [hidden-ptr3 _pointer]
     [hidden-ptr4 _pointer]
     [hidden-ptr5 _pointer]
     [hidden-ptr6 _pointer]
     [hidden-ptr7 _pointer]
     [hidden-ptr8 _pointer]
     [hidden-ptr9 _pointer]))

(define _rw-ops* _rw-ops-pointer)
(define _rw-ops*/null _rw-ops-pointer/null)

(define-sdl2 rw-from-file (_fun _string _string -> _rw-ops*/null))

(define-sdl2 rw-from-fp (_fun _pointer _bool -> _rw-ops*/null))

(define-sdl2 rw-from-mem (_fun _pointer _int -> _rw-ops*/null))

(define-sdl2 rw-from-const-mem (_fun _pointer _int -> _rw-ops*/null))

(define-sdl2 alloc-rw (_fun -> _rw-ops*/null))

(define-sdl2 free-rw! (_fun _rw-ops* -> _void))

(define seek-set 0)
(define seek-cur 1)
(define seek-end 2)

(define (rw-size ctx) ((rw-ops-size ctx) ctx))
(define (rw-seek ctx offset whence) ((rw-ops-seek ctx) ctx offset whence))
(define (rw-tell ctx) ((rw-ops-seek ctx) 0 seek-cur))
(define (rw-read ctx ptr size n) ((rw-ops-read ctx) ctx ptr size n))
(define (rw-write ctx ptr size n) ((rw-ops-write ctx) ctx ptr size n))
(define (rw-close! ctx) ((rw-ops-close ctx) ctx))

(define-sdl2 load-file-rw (_fun _rw-ops* _size _int -> _pointer)
    #:make-fail make-not-available)

(define (load-file file data-size)
    (load-file-rw
     (rw-from-file file "rb")
     data-size 1))

(define-sdl2 read-u8   (_fun _rw-ops* -> _uint8 ))
(define-sdl2 read-le16 (_fun _rw-ops* -> _uint16))
(define-sdl2 read-be16 (_fun _rw-ops* -> _uint16))
(define-sdl2 read-le32 (_fun _rw-ops* -> _uint32))
(define-sdl2 read-be32 (_fun _rw-ops* -> _uint32))
(define-sdl2 read-le64 (_fun _rw-ops* -> _uint64))
(define-sdl2 read-be64 (_fun _rw-ops* -> _uint64))

(define-sdl2 write-u8   (_fun _rw-ops* _uint8  -> _size))
(define-sdl2 write-le16 (_fun _rw-ops* _uint16 -> _size))
(define-sdl2 write-be16 (_fun _rw-ops* _uint16 -> _size))
(define-sdl2 write-le32 (_fun _rw-ops* _uint32 -> _size))
(define-sdl2 write-be32 (_fun _rw-ops* _uint32 -> _size))
(define-sdl2 write-le64 (_fun _rw-ops* _uint64 -> _size))
(define-sdl2 write-be64 (_fun _rw-ops* _uint64 -> _size))


;; SDL_audio.h

(define _audio-format _uint16)

(define audio-mask-bitsize       #xff)
(define audio-mask-datatype      (arithmetic-shift 1 8))
(define audio-mask-endian        (arithmetic-shift 1 12))
(define audio-mask-signed        (arithmetic-shift 1 15))
(define (audio-bitsize x)        (bitwise-and x audio-mask-bitsize))
(define (audio-float? x)        (bitwise-and x audio-mask-datatype))
(define (audio-bigendian? x)    (bitwise-and x audio-mask-endian))
(define (audio-signed? x)       (bitwise-and x audio-mask-signed))
(define (audio-int? x)          (bitwise-not (audio-float? x)))
(define (audio-littleendian? x) (bitwise-not (audio-bigendian? x)))
(define (audio-unsigned? x)     (bitwise-not (audio-signed? x)))

(define audio-u8        #x0008)
(define audio-s8        #x8008)
(define audio-u16-lsb    #x0010)
(define audio-s16-lsb    #x8010)
(define audio-u16-msb    #x1010)
(define audio-s16-msb    #x9010)
(define audio-u16       audio-u16-lsb)
(define audio-s16       audio-s16-lsb)

(define audio-s32-lsb    #x8020)
(define audio-s32-msb    #x9020)
(define audio-s32       audio-s32-lsb)

(define audio-f32-lsb    #x8120)
(define audio-f32-msb    #x9120)
(define audio-f32       audio-f32-lsb)

(define audio-u16-sys    (if (system-big-endian?) audio-u16-msb audio-u16-lsb))
(define audio-s16-sys    (if (system-big-endian?) audio-s16-msb audio-u16-lsb))
(define audio-s32-sys    (if (system-big-endian?) audio-s32-msb audio-u16-lsb))
(define audio-f32-sys    (if (system-big-endian?) audio-f32-msb audio-u16-lsb))

(define audio-allow-frequency-change    #x00000001)
(define audio-allow-format-change       #x00000002)
(define audio-allow-channels-change     #x00000004)
(define audio-allow-samples-change      #x00000008)
(define audio-allow-any-change          (bitwise-ior
                                         audio-allow-frequency-change
                                         audio-allow-format-change
                                         audio-allow-channels-change
                                         audio-allow-samples-change))

(define _audio-callback (_fun _pointer _uint8* _int -> _void))

(define-cstruct _audio-spec
    ([freq _int]
     [format _audio-format]
     [channels _uint8]
     [silence _uint8]
     [samples _uint16]
     [padding _uint16]
     [size _uint32]
     [callback _audio-callback]
     [userdata _pointer]))

(define _audio-spec* _audio-spec-pointer)
(define _audio-spec*/null _audio-spec-pointer/null)

(define _audio-filter (_fun _pointer _audio-format -> _void))

(define audio-cvt-max-filters 9)

(define-cstruct _audio-cvt
    ([needed _int]
     [src-format _audio-format]
     [dst-format _audio-format]
     [rate-incr _double]
     [buf _uint8*]
     [len _int]
     [len-cvt _int]
     [len-mult _int]
     [len-ratio _double]
     [filters (_array _audio-filter (+ audio-cvt-max-filters 1))]
     [filter-index _int])
    #:alignment 1)

(define _audio-cvt* _audio-cvt-pointer)

(define-sdl2 get-num-audio-drivers (_fun -> _int))
(define-sdl2 get-audio-driver (_fun _int -> _string))

(define-sdl2 audio-init (_fun _string -> _int))
(define-sdl2 audio-quit (_fun -> _void))

(define-sdl2 get-current-audio-driver (_fun -> _string))

(define-sdl2 open-audio (_fun _audio-spec* _audio-spec*/null -> _int))

(define _audio-device-id _uint32)

(define-sdl2 get-num-audio-devices (_fun _int -> _int))

(define-sdl2 get-audio-device-name (_fun _int _int -> _string))

(define-sdl2 open-audio-device
    (_fun _string _int _audio-spec* _audio-spec* _int -> _audio-device-id))

(define _audio-status
    (_enum
     '(stopped = 0
       playing
       paused)))

(define-sdl2 get-audio-status (_fun -> _audio-status))

(define-sdl2 get-audio-device-status (_fun _audio-device-id -> _audio-status))

(define-sdl2 pause-audio (_fun _int -> _void))
(define-sdl2 pause-audio-device (_fun _audio-device-id _int -> _void))

(define-sdl2 load-wav-rw
    (_fun _rw-ops* _int _audio-spec* (_ptr o _uint8*) _uint32* -> _audio-spec*/null))

(define (load-wav file spec audio-buf audio-len)
    (load-wav-rw
     (rw-from-file file "rb")
     1 spec audio-buf audio-len))

(define-sdl2 free-wav! (_fun _uint8* -> _void))

(define-sdl2 build-audio-cvt
    (_fun _audio-cvt* _audio-format _uint8 _int _audio-format _uint8 _int -> _int))

(define-sdl2 convert-audio (_fun _audio-cvt* -> _int))

(define-cpointer-type _audio-stream*)

(define-sdl2 new-audio-stream
    (_fun _audio-format _uint8 _int _audio-format _uint8 _int -> _audio-stream*)
    #:make-fail make-not-available)

(define-sdl2 audio-stream-put (_fun _audio-stream* _pointer _int -> _int)
    #:make-fail make-not-available)

(define-sdl2 audio-stream-get (_fun _audio-stream* _pointer _int -> _int)
    #:make-fail make-not-available)

(define-sdl2 audio-stream-available (_fun _audio-stream* -> _int)
    #:make-fail make-not-available)

(define-sdl2 audio-stream-flush (_fun _audio-stream* -> _int)
    #:make-fail make-not-available)

(define-sdl2 audio-stream-clear (_fun _audio-stream* -> _void)
    #:make-fail make-not-available)

(define-sdl2 free-audio-stream! (_fun _audio-stream* -> _void)
    #:make-fail make-not-available)

(define mix-maxvolume 128)

(define-sdl2 mix-audio (_fun _uint8* _uint8* _uint32 _int -> _void))

(define-sdl2 mix-audio-format (_fun _uint8* _uint8* _audio-format _uint32 _int -> _void))

(define-sdl2 queue-audio (_fun _audio-device-id _pointer _uint32 -> _int)
    #:make-fail make-not-available)

(define-sdl2 dequeue-audio (_fun _audio-device-id _pointer _uint32 -> _uint32)
    #:make-fail make-not-available)

(define-sdl2 get-queued-audio-size (_fun _audio-device-id -> _uint32)
    #:make-fail make-not-available)

(define-sdl2 clear-queued-audio! (_fun _audio-device-id -> _void)
    #:make-fail make-not-available)

(define-sdl2 lock-audio (_fun -> _void))
(define-sdl2 lock-audio-device (_fun _audio-device-id -> _void))
(define-sdl2 unlock-audio (_fun -> _void))
(define-sdl2 unlock-audio-device (_fun -> _void))

(define-sdl2 close-audio (_fun -> _void))
(define-sdl2 close-audio-device (_fun _audio-device-id -> _void))


;; SDL_clipboard.h

(define-sdl2 set-clipboard-text! (_fun _string -> _int))

(define-sdl2 get-clipboard-text (_fun -> _string))

(define-sdl2 has-clipboard-text (_fun -> _bool))


;; SDL_cpuinfo.h

(define cacheline-size  128)

(define-sdl2 get-cpu-count (_fun -> _int))

(define-sdl2 get-cpu-cache-line-size (_fun -> _int))

(define-sdl2 has-rdtsc (_fun -> _bool))

(define-sdl2 has-alti-vec (_fun -> _bool))

(define-sdl2 has-mmx (_fun -> _bool))

(define-sdl2 has-3d-now (_fun -> _bool))

(define-sdl2 has-sse (_fun -> _bool))

(define-sdl2 has-sse2 (_fun -> _bool))

(define-sdl2 has-sse3 (_fun -> _bool))

(define-sdl2 has-sse41 (_fun -> _bool))

(define-sdl2 has-sse42 (_fun -> _bool))

(define-sdl2 has-avx (_fun -> _bool)
    #:make-fail make-not-available)

(define-sdl2 has-avx2 (_fun -> _bool)
    #:make-fail make-not-available)

(define-sdl2 has-avx512-f (_fun -> _bool)
    #:make-fail make-not-available)

(define-sdl2 has-neon (_fun -> _bool)
    #:make-fail make-not-available)

(define-sdl2 get-system-ram (_fun -> _int)
    #:make-fail make-not-available)


;; SDL_endian.h

(define lil-endian  1234)
(define big-endian  4321)

(define byteorder (if (system-big-endian?) big-endian lil-endian))

(begin-encourage-inline

    (define/contract (swap16 x)
        (contract/->
         (and/c exact-integer? (lambda (n) (<= (integer-length n) 16)))
         (and/c exact-integer? (lambda (n) (<= (integer-length n) 16))))
        (bitwise-and
         (bitwise-ior
          (arithmetic-shift x 8)
          (arithmetic-shift x -8))
         #xffff))

    (define/contract (swap32 x)
        (contract/->
         (and/c exact-integer? (lambda (n) (<= (integer-length n) 32)))
         (and/c exact-integer? (lambda (n) (<= (integer-length n) 32))))
        (bitwise-and
         (bitwise-ior
          (arithmetic-shift x 24)
          (bitwise-and (arithmetic-shift x  8) #x00FF0000)
          (bitwise-and (arithmetic-shift x -8) #x0000FF00)
          (arithmetic-shift x -24))
         #xffffffff))

    (define/contract (swap64 x)
        (contract/->
         (and/c exact-integer? (lambda (n) (<= (integer-length n) 64)))
         (and/c exact-integer? (lambda (n) (<= (integer-length n) 64))))
        (bitwise-and
         (bitwise-ior
          (arithmetic-shift
           (swap32 (bitwise-and x #xffffffff))
           32)
          (arithmetic-shift
           (swap32 (bitwise-and (arithmetic-shift x -32) #xffffffff))
           32))
         #xffffffffffffffff))

    (define/contract (swap-float x)
        (contract/->
         single-flonum? flonum?)
        ;; TODO : somehow convert _float to single-flonum
        (define _swapper (_union _float _uint32))
        (define s (cast (list x) (_list-struct _float) _swapper))
        (union-set! s 1 (swap32 (union-ref s 1)))
        (union-ref s 0)))

(define (swap-le16 x) (if (system-big-endian?) (swap16 x) x))
(define (swap-le32 x) (if (system-big-endian?) (swap32 x) x))
(define (swap-le64 x) (if (system-big-endian?) (swap64 x) x))
(define (swap-float-le x) (if (system-big-endian?) (swap-float x) x))
(define (swap-be16 x)  (if (system-big-endian?) x (swap16 x)))
(define (swap-be32 x)  (if (system-big-endian?) x (swap32 x)))
(define (swap-be64 x)  (if (system-big-endian?) x (swap64 x)))
(define (swap-float-be x) (if (system-big-endian?) x (swap-float x)))


;; SDL_error.h

(define-sdl2-vararg set-error! SDL_SetError (_string) _int)

(define-sdl2 get-error (_fun -> _string))

(define-sdl2 clear-error! (_fun -> _void))


;; SDL_scancode.h

(define _scancode
    (_enum
     '(unknown = 0

       a = 4
       b = 5
       c = 6
       d = 7
       e = 8
       f = 9
       g = 10
       h = 11
       i = 12
       j = 13
       k = 14
       l = 15
       m = 16
       n = 17
       o = 18
       p = 19
       q = 20
       r = 21
       s = 22
       t = 23
       u = 24
       v = 25
       w = 26
       x = 27
       y = 28
       z = 29

       n-1 = 30
       n-2 = 31
       n-3 = 32
       n-4 = 33
       n-5 = 34
       n-6 = 35
       n-7 = 36
       n-8 = 37
       n-9 = 38
       n-0 = 39

       return = 40
       escape = 41
       backspace = 42
       tab = 43
       space = 44

       minus = 45
       equals = 46
       left-bracket = 47
       right-bracket = 48
       backslash = 49
       non-us-hash = 50
       semicolon = 51
       apostrophe = 52
       grave = 53
       comma = 54
       period = 55
       slash = 56

       caps-lock = 57

       f1 = 58
       f2 = 59
       f3 = 60
       f4 = 61
       f5 = 62
       f6 = 63
       f7 = 64
       f8 = 65
       f9 = 66
       f10 = 67
       f11 = 68
       f12 = 69

       print-screen = 70
       scroll-lock = 71
       pause = 72
       insert = 73
       home = 74
       page-up = 75
       delete = 76
       end = 77
       page-down = 78
       right = 79
       left = 80
       down = 81
       up = 82

       num-lock-clear = 83
       kp-divide = 84
       kp-multiply = 85
       kp-minus = 86
       kp-plus = 87
       kp-enter = 88
       kp-1 = 89
       kp-2 = 90
       kp-3 = 91
       kp-4 = 92
       kp-5 = 93
       kp-6 = 94
       kp-7 = 95
       kp-8 = 96
       kp-9 = 97
       kp-0 = 98
       kp-period = 99

       non-us-backslash = 100
       application = 101
       power = 102
       kp-equals = 103
       f13 = 104
       f14 = 105
       f15 = 106
       f16 = 107
       f17 = 108
       f18 = 109
       f19 = 110
       f20 = 111
       f21 = 112
       f22 = 113
       f23 = 114
       f24 = 115
       execute = 116
       help = 117
       menu = 118
       select = 119
       stop = 120
       again = 121
       undo = 122
       cut = 123
       copy = 124
       paste = 125
       find = 126
       mute = 127
       volume-up = 128
       volume-down = 129
       kp-comma = 133
       kp-equals-as400 = 134

       international1 = 135
       international2 = 136
       international3 = 137
       international4 = 138
       international5 = 139
       international6 = 140
       international7 = 141
       international8 = 142
       international9 = 143
       lang1 = 144
       lang2 = 145
       lang3 = 146
       lang4 = 147
       lang5 = 148
       lang6 = 149
       lang7 = 150
       lang8 = 151
       lang9 = 152

       alt-erase = 153
       sys-req = 154
       cancel = 155
       clear = 156
       prior = 157
       return2 = 158
       separator = 159
       out = 160
       oper = 161
       clear-again = 162
       crsel = 163
       exsel = 164

       kp-00 = 176
       kp-000 = 177
       thousands-separator = 178
       decimal-separator = 179
       currency-unit = 180
       currency-subunit = 181
       kp-left-paren = 182
       kp-right-paren = 183
       kp-left-brace = 184
       kp-right-brace = 185
       kp-tab = 186
       kp-backspace = 187
       kp-a = 188
       kp-b = 189
       kp-c = 190
       kp-d = 191
       kp-e = 192
       kp-f = 193
       kp-xor = 194
       kp-power = 195
       kp-percent = 196
       kp-less = 197
       kp-greater = 198
       kp-ampersand = 199
       kp-dbl-ampersand = 200
       kp-vertical-bar = 201
       kp-dbl-vertical-bar = 202
       kp-colon = 203
       kp-hash = 204
       kp-space = 205
       kp-at = 206
       kp-exclam = 207
       kp-mem-store = 208
       kp-mem-recall = 209
       kp-mem-clear = 210
       kp-mem-add = 211
       kp-mem-subtract = 212
       kp-mem-multiply = 213
       kp-mem-divide = 214
       kp-plus-minus = 215
       kp-clear = 216
       kp-clear-entry = 217
       kp-binary = 218
       kp-octal = 219
       kp-decimal = 220
       kp-hexadecimal = 221

       lctrl = 224
       lshift = 225
       lalt = 226
       lgui = 227
       rctrl = 228
       rshift = 229
       ralt = 230
       rgui = 231

       mode = 257

       audio-next = 258
       audio-prev = 259
       audio-stop = 260
       audio-play = 261
       audio-mute = 262
       media-select = 263
       www = 264
       mail = 265
       calculator = 266
       computer = 267
       ac-search = 268
       ac-home = 269
       ac-back = 270
       ac-forward = 271
       ac-stop = 272
       ac-refresh = 273
       ac-bookmarks = 274

       brightness-down = 275
       brightness-up = 276
       display-switch = 277
       kbd-illum-toggle = 278
       kbd-illum-down = 279
       kbd-illum-up = 280
       eject = 281
       sleep = 282

       app1 = 283
       app2 = 284

       audio-rewind = 285
       audio-fast-forward = 286

       num-scancodes = 51)))


;; SDL_keycode.h

(define scancode-mask (arithmetic-shift 1 30))
(define (scancode-to-keycode x)
    (bitwise-ior (cast x _scancode _ufixint) scancode-mask))

(define _keycode
    (_enum
     `(unknown = 0

       return = ,(char->integer #\return)
       escape = ,(char->integer #\033)
       backspace = ,(char->integer #\backspace)
       tab = ,(char->integer #\tab)
       space = ,(char->integer #\space)
       exclaim = ,(char->integer #\!)
       quote-dbl = ,(char->integer #\")
       hash = ,(char->integer #\#)
       percent = ,(char->integer #\%)
       dollar = ,(char->integer #\$)
       ampersand = ,(char->integer #\&)
       quote = ,(char->integer #\')
       left-paren = ,(char->integer #\()
       right-paren = ,(char->integer #\))
       asterisk = ,(char->integer #\*)
       plus = ,(char->integer #\+)
       comma = ,(char->integer #\,)
       minus = ,(char->integer #\-)
       period = ,(char->integer #\.)
       slash = ,(char->integer #\/)
       n-0 = ,(char->integer #\0)
       n-1 = ,(char->integer #\1)
       n-2 = ,(char->integer #\2)
       n-3 = ,(char->integer #\3)
       n-4 = ,(char->integer #\4)
       n-5 = ,(char->integer #\5)
       n-6 = ,(char->integer #\6)
       n-7 = ,(char->integer #\7)
       n-8 = ,(char->integer #\8)
       n-9 = ,(char->integer #\9)
       colon = ,(char->integer #\:)
       semicolon = ,(char->integer #\;)
       less = ,(char->integer #\<)
       equals = ,(char->integer #\=)
       greater = ,(char->integer #\>)
       question = ,(char->integer #\?)
       at = ,(char->integer #\@)

       left-bracket = ,(char->integer #\[)
       backslash = ,(char->integer #\\)
       right-bracket = ,(char->integer #\])
       caret = ,(char->integer #\^)
       underscore = ,(char->integer #\_)
       backquote = ,(char->integer #\`)
       a = ,(char->integer #\a)
       b = ,(char->integer #\b)
       c = ,(char->integer #\c)
       d = ,(char->integer #\d)
       e = ,(char->integer #\e)
       f = ,(char->integer #\f)
       g = ,(char->integer #\g)
       h = ,(char->integer #\h)
       i = ,(char->integer #\i)
       j = ,(char->integer #\j)
       k = ,(char->integer #\k)
       l = ,(char->integer #\l)
       m = ,(char->integer #\m)
       n = ,(char->integer #\n)
       o = ,(char->integer #\o)
       p = ,(char->integer #\p)
       q = ,(char->integer #\q)
       r = ,(char->integer #\r)
       s = ,(char->integer #\s)
       t = ,(char->integer #\t)
       u = ,(char->integer #\u)
       v = ,(char->integer #\v)
       w = ,(char->integer #\w)
       x = ,(char->integer #\x)
       y = ,(char->integer #\y)
       z = ,(char->integer #\z)

       caps-lock = ,(scancode-to-keycode 'caps-lock)

       f1 = ,(scancode-to-keycode 'f1)
       f2 = ,(scancode-to-keycode 'f2)
       f3 = ,(scancode-to-keycode 'f3)
       f4 = ,(scancode-to-keycode 'f4)
       f5 = ,(scancode-to-keycode 'f5)
       f6 = ,(scancode-to-keycode 'f6)
       f7 = ,(scancode-to-keycode 'f7)
       f8 = ,(scancode-to-keycode 'f8)
       f9 = ,(scancode-to-keycode 'f9)
       f10 = ,(scancode-to-keycode 'f10)
       f11 = ,(scancode-to-keycode 'f11)
       f12 = ,(scancode-to-keycode 'f12)

       print-screen = ,(scancode-to-keycode 'print-screen)
       scroll-lock = ,(scancode-to-keycode 'scroll-lock)
       pause = ,(scancode-to-keycode 'pause)
       insert = ,(scancode-to-keycode 'insert)
       home = ,(scancode-to-keycode 'home)
       page-up = ,(scancode-to-keycode 'page-up)
       delete = ,(char->integer #\177)
       end = ,(scancode-to-keycode 'end)
       page-down = ,(scancode-to-keycode 'page-down)
       right = ,(scancode-to-keycode 'right)
       left = ,(scancode-to-keycode 'left)
       down = ,(scancode-to-keycode 'down)
       up = ,(scancode-to-keycode 'up)

       num-lock-clear = ,(scancode-to-keycode 'num-lock-clear)
       kp-divide = ,(scancode-to-keycode 'kp-divide)
       kp-multiply = ,(scancode-to-keycode 'kp-multiply)
       kp-minus = ,(scancode-to-keycode 'kp-minus)
       kp-plus = ,(scancode-to-keycode 'kp-plus)
       kp-enter = ,(scancode-to-keycode 'kp-enter)
       kp-1 = ,(scancode-to-keycode 'kp-1)
       kp-2 = ,(scancode-to-keycode 'kp-2)
       kp-3 = ,(scancode-to-keycode 'kp-3)
       kp-4 = ,(scancode-to-keycode 'kp-4)
       kp-5 = ,(scancode-to-keycode 'kp-5)
       kp-6 = ,(scancode-to-keycode 'kp-6)
       kp-7 = ,(scancode-to-keycode 'kp-7)
       kp-8 = ,(scancode-to-keycode 'kp-8)
       kp-9 = ,(scancode-to-keycode 'kp-9)
       kp-0 = ,(scancode-to-keycode 'kp-0)
       kp-period = ,(scancode-to-keycode 'kp-period)

       application = ,(scancode-to-keycode 'application)
       power = ,(scancode-to-keycode 'power)
       kp-equals = ,(scancode-to-keycode 'kp-equals)
       f13 = ,(scancode-to-keycode 'f13)
       f14 = ,(scancode-to-keycode 'f14)
       f15 = ,(scancode-to-keycode 'f15)
       f16 = ,(scancode-to-keycode 'f16)
       f17 = ,(scancode-to-keycode 'f17)
       f18 = ,(scancode-to-keycode 'f18)
       f19 = ,(scancode-to-keycode 'f19)
       f20 = ,(scancode-to-keycode 'f20)
       f21 = ,(scancode-to-keycode 'f21)
       f22 = ,(scancode-to-keycode 'f22)
       f23 = ,(scancode-to-keycode 'f23)
       f24 = ,(scancode-to-keycode 'f24)
       execute = ,(scancode-to-keycode 'execute)
       help = ,(scancode-to-keycode 'help)
       menu = ,(scancode-to-keycode 'menu)
       select = ,(scancode-to-keycode 'select)
       stop = ,(scancode-to-keycode 'stop)
       again = ,(scancode-to-keycode 'again)
       undo = ,(scancode-to-keycode 'undo)
       cut = ,(scancode-to-keycode 'cut)
       copy = ,(scancode-to-keycode 'copy)
       paste = ,(scancode-to-keycode 'paste)
       find = ,(scancode-to-keycode 'find)
       mute = ,(scancode-to-keycode 'mute)
       volume-up = ,(scancode-to-keycode 'volume-up)
       volume-down = ,(scancode-to-keycode 'volume-down)
       kp-comma = ,(scancode-to-keycode 'kp-comma)
       kp-equals-as400 = ,(scancode-to-keycode 'kp-equals-as400)

       alt-erase = ,(scancode-to-keycode 'alt-erase)
       sys-req = ,(scancode-to-keycode 'sys-req)
       cancel = ,(scancode-to-keycode 'cancel)
       clear = ,(scancode-to-keycode 'clear)
       prior = ,(scancode-to-keycode 'prior)
       return2 = ,(scancode-to-keycode 'return2)
       separator = ,(scancode-to-keycode 'separator)
       out = ,(scancode-to-keycode 'out)
       oper = ,(scancode-to-keycode 'oper)
       clear-again = ,(scancode-to-keycode 'clear-again)
       crsel = ,(scancode-to-keycode 'crsel)
       exsel = ,(scancode-to-keycode 'exsel)

       kp-00 = ,(scancode-to-keycode 'kp-00)
       kp-000 = ,(scancode-to-keycode 'kp-000)
       thousands-separator = ,(scancode-to-keycode 'thousands-separator)
       decimal-separator = ,(scancode-to-keycode 'decimal-separator)
       currency-unit = ,(scancode-to-keycode 'currency-unit)
       currency-subunit = ,(scancode-to-keycode 'currency-subunit)
       kp-left-paren = ,(scancode-to-keycode 'kp-left-paren)
       kp-right-paren = ,(scancode-to-keycode 'kp-right-paren)
       kp-left-brace = ,(scancode-to-keycode 'kp-left-brace)
       kp-right-brace = ,(scancode-to-keycode 'kp-right-brace)
       kp-tab = ,(scancode-to-keycode 'kp-tab)
       kp-backspace = ,(scancode-to-keycode 'kp-backspace)
       kp-a = ,(scancode-to-keycode 'kp-a)
       kp-b = ,(scancode-to-keycode 'kp-b)
       kp-c = ,(scancode-to-keycode 'kp-c)
       kp-d = ,(scancode-to-keycode 'kp-d)
       kp-e = ,(scancode-to-keycode 'kp-e)
       kp-f = ,(scancode-to-keycode 'kp-f)
       kp-xor = ,(scancode-to-keycode 'kp-xor)
       kp-power = ,(scancode-to-keycode 'kp-power)
       kp-percent = ,(scancode-to-keycode 'kp-percent)
       kp-less = ,(scancode-to-keycode 'kp-less)
       kp-greater = ,(scancode-to-keycode 'kp-greater)
       kp-ampersand = ,(scancode-to-keycode 'kp-ampersand)
       kp-dbl-ampersand = ,(scancode-to-keycode 'kp-dbl-ampersand)
       kp-vertical-bar = ,(scancode-to-keycode 'kp-vertical-bar)
       kp-dbl-verticalbar = ,(scancode-to-keycode 'kp-dbl-vertical-bar)
       kp-colon = ,(scancode-to-keycode 'kp-colon)
       kp-hash = ,(scancode-to-keycode 'kp-hash)
       kp-space = ,(scancode-to-keycode 'kp-space)
       kp-at = ,(scancode-to-keycode 'kp-at)
       kp-exclam = ,(scancode-to-keycode 'kp-exclam)
       kp-mem-store = ,(scancode-to-keycode 'kp-mem-store)
       kp-mem-recall = ,(scancode-to-keycode 'kp-mem-recall)
       kp-mem-clear = ,(scancode-to-keycode 'kp-mem-clear)
       kp-mem-add = ,(scancode-to-keycode 'kp-mem-add)
       kp-mem-subtract = ,(scancode-to-keycode 'kp-mem-subtract)
       kp-mem-multiply = ,(scancode-to-keycode 'kp-mem-multiply)
       kp-mem-divide = ,(scancode-to-keycode 'kp-mem-divide)
       kp-plus-minus = ,(scancode-to-keycode 'kp-plus-minus)
       kp-clear = ,(scancode-to-keycode 'kp-clear)
       kp-clear-entry = ,(scancode-to-keycode 'kp-clear-entry)
       kp-binary = ,(scancode-to-keycode 'kp-binary)
       kp-octal = ,(scancode-to-keycode 'kp-octal)
       kp-decimal = ,(scancode-to-keycode 'kp-decimal)
       kp-hexadecimal = ,(scancode-to-keycode 'kp-hexadecimal)

       lctrl = ,(scancode-to-keycode 'lctrl)
       lshift = ,(scancode-to-keycode 'lshift)
       lalt = ,(scancode-to-keycode 'lalt)
       lgui = ,(scancode-to-keycode 'lgui)
       rctrl = ,(scancode-to-keycode 'rctrl)
       rshift = ,(scancode-to-keycode 'rshift)
       ralt = ,(scancode-to-keycode 'ralt)
       rgui = ,(scancode-to-keycode 'rgui)

       mode = ,(scancode-to-keycode 'mode)

       audio-next = ,(scancode-to-keycode 'audio-next)
       audio-prev = ,(scancode-to-keycode 'audio-prev)
       audio-stop = ,(scancode-to-keycode 'audio-stop)
       audio-play = ,(scancode-to-keycode 'audio-play)
       audio-mute = ,(scancode-to-keycode 'audio-mute)
       media-select = ,(scancode-to-keycode 'media-select)
       www = ,(scancode-to-keycode 'www)
       mail = ,(scancode-to-keycode 'mail)
       calculator = ,(scancode-to-keycode 'calculator)
       computer = ,(scancode-to-keycode 'computer)
       ac-search = ,(scancode-to-keycode 'ac-search)
       ac-home = ,(scancode-to-keycode 'ac-home)
       ac-back = ,(scancode-to-keycode 'ac-back)
       ac-forward = ,(scancode-to-keycode 'ac-forward)
       ac-stop = ,(scancode-to-keycode 'ac-stop)
       ac-refresh = ,(scancode-to-keycode 'ac-refresh)
       ac-bookmarks = ,(scancode-to-keycode 'ac-bookmarks)

       brightness-down = ,(scancode-to-keycode 'brightness-down)
       brightness-up = ,(scancode-to-keycode 'brightness-up)
       display-switch = ,(scancode-to-keycode 'display-switch)
       kbd-illum-toggle = ,(scancode-to-keycode 'kbd-illum-toggle)
       kbd-illum-down = ,(scancode-to-keycode 'kbd-illum-down)
       kbd-illum-up = ,(scancode-to-keycode 'kbd-illum-up)
       eject = ,(scancode-to-keycode 'eject)
       sleep = ,(scancode-to-keycode 'sleep)
       app1 = ,(scancode-to-keycode 'app1)
       app2 = ,(scancode-to-keycode 'app2)

       audio-rewind = ,(scancode-to-keycode 'audio-rewind)
       audio-fast-forward = ,(scancode-to-keycode 'audio-fast-forward))
     _sint32))

(define _Keymod
    (_bitmask
     '(none = #x0000
       lshift = #x0001
       rshift = #x0002
       lctrl = #x0040
       rctrl = #x0080
       lalt = #x0100
       ralt = #x0200
       lgui = #x0400
       rgui = #x0800
       num = #x1000
       caps = #x2000
       mode = #x4000
       reserved = #x8000

       ctrl = #x00c0
       shift = #x0003
       alt = #x0300
       gui = #x0c00)))


;; SDL_rect.h

(define-cstruct _point
    ([x _int]
     [y _int]))

(define _point* _point-pointer)
(define _point*/null _point-pointer/null)

(define-cstruct _rect
    ([x _int]
     [y _int]
     [w _int]
     [h _int]))

(define _rect* _rect-pointer)
(define _rect*/null _rect-pointer/null)

(begin-encourage-inline
    (define (point-in-rect? p r)
        (and
         (>= (point-x p) (rect-x r))
         (< (point-x p) (+ (rect-x r) (rect-w r)))
         (>= (point-y p) (rect-y r))
         (< (point-y p) (+ (rect-y r) (rect-h r)))))

    (define (rect-empty? r)
        (or
         (not r)
         (<= (rect-w r) 0)
         (<= (rect-h r) 0)))

    (define (rect=? a b)
        (and
         a b
         (= (rect-x a) (rect-x b))
         (= (rect-y a) (rect-y b))
         (= (rect-w a) (rect-w b))
         (= (rect-h a) (rect-h b)))))

(define-sdl2 has-intersection (_fun _rect* _rect* -> _bool))

(define-sdl2 intersect-rect (_fun _rect* _rect* _rect* -> _bool))

(define-sdl2 union-rect (_fun _rect* _rect* _rect* -> _void))

(define-sdl2 enclose-points (_fun _point* _int _rect*/null _rect* -> _bool))

(define-sdl2 intersect-rect-and-line (_fun _rect* _int* _int* _int* _int* -> _bool))


;; SDL_pixels.h

(define alpha-opaque 255)
(define alpha-transparent 0)

(define pixeltype-unknown 0)
(define pixeltype-index1 1)
(define pixeltype-index4 2)
(define pixeltype-index8 3)
(define pixeltype-packed8 4)
(define pixeltype-packed16 5)
(define pixeltype-packed32 6)
(define pixeltype-arrayu8 7)
(define pixeltype-arrayu16 8)
(define pixeltype-arrayu32 9)
(define pixeltype-arrayf16 10)
(define pixeltype-arrayf32 11)

(define bitmaporder-none 0)
(define bitmaporder-4321 1)
(define bitmaporder-1234 2)

(define packedorder-none 0)
(define packedorder-xrgb 1)
(define packedorder-rgbx 2)
(define packedorder-argb 3)
(define packedorder-rgba 4)
(define packedorder-xbgr 5)
(define packedorder-bgrx 6)
(define packedorder-abgr 7)
(define packedorder-bgra 8)

(define arrayorder-none 0)
(define arrayorder-rgb 1)
(define arrayorder-rgba 2)
(define arrayorder-argb 3)
(define arrayorder-bgr 4)
(define arrayorder-bgra 5)
(define arrayorder-abgr 6)

(define packedlayout-none 0)
(define packedlayout-332 1)
(define packedlayout-4444 2)
(define packedlayout-1555 3)
(define packedlayout-5551 4)
(define packedlayout-565 5)
(define packedlayout-8888 6)
(define packedlayout-2101010 7)
(define packedlayout-1010102 8)

(define define-pixelfourcc fourcc)

(define (define-pixelformat type order layout bits bytes)
    (bitwise-ior
     (arithmetic-shift 1 28)
     (arithmetic-shift type 24)
     (arithmetic-shift order 20)
     (arithmetic-shift layout 16)
     (arithmetic-shift bits 8)
     (arithmetic-shift bytes 0)))

(define pixelformat-unknown 0)
(define pixelformat-index1-lsb
    (define-pixelformat
     pixeltype-index1
     bitmaporder-4321
     0 1 0))
(define pixelformat-index1-msb
    (define-pixelformat
     pixeltype-index1
     bitmaporder-1234
     0 1 0))
(define pixelformat-index4-lsb
    (define-pixelformat
     pixeltype-index4
     bitmaporder-4321
     0 4 0))
(define pixelformat-index4-msb
    (define-pixelformat
     pixeltype-index4
     bitmaporder-1234
     0 4 0))
(define pixelformat-index8
    (define-pixelformat
     pixeltype-index8
     0 0 8 1))
(define pixelformat-rgb332
    (define-pixelformat
     pixeltype-packed8
     packedorder-xrgb
     packedlayout-332
     8 1))
(define pixelformat-rgb444
    (define-pixelformat
     pixeltype-packed16
     packedorder-xrgb
     packedlayout-4444
     12 2))
(define pixelformat-rgb555
    (define-pixelformat
     pixeltype-packed16
     packedorder-xrgb
     packedlayout-1555
     15 2))
(define pixelformat-bgr555
    (define-pixelformat
     pixeltype-packed16
     packedorder-xbgr
     packedlayout-1555
     15 2))
(define pixelformat-argb4444
    (define-pixelformat
     pixeltype-packed16
     packedorder-argb
     packedlayout-4444
     16 2))
(define pixelformat-rgba4444
    (define-pixelformat
     pixeltype-packed16
     packedorder-rgba
     packedlayout-4444
     16 2))
(define pixelformat-abgr4444
    (define-pixelformat
     pixeltype-packed16
     packedorder-abgr
     packedlayout-4444
     16 2))
(define pixelformat-bgra4444
    (define-pixelformat
     pixeltype-packed16
     packedorder-bgra
     packedlayout-4444
     16 2))
(define pixelformat-argb1555
    (define-pixelformat
     pixeltype-packed16
     packedorder-argb
     packedlayout-1555
     16 2))
(define pixelformat-rgba5551
    (define-pixelformat
     pixeltype-packed16
     packedorder-rgba
     packedlayout-5551
     16 2))
(define pixelformat-abgr1555
    (define-pixelformat
     pixeltype-packed16
     packedorder-abgr
     packedlayout-1555
     16 2))
(define pixelformat-bgra5551
    (define-pixelformat
     pixeltype-packed16
     packedorder-bgra
     packedlayout-5551
     16 2))
(define pixelformat-rgb565
    (define-pixelformat
     pixeltype-packed16
     packedorder-xrgb
     packedlayout-565
     16 2))
(define pixelformat-bgr565
    (define-pixelformat
     pixeltype-packed16
     packedorder-xbgr
     packedlayout-565
     16 2))
(define pixelformat-rgb24
    (define-pixelformat
     pixeltype-arrayu8
     arrayorder-rgb
     0 24 3))
(define pixelformat-bgr24
    (define-pixelformat
     pixeltype-arrayu8
     arrayorder-bgr
     0 24 3))
(define pixelformat-rgb888
    (define-pixelformat
     pixeltype-packed32
     packedorder-xrgb
     packedlayout-8888
     24 4))
(define pixelformat-rgbx8888
    (define-pixelformat
     pixeltype-packed32
     packedorder-rgbx
     packedlayout-8888
     24 4))
(define pixelformat-bgr888
    (define-pixelformat
     pixeltype-packed32
     packedorder-xbgr
     packedlayout-8888
     24 4))
(define pixelformat-bgrx8888
    (define-pixelformat
     pixeltype-packed32
     packedorder-bgrx
     packedlayout-8888
     24 4))
(define pixelformat-argb8888
    (define-pixelformat
     pixeltype-packed32
     packedorder-argb
     packedlayout-8888
     32 4))
(define pixelformat-rgba8888
    (define-pixelformat
     pixeltype-packed32
     packedorder-rgba
     packedlayout-8888
     32 4))
(define pixelformat-abgr8888
    (define-pixelformat
     pixeltype-packed32
     packedorder-abgr
     packedlayout-8888
     32 4))
(define pixelformat-bgra8888
    (define-pixelformat
     pixeltype-packed32
     packedorder-bgra
     packedlayout-8888
     32 4))
(define pixelformat-argb2101010
    (define-pixelformat
     pixeltype-packed32
     packedorder-argb
     packedlayout-2101010
     32 4))

(define pixelformat-rgba32
    (if (system-big-endian?)
        pixelformat-rgba8888
        pixelformat-abgr8888))
(define pixelformat-argb32
    (if (system-big-endian?)
        pixelformat-argb8888
        pixelformat-bgra8888))
(define pixelformat-bgra32
    (if (system-big-endian?)
        pixelformat-bgra8888
        pixelformat-argb8888))
(define pixelformat-abgr32
    (if (system-big-endian?)
        pixelformat-abgr8888
        pixelformat-rgba8888))

(define pixelformat-yv12
    (define-pixelfourcc #\Y  #\V  #\1  #\2))
(define pixelformat-iyuv
    (define-pixelfourcc #\I  #\Y  #\U  #\V))
(define pixelformat-yuy2
    (define-pixelfourcc #\Y  #\U  #\Y  #\2))
(define pixelformat-uyvy
    (define-pixelfourcc #\U  #\Y  #\V  #\Y))
(define pixelformat-yvyu
    (define-pixelfourcc #\Y  #\V  #\Y  #\U))
(define pixelformat-nv12
    (define-pixelfourcc #\N  #\V  #\1  #\2))
(define pixelformat-nv21
    (define-pixelfourcc #\N  #\V  #\2  #\1))
(define pixelformat-external-oes
    (define-pixelfourcc #\O  #\E  #\S  #\ ))

(define (pixel-flag x) (bitwise-and (arithmetic-shift x -28) #x0f))
(define (pixel-type x) (bitwise-and (arithmetic-shift x -24) #x0f))
(define (pixel-order x) (bitwise-and (arithmetic-shift x -20) #x0f))
(define (pixel-layout x) (bitwise-and (arithmetic-shift x -16) #x0f))
(define (bits-per-pixel x) (bitwise-and (arithmetic-shift x -8) #x0f))

(define (pixelformat-fourcc? format)
    (and (not (zero? format)) (not (= (pixel-flag format) 1))))

(define (bytes-per-pixel x)
    (if (pixelformat-fourcc? x)
        (if (or (= x pixelformat-yuy2)
                (= x pixelformat-uyvy)
                (= x pixelformat-yvyu))
            2 1)
        (bitwise-and (arithmetic-shift x 0) #xFF)))

(define (pixelformat-indexed? format)
    (and
     (not (pixelformat-fourcc? format))
     (or
      (= (pixel-type format) pixeltype-index1)
      (= (pixel-type format) pixeltype-index4)
      (= (pixel-type format) pixeltype-index8))))

(define (pixelformat-packed? format)
    (and
     (not (pixelformat-fourcc? format))
     (or
      (= (pixel-type format) pixeltype-packed8)
      (= (pixel-type format) pixeltype-packed16)
      (= (pixel-type format) pixeltype-packed32))))

(define (pixelformat-array? format)
    (and
     (not (pixelformat-fourcc? format))
     (or
      (= (pixel-type format) pixeltype-arrayu8)
      (= (pixel-type format) pixeltype-arrayu16)
      (= (pixel-type format) pixeltype-arrayu32)
      (= (pixel-type format) pixeltype-arrayf16)
      (= (pixel-type format) pixeltype-arrayf32))))

(define (pixelformat-alpha? format)
    (or
     (and
      (pixelformat-fourcc? format)
      (or
       (= (pixel-order format) packedorder-argb)
       (= (pixel-order format) packedorder-rgba)
       (= (pixel-order format) packedorder-abgr)
       (= (pixel-order format) packedorder-bgra)))
     (and
      (pixelformat-array? format)
      (or
       (= (pixel-order format) packedorder-argb)
       (= (pixel-order format) packedorder-rgba)
       (= (pixel-order format) packedorder-abgr)
       (= (pixel-order format) packedorder-bgra)))))

(define-cstruct _color
    ([r _uint8]
     [g _uint8]
     [b _uint8]
     [a _uint8]))

(define _color* _color-pointer)

(define-cstruct _palette
    ([ncolors _int]
     [colors _color*]
     [version _uint32]
     [refcount _int]))

(define _palette* _palette-pointer)
(define _palette*/null _palette-pointer/null)

(define-cstruct _pixel-format
    ([format _uint32]
     [palette _palette*]
     [bits-per-pixel _uint8]
     [bytes-per-pixel _uint8]
     [padding (make-array-type _uint8 2)]
     [r-mask _uint32]
     [g-mask _uint32]
     [b-mask _uint32]
     [a-mask _uint32]
     [r-loss _uint8]
     [g-loss _uint8]
     [b-loss _uint8]
     [a-loss _uint8]
     [r-shift _uint8]
     [g-shift _uint8]
     [b-shift _uint8]
     [a-shift _uint8]
     [refcount _int]))

(define _pixel-format* _pixel-format-pointer)
(define _pixel-format*/null _pixel-format-pointer/null)

(define-sdl2 get-pixel-format-name (_fun _uint32 -> _string))

(define-sdl2 pixel-format-enum-to-masks
    (_fun _uint32 _int* _uint32* _uint32* _uint32* _uint32* -> _bool))

(define-sdl2 masks-to-pixel-format-enum (_fun _int _uint32 _uint32 _uint32 _uint32 -> _uint32))

(define-sdl2 alloc-format (_fun _uint32 -> _pixel-format*/null))

(define-sdl2 free-format! (_fun _pixel-format* -> _void))

(define-sdl2 alloc-palette (_fun _int -> _palette*/null))

(define-sdl2 set-pixel-format-palette (_fun _pixel-format* _palette*/null -> _int))

(define-sdl2 set-palette-colors (_fun _palette* _color* _int _int -> _int))

(define-sdl2 free-palette! (_fun _palette* -> _void))

(define-sdl2 map-rgb (_fun _pixel-format* _uint8 _uint8 _uint8 -> _uint32))

(define-sdl2 map-rgba (_fun _pixel-format* _uint8 _uint8 _uint8 _uint8 -> _uint32))

(define-sdl2 get-rgb (_fun _uint32 _pixel-format* _uint8* _uint8* _uint8* -> _void))

(define-sdl2 get-rgba (_fun _uint32 _pixel-format* _uint8* _uint8* _uint8* _uint8* -> _void))

(define-sdl2 calculate-gamma-ramp (_fun _float _uint16* -> _void))


;; SDL_blendmode.h

(define _blend-mode
    (_enum
     '(none = #x00000000
       blend = #x00000001
       add = #x00000002
       mod = #x00000004
       invalid = #x7fffffff)))

(define-cpointer-type _blend-mode*)

(define _blend-operation
    (_enum
     '(add              = #x1
       subtract         = #x2
       rev-subtract     = #x3
       minimum          = #x4
       maximum          = #x5)))

(define _blend-factor
    (_enum
     '(zero                = #x1
       one                 = #x2
       src-color           = #x3
       one-minus-src-color = #x4
       src-alpha           = #x5
       one-minus-src-alpha = #x6
       dst-color           = #x7
       one-minus-dst-color = #x8
       dst-alpha           = #x9
       one-minus-dst-alpha = #xA)))

(define-sdl2 compose-custom-blend-mode
    (_fun
     _blend-factor
     _blend-factor
     _blend-operation
     _blend-factor
     _blend-factor
     _blend-operation
     -> _blend-mode)
    #:make-fail make-not-available)


;; SDL_surface.h

(define sw-surface       0         )
(define pre-alloc        #x00000001)
(define rle-accel        #x00000002)
(define dont-free        #x00000004)

(define _blit-map* _pointer)

(define-cstruct _surface
    ([flags _uint32]
     [format _pixel-format*]
     [w _int]
     [h _int]
     [pitch _int]
     [pixels _pointer]
     [userdata _pointer]
     [locked _int]
     [lock-data _pointer]
     [clip-rect _rect]
     [map _blit-map*]
     [refcount _int]))

(define _surface* _surface-pointer)
(define _surface*/null _surface-pointer/null)

(define (must-lock s)
    (not (zero? (bitwise-and (surface-flags s) rle-accel))))

(define _blit* (_fun _surface* _rect* _surface* _rect* -> _int))

(define _yuv-conversion-mode
    (_enum
     '(jpeg
       bt601
       bt709
       automatic)))

(define-sdl2 create-rgb-surface
    (_fun _uint32 _int _int _int _uint32 _uint32 _uint32 _uint32 -> _surface*/null))

(define-sdl2 create-rgb-surface-with-format
    (_fun _uint32 _int _int _int _uint32 -> _surface*/null)
    #:make-fail make-not-available)

(define-sdl2 create-rgb-surface-from
    (_fun _pointer _int _int _int _int _uint32 _uint32 _uint32 _uint32 -> _surface*/null))

(define-sdl2 create-rgb-surface-with-format-from
    (_fun _pointer _int _int _int _int _uint32 -> _surface*/null)
    #:make-fail make-not-available)

(define-sdl2 free-surface! (_fun _surface* -> _void))

(define-sdl2 set-surface-palette! (_fun _surface* _palette*/null -> _int))

(define-sdl2 lock-surface! (_fun _surface* -> _int))

(define-sdl2 unlock-surface! (_fun _surface* -> _void))

(define-sdl2 load-bmp-rw (_fun _rw-ops* _int -> _surface*))

(define (load-bmp file) (load-bmp-rw (rw-from-file file "rb") 1))

(define-sdl2 save-bmp-rw! (_fun _surface* _rw-ops* _int -> _int))

(define (save-bmp! surface file) (save-bmp-rw! (rw-from-file file "wb") 1))

(define-sdl2 set-surface-rle! (_fun _surface* _int -> _int))

(define-sdl2 set-color-key! (_fun _surface* _int _uint32 -> _int))

(define-sdl2 has-color-key (_fun _surface* -> _bool)
    #:make-fail make-not-available)

(define-sdl2 get-color-key (_fun _surface* _uint32* -> _int))

(define-sdl2 set-surface-color-mod! (_fun _surface* _uint8 _uint8 _uint8 -> _int))

(define-sdl2 get-surface-color-mod (_fun _surface* _uint8* _uint8* _uint8* -> _int))

(define-sdl2 set-surface-alpha-mod! (_fun _surface* _uint8 -> _int))

(define-sdl2 get-surface-alpha-mod (_fun _surface* _uint8* -> _int))

(define-sdl2 set-surface-blend-mode! (_fun _surface* _blend-mode -> _int))

(define-sdl2 get-surface-blend-mode (_fun _surface* _blend-mode* -> _int))

(define-sdl2 set-clip-rect! (_fun _surface* _rect*/null -> _bool))

(define-sdl2 get-clip-rect (_fun _surface* _rect* -> _void))

(define-sdl2 duplicate-surface (_fun _surface* -> _surface*)
    #:make-fail make-not-available)

(define-sdl2 convert-surface (_fun _surface* _pixel-format* _uint32 -> _surface*/null))
(define-sdl2 convert-surface-format (_fun _surface* _uint32 _uint32 -> _surface*/null))

(define-sdl2 convert-pixels (_fun _int _int _uint32 _pointer _int _uint32 _pointer _int -> _int))

(define-sdl2 fill-rect! (_fun _surface* _rect*/null _uint32 -> _int))
(define-sdl2 fill-rects! (_fun _surface* _rect* _int _uint32 -> _int))

(define-sdl2 upper-blit (_fun _surface* _rect*/null _surface* _rect*/null -> _int))
(define blit-surface! upper-blit)

(define-sdl2 lower-blit (_fun _surface* _rect*/null _surface* _rect*/null -> _int))

(define-sdl2 soft-stretch (_fun _surface* _rect* _surface* _rect* -> _int))

(define-sdl2 upper-blit-scaled
    (_fun _surface* _rect*/null _surface* _rect*/null -> _int))
(define blit-scaled! upper-blit-scaled)

(define-sdl2 lower-blit-scaled
    (_fun _surface* _rect*/null _surface* _rect*/null -> _int))

(define-sdl2 set-yuv-conversion-mode! (_fun _yuv-conversion-mode -> _void)
    #:make-fail make-not-available)

(define-sdl2 get-yuv-conversion-mode (_fun -> _yuv-conversion-mode)
    #:make-fail make-not-available)

(define-sdl2 get-yuv-conversion-mode-for-resolution (_fun _int _int -> _yuv-conversion-mode)
    #:make-fail make-not-available)


;; SDL_video.h

(define-cstruct _display-mode
    ([format _uint32]
     [w _int]
     [h _int]
     [refresh-rate _int]
     [driver-data _pointer]))

(define _display-mode* _display-mode-pointer)
(define _display-mode*/null _display-mode-pointer/null)

(define-cpointer-type _window*)

(define _window-flags
    (_bitmask
     '(fullscreen = #x00000001
       opengl = #x00000002
       shown = #x00000004
       hidden = #x00000008
       borderless = #x00000010
       resizable = #x00000020
       minimized = #x00000040
       maximized = #x00000080
       input-grabbed = #x00000100
       input-focus = #x00000200
       mouse-focus = #x00000400
       fullscreen-desktop = #x00001001
       foreign = #x00000800
       allow-high-dpi = #x00002000
       mouse-capture = #x00004000
       always-on-top = #x00008000
       skip-taskbar  = #x00010000
       utility       = #x00020000
       tooltip       = #x00040000
       popup-menu    = #x00080000
       vulkan        = #x10000000)
     _uint32))

(define window-pos-undefined-mask    #x1FFF0000)
(define (window-pos-undefined-display x)
    (bitwise-ior window-pos-undefined-mask x))
(define window-pos-undefined         (window-pos-undefined-display 0))
(define (window-pos-undefined? x)
    (= (bitwise-and x #xFFFF0000) window-pos-undefined-mask))

(define window-pos-centered-mask    #x2FFF0000)
(define (window-pos-centered-display x)
    (bitwise-ior window-pos-centered-mask x))
(define window-pos-centered         (window-pos-centered-display 0))
(define (window-pos-centered? x)
    (= (bitwise-and x #xFFFF0000) window-pos-centered-mask))

(define _window-event-id
    (_enum
     '(none
       shown
       hidden
       exposed

       moved

       resized
       size-changed

       minimized
       maximized
       restored

       enter
       leave
       focus-gained
       focus-lost
       close
       take-focus
       hit-test)))

(define _display-event-id
    (_enum
     '(none
       orientation)))

(define _display-orientation
    (_enum
     '(unknown
       landscape
       landscape-flipped
       portrait
       portrait-flipped)))

(define-cpointer-type _gl-context)

(define _gl-attr
    (_enum
     '(red-size
       green-size
       blue-size
       alpha-size
       buffer-size
       doublebuffer
       depth-size
       stencil-size
       accum-red-size
       accum-green-size
       accum-blue-size
       accum-alpha-size
       stereo
       multisample-buffers
       multisample-samples
       accelerated-visual
       retained-backing
       context-major-version
       context-minor-version
       context-egl
       context-flags
       context-profile-mask
       share-with-current-context
       framebuffer-srgb-capable
       context-release-behavior
       context-reset-notification
       context-no-error)))

(define _gl-profile
    (_enum
     '(core           = #x0001
       compatibility  = #x0002
       es             = #x0004)))

(define _gl-context-flag
    (_enum
     '(debug-flag              = #x0001
       forward-compatible-flag = #x0002
       robust-access-flag      = #x0004
       reset-isolation-flag    = #x0008)))

(define _gl-context-release-flag
    (_enum
     '(none   = #x0000
       flush  = #x0001)))

(define _gl-context-reset-notification
    (_enum
     '(no-notification = #x0000
       lose-context    = #x0001)))

(define-sdl2 get-num-video-drivers (_fun -> _int))

(define-sdl2 get-video-driver (_fun _int -> _string))

(define-sdl2 video-init (_fun _string -> _int))

(define-sdl2 video-quit (_fun -> _void))

(define-sdl2 get-current-video-driver (_fun -> _string))

(define-sdl2 get-num-video-displays (_fun -> _int))

(define-sdl2 get-display-name (_fun _int -> _string))

(define-sdl2 get-display-bounds (_fun _int _rect* -> _int))

(define-sdl2 get-display-usable-bounds (_fun _int _rect* -> _int)
    #:make-fail make-not-available)

(define-sdl2 get-display-dpi (_fun _int _float* _float* _float* -> _int)
    #:make-fail make-not-available)

(define-sdl2 get-display-orientation (_fun _int -> _display-orientation)
    #:make-fail make-not-available)

(define-sdl2 get-num-display-modes (_fun _int -> _int))

(define-sdl2 get-display-mode (_fun _int _int _display-mode* -> _int))

(define-sdl2 get-desktop-display-mode (_fun _int _display-mode* -> _int))

(define-sdl2 get-current-display-mode (_fun _int _display-mode* -> _int))

(define-sdl2 get-closest-display-mode
    (_fun _int _display-mode* _display-mode* -> _display-mode*))

(define-sdl2 get-window-display-index (_fun _window* -> _int))

(define-sdl2 set-window-display-mode! (_fun _window* _display-mode*/null -> _int))

(define-sdl2 get-window-display-mode (_fun _window* _display-mode* -> _int))

(define-sdl2 get-window-pixel-format (_fun _window* -> _uint32))

(define-sdl2 create-window!
    (_fun _string _int _int _int _int _window-flags -> _window*/null))

(define-sdl2 create-window-from! (_fun _pointer -> _window*/null))

(define-sdl2 get-window-id (_fun _window* -> _uint32))

(define-sdl2 get-window-from-id (_fun _uint32 -> _window*/null))

(define-sdl2 get-window-flags (_fun _window* -> _uint32))

(define-sdl2 set-window-title! (_fun _window* _string -> _void))

(define-sdl2 get-window-title (_fun _window* -> _string))

(define-sdl2 set-window-icon! (_fun _window* _surface* -> _void))

(define-sdl2 set-window-data! (_fun _window* _string _pointer -> _pointer))

(define-sdl2 get-window-data (_fun _window* _string -> _void))

(define-sdl2 set-window-position! (_fun _window* _int _int -> _void))

(define-sdl2 get-window-position (_fun _window* _int*/null _int*/null -> _void))

(define-sdl2 set-window-size! (_fun _window* _int _int -> _void))

(define-sdl2 get-window-size (_fun _window* _int*/null _int*/null -> _void))

(define-sdl2 get-window-borders-size
    (_fun _window* _int*/null _int*/null _int*/null _int*/null -> _int)
    #:make-fail make-not-available)

(define-sdl2 set-window-minimum-size! (_fun _window* _int _int -> _void))

(define-sdl2 get-window-minimum-size (_fun _window* _int*/null _int*/null -> _void))

(define-sdl2 set-window-maximum-size! (_fun _window* _int _int -> _void))

(define-sdl2 get-window-maximum-size (_fun _window* _int*/null _int*/null -> _void))

(define-sdl2 set-window-bordered! (_fun _window* _bool -> _void))

(define-sdl2 set-window-resizable! (_fun _window* _bool -> _void)
    #:make-fail make-not-available)

(define-sdl2 show-window! (_fun _window* -> _void))

(define-sdl2 hide-window! (_fun _window* -> _void))

(define-sdl2 raise-window! (_fun _window* -> _void))

(define-sdl2 maximize-window! (_fun _window* -> _void))

(define-sdl2 minimize-window! (_fun _window* -> _void))

(define-sdl2 restore-window! (_fun _window* -> _void))

(define-sdl2 set-window-fullscreen! (_fun _window* _uint32 -> _int))

(define-sdl2 get-window-surface (_fun _window* -> _surface*/null))

(define-sdl2 update-window-surface! (_fun _window* -> _int))

(define-sdl2 update-window-surface-rects! (_fun _window* _rect* _int -> _int))

(define-sdl2 set-window-grab! (_fun _window* _bool -> _void))

(define-sdl2 get-window-grab (_fun _window* -> _bool))

(define-sdl2 get-grabbed-window (_fun -> _window*/null)
    #:make-fail make-not-available)

(define-sdl2 set-window-brightness! (_fun _window* _float -> _int))

(define-sdl2 get-window-brightness (_fun _window* -> _float))

(define-sdl2 set-window-opacity! (_fun _window* _float -> _int)
    #:make-fail make-not-available)

(define-sdl2 get-window-opacity (_fun _window* _float* -> _int)
    #:make-fail make-not-available)

(define-sdl2 set-window-modal-for! (_fun _window* _window* -> _int)
    #:make-fail make-not-available)

(define-sdl2 set-window-input-focus! (_fun _window* -> _int)
    #:make-fail make-not-available)

(define-sdl2 set-window-gamma-ramp!
    (_fun _window* _uint16*/null _uint16*/null _uint16*/null -> _int))

(define-sdl2 get-window-gamma-ramp
    (_fun _window* _uint16*/null _uint16*/null _uint16*/null -> _int))

(define _hit-test-result
    (_enum
     '(normal
       draggable
       resize-top-left
       resize-top
       resize-top-right
       resize-right
       resize-bottom-right
       resize-bottom
       resize-bottom-left
       resize-left)))

(define _hit-test (_fun _window* _point* _pointer -> _hit-test-result))

(define-sdl2 set-window-hit-test! (_fun _window* _hit-test _pointer -> _int)
    #:make-fail make-not-available)

(define-sdl2 destroy-window! (_fun _window* -> _void))

(define-sdl2 screen-saver-enabled? (_fun -> _bool))

(define-sdl2 enable-screen-saver (_fun -> _void))

(define-sdl2 disable-screen-saver (_fun -> _void))

(define-sdl2 gl-load-library (_fun _string -> _int))

(define-sdl2 gl-get-proc-address (_fun _string -> _pointer))

(define-sdl2 gl-unload-library (_fun -> _void))

(define-sdl2 gl-extension-supported (_fun _string -> _bool))

(define-sdl2 gl-reset-attributes! (_fun -> _void)
    #:make-fail make-not-available)

(define-sdl2 gl-set-attribute! (_fun _gl-attr _int -> _int))

(define-sdl2 gl-get-attribute (_fun _gl-attr _int* -> _int))

(define-sdl2 gl-create-context! (_fun _window* -> _gl-context/null))

(define-sdl2 gl-make-current! (_fun _window* _gl-context -> _int))

(define-sdl2 gl-get-current-window (_fun -> _window*/null))

(define-sdl2 gl-get-current-context (_fun -> _gl-context/null))

(define-sdl2 gl-get-drawable-size (_fun _window* _int*/null _int*/null -> _void)
    #:make-fail make-not-available)

(define-sdl2 gl-set-swap-interval! (_fun _int -> _int))

(define-sdl2 gl-get-swap-interval (_fun -> _int))

(define-sdl2 gl-swap-window! (_fun _window* -> _void))

(define-sdl2 gl-delete-context! (_fun _gl-context -> _void))


;; SDL_vulkan.h

(define-cpointer-type _vk-instance)
(define-cpointer-type _vk-surface-khr*)

(define-sdl2 vulkan-load-library (_fun _string -> _int)
    #:make-fail make-not-available)

(define-sdl2 vulkan-get-vk-get-instance-proc-addr (_fun -> _pointer)
    #:make-fail make-not-available)

(define-sdl2 vulkan-unload-library (_fun -> _void)
    #:make-fail make-not-available)

(define-sdl2 vulkan-get-instance-extensions (_fun _window* _uint* (_ptr o _string) -> _bool)
    #:make-fail make-not-available)

(define-sdl2 vulkan-create-surface (_fun _window* _vk-instance _vk-surface-khr* -> _bool)
    #:make-fail make-not-available)

(define-sdl2 vulkan-get-drawable-size (_fun _window* _int*/null _int*/null -> _void)
    #:make-fail make-not-available)


;; SDL_keyboard.h

(define-cstruct _keysym
    ([scancode _scancode]
     [sym _keycode]
     [mod _uint16]
     [unused _uint32]))

(define-sdl2 get-keyboard-focus (_fun -> _window*/null))

(define-sdl2 get-keyboard-state (_fun _int*/null -> _uint8*))

(define-sdl2 get-mod-state (_fun -> _Keymod))

(define-sdl2 set-mod-state! (_fun _Keymod -> _void))

(define-sdl2 get-key-from-scancode (_fun _scancode -> _keycode))

(define-sdl2 get-scancode-from-key (_fun _keycode -> _scancode))

(define-sdl2 get-scancode-name (_fun _scancode -> _string))

(define-sdl2 get-scancode-from-name (_fun _string -> _scancode))

(define-sdl2 get-key-name (_fun _keycode -> _string))

(define-sdl2 get-key-from-name (_fun _string -> _keycode))

(define-sdl2 start-text-input! (_fun -> _void))

(define-sdl2 text-input-active? (_fun -> _bool))

(define-sdl2 stop-text-input! (_fun -> _void))

(define-sdl2 set-text-input-rect! (_fun _rect* -> _void))

(define-sdl2 has-screen-keyboard-support (_fun -> _bool))

(define-sdl2 screen-keyboard-shown? (_fun _window* -> _bool))


;; SDL_mouse.h

(define-cpointer-type _cursor*)

(define _system-cursor
    (_enum
     '(arrow
       ibeam
       wait
       crosshair
       wait-arrow
       size-nwse
       size-nesw
       size-we
       size-ns
       size-all
       no
       hand
       num-system-cursors)))

(define _mouse-wheel-direction
    (_enum
     '(normal
       flipped)))

(define-sdl2 get-mouse-focus (_fun -> _window*/null))

(define-sdl2 get-mouse-state (_fun _int* _int* -> _uint32))

(define-sdl2 get-global-mouse-state (_fun _int* _int* -> _uint32)
    #:make-fail make-not-available)

(define-sdl2 get-relative-mouse-state (_fun _int* _int* -> _uint32))

(define-sdl2 warp-mouse-in-window (_fun _window*/null _int _int -> _void))

(define-sdl2 warp-mouse-global (_fun _int _int -> _int)
    #:make-fail make-not-available)

(define-sdl2 set-relative-mouse-mode! (_fun _bool -> _int))

(define-sdl2 capture-mouse (_fun _bool -> _int)
    #:make-fail make-not-available)

(define-sdl2 get-relative-mouse-mode (_fun -> _bool))

(define-sdl2 create-cursor (_fun _uint8* _uint8* _int _int _int _int -> _cursor*/null))

(define-sdl2 create-color-cursor (_fun _surface* _int _int -> _cursor*/null))

(define-sdl2 create-system-cursor (_fun _system-cursor -> _cursor*/null))

(define-sdl2 set-cursor! (_fun _cursor*/null -> _void))

(define-sdl2 get-cursor (_fun -> _cursor*/null))

(define-sdl2 get-default-cursor (_fun -> _cursor*/null))

(define-sdl2 free-cursor! (_fun _cursor* -> _void))

(define-sdl2 show-cursor (_fun _int -> _int))

(define (button x)       (arithmetic-shift 1 (- x 1)))
(define button-left     1)
(define button-middle   2)
(define button-right    3)
(define button-x1       4)
(define button-x2       5)
(define button-lmask    (button button-left))
(define button-mmask    (button button-middle))
(define button-rmask    (button button-right))
(define button-x1mask   (button button-x1))
(define button-x2mask   (button button-x2))


;; SDL_joystick.h

(define-cpointer-type _joystick*)

(define-cstruct _joystick-guid
    ([data (_array _uint8 16)]))

(define _joystick-id _sint32)

(define _joystick-type
    (_enum
     '(unknown
       game-controller
       wheel
       arcade-stick
       flight-stick
       dance-pad
       guitar
       drum-kit
       arcade-pad
       throttle)))

(define _joystick-power-level
    (_enum
     '(unknown = -1
       empty
       low
       medium
       full
       wired
       max)))

(define-sdl2 lock-joysticks! (_fun -> _void)
    #:make-fail make-not-available)
(define-sdl2 unlock-joysticks! (_fun -> _void)
    #:make-fail make-not-available)

(define-sdl2 num-joysticks (_fun -> _int))

(define-sdl2 joystick-name-for-index (_fun _int -> _string))

(define-sdl2 joystick-get-device-player-index (_fun _int -> _int)
    #:make-fail make-not-available)

(define-sdl2 joystick-get-device-guid (_fun _int -> _joystick-guid))

(define-sdl2 joystick-get-device-vendor (_fun _int -> _uint16)
    #:make-fail make-not-available)

(define-sdl2 joystick-get-device-product (_fun _int -> _uint16)
    #:make-fail make-not-available)

(define-sdl2 joystick-get-device-product-version (_fun _int -> _uint16)
    #:make-fail make-not-available)

(define-sdl2 joystick-get-device-type (_fun _int -> _joystick-type)
    #:make-fail make-not-available)

(define-sdl2 joystick-get-device-instance-id (_fun _int -> _joystick-id)
    #:make-fail make-not-available)

(define-sdl2 joystick-open! (_fun _int -> _joystick*/null))

(define-sdl2 joystick-from-instance-id (_fun _joystick-id -> _joystick*/null)
    #:make-fail make-not-available)

(define-sdl2 joystick-name (_fun _joystick* -> _string))

(define-sdl2 joystick-get-player-index (_fun _joystick* -> _int)
    #:make-fail make-not-available)

(define-sdl2 joystick-get-guid (_fun _joystick* -> _joystick-guid))

(define-sdl2 joystick-get-vendor (_fun _joystick* -> _uint16)
    #:make-fail make-not-available)

(define-sdl2 joystick-get-product (_fun _joystick* -> _uint16)
    #:make-fail make-not-available)

(define-sdl2 joystick-get-product-version (_fun _joystick* -> _uint16)
    #:make-fail make-not-available)

(define-sdl2 joystick-get-type (_fun _joystick* -> _joystick-type)
    #:make-fail make-not-available)

(define-sdl2 joystick-get-guid-string (_fun _joystick-guid _string _int -> _void))

(define-sdl2 joystick-get-guid-from-string (_fun _string -> _joystick-guid))

(define-sdl2 joystick-get-attached (_fun _joystick* -> _bool))

(define-sdl2 joystick-instance-id (_fun _joystick* -> _joystick-id))

(define-sdl2 joystick-num-axes (_fun _joystick* -> _int))

(define-sdl2 joystick-num-balls (_fun _joystick* -> _int))

(define-sdl2 joystick-num-hats (_fun _joystick* -> _int))

(define-sdl2 joystick-num-buttons (_fun _joystick* -> _int))

(define-sdl2 joystick-update! (_fun -> _void))

(define-sdl2 joystick-event-state (_fun _int -> _int))

(define joystick-axis-max   32767)
(define joystick-axis-min   -32768)

(define-sdl2 joystick-get-axis (_fun _joystick* _int -> _sint16))

(define-sdl2 joystick-get-axis-initial-state (_fun _joystick* _int _sint16 -> _bool)
    #:make-fail make-not-available)

(define hat-centered    #x00)
(define hat-up          #x01)
(define hat-right       #x02)
(define hat-down        #x04)
(define hat-left        #x08)
(define hat-right-up     (bitwise-ior hat-right hat-up))
(define hat-right-down   (bitwise-ior hat-right hat-down))
(define hat-left-up      (bitwise-ior hat-left hat-up))
(define hat-left-down    (bitwise-ior hat-left hat-down))

(define-sdl2 joystick-get-hat (_fun _joystick* _int -> _uint8))

(define-sdl2 joystick-get-ball (_fun _joystick* _int _int* _int* -> _int))

(define-sdl2 joystick-get-button (_fun _joystick* _int -> _uint8))

(define-sdl2 joystick-rumble! (_fun _joystick* _uint16 _uint16 _uint32 -> _int)
    #:make-fail make-not-available)

(define-sdl2 joystick-close! (_fun _joystick* -> _void))

(define-sdl2 joystick-current-power-level (_fun _joystick* -> _joystick-power-level)
    #:make-fail make-not-available)


;; SDL_touch.h

(define _touch-id _sint64)
(define _finger-id _sint64)

(define-cstruct _finger
    ([id _finger-id]
     [x _float]
     [y _float]
     [pressure _float]))

(define _finger* _finger-pointer)
(define _finger*/null _finger-pointer/null)

(define touch-mouse-id #xffffffff)

(define-sdl2 get-num-touch-devices (_fun -> _int))

(define-sdl2 get-touch-device (_fun _int -> _touch-id))

(define-sdl2 get-num-touch-fingers (_fun _touch-id -> _int))

(define-sdl2 get-touch-finger (_fun _touch-id _int -> _finger*/null))


;; SDL_gesture.h

(define _gesture-id _sint64)

(define-sdl2 record-gesture! (_fun _touch-id -> _int))

(define-sdl2 save-all-dollar-templates! (_fun _rw-ops* -> _int))

(define-sdl2 save-dollar-template! (_fun _gesture-id _rw-ops* -> _int))

(define-sdl2 load-dollar-templates (_fun _touch-id _rw-ops* -> _int))


;; SDL_events.h

(define released    0)
(define pressed 1)

(define _event-type
    (_enum
     '(first          = 0
       quit           = #x100

       app-terminating
       app-low-memory
       app-will-enterbackground
       app-did-enter-background
       app-will-enter-foreground
       app-did-enter-foreground

       display        = #x150

       window         = #x200
       sys-wm

       key-down        = #x300
       key-up
       text-editing
       text-input
       keymap-changed

       mouse-motion    = #x400
       mouse-button-down
       mouse-button-up
       mouse-wheel

       joy-axis-motion  = #x600
       joy-ball-motion
       joy-hat-motion
       joy-button-down
       joy-button-up
       joy-device-added
       joy-device-removed

       controller-axis-motion  = #x650
       controller-button-down
       controller-button-up
       controller-device-added
       controller-device-removed
       controller-device-remapped

       finger-down      = #x700
       finger-up
       finger-motion

       dollar-gesture   = #x800
       dollar-record
       multi-gesture

       clipboard-update = #x900

       drop-file        = #x1000
       drop-text
       drop-begin
       drop-complete

       audio-device-added = #x1100
       audio-device-removed

       sensor-update = #x1200

       render-targets-reset = #x2000
       render-device-reset

       user         = #x8000

       last         = #xffff)
     _uint32))

(define-cstruct _common-event
    ([type _uint32]
     [timestamp _uint32]))

(define-cstruct _display-event
    ([type _uint32]
     [timestamp _uint32]
     [display _uint32]
     [event _uint8]
     [padding1 _uint8]
     [padding2 _uint8]
     [padding3 _uint8]
     [data1 _sint32]))

(define-cstruct _window-event
    ([type _uint32]
     [timestamp _uint32]
     [window-id _uint32]
     [event _uint8]
     [padding1 _uint8]
     [padding2 _uint8]
     [padding3 _uint8]
     [data1 _sint32]
     [data2 _sint32]))

(define-cstruct _keyboard-event
    ([type _uint32]
     [timestamp _uint32]
     [window-id _uint32]
     [state _uint8]
     [repeat _uint8]
     [padding2 _uint8]
     [padding3 _uint8]
     [keysym _keysym]))

(define text-editing-event-text-size 32)

(define-cstruct _text-editing-event
    ([type _uint32]
     [timestamp _uint32]
     [window-id _uint32]
     [text (_array _byte text-editing-event-text-size)]
     [start _sint32]
     [length _sint32]))

(define text-input-event-text-size 32)

(define-cstruct _text-input-event
    ([type _uint32]
     [timestamp _uint32]
     [window-id _uint32]
     [text (_array _byte text-input-event-text-size)]))

(define-cstruct _mouse-motion-event
    ([type _uint32]
     [timestamp _uint32]
     [window-id _uint32]
     [which _uint32]
     [state _uint32]
     [x _sint32]
     [y _sint32]
     [xrel _sint32]
     [yrel _sint32]))

(define-cstruct _mouse-button-event
    ([type _uint32]
     [timestamp _uint32]
     [window-id _uint32]
     [which _uint32]
     [button _uint8]
     [state _uint8]
     [clicks _uint8]
     [padding1 _uint8]
     [x _sint32]
     [y _sint32]))

(define-cstruct _mouse-wheel-event
    ([type _uint32]
     [timestamp _uint32]
     [window-id _uint32]
     [which _uint32]
     [x _sint32]
     [y _sint32]
     [direction _uint32]))

(define-cstruct _joy-axis-event
    ([type _uint32]
     [timestamp _uint32]
     [which _joystick-id]
     [axis _uint8]
     [padding1 _uint8]
     [padding2 _uint8]
     [padding3 _uint8]
     [value _sint16]
     [padding4 _uint16]))

(define-cstruct _joy-ball-event
    ([type _uint32]
     [timestamp _uint32]
     [which _joystick-id]
     [ball _uint8]
     [padding1 _uint8]
     [padding2 _uint8]
     [padding3 _uint8]
     [xrel _sint16]
     [yrel _sint16]))

(define-cstruct _joy-hat-event
    ([type _uint32]
     [timestamp _uint32]
     [which _joystick-id]
     [hat _uint8]
     [value _uint8]
     [padding1 _uint8]
     [padding2 _uint8]))

(define-cstruct _joy-button-event
    ([type _uint32]
     [timestamp _uint32]
     [which _joystick-id]
     [button _uint8]
     [state _uint8]
     [padding1 _uint8]
     [padding2 _uint8]))

(define-cstruct _joy-device-event
    ([type _uint32]
     [timestamp _uint32]
     [which _sint32]))

(define-cstruct _controller-axis-event
    ([type _uint32]
     [timestamp _uint32]
     [which _joystick-id]
     [axis _uint8]
     [padding1 _uint8]
     [padding2 _uint8]
     [padding3 _uint8]
     [value _sint16]
     [padding4 _uint16]))

(define-cstruct _controller-button-event
    ([type _uint32]
     [timestamp _uint32]
     [which _joystick-id]
     [button _uint8]
     [state _uint8]
     [padding1 _uint8]
     [padding2 _uint8]))

(define-cstruct _controller-device-event
    ([type _uint32]
     [timestamp _uint32]
     [which _sint32]))

(define-cstruct _audio-device-event
    ([type _uint32]
     [timestamp _uint32]
     [which _uint32]
     [iscapture _uint8]
     [padding1 _uint8]
     [padding2 _uint8]
     [padding3 _uint8]))

(define-cstruct _touch-finger-event
    ([type _uint32]
     [timestamp _uint32]
     [touch-id _touch-id]
     [finger-id _finger-id]
     [x _float]
     [y _float]
     [dx _float]
     [dy _float]
     [pressure _float]))

(define-cstruct _multi-gesture-event
    ([type _uint32]
     [timestamp _uint32]
     [touch-id _touch-id]
     [d-theta _float]
     [d-dist _float]
     [x _float]
     [y _float]
     [num-fingers _uint16]
     [padding _uint16]))

(define-cstruct _dollar-gesture-event
    ([type _uint32]
     [timestamp _uint32]
     [touch-id _touch-id]
     [gesture-id _gesture-id]
     [num-fingers _uint32]
     [error _float]
     [x _float]
     [y _float]))

(define-cstruct _drop-event
    ([type _uint32]
     [timestamp _uint32]
     [file _string]
     [window-id _uint32]))

(define-cstruct _sensor-event
    ([type _uint32]
     [timestamp _uint32]
     [which _sint32]
     [data (_array _float 6)]))

(define-cstruct _quit-event
    ([type _uint32]
     [timestamp _uint32]))

(define-cstruct _os-event
    ([type _uint32]
     [timestamp _uint32]))

(define-cstruct _user-event
    ([type _uint32]
     [timestamp _uint32]
     [window-id _uint32]
     [code _sint32]
     [data1 _pointer]
     [data2 _pointer]))

(define-cpointer-type _sys-wm-msg*)

(define-cstruct _sys-wm-event
    ([type _uint32]
     [timestamp _uint32]
     [msg _sys-wm-msg*]))

(define _event
    (_union
     _event-type
     _common-event
     _display-event
     _window-event
     _keyboard-event
     _text-editing-event
     _text-input-event
     _mouse-motion-event
     _mouse-button-event
     _mouse-wheel-event
     _joy-axis-event
     _joy-ball-event
     _joy-hat-event
     _joy-button-event
     _joy-device-event
     _controller-axis-event
     _controller-button-event
     _controller-device-event
     _audio-device-event
     _sensor-event
     _quit-event
     _user-event
     _sys-wm-event
     _touch-finger-event
     _multi-gesture-event
     _dollar-gesture-event
     _drop-event
     (_array _uint8 56)))

(define-cpointer-type _event*)

(define-sdl2 pump-events! (_fun -> _void))

(define _event-action
    (_enum
     '(add-event
       peek-event
       get-event)))

(define-sdl2 peep-events
    (_fun _event*/null _int _event-action _event-type _event-type -> _int))

(define-sdl2 has-event (_fun _event-type -> _bool))
(define-sdl2 has-events (_fun _event-type _event-type -> _bool))

(define-sdl2 flush-event! (_fun _event-type -> _void))
(define-sdl2 flush-events! (_fun _event-type _event-type -> _void))

(define-sdl2 poll-event! (_fun _event*/null -> _int))

(define-sdl2 wait-event! (_fun _event*/null -> _int))

(define-sdl2 wait-event-timeout! (_fun _event*/null _int -> _int))

(define-sdl2 push-event! (_fun _event* -> _int))

(define _event-filter (_fun _pointer _event* -> _int))

(define-sdl2 set-event-filter! (_fun _event-filter _pointer -> _void))

(define-sdl2 get-event-filter (_fun (_ptr o _event-filter) (_ptr o _pointer) -> _bool))

(define-sdl2 add-event-watch (_fun _event-filter _pointer -> _void))

(define-sdl2 del-event-watch (_fun _event-filter _pointer -> _void))

(define-sdl2 filter-events (_fun _event-filter _pointer -> _void))

(define query   -1)
(define ignore   0)
(define disable  0)
(define enable   1)

(define-sdl2 event-state (_fun _event-type _int -> _uint8))

(define (get-event-state type) (event-state type query))

(define-sdl2 register-events! (_fun _int -> _uint32))


;; SDL_syswm.h

(define-cpointer-type _sys-wm-info*)

(define-sdl2 get-window-wm-info (_fun _window* _sys-wm-info* -> _bool))


;; SDL_quit.h

(define (quit-requested?)
    (pump-events!)
    (> (peep-events #f 0 'peek-event 'quit 'quit) 0))


;; SDL_filesystem.h

(define-sdl2 get-base-path (_fun -> _string)
    #:make-fail make-not-available)

(define-sdl2 get-pref-path (_fun _string _string -> _string)
    #:make-fail make-not-available)


;; SDL_gamecontroller.h

(define-cpointer-type _game-controller*)

(define _game-controller-bind-type
    (_enum
     '(none = 0
       button
       axis
       hat)))

(define-cstruct _game-controller-button-bind
    ([bindType _game-controller-bind-type]
     [value
      (_union
       _int
       _int
       (make-cstruct-type (list _int _int)))]))

(define-sdl2 game-controller-add-mappings-from-rw (_fun _rw-ops* _int -> _int)
    #:make-fail make-not-available)

(define (game-controller-add-mappings-from-file file)
    (game-controller-add-mappings-from-rw
     (rw-from-file file "rb")
     1))

(define-sdl2 game-controller-add-mapping (_fun _string -> _int))

(define-sdl2 game-controller-num-mappings (_fun -> _int)
    #:make-fail make-not-available)

(define-sdl2 game-controller-mapping-for-index (_fun _int -> _string)
    #:make-fail make-not-available)

(define-sdl2 game-controller-mapping-for-guid (_fun _joystick-guid -> _string))

(define-sdl2 game-controller-mapping (_fun _game-controller* -> _string))

(define-sdl2 game-controller? (_fun _int -> _bool))

(define-sdl2 game-controller-name-for-index (_fun _int -> _string))

(define-sdl2 game-controller-mapping-for-device-index (_fun _int -> _string)
    #:make-fail make-not-available)

(define-sdl2 game-controller-open (_fun _int -> _game-controller*/null))

(define-sdl2 game-controller-from-instance-id (_fun _joystick-id -> _game-controller*/null)
    #:make-fail make-not-available)

(define-sdl2 game-controller-name (_fun _game-controller* -> _string))

(define-sdl2 game-controller-get-player-index (_fun _game-controller* -> _int)
    #:make-fail make-not-available)

(define-sdl2 game-controller-get-vendor (_fun _game-controller* -> _uint16)
    #:make-fail make-not-available)

(define-sdl2 game-controller-get-product (_fun _game-controller* -> _uint16)
    #:make-fail make-not-available)

(define-sdl2 game-controller-get-product-version (_fun _game-controller* -> _uint16)
    #:make-fail make-not-available)

(define-sdl2 game-controller-get-attached (_fun _game-controller* -> _bool))

(define-sdl2 game-controller-get-joystick (_fun _game-controller* -> _joystick*/null))

(define-sdl2 game-controller-event-state (_fun _int -> _int))

(define-sdl2 game-controller-update (_fun -> _void))

(define _game-controller-axis
    (_enum
     '(invalid = -1
       left-x
       left-y
       right-x
       right-y
       trigger-left
       trigger-right
       max)))

(define-sdl2 game-controller-get-axis-from-string (_fun _string -> _game-controller-axis))

(define-sdl2 game-controller-get-string-for-axis (_fun _game-controller-axis -> _string))

(define-sdl2 game-controller-get-bind-for-axis
    (_fun _game-controller* _game-controller-axis -> _game-controller-button-bind))

(define-sdl2 game-controller-get-axis (_fun _game-controller* _game-controller-axis -> _sint16))

(define _game-controller-button
    (_enum
     '(invalid = -1
       a
       b
       x
       y
       back
       guide
       start
       left-stick
       right-stick
       left-shoulder
       right-shoulder
       dpad-up
       dpad-down
       dpad-left
       dpad-right
       max)))

(define-sdl2 game-controller-get-button-from-string (_fun _string -> _game-controller-button))

(define-sdl2 game-controller-get-string-for-button (_fun _game-controller-button -> _string))

(define-sdl2 game-controller-get-bind-for-button
    (_fun _game-controller* _game-controller-button -> _game-controller-button-bind))

(define-sdl2 game-controller-get-button
    (_fun _game-controller* _game-controller-button -> _uint8))

(define-sdl2 game-controller-rumble (_fun _game-controller* _uint16 _uint16 _uint32 -> _int)
    #:make-fail make-not-available)

(define-sdl2 game-controller-close (_fun _game-controller* -> _void))


;; SDL_haptic.h

(define-cpointer-type _haptic*)

(define haptic_constant   (arithmetic-shift 1 0))

(define haptic_sine       (arithmetic-shift 1 1))

(define haptic_leftright  (arithmetic-shift 1 2))

(define haptic_triangle   (arithmetic-shift 1 3))

(define haptic_sawtoothup (arithmetic-shift 1 4))

(define haptic_sawtoothdown (arithmetic-shift 1 5))

(define haptic_ramp       (arithmetic-shift 1 6))

(define haptic_spring     (arithmetic-shift 1 7))

(define haptic_damper     (arithmetic-shift 1 8))

(define haptic_inertia    (arithmetic-shift 1 9))

(define haptic_friction   (arithmetic-shift 1 10))

(define haptic_custom     (arithmetic-shift 1 11))

(define haptic_gain       (arithmetic-shift 1 12))

(define haptic_autocenter (arithmetic-shift 1 13))

(define haptic_status     (arithmetic-shift 1 14))

(define haptic_pause      (arithmetic-shift 1 15))

(define haptic_polar      0)

(define haptic_cartesian  1)

(define haptic_spherical  2)

(define haptic_infinity   4294967295)

(define-cstruct _haptic-direction
    ([type _uint8]
     [dir (_array _sint32 3)]))

(define-cstruct _haptic-constant
    ([type _uint16]
     [direction _haptic-direction]
     [length _uint32]
     [delay _uint16]
     [button _uint16]
     [interval _uint16]
     [level _sint16]
     [attack-length _uint16]
     [attack-level _uint16]
     [fade-length _uint16]
     [fade-level _uint16]))

(define-cstruct _haptic-periodic
    ([type _uint16]
     [direction _haptic-direction]
     [length _uint32]
     [delay _uint16]
     [button _uint16]
     [interval _uint16]
     [period _uint16]
     [magnitude _sint16]
     [offset _sint16]
     [phase _uint16]
     [attack-length _uint16]
     [attack-level _uint16]
     [fade-length _uint16]
     [fade-level _uint16]))

(define-cstruct _haptic-condition
    ([type _uint16]
     [direction _haptic-direction]
     [length _uint32]
     [delay _uint16]
     [button _uint16]
     [interval _uint16]
     [right-sat (_array _uint16 3)]
     [left-sat (_array _uint16 3)]
     [right-coeff (_array _sint16 3)]
     [left-coeff (_array _sint16 3)]
     [deadband (_array _uint16 3)]
     [center (_array _sint16 3)]))

(define-cstruct _haptic-ramp
    ([type _uint16]
     [direction _haptic-direction]
     [length _uint32]
     [delay _uint16]
     [button _uint16]
     [interval _uint16]
     [start _sint16]
     [end _sint16]
     [attack-length _uint16]
     [attack-level _uint16]
     [fade-length _uint16]
     [fade-level _uint16]))

(define-cstruct _haptic-left-right
    ([type _uint16]
     [length _uint32]
     [large-magnitude _uint16]
     [small-magnitude _uint16]))

(define-cstruct _haptic-custom
    ([type _uint16]
     [direction _haptic-direction]
     [length _uint32]
     [delay _uint16]
     [button _uint16]
     [interval _uint16]
     [channels _uint8]
     [period _uint16]
     [samples _uint16]
     [data _uint16*]
     [attack-length _uint16]
     [attack-level _uint16]
     [fade-length _uint16]
     [fade-level _uint16]))

(define _haptic-effect
    (_union
     _uint16
     _haptic-constant
     _haptic-periodic
     _haptic-condition
     _haptic-ramp
     _haptic-left-right
     _haptic-custom))

(define-cpointer-type _haptic-effect*)

(define-sdl2 num-haptics (_fun -> _int))

(define-sdl2 haptic-name (_fun _int -> _string))

(define-sdl2 haptic-open! (_fun _int -> _haptic*/null))

(define-sdl2 haptic-opened (_fun _int -> _int))

(define-sdl2 haptic-index (_fun _haptic* -> _int))

(define-sdl2 mouse-is-haptic (_fun -> _int))

(define-sdl2 haptic-open-from-mouse! (_fun -> _haptic*/null))

(define-sdl2 joystick-is-haptic (_fun _joystick* -> _int))

(define-sdl2 haptic-open-from-joystick! (_fun _joystick* -> _haptic*/null))

(define-sdl2 haptic-close! (_fun _haptic* -> _void))

(define-sdl2 haptic-num-effects (_fun _haptic* -> _int))

(define-sdl2 haptic-num-effects-playing (_fun _haptic* -> _int))

(define-sdl2 haptic-query (_fun _haptic* -> _int))

(define-sdl2 haptic-num-axes (_fun _haptic* -> _int))

(define-sdl2 haptic-effect-supported (_fun _haptic* _haptic-effect* -> _int))

(define-sdl2 haptic-new-effect! (_fun _haptic* _haptic-effect* -> _int))

(define-sdl2 haptic-update-effect! (_fun _haptic* _int _haptic-effect* -> _int))

(define-sdl2 haptic-run-effect! (_fun _haptic* _int _uint32 -> _int))

(define-sdl2 haptic-stop-effect! (_fun _haptic* _int -> _int))

(define-sdl2 haptic-destroy-effect! (_fun _haptic* _int -> _void))

(define-sdl2 haptic-get-effect-status (_fun _haptic* _int -> _int))

(define-sdl2 haptic-set-gain! (_fun _haptic* _int -> _int))

(define-sdl2 haptic-set-autocenter! (_fun _haptic* _int -> _int))

(define-sdl2 haptic-pause! (_fun _haptic* -> _int))

(define-sdl2 haptic-unpause! (_fun _haptic* -> _int))

(define-sdl2 haptic-stop-all! (_fun _haptic* -> _int))

(define-sdl2 haptic-rumble-supported (_fun _haptic* -> _int))

(define-sdl2 haptic-rumble-init! (_fun _haptic* -> _int))

(define-sdl2 haptic-rumble-play! (_fun _haptic* _float _uint32 -> _int))

(define-sdl2 haptic-rumble-stop! (_fun _haptic* -> _int))


;; SDL_hints.h

(define framebuffer-acceleration   "FRAMEBUFFER_ACCELERATION")

(define render-driver              "RENDER_DRIVER")

(define render-opengl-shaders      "RENDER_OPENGL_SHADERS")

(define render-direct3d-threadsafe "RENDER_DIRECT3D_THREADSAFE")

(define render-direct3d11-debug    "RENDER_DIRECT3D11_DEBUG")

(define render-logical-size-mode       "RENDER_LOGICAL_SIZE_MODE")

(define render-scale-quality       "RENDER_SCALE_QUALITY")

(define render-vsync               "RENDER_VSYNC")

(define video-allow-screensaver    "VIDEO_ALLOW_SCREENSAVER")

(define video-x11-xvidmode         "VIDEO_X11_XVIDMODE")

(define video-x11-xinerama         "VIDEO_X11_XINERAMA")

(define video-x11-xrandr           "VIDEO_X11_XRANDR")

(define video-x11-net-wm-ping      "VIDEO_X11_NET_WM_PING")

(define video-x11-net-wm-bypass-compositor "VIDEO_X11_NET_WM_BYPASS_COMPOSITOR")

(define window-frame-usable-while-cursor-hidden
    "WINDOW_FRAME_USABLE_WHILE_CURSOR_HIDDEN")

(define windows-intresource-icon       "WINDOWS_INTRESOURCE_ICON")
(define windows-intresource-icon-small "WINDOWS_INTRESOURCE_ICON_SMALL")

(define windows-enable-messageloop "WINDOWS_ENABLE_MESSAGELOOP")

(define grab-keyboard              "GRAB_KEYBOARD")

(define mouse-double-click-time    "MOUSE_DOUBLE_CLICK_TIME")

(define mouse-double-click-radius    "MOUSE_DOUBLE_CLICK_RADIUS")

(define mouse-normal-speed-scale    "MOUSE_NORMAL_SPEED_SCALE")

(define mouse-relative-speed-scale    "MOUSE_RELATIVE_SPEED_SCALE")

(define mouse-relative-mode-warp    "MOUSE_RELATIVE_MODE_WARP")

(define mouse-focus-clickthrough "MOUSE_FOCUS_CLICKTHROUGH")

(define touch-mouse-events    "TOUCH_MOUSE_EVENTS")

(define video-minimize-on-focus-loss   "VIDEO_MINIMIZE_ON_FOCUS_LOSS")

(define idle-timer-disabled "IOS_IDLE_TIMER_DISABLED")

(define orientations "IOS_ORIENTATIONS")

(define apple-tv-controller-ui-events "APPLE_TV_CONTROLLER_UI_EVENTS")

(define apple-tv-remote-allow-rotation "APPLE_TV_REMOTE_ALLOW_ROTATION")

(define ios-hide-home-indicator "IOS_HIDE_HOME_INDICATOR")

(define accelerometer-as-joystick "ACCELEROMETER_AS_JOYSTICK")

(define tv-remote-as-joystick "TV_REMOTE_AS_JOYSTICK")

(define xinput-enabled "XINPUT_ENABLED")

(define xinput-use-old-joystick-mapping "XINPUT_USE_OLD_JOYSTICK_MAPPING")

(define gamecontrollerconfig "GAMECONTROLLERCONFIG")

(define gamecontroller-ignore-devices "GAMECONTROLLER_IGNORE_DEVICES")

(define gamecontroller-ignore-devices-except "GAMECONTROLLER_IGNORE_DEVICES_EXCEPT")

(define joystick-allow-background-events "JOYSTICK_ALLOW_BACKGROUND_EVENTS")

(define joystick-hidapi "JOYSTICK_HIDAPI")

(define joystick-hidapi-ps4 "JOYSTICK_HIDAPI_PS4")

(define joystick-hidapi-ps4-rumble "JOYSTICK_HIDAPI_PS4_RUMBLE")

(define joystick-hidapi-steam "JOYSTICK_HIDAPI_STEAM")

(define joystick-hidapi-switch "JOYSTICK_HIDAPI_SWITCH")

(define joystick-hidapi-xbox   "JOYSTICK_HIDAPI_XBOX")

(define enable-steam-controllers "ENABLE_STEAM_CONTROLLERS")

(define allow-topmost "ALLOW_TOPMOST")

(define timer-resolution "TIMER_RESOLUTION")

(define qtwayland-content-orientation "QTWAYLAND_CONTENT_ORIENTATION")

(define qtwayland-window-flags "QTWAYLAND_WINDOW_FLAGS")

(define thread-stack-size              "THREAD_STACK_SIZE")

(define video-highdpi-disabled "VIDEO_HIGHDPI_DISABLED")

(define mac-ctrl-click-emulate-right-click "MAC_CTRL_CLICK_EMULATE_RIGHT_CLICK")

(define video-win-d3-dcompiler              "VIDEO_WIN_D3DCOMPILER")

(define video-window-share-pixel-format    "VIDEO_WINDOW_SHARE_PIXEL_FORMAT")

(define winrt-privacy-policy-url "WINRT_PRIVACY_POLICY_URL")

(define winrt-privacy-policy-label "WINRT_PRIVACY_POLICY_LABEL")

(define winrt-handle-back-button "WINRT_HANDLE_BACK_BUTTON")

(define video-mac-fullscreen-spaces    "VIDEO_MAC_FULLSCREEN_SPACES")

(define mac-background-app    "MAC_BACKGROUND_APP")

(define android-apk-expansion-main-file-version "ANDROID_APK_EXPANSION_MAIN_FILE_VERSION")

(define android-apk-expansion-patch-file-version
    "ANDROID_APK_EXPANSION_PATCH_FILE_VERSION")

(define ime-internal-editing "IME_INTERNAL_EDITING")

(define android-separate-mouse-and-touch "ANDROID_SEPARATE_MOUSE_AND_TOUCH")

(define android-trap-back-button "ANDROID_TRAP_BACK_BUTTON")

(define return-key-hides-ime "RETURN_KEY_HIDES_IME")

(define emscripten-keyboard-element   "EMSCRIPTEN_KEYBOARD_ELEMENT")

(define no-signal-handlers   "NO_SIGNAL_HANDLERS")

(define windows-no-close-on-alt-f4 "WINDOWS_NO_CLOSE_ON_ALT_F4")

(define bmp-save-legacy-format "BMP_SAVE_LEGACY_FORMAT")

(define windows-disable-thread-naming "WINDOWS_DISABLE_THREAD_NAMING")

(define rpi-video-layer           "RPI_VIDEO_LAYER")

(define video-double-buffer      "VIDEO_DOUBLE_BUFFER")

(define opengl-es-driver   "OPENGL_ES_DRIVER")

(define audio-resampling-mode   "AUDIO_RESAMPLING_MODE")

(define audio-category   "AUDIO_CATEGORY")

(define _hint-priority
    (_enum
     '(default
       normal
       override)))

(define-sdl2 set-hint-with-priority! (_fun _string _string _hint-priority -> _bool))

(define-sdl2 set-hint! (_fun _string _string -> _bool))

(define-sdl2 get-hint (_fun _string -> _string))

(define-sdl2 get-hint-boolean (_fun _string _bool -> _bool)
    #:make-fail make-not-available)

(define _hint-callback (_fun _pointer _string _string _string -> _void))

(define-sdl2 add-hint-callback! (_fun _string _hint-callback _pointer -> _void))

(define-sdl2 del-hint-callback! (_fun _string _hint-callback _pointer -> _void))

(define-sdl2 clear-hints! (_fun -> _void))


;; SDL_loadso.h

(define-sdl2 load-object (_fun _string -> _pointer))

(define-sdl2 load-function (_fun _pointer _string -> _pointer))

(define-sdl2 unload-object (_fun _pointer -> _void))


;; SDL_log.h

(define max-log-message 4096)

(define application 0)
(define error 1)
(define assert 2)
(define system 3)
(define audio 4)
(define video 5)
(define render 6)
(define input 7)
(define test 8)

(define reserved1 9)
(define reserved2 10)
(define reserved3 11)
(define reserved4 12)
(define reserved5 13)
(define reserved6 14)
(define reserved7 15)
(define reserved8 16)
(define reserved9 17)
(define reserved10 18)

(define custom 19)

(define _log-priority
    (_enum
     '(verbose = 1
       debug
       info
       warn
       error
       critical
       num-log-priorities)))

(define-sdl2 log-set-all-priority! (_fun _log-priority -> _void))

(define-sdl2 log-set-priority! (_fun _int _log-priority -> _void))

(define-sdl2 log-get-priority (_fun _int -> _log-priority))

(define-sdl2 log-reset-priorities! (_fun -> _void))

(define-sdl2-vararg log* SDL_Log (_string) _void)

(define-sdl2-vararg log-verbose SDL_LogVerbose (_int _string) _void)

(define-sdl2-vararg log-debug SDL_LogDebug (_int _string) _void)

(define-sdl2-vararg log-info SDL_LogInfo (_int _string) _void)

(define-sdl2-vararg log-warn SDL_LogWarn (_int _string) _void)

(define-sdl2-vararg log-error SDL_LogError (_int _string) _void)

(define-sdl2-vararg log-critical SDL_LogCritical (_int _string) _void)

(define-sdl2-vararg log-message SDL_LogMessage (_int _log-priority _string) _void)

(define _log-output-function (_fun #:async-apply (lambda (thunk) (thunk)) _pointer _int _log-priority _string -> _void))

(define-sdl2 log-get-output-function
    (_fun (callback : (_ptr o _log-output-function)) (userdata : (_ptr o _pointer))
     -> _void
     -> (values callback userdata)))

(define-sdl2 log-set-output-function! (_fun _log-output-function _pointer -> _void))


;; SDL_main.h

(define-sdl2 set-main-ready! (_fun -> _void))

(define-sdl2 register-app (_fun _string _uint32 _pointer -> _int)
    #:make-fail make-not-available)

(define-sdl2 unregister-app (_fun -> _void)
    #:make-fail make-not-available)

(define-sdl2 win-rt-run-app (_fun (_fun _int (_ptr i _string) -> _int) _pointer -> _int)
    #:make-fail make-not-available)


;; SDL_messagebox.h

(define _message-box-flags
    (_enum
     '(error        = #x00000010
       warning      = #x00000020
       information  = #x00000040)
     _uint32))

(define _message-box-button-flags
    (_enum
     '(returnkey-default = #x00000001
       escapekey-default = #x00000002)
     _uint32))

(define-cstruct _message-box-button-data
    ([flags _message-box-button-flags]
     [button-id _int]
     [text _string]))
(define _message-box-button-data* _message-box-button-data-pointer)

(define-cstruct _message-box-color
    ([r _uint8]
     [g _uint8]
     [b _uint8]))

(define _message-box-color-type
    (_enum
     '(background
       text
       button-border
       button-background
       button-selected
       max)))

(define-cstruct _message-box-color-scheme
    ([colors
      (_array _message-box-color
              (cast 'max _message-box-color-type _ufixint))]))
(define _message-box-color-scheme* _message-box-color-scheme-pointer)

(define-cstruct _message-box-data
    ([flags _message-box-flags]
     [window _window*]
     [title _string]
     [message _string]
     [num-buttons _int]
     [buttons _message-box-button-data*]
     [color-scheme _message-box-color-scheme*]))

(define _message-box-data* _message-box-data-pointer)

(define-sdl2 show-message-box (_fun _message-box-data* _int* -> _int))

(define-sdl2 show-simple-message-box (_fun _message-box-flags _string _string _window*/null -> _int))


;; SDL_mutex.h

(define timed-out  1)

(define max-wait   (cast -1 _int _uint32))

(define-cpointer-type _mutex*)

(define-sdl2 create-mutex (_fun -> _mutex*/null))

(define-sdl2 lock-mutex (_fun _mutex* -> _int))
(define mutex-p lock-mutex)

(define-sdl2 try-lock-mutex (_fun _mutex* -> _int))

(define-sdl2 unlock-mutex (_fun _mutex* -> _int))
(define mutex-v unlock-mutex)

(define-sdl2 destroy-mutex (_fun _mutex* -> _void))

(define-cpointer-type _sem*)

(define-sdl2 create-semaphore (_fun _uint32 -> _sem*/null))

(define-sdl2 destroy-semaphore (_fun _sem* -> _void))

(define-sdl2 sem-wait (_fun _sem* -> _int))

(define-sdl2 sem-try-wait (_fun _sem* -> _int))

(define-sdl2 sem-wait-timeout (_fun _sem* _uint32 -> _int))

(define-sdl2 sem-post (_fun _sem* -> _int))

(define-sdl2 sem-value (_fun _sem* -> _uint32))

(define-cpointer-type _cond*)

(define-sdl2 create-cond (_fun -> _cond*/null))

(define-sdl2 destroy-cond (_fun _cond* -> _void))

(define-sdl2 cond-signal (_fun _cond* -> _int))

(define-sdl2 cond-broadcast (_fun _cond* -> _int))

(define-sdl2 cond-wait (_fun _cond* _mutex* -> _int))

(define-sdl2 cond-wait-timeout (_fun _cond* _mutex* _uint32 -> _int))


;; SDL_power.h

(define _power-state
    (_enum
     '(unknown
       on-battery
       no-battery
       charging
       charged)))

(define-sdl2 get-power-info (_fun _int*/null _int*/null -> _power-state))


;; SDL_render.h

(define _renderer-flags
    (_bitmask
     '(software = #x00000001
       accelerated = #x00000002
       present-vsync = #x00000004
       target-texture = #x00000008)
     _uint32))

(define-cstruct _renderer-info
    ([name _string]
     [flags _uint32]
     [num-texture-formats _uint32]
     [texture-formats (_array _uint32 16)]
     [max-texture-width _int]
     [max-texture-height _int]))

(define _renderer-info* _renderer-info-pointer)

(define _texture-access
    (_enum
     '(static
       streaming
       target)
     _int))

(define _texture-modulate
    (_enum
     '(none = #x00000000
       color = #x00000001
       alpha = #x00000002)))

(define _renderer-flip
    (_enum
     '(none = #x00000000
       horizontal = #x00000001
       vertical = #x00000002)))

(define-cpointer-type _renderer*)

(define-cpointer-type _texture*)

(define-sdl2 get-num-render-drivers (_fun -> _int))

(define-sdl2 get-render-driver-info (_fun _int _renderer-info* -> _int))

(define-sdl2 create-window-and-renderer!
    (_fun _int _int _window-flags (_ptr o _window*) (_ptr o _renderer*) -> _int))

(define-sdl2 create-renderer! (_fun _window* _int _renderer-flags -> _renderer*/null))

(define-sdl2 create-software-renderer! (_fun _surface* -> _renderer*/null))

(define-sdl2 get-renderer (_fun _window* -> _renderer*/null))

(define-sdl2 get-renderer-info (_fun _renderer* _renderer-info* -> _int))

(define-sdl2 get-renderer-output-size (_fun _renderer* _int*/null _int*/null -> _int))

(define-sdl2 create-texture
    (_fun _renderer* _uint32 _texture-access _int _int -> _texture*/null))

(define-sdl2 create-texture-from-surface (_fun _renderer* _surface* -> _texture*/null))

(define-sdl2 query-texture
    (_fun _texture* _uint32*/null _int*/null _int*/null _int*/null -> _int))

(define-sdl2 set-texture-color-mod! (_fun _texture* _uint8 _uint8 _uint8 -> _int))

(define-sdl2 get-texture-color-mod
    (_fun _texture* _uint8*/null _uint8*/null _uint8*/null -> _int))

(define-sdl2 set-texture-alpha-mod! (_fun _texture* _uint8 -> _int))

(define-sdl2 get-texture-alpha-mod (_fun _texture* _uint8* -> _int))

(define-sdl2 set-texture-blend-mode! (_fun _texture* _blend-mode -> _int))

(define-sdl2 get-texture-blend-mode (_fun _texture* _blend-mode* -> _int))

(define-sdl2 update-texture! (_fun _texture* _rect*/null _pointer _int -> _int))

(define-sdl2 update-yuv-texture!
    (_fun _texture* _rect*/null _uint8* _int _uint8* _int _uint8* _int -> _int)
    #:make-fail make-not-available)

(define-sdl2 lock-texture! (_fun _texture* _rect*/null (_ptr o _pointer) _int* -> _int))

(define-sdl2 unlock-texture! (_fun _texture* -> _void))

(define-sdl2 render-target-supported (_fun _renderer* -> _bool))

(define-sdl2 set-render-target! (_fun _renderer* _texture*/null -> _int))

(define-sdl2 get-render-target (_fun _renderer* -> _texture*/null))

(define-sdl2 render-set-logical-size! (_fun _renderer* _int _int -> _int))

(define-sdl2 render-get-logical-size (_fun _renderer* _int*/null _int*/null -> _void))

(define-sdl2 render-set-integer-scale! (_fun _renderer* _bool -> _int)
    #:make-fail make-not-available)

(define-sdl2 render-get-integer-scale (_fun _renderer* -> _bool)
    #:make-fail make-not-available)

(define-sdl2 render-set-viewport! (_fun _renderer* _rect*/null -> _int))

(define-sdl2 render-get-viewport (_fun _renderer* _rect* -> _void))

(define-sdl2 render-set-clip-rect! (_fun _renderer* _rect*/null -> _int))

(define-sdl2 render-get-clip-rect (_fun _renderer* _rect* -> _void))

(define-sdl2 render-is-clip-enabled (_fun _renderer* -> _bool)
    #:make-fail make-not-available)

(define-sdl2 render-set-scale! (_fun _renderer* _float _float -> _int))

(define-sdl2 render-get-scale (_fun _renderer* _float*/null _float*/null -> _void))

(define-sdl2 set-render-draw-color! (_fun _renderer* _uint8 _uint8 _uint8 _uint8 -> _int))

(define-sdl2 get-render-draw-color
    (_fun _renderer* _uint8*/null _uint8*/null _uint8*/null _uint8*/null -> _int))

(define-sdl2 set-render-draw-blend-mode! (_fun _renderer* _blend-mode -> _int))

(define-sdl2 get-render-draw-blend-mode (_fun _renderer* _blend-mode* -> _int))

(define-sdl2 render-clear! (_fun _renderer* -> _int))

(define-sdl2 render-draw-point! (_fun _renderer* _int _int -> _int))

(define-sdl2 render-draw-points! (_fun _renderer* _point* _int -> _int))

(define-sdl2 render-draw-line! (_fun _renderer* _int _int _int _int -> _int))

(define-sdl2 render-draw-lines! (_fun _renderer* _point* _int -> _int))

(define-sdl2 render-draw-rect! (_fun _renderer* _rect*/null -> _int))

(define-sdl2 render-draw-rects! (_fun _renderer* _rect* _int -> _int))

(define-sdl2 render-fill-rect! (_fun _renderer* _rect*/null -> _int))

(define-sdl2 render-fill-rects! (_fun _renderer* _rect* _int -> _int))

(define-sdl2 render-copy!
    (_fun _renderer* _texture* _rect*/null _rect*/null -> _int))

(define-sdl2 render-copy-ex!
    (_fun
     _renderer*
     _texture*
     _rect*/null
     _rect*/null
     _double
     _point*/null
     _renderer-flip
     -> _int))

(define-sdl2 render-read-pixels (_fun _renderer* _rect*/null _uint32 _pointer _int -> _int))

(define-sdl2 render-present! (_fun _renderer* -> _void))

(define-sdl2 destroy-texture! (_fun _texture* -> _void))

(define-sdl2 destroy-renderer! (_fun _renderer* -> _void))

(define-sdl2 gl-bind-texture! (_fun _texture* _float*/null _float*/null -> _int))

(define-sdl2 gl-unbind-texture! (_fun _texture* -> _int))

(define-sdl2 render-get-metal-layer (_fun _renderer* -> _pointer)
    #:make-fail make-not-available)

(define-sdl2 render-get-metal-command-encoder (_fun _renderer* -> _pointer)
    #:make-fail make-not-available)


;; SDL_sensor.h

(define-cpointer-type _sensor*)

(define _sensor-id _sint32)

(define _sensor-type
    (_enum
     '(invalid = -1
       unknown
       accel
       gyro)))

(define standard-gravity    9.80665f0)

(define-sdl2 num-sensors (_fun -> _int)
    #:make-fail make-not-available)

(define-sdl2 sensor-get-device-name (_fun _int -> _string)
    #:make-fail make-not-available)

(define-sdl2 sensor-get-device-type (_fun _int -> _sensor-type)
    #:make-fail make-not-available)

(define-sdl2 sensor-get-device-instance-id (_fun _int -> _sensor-id)
    #:make-fail make-not-available)

(define-sdl2 sensor-open (_fun _int -> _sensor*/null)
    #:make-fail make-not-available)

(define-sdl2 sensor-from-instance-id (_fun _sensor-id -> _sensor*/null)
    #:make-fail make-not-available)

(define-sdl2 sensor-get-name (_fun _sensor* -> _string)
    #:make-fail make-not-available)

(define-sdl2 sensor-get-type (_fun _sensor* -> _sensor-type)
    #:make-fail make-not-available)

(define-sdl2 sensor-get-non-portable-type (_fun _sensor* -> _int)
    #:make-fail make-not-available)

(define-sdl2 sensor-get-instance-id (_fun _sensor* -> _sensor-id)
    #:make-fail make-not-available)

(define-sdl2 sensor-get-data (_fun _sensor* _float* _int -> _int)
    #:make-fail make-not-available)

(define-sdl2 sensor-close (_fun _sensor* -> _void)
    #:make-fail make-not-available)

(define-sdl2 sensor-update (_fun -> _void)
    #:make-fail make-not-available)


;; SDL_shape.h

(define nonshapeable-window -1)
(define invalid-shape-argument -2)
(define window-lacks-shape -3)

(define-sdl2 create-shaped-window
    (_fun _string _uint _uint _uint _uint _window-flags -> _window*/null))

(define-sdl2 shaped-window? (_fun _window* -> _bool))

(define _window-shape-mode-enum
    (_enum
     '(default
       binarize-alpha
       reverse-binarize-alpha
       color-key)))

(define (shape-mode-alpha mode)
    (or
     (eq? mode 'default)
     (eq? mode 'binarize-alpha)
     (eq? mode 'reverse-binarize-alpha)))

(define _window-shape-params
    (_union
     _uint8
     _color))

(define-cstruct _window-shape-mode
    ([mode _window-shape-mode-enum]
     [parameters _window-shape-params]))
(define _window-shape-mode* _window-shape-mode-pointer)

(define-sdl2 set-window-shape! (_fun _window* _surface* _window-shape-mode* -> _int))

(define-sdl2 get-shaped-window-mode (_fun _window* _window-shape-mode* -> _int))


;; SDL_system.h

(define _windows-message-hook (_fun _pointer _pointer _uint _uint64 _sint64 -> _void))
(define-sdl2 set-windows-message-hook! (_fun _windows-message-hook _pointer -> _void)
    #:make-fail make-not-available)

(define-sdl2 direct3d9-get-adapter-index (_fun _int -> _int)
    #:make-fail make-not-available)

(define-cpointer-type _i-direct3d-device9*)

(define-sdl2 render-get-d3d9-device (_fun _renderer* -> _i-direct3d-device9*/null)
    #:make-fail make-not-available)

(define-sdl2 dxgi-get-output-info (_fun _int _int* _int* -> _bool)
    #:make-fail make-not-available)

(define-sdl2 linux-set-thread-priority! (_fun _sint64 _int -> _int)
    #:make-fail make-not-available)

(define-sdl2 iphone-set-animation-callback!
    (_fun _window* _int (_fun _pointer -> _void) _pointer -> _int)
    #:make-fail make-not-available)
(define ios-set-animation-callback! iphone-set-animation-callback!)

(define-sdl2 iphone-set-event-pump! (_fun _bool -> _void)
    #:make-fail make-not-available)
(define ios-set-event-pump! iphone-set-event-pump!)

(define-sdl2 android-get-jni-env (_fun -> _pointer)
    #:make-fail make-not-available)

(define-sdl2 android-get-activity (_fun -> _pointer)
    #:make-fail make-not-available)

(define-sdl2 android-tv? (_fun -> _bool)
    #:make-fail make-not-available)

(define-sdl2 chromebook? (_fun -> _bool)
    #:make-fail make-not-available)

(define-sdl2 dex-mode? (_fun -> _bool)
    #:make-fail make-not-available)

(define-sdl2 android-back-button (_fun -> _void)
    #:make-fail make-not-available)

(define android-external-storage-read   #x01)
(define android-external-storage-write  #x02)

(define-sdl2 android-get-internal-storage-path (_fun -> _string)
    #:make-fail make-not-available)

(define-sdl2 android-get-external-storage-state (_fun -> _int)
    #:make-fail make-not-available)

(define-sdl2 android-get-external-storage-path (_fun -> _string)
    #:make-fail make-not-available)

(define _win-rt-path
    (_enum
     '(installed-location
       local-folder
       roaming-folder
       temp-folder)))

(define _win-rt-device-family
    (_enum
     '(unknown
       desktop
       mobile
       xbox)))

(define-sdl2 win-rt-get-fs-path-unicode (_fun _win-rt-path -> _string/ucs-4)
    #:make-fail make-not-available)

(define-sdl2 win-rt-get-fs-path-utf8 (_fun _win-rt-path -> _string)
    #:make-fail make-not-available)

(define-sdl2 win-rt-get-device-family (_fun -> _win-rt-device-family)
    #:make-fail make-not-available)

(define-sdl2 tablet? (_fun -> _bool)
    #:make-fail make-not-available)


;; SDL_thread.h

(define-cpointer-type _thread*)

(define _thread-id _ulong)

(define _tls-id _uint)

(define _thread-priority
    (_enum
     '(low
       normal
       high
       time-critical)))

(define _thread-function (_fun _pointer -> _int))

(define-sdl2 create-thread (_fun _thread-function _string _pointer -> _thread*/null))

(define-sdl2 create-thread-with-stack-size
    (_fun _thread-function _string _size _pointer -> _thread*/null)
    #:make-fail make-not-available)

(define-sdl2 get-thread-name (_fun _thread*/null -> _string))

(define-sdl2 thread-id (_fun -> _thread-id))

(define-sdl2 get-thread-id (_fun _thread*/null -> _thread-id))

(define-sdl2 set-thread-priority! (_fun _thread-priority -> _int))

(define-sdl2 wait-thread (_fun _thread* _int*/null -> _void))

(define-sdl2 detach-thread (_fun _thread* -> _void)
    #:make-fail make-not-available)

(define-sdl2 tls-create (_fun -> _tls-id))

(define-sdl2 tls-get (_fun _tls-id -> _pointer))

(define-sdl2 tls-set (_fun _tls-id _pointer (_fun _pointer -> _void) -> _int))


;; SDL_timer.h

(define-sdl2 get-ticks (_fun -> _uint32))

(define (ticks-passed A B)
    (<= (- B A) 0))

(define-sdl2 get-performance-counter (_fun -> _uint64))

(define-sdl2 get-performance-frequency (_fun -> _uint64))

(define-sdl2 delay! (_fun _uint32 -> _void))

(define _timer-callback (_fun #:async-apply (lambda (thunk) (thunk)) _uint32 _pointer -> _uint32))

(define _timer-id _int)

(define-sdl2 add-timer (_fun _uint32 _timer-callback _pointer -> _timer-id))

(define-sdl2 remove-timer (_fun _timer-id -> _bool))


;; SDL_version.h

(define-cstruct _version
    ([major _uint8]
     [minor _uint8]
     [patch _uint8]))

(define _version* _version-pointer)

(define (version-num X Y Z)
    (+ (* X 1000) (* Y 100) Z))

(define-sdl2 get-version (_fun _version* -> _void))

(define-sdl2 get-revision (_fun -> _string))

(define-sdl2 get-revision-number (_fun -> _int))


;; SDL.h

(define-sdl2 get-platform (_fun -> _string))

(define _init
    (_bitmask
     '(timer           = #x00000001
       audio           = #x00000010
       video           = #x00000020
       joystick        = #x00000200
       haptic          = #x00001000
       game-controller = #x00002000
       events          = #x00004000
       sensor          = #x00008000
       noparachute     = #x00100000
       everything      = #x0000f231)
     _uint32))

(define-sdl2 init! (_fun _init -> _int))

(define-sdl2 init-sub-system! (_fun _init -> _int))

(define-sdl2 quit-sub-system! (_fun _init -> _void))

(define-sdl2 was-init (_fun _init -> _uint32))

(define-sdl2 quit! (_fun -> _void))
