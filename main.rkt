#lang racket/base

(require
 (for-syntax racket/base)
 (for-syntax racket/format)
 (for-syntax racket/syntax)
 ffi/unsafe
 ffi/unsafe/define
 (submod racket/performance-hint begin-encourage-inline)
 (rename-in racket/contract
            [-> contract/->])
 sdl2/private/lib-path)

(provide
 (except-out
  (all-defined-out)
  sdl2-lib
  define-sdl2
  define-sdl2-vararg))

(define sdl2-lib (ffi-lib (lib-path) '("0" #f)))
(define-ffi-definer define-sdl2 sdl2-lib)

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
        [(_ name (arg-types ...) ret-type)
         (with-syntax*
             ([name-str (~a (syntax->datum #'name))]
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

(define SDL_arraysize array-length)
(define SDL_TABLESIZE SDL_arraysize)

(define-syntax-rule (SDL_STRINGIFY_ARG arg)
    (~a arg))

(define (SDL_FOURCC A B C D)
    (let ([a (if (char? A) (char->integer A) A)]
          [b (if (char? B) (char->integer B) B)]
          [c (if (char? C) (char->integer C) C)]
          [d (if (char? D) (char->integer D) D)])
        (bitwise-ior
         (bitwise-and #xffffffff (arithmetic-shift (bitwise-and #xff a) 0))
         (bitwise-and #xffffffff (arithmetic-shift (bitwise-and #xff b) 8))
         (bitwise-and #xffffffff (arithmetic-shift (bitwise-and #xff c) 16))
         (bitwise-and #xffffffff (arithmetic-shift (bitwise-and #xff d) 24)))))

(define SDL_FALSE 0)
(define SDL_TRUE 1)
(define _SDL_bool _int)

(define SDL_MAX_SINT8   #x7F)
(define SDL_MIN_SINT8   (bitwise-not #x7F))

(define SDL_MAX_UINT8   #xFF)
(define SDL_MIN_UINT8   #x00)

(define SDL_MAX_SINT16  #x7FFF)
(define SDL_MIN_SINT16  (bitwise-not #x7FFF))

(define SDL_MAX_UINT16  #xFFFF)
(define SDL_MIN_UINT16  #x0000)

(define SDL_MAX_SINT32  #x7FFFFFFF)
(define SDL_MIN_SINT32  (bitwise-not #x7FFFFFFF))

(define SDL_MAX_UINT32  #xFFFFFFFF)
(define SDL_MIN_UINT32  #x00000000)

(define SDL_MAX_SINT64  #x7FFFFFFFFFFFFFFF)
(define SDL_MIN_SINT64  (bitwise-not #x7FFFFFFFFFFFFFFF))

(define SDL_MAX_UINT64  #xFFFFFFFFFFFFFFFF)
(define SDL_MIN_UINT64  #x0000000000000000)

(define-sdl2 SDL_malloc (_fun _size -> _pointer))
(define-sdl2 SDL_calloc (_fun _size _size -> _pointer))
(define-sdl2 SDL_realloc (_fun _pointer _size -> _pointer))
(define-sdl2 SDL_free (_fun _pointer -> _void))

(define SDL_stack_alloc SDL_malloc)
(define SDL_stack_free SDL_free)

(define _SDL_malloc_func (_fun _size -> _pointer))
(define _SDL_calloc_func (_fun _size _size -> _pointer))
(define _SDL_realloc_func (_fun _pointer _size -> _pointer))
(define _SDL_free_func (_fun _pointer -> _void))

(define-sdl2 SDL_GetMemoryFunctions
    (_fun _SDL_malloc_func _SDL_calloc_func _SDL_realloc_func _SDL_free_func -> _void)
    #:make-fail make-not-available)

(define-sdl2 SDL_SetMemoryFunctions
    (_fun _SDL_malloc_func _SDL_calloc_func _SDL_realloc_func _SDL_free_func -> _int)
    #:make-fail make-not-available)

(define-sdl2 SDL_GetNumAllocations (_fun -> _int)
    #:make-fail make-not-available)

(define-sdl2 SDL_getenv (_fun _string -> _string))
(define-sdl2 SDL_setenv (_fun _string _string _int -> _int))

(define-sdl2 SDL_qsort (_fun _pointer _size _size (_fun _pointer _pointer -> _int) -> _void))

(define-sdl2 SDL_abs (_fun _int -> _int))

(define-syntax-rule (SDL_min x y)
    (if (< x y) x y))

(define-syntax-rule (SDL_max x y)
    (if (> x y) x y))

(define-sdl2 SDL_isdigit (_fun _int -> _int))
(define-sdl2 SDL_isspace (_fun _int -> _int))
(define-sdl2 SDL_toupper (_fun _int -> _int))
(define-sdl2 SDL_tolower (_fun _int -> _int))

(define-sdl2 SDL_memset (_fun _pointer _int _size -> _pointer))

(define-sdl2 SDL_memcpy (_fun _pointer _pointer _size -> _pointer))

(define-sdl2 SDL_memmove (_fun _pointer _pointer _size -> _pointer))

(define-sdl2 SDL_memcmp (_fun _pointer _pointer _size -> _int))

(define-sdl2 SDL_wcsdup (_fun _string/ucs-4 -> _string/ucs-4)
    #:make-fail make-not-available)

(define-sdl2 SDL_wcslen (_fun _string/ucs-4 -> _size))
(define-sdl2 SDL_wcslcpy (_fun _string/ucs-4 _string/ucs-4 _size -> _size))
(define-sdl2 SDL_wcslcat (_fun _string/ucs-4 _string/ucs-4 _size -> _size))
(define-sdl2 SDL_wcscmp (_fun _string/ucs-4 _string/ucs-4 -> _int)
    #:make-fail make-not-available)

(define-sdl2 SDL_strlen (_fun _string -> _size))
(define-sdl2 SDL_strlcpy (_fun _string _string _size -> _size))
(define-sdl2 SDL_utf8strlcpy (_fun _string _string _size -> _size))
(define-sdl2 SDL_strlcat (_fun _string _string _size -> _size))
(define-sdl2 SDL_strdup (_fun _string -> _string))
(define-sdl2 SDL_strrev (_fun _string -> _string))
(define-sdl2 SDL_strupr (_fun _string -> _string))
(define-sdl2 SDL_strlwr (_fun _string -> _string))
(define-sdl2 SDL_strchr (_fun _string _int -> _string))
(define-sdl2 SDL_strrchr (_fun _string _int -> _string))
(define-sdl2 SDL_strstr (_fun _string _string -> _string))
(define-sdl2 SDL_utf8strlen (_fun _string -> _size)
    #:make-fail make-not-available)

(define-sdl2 SDL_itoa (_fun _int _string _int -> _string))
(define-sdl2 SDL_uitoa (_fun _uint _string _int -> _string))
(define-sdl2 SDL_ltoa (_fun _long _string _int -> _string))
(define-sdl2 SDL_ultoa (_fun _ulong _string _int -> _string))
(define-sdl2 SDL_lltoa (_fun _sint64 _string _int -> _string))
(define-sdl2 SDL_ulltoa (_fun _uint64 _string _int -> _string))

(define-sdl2 SDL_atoi (_fun _string -> _int))
(define-sdl2 SDL_atof (_fun _string -> _double))
(define-sdl2 SDL_strtol (_fun _string (_ptr i _string) _int -> _long))
(define-sdl2 SDL_strtoul (_fun _string (_ptr i _string) _int -> _ulong))
(define-sdl2 SDL_strtoll (_fun _string (_ptr i _string) _int -> _sint64))
(define-sdl2 SDL_strtoull (_fun _string (_ptr i _string) _int -> _uint64))
(define-sdl2 SDL_strtod (_fun _string (_ptr i _string) -> _double))

(define-sdl2 SDL_strcmp (_fun _string _string -> _int))
(define-sdl2 SDL_strncmp (_fun _string _string _size -> _int))
(define-sdl2 SDL_strcasecmp (_fun _string _string -> _int))
(define-sdl2 SDL_strncasecmp (_fun _string _string _size -> _int))

(define-sdl2 SDL_acos (_fun _double -> _double)
    #:make-fail make-not-available)
(define-sdl2 SDL_acosf (_fun _float -> _float)
    #:make-fail make-not-available)
(define-sdl2 SDL_asin (_fun _double -> _double)
    #:make-fail make-not-available)
(define-sdl2 SDL_asinf (_fun _float -> _float)
    #:make-fail make-not-available)
(define-sdl2 SDL_atan (_fun _double -> _double))
(define-sdl2 SDL_atanf (_fun _float -> _float)
    #:make-fail make-not-available)
(define-sdl2 SDL_atan2 (_fun _double _double -> _double))
(define-sdl2 SDL_atan2f (_fun _float _float -> _float)
    #:make-fail make-not-available)
(define-sdl2 SDL_ceil (_fun _double -> _double))
(define-sdl2 SDL_ceilf (_fun _float -> _float)
    #:make-fail make-not-available)
(define-sdl2 SDL_copysign (_fun _double _double -> _double))
(define-sdl2 SDL_copysignf (_fun _float _float -> _float)
    #:make-fail make-not-available)
(define-sdl2 SDL_cos (_fun _double -> _double))
(define-sdl2 SDL_cosf (_fun _float -> _float))
(define-sdl2 SDL_exp (_fun _double -> _double)
    #:make-fail make-not-available)
(define-sdl2 SDL_expf (_fun _float -> _float)
    #:make-fail make-not-available)
(define-sdl2 SDL_fabs (_fun _double -> _double))
(define-sdl2 SDL_fabsf (_fun _float -> _float)
    #:make-fail make-not-available)
(define-sdl2 SDL_floor (_fun _double -> _double))
(define-sdl2 SDL_floorf (_fun _float -> _float)
    #:make-fail make-not-available)
(define-sdl2 SDL_fmod (_fun _double _double -> _double)
    #:make-fail make-not-available)
(define-sdl2 SDL_fmodf (_fun _float _float -> _float)
    #:make-fail make-not-available)
(define-sdl2 SDL_log (_fun _double -> _double))
(define-sdl2 SDL_logf (_fun _float -> _float)
    #:make-fail make-not-available)
(define-sdl2 SDL_log10 (_fun _double -> _double)
    #:make-fail make-not-available)
(define-sdl2 SDL_log10f (_fun _float -> _float)
    #:make-fail make-not-available)
(define-sdl2 SDL_pow (_fun _double _double -> _double))
(define-sdl2 SDL_powf (_fun _float _float -> _float)
    #:make-fail make-not-available)
(define-sdl2 SDL_scalbn (_fun _double _int -> _double))
(define-sdl2 SDL_scalbnf (_fun _float _int -> _float)
    #:make-fail make-not-available)
(define-sdl2 SDL_sin (_fun _double -> _double))
(define-sdl2 SDL_sinf (_fun _float -> _float))
(define-sdl2 SDL_sqrt (_fun _double -> _double))
(define-sdl2 SDL_sqrtf (_fun _float -> _float)
    #:make-fail make-not-available)
(define-sdl2 SDL_tan (_fun _double -> _double)
    #:make-fail make-not-available)
(define-sdl2 SDL_tanf (_fun _float -> _float)
    #:make-fail make-not-available)

(define SDL_ICONV_ERROR     (cast -1 _intptr _size))
(define SDL_ICONV_E2BIG     (cast -2 _intptr _size))
(define SDL_ICONV_EILSEQ    (cast -3 _intptr _size))
(define SDL_ICONV_EINVAL    (cast -4 _intptr _size))

(define-cpointer-type _SDL_iconv_t*)

(define-sdl2 SDL_iconv_open (_fun _string _string -> _SDL_iconv_t*))
(define-sdl2 SDL_iconv_close (_fun _SDL_iconv_t* -> _int))
(define-sdl2 SDL_iconv (_fun _SDL_iconv_t* (_ptr i _string) _size* (_ptr o _string) _size* -> _size))
(define-sdl2 SDL_iconv_string (_fun _string _string _string _size -> _string))
(define (SDL_iconv_utf8_locale S)
    (SDL_iconv_string "" "UTF-8" S (+ (SDL_strlen S) 1)))
(define (SDL_iconv_utf8_ucs2 S)
    (cast
     (SDL_iconv_string "UCS-2-INTERNAL" "UTF-8" S (+ (SDL_strlen S) 1))
     _string
     _uint16*/null))
(define (SDL_iconv_utf8_ucs4 S)
    (cast
     (SDL_iconv_string "UCS-4-INTERNAL" "UTF-8" S (+ (SDL_strlen S) 1))
     _string
     _uint32*/null))

(define (SDL_memcpy4 dst src dwords)
    (SDL_memcpy dst src (* dwords 4)))


;; SDL_assert.h

(define _SDL_AssertState
    (_enum
     '(SDL_ASSERTION_RETRY
       SDL_ASSERTION_BREAK
       SDL_ASSERTION_ABORT
       SDL_ASSERTION_IGNORE
       SDL_ASSERTION_ALWAYS_IGNORE)))

(define-cstruct _SDL_AssertData
    ([always_ignore _int]
     [trigger_count _uint]
     [condition _string]
     [filename _string]
     [linenum _int]
     [function _string]
     [next _pointer]))

(define _SDL_AssertData* _SDL_AssertData-pointer)

(define-sdl2 SDL_ReportAssertion (_fun _SDL_AssertData* _string _string _int -> _SDL_AssertState))

(define _SDL_AssertionHandler (_fun _SDL_AssertData* _pointer -> _SDL_AssertState))

(define-sdl2 SDL_SetAssertionHandler (_fun _SDL_AssertionHandler _pointer -> _void))

(define-sdl2 SDL_GetDefaultAssertionHandler (_fun -> _SDL_AssertionHandler)
    #:make-fail make-not-available)

(define-sdl2 SDL_GetAssertionHandler (_fun (_ptr o _pointer) -> _SDL_AssertionHandler)
    #:make-fail make-not-available)

(define-sdl2 SDL_GetAssertionReport (_fun -> _SDL_AssertData*))

(define-sdl2 SDL_ResetAssertionReport (_fun -> _void))


;; SDL_atomic.h

(define _SDL_SpinLock _int)
(define _SDL_SpinLock* _int*)

(define-sdl2 SDL_AtomicTryLock (_fun _SDL_SpinLock* -> _SDL_bool))

(define-sdl2 SDL_AtomicLock (_fun _SDL_SpinLock* -> _void))

(define-sdl2 SDL_AtomicUnlock (_fun _SDL_SpinLock* -> _void))

(define (SDL_CompilerBarrier)
    (define tmp (cast (malloc (ctype-sizeof _int)) _pointer _int*))
    (ptr-set! tmp _int 0)
    (SDL_AtomicLock tmp)
    (SDL_AtomicUnlock tmp))

(define SDL_MemoryBarrierRelease SDL_CompilerBarrier)

(define SDL_MemoryBarrierAcquire SDL_CompilerBarrier)

(define-cstruct _SDL_atomic_t
    ([value _int]))

(define _SDL_atomic_t* _SDL_atomic_t-pointer)

(define-sdl2 SDL_AtomicCAS (_fun _SDL_atomic_t* _int _int -> _SDL_bool))

(define-sdl2 SDL_AtomicGet (_fun _SDL_atomic_t* -> _int)
    #:make-fail make-not-available)

(define-sdl2 SDL_AtomicAdd (_fun _SDL_atomic_t* _int -> _int)
    #:make-fail make-not-available)

(define (SDL_AtomicIncRef a) (SDL_AtomicAdd a 1))

(define (SDL_AtomicDecRef a) (= 1 (SDL_AtomicAdd a -1)))

(define-sdl2 SDL_AtomicCASPtr (_fun (_ptr io _pointer) _pointer _pointer -> _SDL_bool))

(define-sdl2 SDL_AtomicSetPtr (_fun (_ptr io _pointer) _pointer -> _pointer)
    #:make-fail make-not-available)

(define-sdl2 SDL_AtomicGetPtr (_fun (_ptr i _pointer) -> _pointer)
    #:make-fail make-not-available)


;; SDL_rwops.h

(define SDL_RWOPS_UNKNOWN   0)
(define SDL_RWOPS_WINFILE   1)
(define SDL_RWOPS_STDFILE   2)
(define SDL_RWOPS_JNIFILE   3)
(define SDL_RWOPS_MEMORY    4)
(define SDL_RWOPS_MEMORY_RO 5)

(define-cstruct _SDL_RWops
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

(define _SDL_RWops* _SDL_RWops-pointer)
(define _SDL_RWops*/null _SDL_RWops-pointer/null)

(define-sdl2 SDL_RWFromFile (_fun _string _string -> _SDL_RWops*/null))

(define-sdl2 SDL_RWFromFP (_fun _pointer _SDL_bool -> _SDL_RWops*/null))

(define-sdl2 SDL_RWFromMem (_fun _pointer _int -> _SDL_RWops*/null))

(define-sdl2 SDL_RWFromConstMem (_fun _pointer _int -> _SDL_RWops*/null))

(define-sdl2 SDL_AllocRW (_fun -> _SDL_RWops*/null))

(define-sdl2 SDL_FreeRW (_fun _SDL_RWops* -> _void))

(define RW_SEEK_SET 0)
(define RW_SEEK_CUR 1)
(define RW_SEEK_END 2)

(define (SDL_RWsize ctx) ((SDL_RWops-size ctx) ctx))
(define (SDL_RWseek ctx offset whence) ((SDL_RWops-seek ctx) ctx offset whence))
(define (SDL_RWtell ctx) ((SDL_RWops-seek ctx) 0 RW_SEEK_CUR))
(define (SDL_RWread ctx ptr size n) ((SDL_RWops-read ctx) ctx ptr size n))
(define (SDL_RWwrite ctx ptr size n) ((SDL_RWops-write ctx) ctx ptr size n))
(define (SDL_RWclose ctx) ((SDL_RWops-close ctx) ctx))

(define-sdl2 SDL_LoadFile_RW (_fun _SDL_RWops* _size _int -> _pointer)
    #:make-fail make-not-available)

(define (SDL_LoadFile file datasize)
    (SDL_LoadFile_RW
     (SDL_RWFromFile file "rb")
     datasize 1))

(define-sdl2 SDL_ReadU8   (_fun _SDL_RWops* -> _uint8 ))
(define-sdl2 SDL_ReadLE16 (_fun _SDL_RWops* -> _uint16))
(define-sdl2 SDL_ReadBE16 (_fun _SDL_RWops* -> _uint16))
(define-sdl2 SDL_ReadLE32 (_fun _SDL_RWops* -> _uint32))
(define-sdl2 SDL_ReadBE32 (_fun _SDL_RWops* -> _uint32))
(define-sdl2 SDL_ReadLE64 (_fun _SDL_RWops* -> _uint64))
(define-sdl2 SDL_ReadBE64 (_fun _SDL_RWops* -> _uint64))

(define-sdl2 SDL_WriteU8   (_fun _SDL_RWops* _uint8  -> _size))
(define-sdl2 SDL_WriteLE16 (_fun _SDL_RWops* _uint16 -> _size))
(define-sdl2 SDL_WriteBE16 (_fun _SDL_RWops* _uint16 -> _size))
(define-sdl2 SDL_WriteLE32 (_fun _SDL_RWops* _uint32 -> _size))
(define-sdl2 SDL_WriteBE32 (_fun _SDL_RWops* _uint32 -> _size))
(define-sdl2 SDL_WriteLE64 (_fun _SDL_RWops* _uint64 -> _size))
(define-sdl2 SDL_WriteBE64 (_fun _SDL_RWops* _uint64 -> _size))


;; SDL_audio.h

(define _SDL_AudioFormat _uint16)

(define SDL_AUDIO_MASK_BITSIZE       #xFF)
(define SDL_AUDIO_MASK_DATATYPE      (arithmetic-shift 1 8))
(define SDL_AUDIO_MASK_ENDIAN        (arithmetic-shift 1 12))
(define SDL_AUDIO_MASK_SIGNED        (arithmetic-shift 1 15))
(define (SDL_AUDIO_BITSIZE x)        (bitwise-and x SDL_AUDIO_MASK_BITSIZE))
(define (SDL_AUDIO_ISFLOAT x)        (bitwise-and x SDL_AUDIO_MASK_DATATYPE))
(define (SDL_AUDIO_ISBIGENDIAN x)    (bitwise-and x SDL_AUDIO_MASK_ENDIAN))
(define (SDL_AUDIO_ISSIGNED x)       (bitwise-and x SDL_AUDIO_MASK_SIGNED))
(define (SDL_AUDIO_ISINT x)          (bitwise-not (SDL_AUDIO_ISFLOAT x)))
(define (SDL_AUDIO_ISLITTLEENDIAN x) (bitwise-not (SDL_AUDIO_ISBIGENDIAN x)))
(define (SDL_AUDIO_ISUNSIGNED x)     (bitwise-not (SDL_AUDIO_ISSIGNED x)))

(define AUDIO_U8        #x0008)
(define AUDIO_S8        #x8008)
(define AUDIO_U16LSB    #x0010)
(define AUDIO_S16LSB    #x8010)
(define AUDIO_U16MSB    #x1010)
(define AUDIO_S16MSB    #x9010)
(define AUDIO_U16       AUDIO_U16LSB)
(define AUDIO_S16       AUDIO_S16LSB)

(define AUDIO_S32LSB    #x8020)
(define AUDIO_S32MSB    #x9020)
(define AUDIO_S32       AUDIO_S32LSB)

(define AUDIO_F32LSB    #x8120)
(define AUDIO_F32MSB    #x9120)
(define AUDIO_F32       AUDIO_F32LSB)

(define AUDIO_U16SYS    (if (system-big-endian?) AUDIO_U16MSB AUDIO_U16LSB))
(define AUDIO_S16SYS    (if (system-big-endian?) AUDIO_S16MSB AUDIO_U16LSB))
(define AUDIO_S32SYS    (if (system-big-endian?) AUDIO_S32MSB AUDIO_U16LSB))
(define AUDIO_F32SYS    (if (system-big-endian?) AUDIO_F32MSB AUDIO_U16LSB))

(define SDL_AUDIO_ALLOW_FREQUENCY_CHANGE    #x00000001)
(define SDL_AUDIO_ALLOW_FORMAT_CHANGE       #x00000002)
(define SDL_AUDIO_ALLOW_CHANNELS_CHANGE     #x00000004)
(define SDL_AUDIO_ALLOW_SAMPLES_CHANGE      #x00000008)
(define SDL_AUDIO_ALLOW_ANY_CHANGE          (bitwise-ior
                                             SDL_AUDIO_ALLOW_FREQUENCY_CHANGE
                                             SDL_AUDIO_ALLOW_FORMAT_CHANGE
                                             SDL_AUDIO_ALLOW_CHANNELS_CHANGE
                                             SDL_AUDIO_ALLOW_SAMPLES_CHANGE))

(define _SDL_AudioCallback (_fun _pointer _uint8* _int -> _void))

(define-cstruct _SDL_AudioSpec
    ([freq _int]
     [format _SDL_AudioFormat]
     [channels _uint8]
     [silence _uint8]
     [samples _uint16]
     [padding _uint16]
     [size _uint32]
     [callback _SDL_AudioCallback]
     [userdata _pointer]))

(define _SDL_AudioSpec* _SDL_AudioSpec-pointer)
(define _SDL_AudioSpec*/null _SDL_AudioSpec-pointer/null)

(define _SDL_AudioFilter (_fun _pointer _SDL_AudioFormat -> _void))

(define SDL_AUDIOCVT_MAX_FILTERS 9)

(define-cstruct _SDL_AudioCVT
    ([needed _int]
     [src_format _SDL_AudioFormat]
     [dst_format _SDL_AudioFormat]
     [rate_incr _double]
     [buf _uint8*]
     [len _int]
     [len_cvt _int]
     [len_mult _int]
     [len_ratio _double]
     [filters (_array _SDL_AudioFilter (+ SDL_AUDIOCVT_MAX_FILTERS 1))]
     [filter_index _int])
    #:alignment 1)

(define _SDL_AudioCVT* _SDL_AudioCVT-pointer)

(define-sdl2 SDL_GetNumAudioDrivers (_fun -> _int))
(define-sdl2 SDL_GetAudioDriver (_fun _int -> _string))

(define-sdl2 SDL_AudioInit (_fun _string -> _int))
(define-sdl2 SDL_AudioQuit (_fun -> _void))

(define-sdl2 SDL_GetCurrentAudioDriver (_fun -> _string))

(define-sdl2 SDL_OpenAudio (_fun _SDL_AudioSpec* _SDL_AudioSpec*/null -> _int))

(define _SDL_AudioDeviceID _uint32)

(define-sdl2 SDL_GetNumAudioDevices (_fun _int -> _int))

(define-sdl2 SDL_GetAudioDeviceName (_fun _int _int -> _string))

(define-sdl2 SDL_OpenAudioDevice
    (_fun _string _int _SDL_AudioSpec* _SDL_AudioSpec* _int -> _SDL_AudioDeviceID))

(define _SDL_AudioStatus
    (_enum
     '(SDL_AUDIO_STOPPED = 0
       SDL_AUDIO_PLAYING
       SDL_AUDIO_PAUSED)))

(define-sdl2 SDL_GetAudioStatus (_fun -> _SDL_AudioStatus))

(define-sdl2 SDL_GetAudioDeviceStatus (_fun _SDL_AudioDeviceID -> _SDL_AudioStatus))

(define-sdl2 SDL_PauseAudio (_fun _int -> _void))
(define-sdl2 SDL_PauseAudioDevice (_fun _SDL_AudioDeviceID _int -> _void))

(define-sdl2 SDL_LoadWAV_RW
    (_fun _SDL_RWops* _int _SDL_AudioSpec* (_ptr o _uint8*) _uint32* -> _SDL_AudioSpec*/null))

(define (SDL_LoadWAV file spec audio_buf audio_len)
    (SDL_LoadWAV_RW
     (SDL_RWFromFile file "rb")
     1 spec audio_buf audio_len))

(define-sdl2 SDL_FreeWAV (_fun _uint8* -> _void))

(define-sdl2 SDL_BuildAudioCVT
    (_fun _SDL_AudioCVT* _SDL_AudioFormat _uint8 _int _SDL_AudioFormat _uint8 _int -> _int))

(define-sdl2 SDL_ConvertAudio (_fun _SDL_AudioCVT* -> _int))

(define-cpointer-type _SDL_AudioStream*)

(define-sdl2 SDL_NewAudioStream
    (_fun _SDL_AudioFormat _uint8 _int _SDL_AudioFormat _uint8 _int -> _SDL_AudioStream*)
    #:make-fail make-not-available)

(define-sdl2 SDL_AudioStreamPut (_fun _SDL_AudioStream* _pointer _int -> _int)
    #:make-fail make-not-available)

(define-sdl2 SDL_AudioStreamGet (_fun _SDL_AudioStream* _pointer _int -> _int)
    #:make-fail make-not-available)

(define-sdl2 SDL_AudioStreamAvailable (_fun _SDL_AudioStream* -> _int)
    #:make-fail make-not-available)

(define-sdl2 SDL_AudioStreamFlush (_fun _SDL_AudioStream* -> _int)
    #:make-fail make-not-available)

(define-sdl2 SDL_AudioStreamClear (_fun _SDL_AudioStream* -> _void)
    #:make-fail make-not-available)

(define-sdl2 SDL_FreeAudioStream (_fun _SDL_AudioStream* -> _void)
    #:make-fail make-not-available)

(define SDL_MIX_MAXVOLUME 128)

(define-sdl2 SDL_MixAudio (_fun _uint8* _uint8* _uint32 _int -> _void))

(define-sdl2 SDL_MixAudioFormat (_fun _uint8* _uint8* _SDL_AudioFormat _uint32 _int -> _void))

(define-sdl2 SDL_QueueAudio (_fun _SDL_AudioDeviceID _pointer _uint32 -> _int)
    #:make-fail make-not-available)

(define-sdl2 SDL_DequeueAudio (_fun _SDL_AudioDeviceID _pointer _uint32 -> _uint32)
    #:make-fail make-not-available)

(define-sdl2 SDL_GetQueuedAudioSize (_fun _SDL_AudioDeviceID -> _uint32)
    #:make-fail make-not-available)

(define-sdl2 SDL_ClearQueuedAudio (_fun _SDL_AudioDeviceID -> _void)
    #:make-fail make-not-available)

(define-sdl2 SDL_LockAudio (_fun -> _void))
(define-sdl2 SDL_LockAudioDevice (_fun _SDL_AudioDeviceID -> _void))
(define-sdl2 SDL_UnlockAudio (_fun -> _void))
(define-sdl2 SDL_UnlockAudioDevice (_fun -> _void))

(define-sdl2 SDL_CloseAudio (_fun -> _void))
(define-sdl2 SDL_CloseAudioDevice (_fun _SDL_AudioDeviceID -> _void))


;; SDL_clipboard.h

(define-sdl2 SDL_SetClipboardText (_fun _string -> _int))

(define-sdl2 SDL_GetClipboardText (_fun -> _string))

(define-sdl2 SDL_HasClipboardText (_fun -> _SDL_bool))


;; SDL_cpuinfo.h

(define SDL_CACHELINE_SIZE  128)

(define-sdl2 SDL_GetCPUCount (_fun -> _int))

(define-sdl2 SDL_GetCPUCacheLineSize (_fun -> _int))

(define-sdl2 SDL_HasRDTSC (_fun -> _SDL_bool))

(define-sdl2 SDL_HasAltiVec (_fun -> _SDL_bool))

(define-sdl2 SDL_HasMMX (_fun -> _SDL_bool))

(define-sdl2 SDL_Has3DNow (_fun -> _SDL_bool))

(define-sdl2 SDL_HasSSE (_fun -> _SDL_bool))

(define-sdl2 SDL_HasSSE2 (_fun -> _SDL_bool))

(define-sdl2 SDL_HasSSE3 (_fun -> _SDL_bool))

(define-sdl2 SDL_HasSSE41 (_fun -> _SDL_bool))

(define-sdl2 SDL_HasSSE42 (_fun -> _SDL_bool))

(define-sdl2 SDL_HasAVX (_fun -> _SDL_bool)
    #:make-fail make-not-available)

(define-sdl2 SDL_HasAVX2 (_fun -> _SDL_bool)
    #:make-fail make-not-available)

(define-sdl2 SDL_HasAVX512F (_fun -> _SDL_bool)
    #:make-fail make-not-available)

(define-sdl2 SDL_HasNEON (_fun -> _SDL_bool)
    #:make-fail make-not-available)

(define-sdl2 SDL_GetSystemRAM (_fun -> _int)
    #:make-fail make-not-available)


;; SDL_endian.h

(define SDL_LIL_ENDIAN  1234)
(define SDL_BIG_ENDIAN  4321)

(define SDL_BYTEORDER (if (system-big-endian?) SDL_BIG_ENDIAN SDL_LIL_ENDIAN))

(begin-encourage-inline

    (define/contract (SDL_Swap16 x)
        (contract/->
         (and/c exact-integer? (lambda (n) (<= (integer-length n) 16)))
         (and/c exact-integer? (lambda (n) (<= (integer-length n) 16))))
        (bitwise-and
         (bitwise-ior
          (arithmetic-shift x 8)
          (arithmetic-shift x -8))
         #xffff))

    (define/contract (SDL_Swap32 x)
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

    (define/contract (SDL_Swap64 x)
        (contract/->
         (and/c exact-integer? (lambda (n) (<= (integer-length n) 64)))
         (and/c exact-integer? (lambda (n) (<= (integer-length n) 64))))
        (bitwise-and
         (bitwise-ior
          (arithmetic-shift
           (SDL_Swap32 (bitwise-and x #xffffffff))
           32)
          (arithmetic-shift
           (SDL_Swap32 (bitwise-and (arithmetic-shift x -32) #xffffffff))
           32))
         #xffffffffffffffff))

    (define/contract (SDL_SwapFloat x)
        (contract/->
         single-flonum? flonum?)
        ;; TODO : somehow convert _float to single-flonum
        (define _swapper (_union _float _uint32))
        (define s (cast (list x) (_list-struct _float) _swapper))
        (union-set! s 1 (SDL_Swap32 (union-ref s 1)))
        (union-ref s 0)))

(define (SDL_SwapLE16 X) (if (system-big-endian?) (SDL_Swap16 X) X))
(define (SDL_SwapLE32 X) (if (system-big-endian?) (SDL_Swap32 X) X))
(define (SDL_SwapLE64 X) (if (system-big-endian?) (SDL_Swap64 X) X))
(define (SDL_SwapFloatLE X) (if (system-big-endian?) (SDL_SwapFloat X) X))
(define (SDL_SwapBE16 X)  (if (system-big-endian?) X (SDL_Swap16 X)))
(define (SDL_SwapBE32 X)  (if (system-big-endian?) X (SDL_Swap32 X)))
(define (SDL_SwapBE64 X)  (if (system-big-endian?) X (SDL_Swap64 X)))
(define (SDL_SwapFloatBE X) (if (system-big-endian?) X (SDL_SwapFloat X)))


;; SDL_error.h

(define-sdl2-vararg SDL_SetError (_string) _int)

(define-sdl2 SDL_GetError (_fun -> _string))

(define-sdl2 SDL_ClearError (_fun -> _void))


;; SDL_scancode.h

(define _SDL_Scancode
    (_enum
     '(SDL_SCANCODE_UNKNOWN = 0

       SDL_SCANCODE_A = 4
       SDL_SCANCODE_B = 5
       SDL_SCANCODE_C = 6
       SDL_SCANCODE_D = 7
       SDL_SCANCODE_E = 8
       SDL_SCANCODE_F = 9
       SDL_SCANCODE_G = 10
       SDL_SCANCODE_H = 11
       SDL_SCANCODE_I = 12
       SDL_SCANCODE_J = 13
       SDL_SCANCODE_K = 14
       SDL_SCANCODE_L = 15
       SDL_SCANCODE_M = 16
       SDL_SCANCODE_N = 17
       SDL_SCANCODE_O = 18
       SDL_SCANCODE_P = 19
       SDL_SCANCODE_Q = 20
       SDL_SCANCODE_R = 21
       SDL_SCANCODE_S = 22
       SDL_SCANCODE_T = 23
       SDL_SCANCODE_U = 24
       SDL_SCANCODE_V = 25
       SDL_SCANCODE_W = 26
       SDL_SCANCODE_X = 27
       SDL_SCANCODE_Y = 28
       SDL_SCANCODE_Z = 29

       SDL_SCANCODE_1 = 30
       SDL_SCANCODE_2 = 31
       SDL_SCANCODE_3 = 32
       SDL_SCANCODE_4 = 33
       SDL_SCANCODE_5 = 34
       SDL_SCANCODE_6 = 35
       SDL_SCANCODE_7 = 36
       SDL_SCANCODE_8 = 37
       SDL_SCANCODE_9 = 38
       SDL_SCANCODE_0 = 39

       SDL_SCANCODE_RETURN = 40
       SDL_SCANCODE_ESCAPE = 41
       SDL_SCANCODE_BACKSPACE = 42
       SDL_SCANCODE_TAB = 43
       SDL_SCANCODE_SPACE = 44

       SDL_SCANCODE_MINUS = 45
       SDL_SCANCODE_EQUALS = 46
       SDL_SCANCODE_LEFTBRACKET = 47
       SDL_SCANCODE_RIGHTBRACKET = 48
       SDL_SCANCODE_BACKSLASH = 49
       SDL_SCANCODE_NONUSHASH = 50
       SDL_SCANCODE_SEMICOLON = 51
       SDL_SCANCODE_APOSTROPHE = 52
       SDL_SCANCODE_GRAVE = 53
       SDL_SCANCODE_COMMA = 54
       SDL_SCANCODE_PERIOD = 55
       SDL_SCANCODE_SLASH = 56

       SDL_SCANCODE_CAPSLOCK = 57

       SDL_SCANCODE_F1 = 58
       SDL_SCANCODE_F2 = 59
       SDL_SCANCODE_F3 = 60
       SDL_SCANCODE_F4 = 61
       SDL_SCANCODE_F5 = 62
       SDL_SCANCODE_F6 = 63
       SDL_SCANCODE_F7 = 64
       SDL_SCANCODE_F8 = 65
       SDL_SCANCODE_F9 = 66
       SDL_SCANCODE_F10 = 67
       SDL_SCANCODE_F11 = 68
       SDL_SCANCODE_F12 = 69

       SDL_SCANCODE_PRINTSCREEN = 70
       SDL_SCANCODE_SCROLLLOCK = 71
       SDL_SCANCODE_PAUSE = 72
       SDL_SCANCODE_INSERT = 73
       SDL_SCANCODE_HOME = 74
       SDL_SCANCODE_PAGEUP = 75
       SDL_SCANCODE_DELETE = 76
       SDL_SCANCODE_END = 77
       SDL_SCANCODE_PAGEDOWN = 78
       SDL_SCANCODE_RIGHT = 79
       SDL_SCANCODE_LEFT = 80
       SDL_SCANCODE_DOWN = 81
       SDL_SCANCODE_UP = 82

       SDL_SCANCODE_NUMLOCKCLEAR = 83
       SDL_SCANCODE_KP_DIVIDE = 84
       SDL_SCANCODE_KP_MULTIPLY = 85
       SDL_SCANCODE_KP_MINUS = 86
       SDL_SCANCODE_KP_PLUS = 87
       SDL_SCANCODE_KP_ENTER = 88
       SDL_SCANCODE_KP_1 = 89
       SDL_SCANCODE_KP_2 = 90
       SDL_SCANCODE_KP_3 = 91
       SDL_SCANCODE_KP_4 = 92
       SDL_SCANCODE_KP_5 = 93
       SDL_SCANCODE_KP_6 = 94
       SDL_SCANCODE_KP_7 = 95
       SDL_SCANCODE_KP_8 = 96
       SDL_SCANCODE_KP_9 = 97
       SDL_SCANCODE_KP_0 = 98
       SDL_SCANCODE_KP_PERIOD = 99

       SDL_SCANCODE_NONUSBACKSLASH = 100
       SDL_SCANCODE_APPLICATION = 101
       SDL_SCANCODE_POWER = 102
       SDL_SCANCODE_KP_EQUALS = 103
       SDL_SCANCODE_F13 = 104
       SDL_SCANCODE_F14 = 105
       SDL_SCANCODE_F15 = 106
       SDL_SCANCODE_F16 = 107
       SDL_SCANCODE_F17 = 108
       SDL_SCANCODE_F18 = 109
       SDL_SCANCODE_F19 = 110
       SDL_SCANCODE_F20 = 111
       SDL_SCANCODE_F21 = 112
       SDL_SCANCODE_F22 = 113
       SDL_SCANCODE_F23 = 114
       SDL_SCANCODE_F24 = 115
       SDL_SCANCODE_EXECUTE = 116
       SDL_SCANCODE_HELP = 117
       SDL_SCANCODE_MENU = 118
       SDL_SCANCODE_SELECT = 119
       SDL_SCANCODE_STOP = 120
       SDL_SCANCODE_AGAIN = 121
       SDL_SCANCODE_UNDO = 122
       SDL_SCANCODE_CUT = 123
       SDL_SCANCODE_COPY = 124
       SDL_SCANCODE_PASTE = 125
       SDL_SCANCODE_FIND = 126
       SDL_SCANCODE_MUTE = 127
       SDL_SCANCODE_VOLUMEUP = 128
       SDL_SCANCODE_VOLUMEDOWN = 129
       SDL_SCANCODE_KP_COMMA = 133
       SDL_SCANCODE_KP_EQUALSAS400 = 134

       SDL_SCANCODE_INTERNATIONAL1 = 135
       SDL_SCANCODE_INTERNATIONAL2 = 136
       SDL_SCANCODE_INTERNATIONAL3 = 137
       SDL_SCANCODE_INTERNATIONAL4 = 138
       SDL_SCANCODE_INTERNATIONAL5 = 139
       SDL_SCANCODE_INTERNATIONAL6 = 140
       SDL_SCANCODE_INTERNATIONAL7 = 141
       SDL_SCANCODE_INTERNATIONAL8 = 142
       SDL_SCANCODE_INTERNATIONAL9 = 143
       SDL_SCANCODE_LANG1 = 144
       SDL_SCANCODE_LANG2 = 145
       SDL_SCANCODE_LANG3 = 146
       SDL_SCANCODE_LANG4 = 147
       SDL_SCANCODE_LANG5 = 148
       SDL_SCANCODE_LANG6 = 149
       SDL_SCANCODE_LANG7 = 150
       SDL_SCANCODE_LANG8 = 151
       SDL_SCANCODE_LANG9 = 152

       SDL_SCANCODE_ALTERASE = 153
       SDL_SCANCODE_SYSREQ = 154
       SDL_SCANCODE_CANCEL = 155
       SDL_SCANCODE_CLEAR = 156
       SDL_SCANCODE_PRIOR = 157
       SDL_SCANCODE_RETURN2 = 158
       SDL_SCANCODE_SEPARATOR = 159
       SDL_SCANCODE_OUT = 160
       SDL_SCANCODE_OPER = 161
       SDL_SCANCODE_CLEARAGAIN = 162
       SDL_SCANCODE_CRSEL = 163
       SDL_SCANCODE_EXSEL = 164

       SDL_SCANCODE_KP_00 = 176
       SDL_SCANCODE_KP_000 = 177
       SDL_SCANCODE_THOUSANDSSEPARATOR = 178
       SDL_SCANCODE_DECIMALSEPARATOR = 179
       SDL_SCANCODE_CURRENCYUNIT = 180
       SDL_SCANCODE_CURRENCYSUBUNIT = 181
       SDL_SCANCODE_KP_LEFTPAREN = 182
       SDL_SCANCODE_KP_RIGHTPAREN = 183
       SDL_SCANCODE_KP_LEFTBRACE = 184
       SDL_SCANCODE_KP_RIGHTBRACE = 185
       SDL_SCANCODE_KP_TAB = 186
       SDL_SCANCODE_KP_BACKSPACE = 187
       SDL_SCANCODE_KP_A = 188
       SDL_SCANCODE_KP_B = 189
       SDL_SCANCODE_KP_C = 190
       SDL_SCANCODE_KP_D = 191
       SDL_SCANCODE_KP_E = 192
       SDL_SCANCODE_KP_F = 193
       SDL_SCANCODE_KP_XOR = 194
       SDL_SCANCODE_KP_POWER = 195
       SDL_SCANCODE_KP_PERCENT = 196
       SDL_SCANCODE_KP_LESS = 197
       SDL_SCANCODE_KP_GREATER = 198
       SDL_SCANCODE_KP_AMPERSAND = 199
       SDL_SCANCODE_KP_DBLAMPERSAND = 200
       SDL_SCANCODE_KP_VERTICALBAR = 201
       SDL_SCANCODE_KP_DBLVERTICALBAR = 202
       SDL_SCANCODE_KP_COLON = 203
       SDL_SCANCODE_KP_HASH = 204
       SDL_SCANCODE_KP_SPACE = 205
       SDL_SCANCODE_KP_AT = 206
       SDL_SCANCODE_KP_EXCLAM = 207
       SDL_SCANCODE_KP_MEMSTORE = 208
       SDL_SCANCODE_KP_MEMRECALL = 209
       SDL_SCANCODE_KP_MEMCLEAR = 210
       SDL_SCANCODE_KP_MEMADD = 211
       SDL_SCANCODE_KP_MEMSUBTRACT = 212
       SDL_SCANCODE_KP_MEMMULTIPLY = 213
       SDL_SCANCODE_KP_MEMDIVIDE = 214
       SDL_SCANCODE_KP_PLUSMINUS = 215
       SDL_SCANCODE_KP_CLEAR = 216
       SDL_SCANCODE_KP_CLEARENTRY = 217
       SDL_SCANCODE_KP_BINARY = 218
       SDL_SCANCODE_KP_OCTAL = 219
       SDL_SCANCODE_KP_DECIMAL = 220
       SDL_SCANCODE_KP_HEXADECIMAL = 221

       SDL_SCANCODE_LCTRL = 224
       SDL_SCANCODE_LSHIFT = 225
       SDL_SCANCODE_LALT = 226
       SDL_SCANCODE_LGUI = 227
       SDL_SCANCODE_RCTRL = 228
       SDL_SCANCODE_RSHIFT = 229
       SDL_SCANCODE_RALT = 230
       SDL_SCANCODE_RGUI = 231

       SDL_SCANCODE_MODE = 257

       SDL_SCANCODE_AUDIONEXT = 258
       SDL_SCANCODE_AUDIOPREV = 259
       SDL_SCANCODE_AUDIOSTOP = 260
       SDL_SCANCODE_AUDIOPLAY = 261
       SDL_SCANCODE_AUDIOMUTE = 262
       SDL_SCANCODE_MEDIASELECT = 263
       SDL_SCANCODE_WWW = 264
       SDL_SCANCODE_MAIL = 265
       SDL_SCANCODE_CALCULATOR = 266
       SDL_SCANCODE_COMPUTER = 267
       SDL_SCANCODE_AC_SEARCH = 268
       SDL_SCANCODE_AC_HOME = 269
       SDL_SCANCODE_AC_BACK = 270
       SDL_SCANCODE_AC_FORWARD = 271
       SDL_SCANCODE_AC_STOP = 272
       SDL_SCANCODE_AC_REFRESH = 273
       SDL_SCANCODE_AC_BOOKMARKS = 274

       SDL_SCANCODE_BRIGHTNESSDOWN = 275
       SDL_SCANCODE_BRIGHTNESSUP = 276
       SDL_SCANCODE_DISPLAYSWITCH = 277
       SDL_SCANCODE_KBDILLUMTOGGLE = 278
       SDL_SCANCODE_KBDILLUMDOWN = 279
       SDL_SCANCODE_KBDILLUMUP = 280
       SDL_SCANCODE_EJECT = 281
       SDL_SCANCODE_SLEEP = 282

       SDL_SCANCODE_APP1 = 283
       SDL_SCANCODE_APP2 = 284

       SDL_SCANCODE_AUDIOREWIND = 285
       SDL_SCANCODE_AUDIOFASTFORWARD = 286

       SDL_NUM_SCANCODES = 51)))


;; SDL_keycode.h

(define SDLK_SCANCODE_MASK (arithmetic-shift 1 30))
(define (SDL_SCANCODE_TO_KEYCODE X)
    (bitwise-ior (cast X _SDL_Scancode _ufixint) SDLK_SCANCODE_MASK))

(define _SDL_Keycode
    (_enum
     `(SDLK_UNKNOWN = 0

       SDLK_RETURN = ,(char->integer #\return)
       SDLK_ESCAPE = ,(char->integer #\033)
       SDLK_BACKSPACE = ,(char->integer #\backspace)
       SDLK_TAB = ,(char->integer #\tab)
       SDLK_SPACE = ,(char->integer #\space)
       SDLK_EXCLAIM = ,(char->integer #\!)
       SDLK_QUOTEDBL = ,(char->integer #\")
       SDLK_HASH = ,(char->integer #\#)
       SDLK_PERCENT = ,(char->integer #\%)
       SDLK_DOLLAR = ,(char->integer #\$)
       SDLK_AMPERSAND = ,(char->integer #\&)
       SDLK_QUOTE = ,(char->integer #\')
       SDLK_LEFTPAREN = ,(char->integer #\()
       SDLK_RIGHTPAREN = ,(char->integer #\))
       SDLK_ASTERISK = ,(char->integer #\*)
       SDLK_PLUS = ,(char->integer #\+)
       SDLK_COMMA = ,(char->integer #\,)
       SDLK_MINUS = ,(char->integer #\-)
       SDLK_PERIOD = ,(char->integer #\.)
       SDLK_SLASH = ,(char->integer #\/)
       SDLK_0 = ,(char->integer #\0)
       SDLK_1 = ,(char->integer #\1)
       SDLK_2 = ,(char->integer #\2)
       SDLK_3 = ,(char->integer #\3)
       SDLK_4 = ,(char->integer #\4)
       SDLK_5 = ,(char->integer #\5)
       SDLK_6 = ,(char->integer #\6)
       SDLK_7 = ,(char->integer #\7)
       SDLK_8 = ,(char->integer #\8)
       SDLK_9 = ,(char->integer #\9)
       SDLK_COLON = ,(char->integer #\:)
       SDLK_SEMICOLON = ,(char->integer #\;)
       SDLK_LESS = ,(char->integer #\<)
       SDLK_EQUALS = ,(char->integer #\=)
       SDLK_GREATER = ,(char->integer #\>)
       SDLK_QUESTION = ,(char->integer #\?)
       SDLK_AT = ,(char->integer #\@)

       SDLK_LEFTBRACKET = ,(char->integer #\[)
       SDLK_BACKSLASH = ,(char->integer #\\)
       SDLK_RIGHTBRACKET = ,(char->integer #\])
       SDLK_CARET = ,(char->integer #\^)
       SDLK_UNDERSCORE = ,(char->integer #\_)
       SDLK_BACKQUOTE = ,(char->integer #\`)
       SDLK_a = ,(char->integer #\a)
       SDLK_b = ,(char->integer #\b)
       SDLK_c = ,(char->integer #\c)
       SDLK_d = ,(char->integer #\d)
       SDLK_e = ,(char->integer #\e)
       SDLK_f = ,(char->integer #\f)
       SDLK_g = ,(char->integer #\g)
       SDLK_h = ,(char->integer #\h)
       SDLK_i = ,(char->integer #\i)
       SDLK_j = ,(char->integer #\j)
       SDLK_k = ,(char->integer #\k)
       SDLK_l = ,(char->integer #\l)
       SDLK_m = ,(char->integer #\m)
       SDLK_n = ,(char->integer #\n)
       SDLK_o = ,(char->integer #\o)
       SDLK_p = ,(char->integer #\p)
       SDLK_q = ,(char->integer #\q)
       SDLK_r = ,(char->integer #\r)
       SDLK_s = ,(char->integer #\s)
       SDLK_t = ,(char->integer #\t)
       SDLK_u = ,(char->integer #\u)
       SDLK_v = ,(char->integer #\v)
       SDLK_w = ,(char->integer #\w)
       SDLK_x = ,(char->integer #\x)
       SDLK_y = ,(char->integer #\y)
       SDLK_z = ,(char->integer #\z)

       SDLK_CAPSLOCK = ,(SDL_SCANCODE_TO_KEYCODE 'SDL_SCANCODE_CAPSLOCK)

       SDLK_F1 = ,(SDL_SCANCODE_TO_KEYCODE 'SDL_SCANCODE_F1)
       SDLK_F2 = ,(SDL_SCANCODE_TO_KEYCODE 'SDL_SCANCODE_F2)
       SDLK_F3 = ,(SDL_SCANCODE_TO_KEYCODE 'SDL_SCANCODE_F3)
       SDLK_F4 = ,(SDL_SCANCODE_TO_KEYCODE 'SDL_SCANCODE_F4)
       SDLK_F5 = ,(SDL_SCANCODE_TO_KEYCODE 'SDL_SCANCODE_F5)
       SDLK_F6 = ,(SDL_SCANCODE_TO_KEYCODE 'SDL_SCANCODE_F6)
       SDLK_F7 = ,(SDL_SCANCODE_TO_KEYCODE 'SDL_SCANCODE_F7)
       SDLK_F8 = ,(SDL_SCANCODE_TO_KEYCODE 'SDL_SCANCODE_F8)
       SDLK_F9 = ,(SDL_SCANCODE_TO_KEYCODE 'SDL_SCANCODE_F9)
       SDLK_F10 = ,(SDL_SCANCODE_TO_KEYCODE 'SDL_SCANCODE_F10)
       SDLK_F11 = ,(SDL_SCANCODE_TO_KEYCODE 'SDL_SCANCODE_F11)
       SDLK_F12 = ,(SDL_SCANCODE_TO_KEYCODE 'SDL_SCANCODE_F12)

       SDLK_PRINTSCREEN = ,(SDL_SCANCODE_TO_KEYCODE 'SDL_SCANCODE_PRINTSCREEN)
       SDLK_SCROLLLOCK = ,(SDL_SCANCODE_TO_KEYCODE 'SDL_SCANCODE_SCROLLLOCK)
       SDLK_PAUSE = ,(SDL_SCANCODE_TO_KEYCODE 'SDL_SCANCODE_PAUSE)
       SDLK_INSERT = ,(SDL_SCANCODE_TO_KEYCODE 'SDL_SCANCODE_INSERT)
       SDLK_HOME = ,(SDL_SCANCODE_TO_KEYCODE 'SDL_SCANCODE_HOME)
       SDLK_PAGEUP = ,(SDL_SCANCODE_TO_KEYCODE 'SDL_SCANCODE_PAGEUP)
       SDLK_DELETE = ,(char->integer #\177)
       SDLK_END = ,(SDL_SCANCODE_TO_KEYCODE 'SDL_SCANCODE_END)
       SDLK_PAGEDOWN = ,(SDL_SCANCODE_TO_KEYCODE 'SDL_SCANCODE_PAGEDOWN)
       SDLK_RIGHT = ,(SDL_SCANCODE_TO_KEYCODE 'SDL_SCANCODE_RIGHT)
       SDLK_LEFT = ,(SDL_SCANCODE_TO_KEYCODE 'SDL_SCANCODE_LEFT)
       SDLK_DOWN = ,(SDL_SCANCODE_TO_KEYCODE 'SDL_SCANCODE_DOWN)
       SDLK_UP = ,(SDL_SCANCODE_TO_KEYCODE 'SDL_SCANCODE_UP)

       SDLK_NUMLOCKCLEAR = ,(SDL_SCANCODE_TO_KEYCODE 'SDL_SCANCODE_NUMLOCKCLEAR)
       SDLK_KP_DIVIDE = ,(SDL_SCANCODE_TO_KEYCODE 'SDL_SCANCODE_KP_DIVIDE)
       SDLK_KP_MULTIPLY = ,(SDL_SCANCODE_TO_KEYCODE 'SDL_SCANCODE_KP_MULTIPLY)
       SDLK_KP_MINUS = ,(SDL_SCANCODE_TO_KEYCODE 'SDL_SCANCODE_KP_MINUS)
       SDLK_KP_PLUS = ,(SDL_SCANCODE_TO_KEYCODE 'SDL_SCANCODE_KP_PLUS)
       SDLK_KP_ENTER = ,(SDL_SCANCODE_TO_KEYCODE 'SDL_SCANCODE_KP_ENTER)
       SDLK_KP_1 = ,(SDL_SCANCODE_TO_KEYCODE 'SDL_SCANCODE_KP_1)
       SDLK_KP_2 = ,(SDL_SCANCODE_TO_KEYCODE 'SDL_SCANCODE_KP_2)
       SDLK_KP_3 = ,(SDL_SCANCODE_TO_KEYCODE 'SDL_SCANCODE_KP_3)
       SDLK_KP_4 = ,(SDL_SCANCODE_TO_KEYCODE 'SDL_SCANCODE_KP_4)
       SDLK_KP_5 = ,(SDL_SCANCODE_TO_KEYCODE 'SDL_SCANCODE_KP_5)
       SDLK_KP_6 = ,(SDL_SCANCODE_TO_KEYCODE 'SDL_SCANCODE_KP_6)
       SDLK_KP_7 = ,(SDL_SCANCODE_TO_KEYCODE 'SDL_SCANCODE_KP_7)
       SDLK_KP_8 = ,(SDL_SCANCODE_TO_KEYCODE 'SDL_SCANCODE_KP_8)
       SDLK_KP_9 = ,(SDL_SCANCODE_TO_KEYCODE 'SDL_SCANCODE_KP_9)
       SDLK_KP_0 = ,(SDL_SCANCODE_TO_KEYCODE 'SDL_SCANCODE_KP_0)
       SDLK_KP_PERIOD = ,(SDL_SCANCODE_TO_KEYCODE 'SDL_SCANCODE_KP_PERIOD)

       SDLK_APPLICATION = ,(SDL_SCANCODE_TO_KEYCODE 'SDL_SCANCODE_APPLICATION)
       SDLK_POWER = ,(SDL_SCANCODE_TO_KEYCODE 'SDL_SCANCODE_POWER)
       SDLK_KP_EQUALS = ,(SDL_SCANCODE_TO_KEYCODE 'SDL_SCANCODE_KP_EQUALS)
       SDLK_F13 = ,(SDL_SCANCODE_TO_KEYCODE 'SDL_SCANCODE_F13)
       SDLK_F14 = ,(SDL_SCANCODE_TO_KEYCODE 'SDL_SCANCODE_F14)
       SDLK_F15 = ,(SDL_SCANCODE_TO_KEYCODE 'SDL_SCANCODE_F15)
       SDLK_F16 = ,(SDL_SCANCODE_TO_KEYCODE 'SDL_SCANCODE_F16)
       SDLK_F17 = ,(SDL_SCANCODE_TO_KEYCODE 'SDL_SCANCODE_F17)
       SDLK_F18 = ,(SDL_SCANCODE_TO_KEYCODE 'SDL_SCANCODE_F18)
       SDLK_F19 = ,(SDL_SCANCODE_TO_KEYCODE 'SDL_SCANCODE_F19)
       SDLK_F20 = ,(SDL_SCANCODE_TO_KEYCODE 'SDL_SCANCODE_F20)
       SDLK_F21 = ,(SDL_SCANCODE_TO_KEYCODE 'SDL_SCANCODE_F21)
       SDLK_F22 = ,(SDL_SCANCODE_TO_KEYCODE 'SDL_SCANCODE_F22)
       SDLK_F23 = ,(SDL_SCANCODE_TO_KEYCODE 'SDL_SCANCODE_F23)
       SDLK_F24 = ,(SDL_SCANCODE_TO_KEYCODE 'SDL_SCANCODE_F24)
       SDLK_EXECUTE = ,(SDL_SCANCODE_TO_KEYCODE 'SDL_SCANCODE_EXECUTE)
       SDLK_HELP = ,(SDL_SCANCODE_TO_KEYCODE 'SDL_SCANCODE_HELP)
       SDLK_MENU = ,(SDL_SCANCODE_TO_KEYCODE 'SDL_SCANCODE_MENU)
       SDLK_SELECT = ,(SDL_SCANCODE_TO_KEYCODE 'SDL_SCANCODE_SELECT)
       SDLK_STOP = ,(SDL_SCANCODE_TO_KEYCODE 'SDL_SCANCODE_STOP)
       SDLK_AGAIN = ,(SDL_SCANCODE_TO_KEYCODE 'SDL_SCANCODE_AGAIN)
       SDLK_UNDO = ,(SDL_SCANCODE_TO_KEYCODE 'SDL_SCANCODE_UNDO)
       SDLK_CUT = ,(SDL_SCANCODE_TO_KEYCODE 'SDL_SCANCODE_CUT)
       SDLK_COPY = ,(SDL_SCANCODE_TO_KEYCODE 'SDL_SCANCODE_COPY)
       SDLK_PASTE = ,(SDL_SCANCODE_TO_KEYCODE 'SDL_SCANCODE_PASTE)
       SDLK_FIND = ,(SDL_SCANCODE_TO_KEYCODE 'SDL_SCANCODE_FIND)
       SDLK_MUTE = ,(SDL_SCANCODE_TO_KEYCODE 'SDL_SCANCODE_MUTE)
       SDLK_VOLUMEUP = ,(SDL_SCANCODE_TO_KEYCODE 'SDL_SCANCODE_VOLUMEUP)
       SDLK_VOLUMEDOWN = ,(SDL_SCANCODE_TO_KEYCODE 'SDL_SCANCODE_VOLUMEDOWN)
       SDLK_KP_COMMA = ,(SDL_SCANCODE_TO_KEYCODE 'SDL_SCANCODE_KP_COMMA)
       SDLK_KP_EQUALSAS400 = ,(SDL_SCANCODE_TO_KEYCODE 'SDL_SCANCODE_KP_EQUALSAS400)

       SDLK_ALTERASE = ,(SDL_SCANCODE_TO_KEYCODE 'SDL_SCANCODE_ALTERASE)
       SDLK_SYSREQ = ,(SDL_SCANCODE_TO_KEYCODE 'SDL_SCANCODE_SYSREQ)
       SDLK_CANCEL = ,(SDL_SCANCODE_TO_KEYCODE 'SDL_SCANCODE_CANCEL)
       SDLK_CLEAR = ,(SDL_SCANCODE_TO_KEYCODE 'SDL_SCANCODE_CLEAR)
       SDLK_PRIOR = ,(SDL_SCANCODE_TO_KEYCODE 'SDL_SCANCODE_PRIOR)
       SDLK_RETURN2 = ,(SDL_SCANCODE_TO_KEYCODE 'SDL_SCANCODE_RETURN2)
       SDLK_SEPARATOR = ,(SDL_SCANCODE_TO_KEYCODE 'SDL_SCANCODE_SEPARATOR)
       SDLK_OUT = ,(SDL_SCANCODE_TO_KEYCODE 'SDL_SCANCODE_OUT)
       SDLK_OPER = ,(SDL_SCANCODE_TO_KEYCODE 'SDL_SCANCODE_OPER)
       SDLK_CLEARAGAIN = ,(SDL_SCANCODE_TO_KEYCODE 'SDL_SCANCODE_CLEARAGAIN)
       SDLK_CRSEL = ,(SDL_SCANCODE_TO_KEYCODE 'SDL_SCANCODE_CRSEL)
       SDLK_EXSEL = ,(SDL_SCANCODE_TO_KEYCODE 'SDL_SCANCODE_EXSEL)

       SDLK_KP_00 = ,(SDL_SCANCODE_TO_KEYCODE 'SDL_SCANCODE_KP_00)
       SDLK_KP_000 = ,(SDL_SCANCODE_TO_KEYCODE 'SDL_SCANCODE_KP_000)
       SDLK_THOUSANDSSEPARATOR = ,(SDL_SCANCODE_TO_KEYCODE 'SDL_SCANCODE_THOUSANDSSEPARATOR)
       SDLK_DECIMALSEPARATOR = ,(SDL_SCANCODE_TO_KEYCODE 'SDL_SCANCODE_DECIMALSEPARATOR)
       SDLK_CURRENCYUNIT = ,(SDL_SCANCODE_TO_KEYCODE 'SDL_SCANCODE_CURRENCYUNIT)
       SDLK_CURRENCYSUBUNIT = ,(SDL_SCANCODE_TO_KEYCODE 'SDL_SCANCODE_CURRENCYSUBUNIT)
       SDLK_KP_LEFTPAREN = ,(SDL_SCANCODE_TO_KEYCODE 'SDL_SCANCODE_KP_LEFTPAREN)
       SDLK_KP_RIGHTPAREN = ,(SDL_SCANCODE_TO_KEYCODE 'SDL_SCANCODE_KP_RIGHTPAREN)
       SDLK_KP_LEFTBRACE = ,(SDL_SCANCODE_TO_KEYCODE 'SDL_SCANCODE_KP_LEFTBRACE)
       SDLK_KP_RIGHTBRACE = ,(SDL_SCANCODE_TO_KEYCODE 'SDL_SCANCODE_KP_RIGHTBRACE)
       SDLK_KP_TAB = ,(SDL_SCANCODE_TO_KEYCODE 'SDL_SCANCODE_KP_TAB)
       SDLK_KP_BACKSPACE = ,(SDL_SCANCODE_TO_KEYCODE 'SDL_SCANCODE_KP_BACKSPACE)
       SDLK_KP_A = ,(SDL_SCANCODE_TO_KEYCODE 'SDL_SCANCODE_KP_A)
       SDLK_KP_B = ,(SDL_SCANCODE_TO_KEYCODE 'SDL_SCANCODE_KP_B)
       SDLK_KP_C = ,(SDL_SCANCODE_TO_KEYCODE 'SDL_SCANCODE_KP_C)
       SDLK_KP_D = ,(SDL_SCANCODE_TO_KEYCODE 'SDL_SCANCODE_KP_D)
       SDLK_KP_E = ,(SDL_SCANCODE_TO_KEYCODE 'SDL_SCANCODE_KP_E)
       SDLK_KP_F = ,(SDL_SCANCODE_TO_KEYCODE 'SDL_SCANCODE_KP_F)
       SDLK_KP_XOR = ,(SDL_SCANCODE_TO_KEYCODE 'SDL_SCANCODE_KP_XOR)
       SDLK_KP_POWER = ,(SDL_SCANCODE_TO_KEYCODE 'SDL_SCANCODE_KP_POWER)
       SDLK_KP_PERCENT = ,(SDL_SCANCODE_TO_KEYCODE 'SDL_SCANCODE_KP_PERCENT)
       SDLK_KP_LESS = ,(SDL_SCANCODE_TO_KEYCODE 'SDL_SCANCODE_KP_LESS)
       SDLK_KP_GREATER = ,(SDL_SCANCODE_TO_KEYCODE 'SDL_SCANCODE_KP_GREATER)
       SDLK_KP_AMPERSAND = ,(SDL_SCANCODE_TO_KEYCODE 'SDL_SCANCODE_KP_AMPERSAND)
       SDLK_KP_DBLAMPERSAND = ,(SDL_SCANCODE_TO_KEYCODE 'SDL_SCANCODE_KP_DBLAMPERSAND)
       SDLK_KP_VERTICALBAR = ,(SDL_SCANCODE_TO_KEYCODE 'SDL_SCANCODE_KP_VERTICALBAR)
       SDLK_KP_DBLVERTICALBAR = ,(SDL_SCANCODE_TO_KEYCODE 'SDL_SCANCODE_KP_DBLVERTICALBAR)
       SDLK_KP_COLON = ,(SDL_SCANCODE_TO_KEYCODE 'SDL_SCANCODE_KP_COLON)
       SDLK_KP_HASH = ,(SDL_SCANCODE_TO_KEYCODE 'SDL_SCANCODE_KP_HASH)
       SDLK_KP_SPACE = ,(SDL_SCANCODE_TO_KEYCODE 'SDL_SCANCODE_KP_SPACE)
       SDLK_KP_AT = ,(SDL_SCANCODE_TO_KEYCODE 'SDL_SCANCODE_KP_AT)
       SDLK_KP_EXCLAM = ,(SDL_SCANCODE_TO_KEYCODE 'SDL_SCANCODE_KP_EXCLAM)
       SDLK_KP_MEMSTORE = ,(SDL_SCANCODE_TO_KEYCODE 'SDL_SCANCODE_KP_MEMSTORE)
       SDLK_KP_MEMRECALL = ,(SDL_SCANCODE_TO_KEYCODE 'SDL_SCANCODE_KP_MEMRECALL)
       SDLK_KP_MEMCLEAR = ,(SDL_SCANCODE_TO_KEYCODE 'SDL_SCANCODE_KP_MEMCLEAR)
       SDLK_KP_MEMADD = ,(SDL_SCANCODE_TO_KEYCODE 'SDL_SCANCODE_KP_MEMADD)
       SDLK_KP_MEMSUBTRACT = ,(SDL_SCANCODE_TO_KEYCODE 'SDL_SCANCODE_KP_MEMSUBTRACT)
       SDLK_KP_MEMMULTIPLY = ,(SDL_SCANCODE_TO_KEYCODE 'SDL_SCANCODE_KP_MEMMULTIPLY)
       SDLK_KP_MEMDIVIDE = ,(SDL_SCANCODE_TO_KEYCODE 'SDL_SCANCODE_KP_MEMDIVIDE)
       SDLK_KP_PLUSMINUS = ,(SDL_SCANCODE_TO_KEYCODE 'SDL_SCANCODE_KP_PLUSMINUS)
       SDLK_KP_CLEAR = ,(SDL_SCANCODE_TO_KEYCODE 'SDL_SCANCODE_KP_CLEAR)
       SDLK_KP_CLEARENTRY = ,(SDL_SCANCODE_TO_KEYCODE 'SDL_SCANCODE_KP_CLEARENTRY)
       SDLK_KP_BINARY = ,(SDL_SCANCODE_TO_KEYCODE 'SDL_SCANCODE_KP_BINARY)
       SDLK_KP_OCTAL = ,(SDL_SCANCODE_TO_KEYCODE 'SDL_SCANCODE_KP_OCTAL)
       SDLK_KP_DECIMAL = ,(SDL_SCANCODE_TO_KEYCODE 'SDL_SCANCODE_KP_DECIMAL)
       SDLK_KP_HEXADECIMAL = ,(SDL_SCANCODE_TO_KEYCODE 'SDL_SCANCODE_KP_HEXADECIMAL)

       SDLK_LCTRL = ,(SDL_SCANCODE_TO_KEYCODE 'SDL_SCANCODE_LCTRL)
       SDLK_LSHIFT = ,(SDL_SCANCODE_TO_KEYCODE 'SDL_SCANCODE_LSHIFT)
       SDLK_LALT = ,(SDL_SCANCODE_TO_KEYCODE 'SDL_SCANCODE_LALT)
       SDLK_LGUI = ,(SDL_SCANCODE_TO_KEYCODE 'SDL_SCANCODE_LGUI)
       SDLK_RCTRL = ,(SDL_SCANCODE_TO_KEYCODE 'SDL_SCANCODE_RCTRL)
       SDLK_RSHIFT = ,(SDL_SCANCODE_TO_KEYCODE 'SDL_SCANCODE_RSHIFT)
       SDLK_RALT = ,(SDL_SCANCODE_TO_KEYCODE 'SDL_SCANCODE_RALT)
       SDLK_RGUI = ,(SDL_SCANCODE_TO_KEYCODE 'SDL_SCANCODE_RGUI)

       SDLK_MODE = ,(SDL_SCANCODE_TO_KEYCODE 'SDL_SCANCODE_MODE)

       SDLK_AUDIONEXT = ,(SDL_SCANCODE_TO_KEYCODE 'SDL_SCANCODE_AUDIONEXT)
       SDLK_AUDIOPREV = ,(SDL_SCANCODE_TO_KEYCODE 'SDL_SCANCODE_AUDIOPREV)
       SDLK_AUDIOSTOP = ,(SDL_SCANCODE_TO_KEYCODE 'SDL_SCANCODE_AUDIOSTOP)
       SDLK_AUDIOPLAY = ,(SDL_SCANCODE_TO_KEYCODE 'SDL_SCANCODE_AUDIOPLAY)
       SDLK_AUDIOMUTE = ,(SDL_SCANCODE_TO_KEYCODE 'SDL_SCANCODE_AUDIOMUTE)
       SDLK_MEDIASELECT = ,(SDL_SCANCODE_TO_KEYCODE 'SDL_SCANCODE_MEDIASELECT)
       SDLK_WWW = ,(SDL_SCANCODE_TO_KEYCODE 'SDL_SCANCODE_WWW)
       SDLK_MAIL = ,(SDL_SCANCODE_TO_KEYCODE 'SDL_SCANCODE_MAIL)
       SDLK_CALCULATOR = ,(SDL_SCANCODE_TO_KEYCODE 'SDL_SCANCODE_CALCULATOR)
       SDLK_COMPUTER = ,(SDL_SCANCODE_TO_KEYCODE 'SDL_SCANCODE_COMPUTER)
       SDLK_AC_SEARCH = ,(SDL_SCANCODE_TO_KEYCODE 'SDL_SCANCODE_AC_SEARCH)
       SDLK_AC_HOME = ,(SDL_SCANCODE_TO_KEYCODE 'SDL_SCANCODE_AC_HOME)
       SDLK_AC_BACK = ,(SDL_SCANCODE_TO_KEYCODE 'SDL_SCANCODE_AC_BACK)
       SDLK_AC_FORWARD = ,(SDL_SCANCODE_TO_KEYCODE 'SDL_SCANCODE_AC_FORWARD)
       SDLK_AC_STOP = ,(SDL_SCANCODE_TO_KEYCODE 'SDL_SCANCODE_AC_STOP)
       SDLK_AC_REFRESH = ,(SDL_SCANCODE_TO_KEYCODE 'SDL_SCANCODE_AC_REFRESH)
       SDLK_AC_BOOKMARKS = ,(SDL_SCANCODE_TO_KEYCODE 'SDL_SCANCODE_AC_BOOKMARKS)

       SDLK_BRIGHTNESSDOWN = ,(SDL_SCANCODE_TO_KEYCODE 'SDL_SCANCODE_BRIGHTNESSDOWN)
       SDLK_BRIGHTNESSUP = ,(SDL_SCANCODE_TO_KEYCODE 'SDL_SCANCODE_BRIGHTNESSUP)
       SDLK_DISPLAYSWITCH = ,(SDL_SCANCODE_TO_KEYCODE 'SDL_SCANCODE_DISPLAYSWITCH)
       SDLK_KBDILLUMTOGGLE = ,(SDL_SCANCODE_TO_KEYCODE 'SDL_SCANCODE_KBDILLUMTOGGLE)
       SDLK_KBDILLUMDOWN = ,(SDL_SCANCODE_TO_KEYCODE 'SDL_SCANCODE_KBDILLUMDOWN)
       SDLK_KBDILLUMUP = ,(SDL_SCANCODE_TO_KEYCODE 'SDL_SCANCODE_KBDILLUMUP)
       SDLK_EJECT = ,(SDL_SCANCODE_TO_KEYCODE 'SDL_SCANCODE_EJECT)
       SDLK_SLEEP = ,(SDL_SCANCODE_TO_KEYCODE 'SDL_SCANCODE_SLEEP)
       SDLK_APP1 = ,(SDL_SCANCODE_TO_KEYCODE 'SDL_SCANCODE_APP1)
       SDLK_APP2 = ,(SDL_SCANCODE_TO_KEYCODE 'SDL_SCANCODE_APP2)

       SDLK_AUDIOREWIND = ,(SDL_SCANCODE_TO_KEYCODE 'SDL_SCANCODE_AUDIOREWIND)
       SDLK_AUDIOFASTFORWARD = ,(SDL_SCANCODE_TO_KEYCODE 'SDL_SCANCODE_AUDIOFASTFORWARD))
     _sint32))

(define _SDL_Keymod
    (_bitmask
     '(KMOD_NONE = #x0000
       KMOD_LSHIFT = #x0001
       KMOD_RSHIFT = #x0002
       KMOD_LCTRL = #x0040
       KMOD_RCTRL = #x0080
       KMOD_LALT = #x0100
       KMOD_RALT = #x0200
       KMOD_LGUI = #x0400
       KMOD_RGUI = #x0800
       KMOD_NUM = #x1000
       KMOD_CAPS = #x2000
       KMOD_MODE = #x4000
       KMOD_RESERVED = #x8000

       KMOD_CTRL = #x00c0
       KMOD_SHIFT = #x0003
       KMOD_ALT = #x0300
       KMOD_GUI = #x0c00)))


;; SDL_rect.h

(define-cstruct _SDL_Point
    ([x _int]
     [y _int]))

(define _SDL_Point* _SDL_Point-pointer)
(define _SDL_Point*/null _SDL_Point-pointer/null)

(define-cstruct _SDL_Rect
    ([x _int]
     [y _int]
     [w _int]
     [h _int]))

(define _SDL_Rect* _SDL_Rect-pointer)
(define _SDL_Rect*/null _SDL_Rect-pointer/null)

(begin-encourage-inline
    (define (SDL_PointInRect p r)
        (and
         (>= (SDL_Point-x p) (SDL_Rect-x r))
         (< (SDL_Point-x p) (+ (SDL_Rect-x r) (SDL_Rect-w r)))
         (>= (SDL_Point-y p) (SDL_Rect-y r))
         (< (SDL_Point-y p) (+ (SDL_Rect-y r) (SDL_Rect-h r)))))

    (define (SDL_RectEmpty r)
        (or
         (not r)
         (<= (SDL_Rect-w r) 0)
         (<= (SDL_Rect-h r) 0)))

    (define (SDL_RectEquals a b)
        (and
         a b
         (= (SDL_Rect-x a) (SDL_Rect-x b))
         (= (SDL_Rect-y a) (SDL_Rect-y b))
         (= (SDL_Rect-w a) (SDL_Rect-w b))
         (= (SDL_Rect-h a) (SDL_Rect-h b)))))

(define-sdl2 SDL_HasIntersection (_fun _SDL_Rect* _SDL_Rect* -> _SDL_bool))

(define-sdl2 SDL_IntersectRect (_fun _SDL_Rect* _SDL_Rect* _SDL_Rect* -> _SDL_bool))

(define-sdl2 SDL_UnionRect (_fun _SDL_Rect* _SDL_Rect* _SDL_Rect* -> _void))

(define-sdl2 SDL_EnclosePoints (_fun _SDL_Point* _int _SDL_Rect*/null _SDL_Rect* -> _SDL_bool))

(define-sdl2 SDL_IntersectRectAndLine (_fun _SDL_Rect* _int* _int* _int* _int* -> _SDL_bool))


;; SDL_pixels.h

(define SDL_ALPHA_OPAQUE 255)
(define SDL_ALPHA_TRANSPARENT 0)

(define SDL_PIXELTYPE_UNKNOWN 0)
(define SDL_PIXELTYPE_INDEX1 1)
(define SDL_PIXELTYPE_INDEX4 2)
(define SDL_PIXELTYPE_INDEX8 3)
(define SDL_PIXELTYPE_PACKED8 4)
(define SDL_PIXELTYPE_PACKED16 5)
(define SDL_PIXELTYPE_PACKED32 6)
(define SDL_PIXELTYPE_ARRAYU8 7)
(define SDL_PIXELTYPE_ARRAYU16 8)
(define SDL_PIXELTYPE_ARRAYU32 9)
(define SDL_PIXELTYPE_ARRAYF16 10)
(define SDL_PIXELTYPE_ARRAYF32 11)

(define SDL_BITMAPORDER_NONE 0)
(define SDL_BITMAPORDER_4321 1)
(define SDL_BITMAPORDER_1234 2)

(define SDL_PACKEDORDER_NONE 0)
(define SDL_PACKEDORDER_XRGB 1)
(define SDL_PACKEDORDER_RGBX 2)
(define SDL_PACKEDORDER_ARGB 3)
(define SDL_PACKEDORDER_RGBA 4)
(define SDL_PACKEDORDER_XBGR 5)
(define SDL_PACKEDORDER_BGRX 6)
(define SDL_PACKEDORDER_ABGR 7)
(define SDL_PACKEDORDER_BGRA 8)

(define SDL_ARRAYORDER_NONE 0)
(define SDL_ARRAYORDER_RGB 1)
(define SDL_ARRAYORDER_RGBA 2)
(define SDL_ARRAYORDER_ARGB 3)
(define SDL_ARRAYORDER_BGR 4)
(define SDL_ARRAYORDER_BGRA 5)
(define SDL_ARRAYORDER_ABGR 6)

(define SDL_PACKEDLAYOUT_NONE 0)
(define SDL_PACKEDLAYOUT_332 1)
(define SDL_PACKEDLAYOUT_4444 2)
(define SDL_PACKEDLAYOUT_1555 3)
(define SDL_PACKEDLAYOUT_5551 4)
(define SDL_PACKEDLAYOUT_565 5)
(define SDL_PACKEDLAYOUT_8888 6)
(define SDL_PACKEDLAYOUT_2101010 7)
(define SDL_PACKEDLAYOUT_1010102 8)

(define SDL_DEFINE_PIXELFOURCC SDL_FOURCC)

(define (SDL_DEFINE_PIXELFORMAT type order layout bits bytes)
    (bitwise-ior
     (arithmetic-shift 1 28)
     (arithmetic-shift type 24)
     (arithmetic-shift order 20)
     (arithmetic-shift layout 16)
     (arithmetic-shift bits 8)
     (arithmetic-shift bytes 0)))

(define SDL_PIXELFORMAT_UNKNOWN 0)
(define SDL_PIXELFORMAT_INDEX1LSB
    (SDL_DEFINE_PIXELFORMAT
     SDL_PIXELTYPE_INDEX1
     SDL_BITMAPORDER_4321
     0 1 0))
(define SDL_PIXELFORMAT_INDEX1MSB
    (SDL_DEFINE_PIXELFORMAT
     SDL_PIXELTYPE_INDEX1
     SDL_BITMAPORDER_1234
     0 1 0))
(define SDL_PIXELFORMAT_INDEX4LSB
    (SDL_DEFINE_PIXELFORMAT
     SDL_PIXELTYPE_INDEX4
     SDL_BITMAPORDER_4321
     0 4 0))
(define SDL_PIXELFORMAT_INDEX4MSB
    (SDL_DEFINE_PIXELFORMAT
     SDL_PIXELTYPE_INDEX4
     SDL_BITMAPORDER_1234
     0 4 0))
(define SDL_PIXELFORMAT_INDEX8
    (SDL_DEFINE_PIXELFORMAT
     SDL_PIXELTYPE_INDEX8
     0 0 8 1))
(define SDL_PIXELFORMAT_RGB332
    (SDL_DEFINE_PIXELFORMAT
     SDL_PIXELTYPE_PACKED8
     SDL_PACKEDORDER_XRGB
     SDL_PACKEDLAYOUT_332
     8 1))
(define SDL_PIXELFORMAT_RGB444
    (SDL_DEFINE_PIXELFORMAT
     SDL_PIXELTYPE_PACKED16
     SDL_PACKEDORDER_XRGB
     SDL_PACKEDLAYOUT_4444
     12 2))
(define SDL_PIXELFORMAT_RGB555
    (SDL_DEFINE_PIXELFORMAT
     SDL_PIXELTYPE_PACKED16
     SDL_PACKEDORDER_XRGB
     SDL_PACKEDLAYOUT_1555
     15 2))
(define SDL_PIXELFORMAT_BGR555
    (SDL_DEFINE_PIXELFORMAT
     SDL_PIXELTYPE_PACKED16
     SDL_PACKEDORDER_XBGR
     SDL_PACKEDLAYOUT_1555
     15 2))
(define SDL_PIXELFORMAT_ARGB4444
    (SDL_DEFINE_PIXELFORMAT
     SDL_PIXELTYPE_PACKED16
     SDL_PACKEDORDER_ARGB
     SDL_PACKEDLAYOUT_4444
     16 2))
(define SDL_PIXELFORMAT_RGBA4444
    (SDL_DEFINE_PIXELFORMAT
     SDL_PIXELTYPE_PACKED16
     SDL_PACKEDORDER_RGBA
     SDL_PACKEDLAYOUT_4444
     16 2))
(define SDL_PIXELFORMAT_ABGR4444
    (SDL_DEFINE_PIXELFORMAT
     SDL_PIXELTYPE_PACKED16
     SDL_PACKEDORDER_ABGR
     SDL_PACKEDLAYOUT_4444
     16 2))
(define SDL_PIXELFORMAT_BGRA4444
    (SDL_DEFINE_PIXELFORMAT
     SDL_PIXELTYPE_PACKED16
     SDL_PACKEDORDER_BGRA
     SDL_PACKEDLAYOUT_4444
     16 2))
(define SDL_PIXELFORMAT_ARGB1555
    (SDL_DEFINE_PIXELFORMAT
     SDL_PIXELTYPE_PACKED16
     SDL_PACKEDORDER_ARGB
     SDL_PACKEDLAYOUT_1555
     16 2))
(define SDL_PIXELFORMAT_RGBA5551
    (SDL_DEFINE_PIXELFORMAT
     SDL_PIXELTYPE_PACKED16
     SDL_PACKEDORDER_RGBA
     SDL_PACKEDLAYOUT_5551
     16 2))
(define SDL_PIXELFORMAT_ABGR1555
    (SDL_DEFINE_PIXELFORMAT
     SDL_PIXELTYPE_PACKED16
     SDL_PACKEDORDER_ABGR
     SDL_PACKEDLAYOUT_1555
     16 2))
(define SDL_PIXELFORMAT_BGRA5551
    (SDL_DEFINE_PIXELFORMAT
     SDL_PIXELTYPE_PACKED16
     SDL_PACKEDORDER_BGRA
     SDL_PACKEDLAYOUT_5551
     16 2))
(define SDL_PIXELFORMAT_RGB565
    (SDL_DEFINE_PIXELFORMAT
     SDL_PIXELTYPE_PACKED16
     SDL_PACKEDORDER_XRGB
     SDL_PACKEDLAYOUT_565
     16 2))
(define SDL_PIXELFORMAT_BGR565
    (SDL_DEFINE_PIXELFORMAT
     SDL_PIXELTYPE_PACKED16
     SDL_PACKEDORDER_XBGR
     SDL_PACKEDLAYOUT_565
     16 2))
(define SDL_PIXELFORMAT_RGB24
    (SDL_DEFINE_PIXELFORMAT
     SDL_PIXELTYPE_ARRAYU8
     SDL_ARRAYORDER_RGB
     0 24 3))
(define SDL_PIXELFORMAT_BGR24
    (SDL_DEFINE_PIXELFORMAT
     SDL_PIXELTYPE_ARRAYU8
     SDL_ARRAYORDER_BGR
     0 24 3))
(define SDL_PIXELFORMAT_RGB888
    (SDL_DEFINE_PIXELFORMAT
     SDL_PIXELTYPE_PACKED32
     SDL_PACKEDORDER_XRGB
     SDL_PACKEDLAYOUT_8888
     24 4))
(define SDL_PIXELFORMAT_RGBX8888
    (SDL_DEFINE_PIXELFORMAT
     SDL_PIXELTYPE_PACKED32
     SDL_PACKEDORDER_RGBX
     SDL_PACKEDLAYOUT_8888
     24 4))
(define SDL_PIXELFORMAT_BGR888
    (SDL_DEFINE_PIXELFORMAT
     SDL_PIXELTYPE_PACKED32
     SDL_PACKEDORDER_XBGR
     SDL_PACKEDLAYOUT_8888
     24 4))
(define SDL_PIXELFORMAT_BGRX8888
    (SDL_DEFINE_PIXELFORMAT
     SDL_PIXELTYPE_PACKED32
     SDL_PACKEDORDER_BGRX
     SDL_PACKEDLAYOUT_8888
     24 4))
(define SDL_PIXELFORMAT_ARGB8888
    (SDL_DEFINE_PIXELFORMAT
     SDL_PIXELTYPE_PACKED32
     SDL_PACKEDORDER_ARGB
     SDL_PACKEDLAYOUT_8888
     32 4))
(define SDL_PIXELFORMAT_RGBA8888
    (SDL_DEFINE_PIXELFORMAT
     SDL_PIXELTYPE_PACKED32
     SDL_PACKEDORDER_RGBA
     SDL_PACKEDLAYOUT_8888
     32 4))
(define SDL_PIXELFORMAT_ABGR8888
    (SDL_DEFINE_PIXELFORMAT
     SDL_PIXELTYPE_PACKED32
     SDL_PACKEDORDER_ABGR
     SDL_PACKEDLAYOUT_8888
     32 4))
(define SDL_PIXELFORMAT_BGRA8888
    (SDL_DEFINE_PIXELFORMAT
     SDL_PIXELTYPE_PACKED32
     SDL_PACKEDORDER_BGRA
     SDL_PACKEDLAYOUT_8888
     32 4))
(define SDL_PIXELFORMAT_ARGB2101010
    (SDL_DEFINE_PIXELFORMAT
     SDL_PIXELTYPE_PACKED32
     SDL_PACKEDORDER_ARGB
     SDL_PACKEDLAYOUT_2101010
     32 4))

(define SDL_PIXELFORMAT_RGBA32
    (if (system-big-endian?)
        SDL_PIXELFORMAT_RGBA8888
        SDL_PIXELFORMAT_ABGR8888))
(define SDL_PIXELFORMAT_ARGB32
    (if (system-big-endian?)
        SDL_PIXELFORMAT_ARGB8888
        SDL_PIXELFORMAT_BGRA8888))
(define SDL_PIXELFORMAT_BGRA32
    (if (system-big-endian?)
        SDL_PIXELFORMAT_BGRA8888
        SDL_PIXELFORMAT_ARGB8888))
(define SDL_PIXELFORMAT_ABGR32
    (if (system-big-endian?)
        SDL_PIXELFORMAT_ABGR8888
        SDL_PIXELFORMAT_RGBA8888))

(define SDL_PIXELFORMAT_YV12
    (SDL_DEFINE_PIXELFOURCC #\Y  #\V  #\1  #\2))
(define SDL_PIXELFORMAT_IYUV
    (SDL_DEFINE_PIXELFOURCC #\I  #\Y  #\U  #\V))
(define SDL_PIXELFORMAT_YUY2
    (SDL_DEFINE_PIXELFOURCC #\Y  #\U  #\Y  #\2))
(define SDL_PIXELFORMAT_UYVY
    (SDL_DEFINE_PIXELFOURCC #\U  #\Y  #\V  #\Y))
(define SDL_PIXELFORMAT_YVYU
    (SDL_DEFINE_PIXELFOURCC #\Y  #\V  #\Y  #\U))
(define SDL_PIXELFORMAT_NV12
    (SDL_DEFINE_PIXELFOURCC #\N  #\V  #\1  #\2))
(define SDL_PIXELFORMAT_NV21
    (SDL_DEFINE_PIXELFOURCC #\N  #\V  #\2  #\1))
(define SDL_PIXELFORMAT_EXTERNAL_OES
    (SDL_DEFINE_PIXELFOURCC #\O  #\E  #\S  #\ ))

(define (SDL_PIXELFLAG X) (bitwise-and (arithmetic-shift X -28) #x0F))
(define (SDL_PIXELTYPE X) (bitwise-and (arithmetic-shift X -24) #x0F))
(define (SDL_PIXELORDER X) (bitwise-and (arithmetic-shift X -20) #x0F))
(define (SDL_PIXELLAYOUT X) (bitwise-and (arithmetic-shift X -16) #x0F))
(define (SDL_BITSPERPIXEL X) (bitwise-and (arithmetic-shift X -8) #x0F))

(define (SDL_ISPIXELFORMAT_FOURCC format)
    (and (not (zero? format)) (not (= (SDL_PIXELFLAG format) 1))))

(define (SDL_BYTESPERPIXEL X)
    (if (SDL_ISPIXELFORMAT_FOURCC X)
        (if (or (= X SDL_PIXELFORMAT_YUY2)
                (= X SDL_PIXELFORMAT_UYVY)
                (= X SDL_PIXELFORMAT_YVYU))
            2 1)
        (bitwise-and (arithmetic-shift X 0) #xFF)))

(define (SDL_ISPIXELFORMAT_INDEXED format)
    (and
     (not (SDL_ISPIXELFORMAT_FOURCC format))
     (or
      (= (SDL_PIXELTYPE format) SDL_PIXELTYPE_INDEX1)
      (= (SDL_PIXELTYPE format) SDL_PIXELTYPE_INDEX4)
      (= (SDL_PIXELTYPE format) SDL_PIXELTYPE_INDEX8))))

(define (SDL_ISPIXELFORMAT_PACKED format)
    (and
     (not (SDL_ISPIXELFORMAT_FOURCC format))
     (or
      (= (SDL_PIXELTYPE format) SDL_PIXELTYPE_PACKED8)
      (= (SDL_PIXELTYPE format) SDL_PIXELTYPE_PACKED16)
      (= (SDL_PIXELTYPE format) SDL_PIXELTYPE_PACKED32))))

(define (SDL_ISPIXELFORMAT_ARRAY format)
    (and
     (not (SDL_ISPIXELFORMAT_FOURCC format))
     (or
      (= (SDL_PIXELTYPE format) SDL_PIXELTYPE_ARRAYU8)
      (= (SDL_PIXELTYPE format) SDL_PIXELTYPE_ARRAYU16)
      (= (SDL_PIXELTYPE format) SDL_PIXELTYPE_ARRAYU32)
      (= (SDL_PIXELTYPE format) SDL_PIXELTYPE_ARRAYF16)
      (= (SDL_PIXELTYPE format) SDL_PIXELTYPE_ARRAYF32))))

(define (SDL_ISPIXELFORMAT_ALPHA format)
    (or
     (and
      (SDL_ISPIXELFORMAT_FOURCC format)
      (or
       (= (SDL_PIXELORDER format) SDL_PACKEDORDER_ARGB)
       (= (SDL_PIXELORDER format) SDL_PACKEDORDER_RGBA)
       (= (SDL_PIXELORDER format) SDL_PACKEDORDER_ABGR)
       (= (SDL_PIXELORDER format) SDL_PACKEDORDER_BGRA)))
     (and
      (SDL_ISPIXELFORMAT_ARRAY format)
      (or
       (= (SDL_PIXELORDER format) SDL_PACKEDORDER_ARGB)
       (= (SDL_PIXELORDER format) SDL_PACKEDORDER_RGBA)
       (= (SDL_PIXELORDER format) SDL_PACKEDORDER_ABGR)
       (= (SDL_PIXELORDER format) SDL_PACKEDORDER_BGRA)))))

(define-cstruct _SDL_Color
    ([r _uint8]
     [g _uint8]
     [b _uint8]
     [a _uint8]))

(define _SDL_Color* _SDL_Color-pointer)

(define-cstruct _SDL_Palette
    ([ncolors _int]
     [colors _SDL_Color*]
     [version _uint32]
     [refcount _int]))

(define _SDL_Palette* _SDL_Palette-pointer)
(define _SDL_Palette*/null _SDL_Palette-pointer/null)

(define-cstruct _SDL_PixelFormat
    ([format _uint32]
     [palette _SDL_Palette*]
     [BitsPerPixel _uint8]
     [BytesPerPixel _uint8]
     [padding (make-array-type _uint8 2)]
     [Rmask _uint32]
     [Gmask _uint32]
     [Bmask _uint32]
     [Amask _uint32]
     [Rloss _uint8]
     [Gloss _uint8]
     [Bloss _uint8]
     [Aloss _uint8]
     [Rshift _uint8]
     [Gshift _uint8]
     [Bshift _uint8]
     [Ashift _uint8]
     [refcount _int]))

(define _SDL_PixelFormat* _SDL_PixelFormat-pointer)
(define _SDL_PixelFormat*/null _SDL_PixelFormat-pointer/null)

(define-sdl2 SDL_GetPixelFormatName (_fun _uint32 -> _string))

(define-sdl2 SDL_PixelFormatEnumToMasks
    (_fun _uint32 _int* _uint32* _uint32* _uint32* _uint32* -> _SDL_bool))

(define-sdl2 SDL_MasksToPixelFormatEnum (_fun _int _uint32 _uint32 _uint32 _uint32 -> _uint32))

(define-sdl2 SDL_AllocFormat (_fun _uint32 -> _SDL_PixelFormat*/null))

(define-sdl2 SDL_FreeFormat (_fun _SDL_PixelFormat* -> _void))

(define-sdl2 SDL_AllocPalette (_fun _int -> _SDL_Palette*/null))

(define-sdl2 SDL_SetPixelFormatPalette (_fun _SDL_PixelFormat* _SDL_Palette*/null -> _int))

(define-sdl2 SDL_SetPaletteColors (_fun _SDL_Palette* _SDL_Color* _int _int -> _int))

(define-sdl2 SDL_FreePalette (_fun _SDL_Palette* -> _void))

(define-sdl2 SDL_MapRGB (_fun _SDL_PixelFormat* _uint8 _uint8 _uint8 -> _uint32))

(define-sdl2 SDL_MapRGBA (_fun _SDL_PixelFormat* _uint8 _uint8 _uint8 _uint8 -> _uint32))

(define-sdl2 SDL_GetRGB (_fun _uint32 _SDL_PixelFormat* _uint8* _uint8* _uint8* -> _void))

(define-sdl2 SDL_GetRGBA (_fun _uint32 _SDL_PixelFormat* _uint8* _uint8* _uint8* _uint8* -> _void))

(define-sdl2 SDL_CalculateGammaRamp (_fun _float _uint16* -> _void))


;; SDL_blendmode.h

(define _SDL_BlendMode
    (_enum
     '(SDL_BLENDMODE_NONE = #x00000000
       SDL_BLENDMODE_BLEND = #x00000001
       SDL_BLENDMODE_ADD = #x00000002
       SDL_BLENDMODE_MOD = #x00000004
       SDL_BLENDMODE_INVALID = #x7FFFFFFF)))

(define-cpointer-type _SDL_BlendMode*)

(define _SDL_BlendOperation
    (_enum
     '(SDL_BLENDOPERATION_ADD              = #x1
       SDL_BLENDOPERATION_SUBTRACT         = #x2
       SDL_BLENDOPERATION_REV_SUBTRACT     = #x3
       SDL_BLENDOPERATION_MINIMUM          = #x4
       SDL_BLENDOPERATION_MAXIMUM          = #x5)))

(define _SDL_BlendFactor
    (_enum
     '(SDL_BLENDFACTOR_ZERO                = #x1
       SDL_BLENDFACTOR_ONE                 = #x2
       SDL_BLENDFACTOR_SRC_COLOR           = #x3
       SDL_BLENDFACTOR_ONE_MINUS_SRC_COLOR = #x4
       SDL_BLENDFACTOR_SRC_ALPHA           = #x5
       SDL_BLENDFACTOR_ONE_MINUS_SRC_ALPHA = #x6
       SDL_BLENDFACTOR_DST_COLOR           = #x7
       SDL_BLENDFACTOR_ONE_MINUS_DST_COLOR = #x8
       SDL_BLENDFACTOR_DST_ALPHA           = #x9
       SDL_BLENDFACTOR_ONE_MINUS_DST_ALPHA = #xA)))

(define-sdl2 SDL_ComposeCustomBlendMode
    (_fun
     _SDL_BlendFactor
     _SDL_BlendFactor
     _SDL_BlendOperation
     _SDL_BlendFactor
     _SDL_BlendFactor
     _SDL_BlendOperation
     -> _SDL_BlendMode)
    #:make-fail make-not-available)


;; SDL_surface.h

(define SDL_SWSURFACE       0         )
(define SDL_PREALLOC        #x00000001)
(define SDL_RLEACCEL        #x00000002)
(define SDL_DONTFREE        #x00000004)

(define _SDL_BlitMap* _pointer)

(define-cstruct _SDL_Surface
    ([flags _uint32]
     [format _SDL_PixelFormat*]
     [w _int]
     [h _int]
     [pitch _int]
     [pixels _pointer]
     [userdata _pointer]
     [locked _int]
     [lock_data _pointer]
     [clip_rect _SDL_Rect]
     [map _SDL_BlitMap*]
     [refcount _int]))

(define _SDL_Surface* _SDL_Surface-pointer)
(define _SDL_Surface*/null _SDL_Surface-pointer/null)

(define (SDL_MUSTLOCK S)
    (not (zero? (bitwise-and (SDL_Surface-flags S) SDL_RLEACCEL))))

(define _SDL_blit* (_fun _SDL_Surface* _SDL_Rect* _SDL_Surface* _SDL_Rect* -> _int))

(define _SDL_YUV_CONVERSION_MODE
    (_enum
     '(SDL_YUV_CONVERSION_JPEG
       SDL_YUV_CONVERSION_BT601
       SDL_YUV_CONVERSION_BT709
       SDL_YUV_CONVERSION_AUTOMATIC)))

(define-sdl2 SDL_CreateRGBSurface
    (_fun _uint32 _int _int _int _uint32 _uint32 _uint32 _uint32 -> _SDL_Surface*/null))

(define-sdl2 SDL_CreateRGBSurfaceWithFormat
    (_fun _uint32 _int _int _int _uint32 -> _SDL_Surface*/null)
    #:make-fail make-not-available)

(define-sdl2 SDL_CreateRGBSurfaceFrom
    (_fun _pointer _int _int _int _int _uint32 _uint32 _uint32 _uint32 -> _SDL_Surface*/null))

(define-sdl2 SDL_CreateRGBSurfaceWithFormatFrom
    (_fun _pointer _int _int _int _int _uint32 -> _SDL_Surface*/null)
    #:make-fail make-not-available)

(define-sdl2 SDL_FreeSurface (_fun _SDL_Surface* -> _void))

(define-sdl2 SDL_SetSurfacePalette (_fun _SDL_Surface* _SDL_Palette*/null -> _int))

(define-sdl2 SDL_LockSurface (_fun _SDL_Surface* -> _int))

(define-sdl2 SDL_UnlockSurface (_fun _SDL_Surface* -> _void))

(define-sdl2 SDL_LoadBMP_RW (_fun _SDL_RWops* _int -> _SDL_Surface*))

(define (SDL_LoadBMP file) (SDL_LoadBMP_RW (SDL_RWFromFile file "rb") 1))

(define-sdl2 SDL_SaveBMP_RW (_fun _SDL_Surface* _SDL_RWops* _int -> _int))

(define (SDL_SaveBMP surface file) (SDL_SaveBMP_RW (SDL_RWFromFile file "wb") 1))

(define-sdl2 SDL_SetSurfaceRLE (_fun _SDL_Surface* _int -> _int))

(define-sdl2 SDL_SetColorKey (_fun _SDL_Surface* _int _uint32 -> _int))

(define-sdl2 SDL_HasColorKey (_fun _SDL_Surface* -> _SDL_bool)
    #:make-fail make-not-available)

(define-sdl2 SDL_GetColorKey (_fun _SDL_Surface* _uint32* -> _int))

(define-sdl2 SDL_SetSurfaceColorMod (_fun _SDL_Surface* _uint8 _uint8 _uint8 -> _int))

(define-sdl2 SDL_GetSurfaceColorMod (_fun _SDL_Surface* _uint8* _uint8* _uint8* -> _int))

(define-sdl2 SDL_SetSurfaceAlphaMod (_fun _SDL_Surface* _uint8 -> _int))

(define-sdl2 SDL_GetSurfaceAlphaMod (_fun _SDL_Surface* _uint8* -> _int))

(define-sdl2 SDL_SetSurfaceBlendMode (_fun _SDL_Surface* _SDL_BlendMode -> _int))

(define-sdl2 SDL_GetSurfaceBlendMode (_fun _SDL_Surface* _SDL_BlendMode* -> _int))

(define-sdl2 SDL_SetClipRect (_fun _SDL_Surface* _SDL_Rect*/null -> _SDL_bool))

(define-sdl2 SDL_GetClipRect (_fun _SDL_Surface* _SDL_Rect* -> _void))

(define-sdl2 SDL_DuplicateSurface (_fun _SDL_Surface* -> _SDL_Surface*)
    #:make-fail make-not-available)

(define-sdl2 SDL_ConvertSurface (_fun _SDL_Surface* _SDL_PixelFormat* _uint32 -> _SDL_Surface*/null))
(define-sdl2 SDL_ConvertSurfaceFormat (_fun _SDL_Surface* _uint32 _uint32 -> _SDL_Surface*/null))

(define-sdl2 SDL_ConvertPixels (_fun _int _int _uint32 _pointer _int _uint32 _pointer _int -> _int))

(define-sdl2 SDL_FillRect (_fun _SDL_Surface* _SDL_Rect*/null _uint32 -> _int))
(define-sdl2 SDL_FillRects (_fun _SDL_Surface* _SDL_Rect* _int _uint32 -> _int))

(define-sdl2 SDL_UpperBlit (_fun _SDL_Surface* _SDL_Rect*/null _SDL_Surface* _SDL_Rect*/null -> _int))
(define SDL_BlitSurface SDL_UpperBlit)

(define-sdl2 SDL_LowerBlit (_fun _SDL_Surface* _SDL_Rect*/null _SDL_Surface* _SDL_Rect*/null -> _int))

(define-sdl2 SDL_SoftStretch (_fun _SDL_Surface* _SDL_Rect* _SDL_Surface* _SDL_Rect* -> _int))

(define-sdl2 SDL_UpperBlitScaled
    (_fun _SDL_Surface* _SDL_Rect*/null _SDL_Surface* _SDL_Rect*/null -> _int))
(define SDL_BlitScaled SDL_UpperBlitScaled)

(define-sdl2 SDL_LowerBlitScaled
    (_fun _SDL_Surface* _SDL_Rect*/null _SDL_Surface* _SDL_Rect*/null -> _int))

(define-sdl2 SDL_SetYUVConversionMode (_fun _SDL_YUV_CONVERSION_MODE -> _void)
    #:make-fail make-not-available)

(define-sdl2 SDL_GetYUVConversionMode (_fun -> _SDL_YUV_CONVERSION_MODE)
    #:make-fail make-not-available)

(define-sdl2 SDL_GetYUVConversionModeForResolution (_fun _int _int -> _SDL_YUV_CONVERSION_MODE)
    #:make-fail make-not-available)


;; SDL_video.h

(define-cstruct _SDL_DisplayMode
    ([format _uint32]
     [w _int]
     [h _int]
     [refresh_rate _int]
     [driverdata _pointer]))

(define _SDL_DisplayMode* _SDL_DisplayMode-pointer)
(define _SDL_DisplayMode*/null _SDL_DisplayMode-pointer/null)

(define-cpointer-type _SDL_Window*)

(define _SDL_WindowFlags
    (_bitmask
     '(SDL_WINDOW_FULLSCREEN = #x00000001
       SDL_WINDOW_OPENGL = #x00000002
       SDL_WINDOW_SHOWN = #x00000004
       SDL_WINDOW_HIDDEN = #x00000008
       SDL_WINDOW_BORDERLESS = #x00000010
       SDL_WINDOW_RESIZABLE = #x00000020
       SDL_WINDOW_MINIMIZED = #x00000040
       SDL_WINDOW_MAXIMIZED = #x00000080
       SDL_WINDOW_INPUT_GRABBED = #x00000100
       SDL_WINDOW_INPUT_FOCUS = #x00000200
       SDL_WINDOW_MOUSE_FOCUS = #x00000400
       SDL_WINDOW_FULLSCREEN_DESKTOP = #x00001001
       SDL_WINDOW_FOREIGN = #x00000800
       SDL_WINDOW_ALLOW_HIGHDPI = #x00002000
       SDL_WINDOW_MOUSE_CAPTURE = #x00004000
       SDL_WINDOW_ALWAYS_ON_TOP = #x00008000
       SDL_WINDOW_SKIP_TASKBAR  = #x00010000
       SDL_WINDOW_UTILITY       = #x00020000
       SDL_WINDOW_TOOLTIP       = #x00040000
       SDL_WINDOW_POPUP_MENU    = #x00080000
       SDL_WINDOW_VULKAN        = #x10000000)
     _uint32))

(define SDL_WINDOWPOS_UNDEFINED_MASK    #x1FFF0000)
(define (SDL_WINDOWPOS_UNDEFINED_DISPLAY X)
    (bitwise-ior SDL_WINDOWPOS_UNDEFINED_MASK X))
(define SDL_WINDOWPOS_UNDEFINED         (SDL_WINDOWPOS_UNDEFINED_DISPLAY 0))
(define (SDL_WINDOWPOS_ISUNDEFINED X)
    (= (bitwise-and X #xFFFF0000) SDL_WINDOWPOS_UNDEFINED_MASK))

(define SDL_WINDOWPOS_CENTERED_MASK    #x2FFF0000)
(define (SDL_WINDOWPOS_CENTERED_DISPLAY X)
    (bitwise-ior SDL_WINDOWPOS_CENTERED_MASK X))
(define SDL_WINDOWPOS_CENTERED         (SDL_WINDOWPOS_CENTERED_DISPLAY 0))
(define (SDL_WINDOWPOS_ISCENTERED X)
    (= (bitwise-and X #xFFFF0000) SDL_WINDOWPOS_CENTERED_MASK))

(define _SDL_WindowEventID
    (_enum
     '(SDL_WINDOWEVENT_NONE
       SDL_WINDOWEVENT_SHOWN
       SDL_WINDOWEVENT_HIDDEN
       SDL_WINDOWEVENT_EXPOSED

       SDL_WINDOWEVENT_MOVED

       SDL_WINDOWEVENT_RESIZED
       SDL_WINDOWEVENT_SIZE_CHANGED


       SDL_WINDOWEVENT_MINIMIZED
       SDL_WINDOWEVENT_MAXIMIZED
       SDL_WINDOWEVENT_RESTORED

       SDL_WINDOWEVENT_ENTER
       SDL_WINDOWEVENT_LEAVE
       SDL_WINDOWEVENT_FOCUS_GAINED
       SDL_WINDOWEVENT_FOCUS_LOST
       SDL_WINDOWEVENT_CLOSE
       SDL_WINDOWEVENT_TAKE_FOCUS
       SDL_WINDOWEVENT_HIT_TEST)))

(define _SDL_DisplayEventID
    (_enum
     '(SDL_DISPLAYEVENT_NONE
       SDL_DISPLAYEVENT_ORIENTATION)))

(define _SDL_DisplayOrientation
    (_enum
     '(SDL_ORIENTATION_UNKNOWN
       SDL_ORIENTATION_LANDSCAPE
       SDL_ORIENTATION_LANDSCAPE_FLIPPED
       SDL_ORIENTATION_PORTRAIT
       SDL_ORIENTATION_PORTRAIT_FLIPPED)))

(define-cpointer-type _SDL_GLContext)

(define _SDL_GLattr
    (_enum
     '(SDL_GL_RED_SIZE
       SDL_GL_GREEN_SIZE
       SDL_GL_BLUE_SIZE
       SDL_GL_ALPHA_SIZE
       SDL_GL_BUFFER_SIZE
       SDL_GL_DOUBLEBUFFER
       SDL_GL_DEPTH_SIZE
       SDL_GL_STENCIL_SIZE
       SDL_GL_ACCUM_RED_SIZE
       SDL_GL_ACCUM_GREEN_SIZE
       SDL_GL_ACCUM_BLUE_SIZE
       SDL_GL_ACCUM_ALPHA_SIZE
       SDL_GL_STEREO
       SDL_GL_MULTISAMPLEBUFFERS
       SDL_GL_MULTISAMPLESAMPLES
       SDL_GL_ACCELERATED_VISUAL
       SDL_GL_RETAINED_BACKING
       SDL_GL_CONTEXT_MAJOR_VERSION
       SDL_GL_CONTEXT_MINOR_VERSION
       SDL_GL_CONTEXT_EGL
       SDL_GL_CONTEXT_FLAGS
       SDL_GL_CONTEXT_PROFILE_MASK
       SDL_GL_SHARE_WITH_CURRENT_CONTEXT
       SDL_GL_FRAMEBUFFER_SRGB_CAPABLE
       SDL_GL_CONTEXT_RELEASE_BEHAVIOR
       SDL_GL_CONTEXT_RESET_NOTIFICATION
       SDL_GL_CONTEXT_NO_ERROR)))

(define _SDL_GLprofile
    (_enum
     '(SDL_GL_CONTEXT_PROFILE_CORE           = #x0001
       SDL_GL_CONTEXT_PROFILE_COMPATIBILITY  = #x0002
       SDL_GL_CONTEXT_PROFILE_ES             = #x0004)))

(define _SDL_GLcontextFlag
    (_enum
     '(SDL_GL_CONTEXT_DEBUG_FLAG              = #x0001
       SDL_GL_CONTEXT_FORWARD_COMPATIBLE_FLAG = #x0002
       SDL_GL_CONTEXT_ROBUST_ACCESS_FLAG      = #x0004
       SDL_GL_CONTEXT_RESET_ISOLATION_FLAG    = #x0008)))

(define _SDL_GLcontextReleaseFlag
    (_enum
     '(SDL_GL_CONTEXT_RELEASE_BEHAVIOR_NONE   = #x0000
       SDL_GL_CONTEXT_RELEASE_BEHAVIOR_FLUSH  = #x0001)))

(define _SDL_GLContextResetNotification
    (_enum
     '(SDL_GL_CONTEXT_RESET_NO_NOTIFICATION = #x0000
       SDL_GL_CONTEXT_RESET_LOSE_CONTEXT    = #x0001)))

(define-sdl2 SDL_GetNumVideoDrivers (_fun -> _int))

(define-sdl2 SDL_GetVideoDriver (_fun _int -> _string))

(define-sdl2 SDL_VideoInit (_fun _string -> _int))

(define-sdl2 SDL_VideoQuit (_fun -> _void))

(define-sdl2 SDL_GetCurrentVideoDriver (_fun -> _string))

(define-sdl2 SDL_GetNumVideoDisplays (_fun -> _int))

(define-sdl2 SDL_GetDisplayName (_fun _int -> _string))

(define-sdl2 SDL_GetDisplayBounds (_fun _int _SDL_Rect* -> _int))

(define-sdl2 SDL_GetDisplayUsableBounds (_fun _int _SDL_Rect* -> _int)
    #:make-fail make-not-available)

(define-sdl2 SDL_GetDisplayDPI (_fun _int _float* _float* _float* -> _int)
    #:make-fail make-not-available)

(define-sdl2 SDL_GetDisplayOrientation (_fun _int -> _SDL_DisplayOrientation)
    #:make-fail make-not-available)

(define-sdl2 SDL_GetNumDisplayModes (_fun _int -> _int))

(define-sdl2 SDL_GetDisplayMode (_fun _int _int _SDL_DisplayMode* -> _int))

(define-sdl2 SDL_GetDesktopDisplayMode (_fun _int _SDL_DisplayMode* -> _int))

(define-sdl2 SDL_GetCurrentDisplayMode (_fun _int _SDL_DisplayMode* -> _int))

(define-sdl2 SDL_GetClosestDisplayMode
    (_fun _int _SDL_DisplayMode* _SDL_DisplayMode* -> _SDL_DisplayMode*))

(define-sdl2 SDL_GetWindowDisplayIndex (_fun _SDL_Window* -> _int))

(define-sdl2 SDL_SetWindowDisplayMode (_fun _SDL_Window* _SDL_DisplayMode*/null -> _int))

(define-sdl2 SDL_GetWindowDisplayMode (_fun _SDL_Window* _SDL_DisplayMode* -> _int))

(define-sdl2 SDL_GetWindowPixelFormat (_fun _SDL_Window* -> _uint32))

(define-sdl2 SDL_CreateWindow
    (_fun _string _int _int _int _int _SDL_WindowFlags -> _SDL_Window*/null))

(define-sdl2 SDL_CreateWindowFrom (_fun _pointer -> _SDL_Window*/null))

(define-sdl2 SDL_GetWindowID (_fun _SDL_Window* -> _uint32))

(define-sdl2 SDL_GetWindowFromID (_fun _uint32 -> _SDL_Window*/null))

(define-sdl2 SDL_GetWindowFlags (_fun _SDL_Window* -> _uint32))

(define-sdl2 SDL_SetWindowTitle (_fun _SDL_Window* _string -> _void))

(define-sdl2 SDL_GetWindowTitle (_fun _SDL_Window* -> _string))

(define-sdl2 SDL_SetWindowIcon (_fun _SDL_Window* _SDL_Surface* -> _void))

(define-sdl2 SDL_SetWindowData (_fun _SDL_Window* _string _pointer -> _pointer))

(define-sdl2 SDL_GetWindowData (_fun _SDL_Window* _string -> _void))

(define-sdl2 SDL_SetWindowPosition (_fun _SDL_Window* _int _int -> _void))

(define-sdl2 SDL_GetWindowPosition (_fun _SDL_Window* _int*/null _int*/null -> _void))

(define-sdl2 SDL_SetWindowSize (_fun _SDL_Window* _int _int -> _void))

(define-sdl2 SDL_GetWindowSize (_fun _SDL_Window* _int*/null _int*/null -> _void))

(define-sdl2 SDL_GetWindowBordersSize
    (_fun _SDL_Window* _int*/null _int*/null _int*/null _int*/null -> _int)
    #:make-fail make-not-available)

(define-sdl2 SDL_SetWindowMinimumSize (_fun _SDL_Window* _int _int -> _void))

(define-sdl2 SDL_GetWindowMinimumSize (_fun _SDL_Window* _int*/null _int*/null -> _void))

(define-sdl2 SDL_SetWindowMaximumSize (_fun _SDL_Window* _int _int -> _void))

(define-sdl2 SDL_GetWindowMaximumSize (_fun _SDL_Window* _int*/null _int*/null -> _void))

(define-sdl2 SDL_SetWindowBordered (_fun _SDL_Window* _SDL_bool -> _void))

(define-sdl2 SDL_SetWindowResizable (_fun _SDL_Window* _SDL_bool -> _void)
    #:make-fail make-not-available)

(define-sdl2 SDL_ShowWindow (_fun _SDL_Window* -> _void))

(define-sdl2 SDL_HideWindow (_fun _SDL_Window* -> _void))

(define-sdl2 SDL_RaiseWindow (_fun _SDL_Window* -> _void))

(define-sdl2 SDL_MaximizeWindow (_fun _SDL_Window* -> _void))

(define-sdl2 SDL_MinimizeWindow (_fun _SDL_Window* -> _void))

(define-sdl2 SDL_RestoreWindow (_fun _SDL_Window* -> _void))

(define-sdl2 SDL_SetWindowFullscreen (_fun _SDL_Window* _uint32 -> _int))

(define-sdl2 SDL_GetWindowSurface (_fun _SDL_Window* -> _SDL_Surface*/null))

(define-sdl2 SDL_UpdateWindowSurface (_fun _SDL_Window* -> _int))

(define-sdl2 SDL_UpdateWindowSurfaceRects (_fun _SDL_Window* _SDL_Rect* _int -> _int))

(define-sdl2 SDL_SetWindowGrab (_fun _SDL_Window* _SDL_bool -> _void))

(define-sdl2 SDL_GetWindowGrab (_fun _SDL_Window* -> _SDL_bool))

(define-sdl2 SDL_GetGrabbedWindow (_fun -> _SDL_Window*/null)
    #:make-fail make-not-available)

(define-sdl2 SDL_SetWindowBrightness (_fun _SDL_Window* _float -> _int))

(define-sdl2 SDL_GetWindowBrightness (_fun _SDL_Window* -> _float))

(define-sdl2 SDL_SetWindowOpacity (_fun _SDL_Window* _float -> _int)
    #:make-fail make-not-available)

(define-sdl2 SDL_GetWindowOpacity (_fun _SDL_Window* _float* -> _int)
    #:make-fail make-not-available)

(define-sdl2 SDL_SetWindowModalFor (_fun _SDL_Window* _SDL_Window* -> _int)
    #:make-fail make-not-available)

(define-sdl2 SDL_SetWindowInputFocus (_fun _SDL_Window* -> _int)
    #:make-fail make-not-available)

(define-sdl2 SDL_SetWindowGammaRamp
    (_fun _SDL_Window* _uint16*/null _uint16*/null _uint16*/null -> _int))

(define-sdl2 SDL_GetWindowGammaRamp
    (_fun _SDL_Window* _uint16*/null _uint16*/null _uint16*/null -> _int))

(define _SDL_HitTestResult
    (_enum
     '(SDL_HITTEST_NORMAL
       SDL_HITTEST_DRAGGABLE
       SDL_HITTEST_RESIZE_TOPLEFT
       SDL_HITTEST_RESIZE_TOP
       SDL_HITTEST_RESIZE_TOPRIGHT
       SDL_HITTEST_RESIZE_RIGHT
       SDL_HITTEST_RESIZE_BOTTOMRIGHT
       SDL_HITTEST_RESIZE_BOTTOM
       SDL_HITTEST_RESIZE_BOTTOMLEFT
       SDL_HITTEST_RESIZE_LEFT)))

(define _SDL_HitTest (_fun _SDL_Window* _SDL_Point* _pointer -> _SDL_HitTestResult))

(define-sdl2 SDL_SetWindowHitTest (_fun _SDL_Window* _SDL_HitTest _pointer -> _int)
    #:make-fail make-not-available)

(define-sdl2 SDL_DestroyWindow (_fun _SDL_Window* -> _void))

(define-sdl2 SDL_IsScreenSaverEnabled (_fun -> _SDL_bool))

(define-sdl2 SDL_EnableScreenSaver (_fun -> _void))

(define-sdl2 SDL_DisableScreenSaver (_fun -> _void))

(define-sdl2 SDL_GL_LoadLibrary (_fun _string -> _int))

(define-sdl2 SDL_GL_GetProcAddress (_fun _string -> _pointer))

(define-sdl2 SDL_GL_UnloadLibrary (_fun -> _void))

(define-sdl2 SDL_GL_ExtensionSupported (_fun _string -> _SDL_bool))

(define-sdl2 SDL_GL_ResetAttributes (_fun -> _void)
    #:make-fail make-not-available)

(define-sdl2 SDL_GL_SetAttribute (_fun _SDL_GLattr _int -> _int))

(define-sdl2 SDL_GL_GetAttribute (_fun _SDL_GLattr _int* -> _int))

(define-sdl2 SDL_GL_CreateContext (_fun _SDL_Window* -> _SDL_GLContext/null))

(define-sdl2 SDL_GL_MakeCurrent (_fun _SDL_Window* _SDL_GLContext -> _int))

(define-sdl2 SDL_GL_GetCurrentWindow (_fun -> _SDL_Window*/null))

(define-sdl2 SDL_GL_GetCurrentContext (_fun -> _SDL_GLContext/null))

(define-sdl2 SDL_GL_GetDrawableSize (_fun _SDL_Window* _int*/null _int*/null -> _void)
    #:make-fail make-not-available)

(define-sdl2 SDL_GL_SetSwapInterval (_fun _int -> _int))

(define-sdl2 SDL_GL_GetSwapInterval (_fun -> _int))

(define-sdl2 SDL_GL_SwapWindow (_fun _SDL_Window* -> _void))

(define-sdl2 SDL_GL_DeleteContext (_fun _SDL_GLContext -> _void))


;; SDL_vulkan.h

(define-cpointer-type _VkInstance)
(define-cpointer-type _VkSurfaceKHR*)

(define-sdl2 SDL_Vulkan_LoadLibrary (_fun _string -> _int)
    #:make-fail make-not-available)

(define-sdl2 SDL_Vulkan_GetVkGetInstanceProcAddr (_fun -> _pointer)
    #:make-fail make-not-available)

(define-sdl2 SDL_Vulkan_UnloadLibrary (_fun -> _void)
    #:make-fail make-not-available)

(define-sdl2 SDL_Vulkan_GetInstanceExtensions (_fun _SDL_Window* _uint* _pointer) -> _SDL_bool)
    #:make-fail make-not-available)

(define-sdl2 SDL_Vulkan_CreateSurface (_fun _SDL_Window* _VkInstance _VkSurfaceKHR* -> _SDL_bool)
    #:make-fail make-not-available)

(define-sdl2 SDL_Vulkan_GetDrawableSize (_fun _SDL_Window* _int*/null _int*/null -> _void)
    #:make-fail make-not-available)


;; SDL_keyboard.h

(define-cstruct _SDL_Keysym
    ([scancode _SDL_Scancode]
     [sym _SDL_Keycode]
     [mod _uint16]
     [unused _uint32]))

(define-sdl2 SDL_GetKeyboardFocus (_fun -> _SDL_Window*/null))

(define-sdl2 SDL_GetKeyboardState (_fun _int*/null -> _uint8*))

(define-sdl2 SDL_GetModState (_fun -> _SDL_Keymod))

(define-sdl2 SDL_SetModState (_fun _SDL_Keymod -> _void))

(define-sdl2 SDL_GetKeyFromScancode (_fun _SDL_Scancode -> _SDL_Keycode))

(define-sdl2 SDL_GetScancodeFromKey (_fun _SDL_Keycode -> _SDL_Scancode))

(define-sdl2 SDL_GetScancodeName (_fun _SDL_Scancode -> _string))

(define-sdl2 SDL_GetScancodeFromName (_fun _string -> _SDL_Scancode))

(define-sdl2 SDL_GetKeyName (_fun _SDL_Keycode -> _string))

(define-sdl2 SDL_GetKeyFromName (_fun _string -> _SDL_Keycode))

(define-sdl2 SDL_StartTextInput (_fun -> _void))

(define-sdl2 SDL_IsTextInputActive (_fun -> _SDL_bool))

(define-sdl2 SDL_StopTextInput (_fun -> _void))

(define-sdl2 SDL_SetTextInputRect (_fun _SDL_Rect* -> _void))

(define-sdl2 SDL_HasScreenKeyboardSupport (_fun -> _SDL_bool))

(define-sdl2 SDL_IsScreenKeyboardShown (_fun _SDL_Window* -> _SDL_bool))


;; SDL_mouse.h

(define-cpointer-type _SDL_Cursor*)

(define _SDL_SystemCursor
    (_enum
     '(SDL_SYSTEM_CURSOR_ARROW
       SDL_SYSTEM_CURSOR_IBEAM
       SDL_SYSTEM_CURSOR_WAIT
       SDL_SYSTEM_CURSOR_CROSSHAIR
       SDL_SYSTEM_CURSOR_WAITARROW
       SDL_SYSTEM_CURSOR_SIZENWSE
       SDL_SYSTEM_CURSOR_SIZENESW
       SDL_SYSTEM_CURSOR_SIZEWE
       SDL_SYSTEM_CURSOR_SIZENS
       SDL_SYSTEM_CURSOR_SIZEALL
       SDL_SYSTEM_CURSOR_NO
       SDL_SYSTEM_CURSOR_HAND
       SDL_NUM_SYSTEM_CURSORS)))

(define _SDL_MouseWheelDirection
    (_enum
     '(SDL_MOUSEWHEEL_NORMAL
       SDL_MOUSEWHEEL_FLIPPED)))

(define-sdl2 SDL_GetMouseFocus (_fun -> _SDL_Window*/null))

(define-sdl2 SDL_GetMouseState (_fun _int* _int* -> _uint32))

(define-sdl2 SDL_GetGlobalMouseState (_fun _int* _int* -> _uint32)
    #:make-fail make-not-available)

(define-sdl2 SDL_GetRelativeMouseState (_fun _int* _int* -> _uint32))

(define-sdl2 SDL_WarpMouseInWindow (_fun _SDL_Window*/null _int _int -> _void))

(define-sdl2 SDL_WarpMouseGlobal (_fun _int _int -> _int)
    #:make-fail make-not-available)

(define-sdl2 SDL_SetRelativeMouseMode (_fun _SDL_bool -> _int))

(define-sdl2 SDL_CaptureMouse (_fun _SDL_bool -> _int)
    #:make-fail make-not-available)

(define-sdl2 SDL_GetRelativeMouseMode (_fun -> _SDL_bool))

(define-sdl2 SDL_CreateCursor (_fun _uint8* _uint8* _int _int _int _int -> _SDL_Cursor*/null))

(define-sdl2 SDL_CreateColorCursor (_fun _SDL_Surface* _int _int -> _SDL_Cursor*/null))

(define-sdl2 SDL_CreateSystemCursor (_fun _SDL_SystemCursor -> _SDL_Cursor*/null))

(define-sdl2 SDL_SetCursor (_fun _SDL_Cursor*/null -> _void))

(define-sdl2 SDL_GetCursor (_fun -> _SDL_Cursor*/null))

(define-sdl2 SDL_GetDefaultCursor (_fun -> _SDL_Cursor*/null))

(define-sdl2 SDL_FreeCursor (_fun _SDL_Cursor* -> _void))

(define-sdl2 SDL_ShowCursor (_fun _int -> _int))

(define (SDL_BUTTON X)       (arithmetic-shift 1 (- X 1)))
(define SDL_BUTTON_LEFT     1)
(define SDL_BUTTON_MIDDLE   2)
(define SDL_BUTTON_RIGHT    3)
(define SDL_BUTTON_X1       4)
(define SDL_BUTTON_X2       5)
(define SDL_BUTTON_LMASK    (SDL_BUTTON SDL_BUTTON_LEFT))
(define SDL_BUTTON_MMASK    (SDL_BUTTON SDL_BUTTON_MIDDLE))
(define SDL_BUTTON_RMASK    (SDL_BUTTON SDL_BUTTON_RIGHT))
(define SDL_BUTTON_X1MASK   (SDL_BUTTON SDL_BUTTON_X1))
(define SDL_BUTTON_X2MASK   (SDL_BUTTON SDL_BUTTON_X2))


;; SDL_joystick.h

(define-cpointer-type _SDL_Joystick*)

(define-cstruct _SDL_JoystickGUID
    ([data (_array _uint8 16)]))

(define _SDL_JoystickID _sint32)

(define _SDL_JoystickType
    (_enum
     '(SDL_JOYSTICK_TYPE_UNKNOWN
       SDL_JOYSTICK_TYPE_GAMECONTROLLER
       SDL_JOYSTICK_TYPE_WHEEL
       SDL_JOYSTICK_TYPE_ARCADE_STICK
       SDL_JOYSTICK_TYPE_FLIGHT_STICK
       SDL_JOYSTICK_TYPE_DANCE_PAD
       SDL_JOYSTICK_TYPE_GUITAR
       SDL_JOYSTICK_TYPE_DRUM_KIT
       SDL_JOYSTICK_TYPE_ARCADE_PAD
       SDL_JOYSTICK_TYPE_THROTTLE)))

(define _SDL_JoystickPowerLevel
    (_enum
     '(SDL_JOYSTICK_POWER_UNKNOWN = -1
       SDL_JOYSTICK_POWER_EMPTY
       SDL_JOYSTICK_POWER_LOW
       SDL_JOYSTICK_POWER_MEDIUM
       SDL_JOYSTICK_POWER_FULL
       SDL_JOYSTICK_POWER_WIRED
       SDL_JOYSTICK_POWER_MAX)))

(define-sdl2 SDL_LockJoysticks (_fun -> _void)
    #:make-fail make-not-available)
(define-sdl2 SDL_UnlockJoysticks (_fun -> _void)
    #:make-fail make-not-available)

(define-sdl2 SDL_NumJoysticks (_fun -> _int))

(define-sdl2 SDL_JoystickNameForIndex (_fun _int -> _string))

(define-sdl2 SDL_JoystickGetDevicePlayerIndex (_fun _int -> _int)
    #:make-fail make-not-available)

(define-sdl2 SDL_JoystickGetDeviceGUID (_fun _int -> _SDL_JoystickGUID))

(define-sdl2 SDL_JoystickGetDeviceVendor (_fun _int -> _uint16)
    #:make-fail make-not-available)

(define-sdl2 SDL_JoystickGetDeviceProduct (_fun _int -> _uint16)
    #:make-fail make-not-available)

(define-sdl2 SDL_JoystickGetDeviceProductVersion (_fun _int -> _uint16)
    #:make-fail make-not-available)

(define-sdl2 SDL_JoystickGetDeviceType (_fun _int -> _SDL_JoystickType)
    #:make-fail make-not-available)

(define-sdl2 SDL_JoystickGetDeviceInstanceID (_fun _int -> _SDL_JoystickID)
    #:make-fail make-not-available)

(define-sdl2 SDL_JoystickOpen (_fun _int -> _SDL_Joystick*/null))

(define-sdl2 SDL_JoystickFromInstanceID (_fun _SDL_JoystickID -> _SDL_Joystick*/null)
    #:make-fail make-not-available)

(define-sdl2 SDL_JoystickName (_fun _SDL_Joystick* -> _string))

(define-sdl2 SDL_JoystickGetPlayerIndex (_fun _SDL_Joystick* -> _int)
    #:make-fail make-not-available)

(define-sdl2 SDL_JoystickGetGUID (_fun _SDL_Joystick* -> _SDL_JoystickGUID))

(define-sdl2 SDL_JoystickGetVendor (_fun _SDL_Joystick* -> _uint16)
    #:make-fail make-not-available)

(define-sdl2 SDL_JoystickGetProduct (_fun _SDL_Joystick* -> _uint16)
    #:make-fail make-not-available)

(define-sdl2 SDL_JoystickGetProductVersion (_fun _SDL_Joystick* -> _uint16)
    #:make-fail make-not-available)

(define-sdl2 SDL_JoystickGetType (_fun _SDL_Joystick* -> _SDL_JoystickType)
    #:make-fail make-not-available)

(define-sdl2 SDL_JoystickGetGUIDString (_fun _SDL_JoystickGUID _string _int -> _void))

(define-sdl2 SDL_JoystickGetGUIDFromString (_fun _string -> _SDL_JoystickGUID))

(define-sdl2 SDL_JoystickGetAttached (_fun _SDL_Joystick* -> _SDL_bool))

(define-sdl2 SDL_JoystickInstanceID (_fun _SDL_Joystick* -> _SDL_JoystickID))

(define-sdl2 SDL_JoystickNumAxes (_fun _SDL_Joystick* -> _int))

(define-sdl2 SDL_JoystickNumBalls (_fun _SDL_Joystick* -> _int))

(define-sdl2 SDL_JoystickNumHats (_fun _SDL_Joystick* -> _int))

(define-sdl2 SDL_JoystickNumButtons (_fun _SDL_Joystick* -> _int))

(define-sdl2 SDL_JoystickUpdate (_fun -> _void))

(define-sdl2 SDL_JoystickEventState (_fun _int -> _int))

(define SDL_JOYSTICK_AXIS_MAX   32767)
(define SDL_JOYSTICK_AXIS_MIN   -32768)

(define-sdl2 SDL_JoystickGetAxis (_fun _SDL_Joystick* _int -> _sint16))

(define-sdl2 SDL_JoystickGetAxisInitialState (_fun _SDL_Joystick* _int _sint16 -> _SDL_bool)
    #:make-fail make-not-available)

(define SDL_HAT_CENTERED    #x00)
(define SDL_HAT_UP          #x01)
(define SDL_HAT_RIGHT       #x02)
(define SDL_HAT_DOWN        #x04)
(define SDL_HAT_LEFT        #x08)
(define SDL_HAT_RIGHTUP     (bitwise-ior SDL_HAT_RIGHT SDL_HAT_UP))
(define SDL_HAT_RIGHTDOWN   (bitwise-ior SDL_HAT_RIGHT SDL_HAT_DOWN))
(define SDL_HAT_LEFTUP      (bitwise-ior SDL_HAT_LEFT SDL_HAT_UP))
(define SDL_HAT_LEFTDOWN    (bitwise-ior SDL_HAT_LEFT SDL_HAT_DOWN))

(define-sdl2 SDL_JoystickGetHat (_fun _SDL_Joystick* _int -> _uint8))

(define-sdl2 SDL_JoystickGetBall (_fun _SDL_Joystick* _int _int* _int* -> _int))

(define-sdl2 SDL_JoystickGetButton (_fun _SDL_Joystick* _int -> _uint8))

(define-sdl2 SDL_JoystickRumble (_fun _SDL_Joystick* _uint16 _uint16 _uint32 -> _int)
    #:make-fail make-not-available)

(define-sdl2 SDL_JoystickClose (_fun _SDL_Joystick* -> _void))

(define-sdl2 SDL_JoystickCurrentPowerLevel (_fun _SDL_Joystick* -> _SDL_JoystickPowerLevel)
    #:make-fail make-not-available)


;; SDL_touch.h

(define _SDL_TouchID _sint64)
(define _SDL_FingerID _sint64)

(define-cstruct _SDL_Finger
    ([id _SDL_FingerID]
     [x _float]
     [y _float]
     [pressure _float]))

(define _SDL_Finger* _SDL_Finger-pointer)
(define _SDL_Finger*/null _SDL_Finger-pointer/null)

(define SDL_TOUCH_MOUSEID #xffffffff)

(define-sdl2 SDL_GetNumTouchDevices (_fun -> _int))

(define-sdl2 SDL_GetTouchDevice (_fun _int -> _SDL_TouchID))

(define-sdl2 SDL_GetNumTouchFingers (_fun _SDL_TouchID -> _int))

(define-sdl2 SDL_GetTouchFinger (_fun _SDL_TouchID _int -> _SDL_Finger*/null))


;; SDL_gesture.h

(define _SDL_GestureID _sint64)

(define-sdl2 SDL_RecordGesture (_fun _SDL_TouchID -> _int))

(define-sdl2 SDL_SaveAllDollarTemplates (_fun _SDL_RWops* -> _int))

(define-sdl2 SDL_SaveDollarTemplate (_fun _SDL_GestureID _SDL_RWops* -> _int))

(define-sdl2 SDL_LoadDollarTemplates (_fun _SDL_TouchID _SDL_RWops* -> _int))


;; SDL_events.h

(define SDL_RELEASED    0)
(define SDL_PRESSED 1)

(define _SDL_EventType
    (_enum
     '(SDL_FIRSTEVENT     = 0
       SDL_QUIT           = #x100

       SDL_APP_TERMINATING
       SDL_APP_LOWMEMORY
       SDL_APP_WILLENTERBACKGROUND
       SDL_APP_DIDENTERBACKGROUND
       SDL_APP_WILLENTERFOREGROUND
       SDL_APP_DIDENTERFOREGROUND

       SDL_DISPLAYEVENT   = #x150

       SDL_WINDOWEVENT    = #x200
       SDL_SYSWMEVENT

       SDL_KEYDOWN        = #x300
       SDL_KEYUP
       SDL_TEXTEDITING
       SDL_TEXTINPUT
       SDL_KEYMAPCHANGED

       SDL_MOUSEMOTION    = #x400
       SDL_MOUSEBUTTONDOWN
       SDL_MOUSEBUTTONUP
       SDL_MOUSEWHEEL

       SDL_JOYAXISMOTION  = #x600
       SDL_JOYBALLMOTION
       SDL_JOYHATMOTION
       SDL_JOYBUTTONDOWN
       SDL_JOYBUTTONUP
       SDL_JOYDEVICEADDED
       SDL_JOYDEVICEREMOVED

       SDL_CONTROLLERAXISMOTION  = #x650
       SDL_CONTROLLERBUTTONDOWN
       SDL_CONTROLLERBUTTONUP
       SDL_CONTROLLERDEVICEADDED
       SDL_CONTROLLERDEVICEREMOVED
       SDL_CONTROLLERDEVICEREMAPPED

       SDL_FINGERDOWN      = #x700
       SDL_FINGERUP
       SDL_FINGERMOTION

       SDL_DOLLARGESTURE   = #x800
       SDL_DOLLARRECORD
       SDL_MULTIGESTURE

       SDL_CLIPBOARDUPDATE = #x900

       SDL_DROPFILE        = #x1000
       SDL_DROPTEXT
       SDL_DROPBEGIN
       SDL_DROPCOMPLETE

       SDL_AUDIODEVICEADDED = #x1100
       SDL_AUDIODEVICEREMOVED

       SDL_SENSORUPDATE = #x1200

       SDL_RENDER_TARGETS_RESET = #x2000
       SDL_RENDER_DEVICE_RESET

       SDL_USEREVENT    = #x8000

       SDL_LASTEVENT    = #xFFFF)
     _uint32))

(define-cstruct _SDL_CommonEvent
    ([type _uint32]
     [timestamp _uint32]))

(define-cstruct _SDL_DisplayEvent
    ([type _uint32]
     [timestamp _uint32]
     [display _uint32]
     [event _uint8]
     [padding1 _uint8]
     [padding2 _uint8]
     [padding3 _uint8]
     [data1 _sint32]))

(define-cstruct _SDL_WindowEvent
    ([type _uint32]
     [timestamp _uint32]
     [windowID _uint32]
     [event _uint8]
     [padding1 _uint8]
     [padding2 _uint8]
     [padding3 _uint8]
     [data1 _sint32]
     [data2 _sint32]))

(define-cstruct _SDL_KeyboardEvent
    ([type _uint32]
     [timestamp _uint32]
     [windowID _uint32]
     [state _uint8]
     [repeat _uint8]
     [padding2 _uint8]
     [padding3 _uint8]
     [keysym _SDL_Keysym]))

(define SDL_TEXTEDITINGEVENT_TEXT_SIZE 32)

(define-cstruct _SDL_TextEditingEvent
    ([type _uint32]
     [timestamp _uint32]
     [windowID _uint32]
     [text (_array _byte SDL_TEXTEDITINGEVENT_TEXT_SIZE)]
     [start _sint32]
     [length _sint32]))

(define SDL_TEXTINPUTEVENT_TEXT_SIZE 32)

(define-cstruct _SDL_TextInputEvent
    ([type _uint32]
     [timestamp _uint32]
     [windowID _uint32]
     [text (_array _byte SDL_TEXTINPUTEVENT_TEXT_SIZE)]))

(define-cstruct _SDL_MouseMotionEvent
    ([type _uint32]
     [timestamp _uint32]
     [windowID _uint32]
     [which _uint32]
     [state _uint32]
     [x _sint32]
     [y _sint32]
     [xrel _sint32]
     [yrel _sint32]))

(define-cstruct _SDL_MouseButtonEvent
    ([type _uint32]
     [timestamp _uint32]
     [windowID _uint32]
     [which _uint32]
     [button _uint8]
     [state _uint8]
     [clicks _uint8]
     [padding1 _uint8]
     [x _sint32]
     [y _sint32]))

(define-cstruct _SDL_MouseWheelEvent
    ([type _uint32]
     [timestamp _uint32]
     [windowID _uint32]
     [which _uint32]
     [x _sint32]
     [y _sint32]
     [direction _uint32]))

(define-cstruct _SDL_JoyAxisEvent
    ([type _uint32]
     [timestamp _uint32]
     [which _SDL_JoystickID]
     [axis _uint8]
     [padding1 _uint8]
     [padding2 _uint8]
     [padding3 _uint8]
     [value _sint16]
     [padding4 _uint16]))

(define-cstruct _SDL_JoyBallEvent
    ([type _uint32]
     [timestamp _uint32]
     [which _SDL_JoystickID]
     [ball _uint8]
     [padding1 _uint8]
     [padding2 _uint8]
     [padding3 _uint8]
     [xrel _sint16]
     [yrel _sint16]))

(define-cstruct _SDL_JoyHatEvent
    ([type _uint32]
     [timestamp _uint32]
     [which _SDL_JoystickID]
     [hat _uint8]
     [value _uint8]
     [padding1 _uint8]
     [padding2 _uint8]))

(define-cstruct _SDL_JoyButtonEvent
    ([type _uint32]
     [timestamp _uint32]
     [which _SDL_JoystickID]
     [button _uint8]
     [state _uint8]
     [padding1 _uint8]
     [padding2 _uint8]))

(define-cstruct _SDL_JoyDeviceEvent
    ([type _uint32]
     [timestamp _uint32]
     [which _sint32]))

(define-cstruct _SDL_ControllerAxisEvent
    ([type _uint32]
     [timestamp _uint32]
     [which _SDL_JoystickID]
     [axis _uint8]
     [padding1 _uint8]
     [padding2 _uint8]
     [padding3 _uint8]
     [value _sint16]
     [padding4 _uint16]))

(define-cstruct _SDL_ControllerButtonEvent
    ([type _uint32]
     [timestamp _uint32]
     [which _SDL_JoystickID]
     [button _uint8]
     [state _uint8]
     [padding1 _uint8]
     [padding2 _uint8]))

(define-cstruct _SDL_ControllerDeviceEvent
    ([type _uint32]
     [timestamp _uint32]
     [which _sint32]))

(define-cstruct _SDL_AudioDeviceEvent
    ([type _uint32]
     [timestamp _uint32]
     [which _uint32]
     [iscapture _uint8]
     [padding1 _uint8]
     [padding2 _uint8]
     [padding3 _uint8]))

(define-cstruct _SDL_TouchFingerEvent
    ([type _uint32]
     [timestamp _uint32]
     [touchId _SDL_TouchID]
     [fingerID _SDL_FingerID]
     [x _float]
     [y _float]
     [dx _float]
     [dy _float]
     [pressure _float]))

(define-cstruct _SDL_MultiGestureEvent
    ([type _uint32]
     [timestamp _uint32]
     [touchId _SDL_TouchID]
     [dTheta _float]
     [dDist _float]
     [x _float]
     [y _float]
     [numFingers _uint16]
     [padding _uint16]))

(define-cstruct _SDL_DollarGestureEvent
    ([type _uint32]
     [timestamp _uint32]
     [touchId _SDL_TouchID]
     [gestureId _SDL_GestureID]
     [numFingers _uint32]
     [error _float]
     [x _float]
     [y _float]))

(define-cstruct _SDL_DropEvent
    ([type _uint32]
     [timestamp _uint32]
     [file _string]
     [windowID _uint32]))

(define-cstruct _SDL_SensorEvent
    ([type _uint32]
     [timestamp _uint32]
     [which _sint32]
     [data (_array _float 6)]))

(define-cstruct _SDL_QuitEvent
    ([type _uint32]
     [timestamp _uint32]))

(define-cstruct _SDL_OSEvent
    ([type _uint32]
     [timestamp _uint32]))

(define-cstruct _SDL_UserEvent
    ([type _uint32]
     [timestamp _uint32]
     [windowID _uint32]
     [code _sint32]
     [data1 _pointer]
     [data2 _pointer]))

(define-cpointer-type _SDL_SysWMmsg*)

(define-cstruct _SDL_SysWMEvent
    ([type _uint32]
     [timestamp _uint32]
     [msg _SDL_SysWMmsg*]))

(define _SDL_Event
    (_union
     _SDL_EventType
     _SDL_CommonEvent
     _SDL_DisplayEvent
     _SDL_WindowEvent
     _SDL_KeyboardEvent
     _SDL_TextEditingEvent
     _SDL_TextInputEvent
     _SDL_MouseMotionEvent
     _SDL_MouseButtonEvent
     _SDL_MouseWheelEvent
     _SDL_JoyAxisEvent
     _SDL_JoyBallEvent
     _SDL_JoyHatEvent
     _SDL_JoyButtonEvent
     _SDL_JoyDeviceEvent
     _SDL_ControllerAxisEvent
     _SDL_ControllerButtonEvent
     _SDL_ControllerDeviceEvent
     _SDL_AudioDeviceEvent
     _SDL_SensorEvent
     _SDL_QuitEvent
     _SDL_UserEvent
     _SDL_SysWMEvent
     _SDL_TouchFingerEvent
     _SDL_MultiGestureEvent
     _SDL_DollarGestureEvent
     _SDL_DropEvent
     (_array _uint8 56)))

(define-cpointer-type _SDL_Event*)

(define-sdl2 SDL_PumpEvents (_fun -> _void))

(define _SDL_eventaction
    (_enum
     '(SDL_ADDEVENT
       SDL_PEEKEVENT
       SDL_GETEVENT)))

(define-sdl2 SDL_PeepEvents
    (_fun _SDL_Event*/null _int _SDL_eventaction _SDL_EventType _SDL_EventType -> _int))

(define-sdl2 SDL_HasEvent (_fun _SDL_EventType -> _SDL_bool))
(define-sdl2 SDL_HasEvents (_fun _SDL_EventType _SDL_EventType -> _SDL_bool))

(define-sdl2 SDL_FlushEvent (_fun _SDL_EventType -> _void))
(define-sdl2 SDL_FlushEvents (_fun _SDL_EventType _SDL_EventType -> _void))

(define-sdl2 SDL_PollEvent (_fun _SDL_Event*/null -> _int))

(define-sdl2 SDL_WaitEvent (_fun _SDL_Event*/null -> _int))

(define-sdl2 SDL_WaitEventTimeout (_fun _SDL_Event*/null _int -> _int))

(define-sdl2 SDL_PushEvent (_fun _SDL_Event* -> _int))

(define _SDL_EventFilter (_fun _pointer _SDL_Event* -> _int))

(define-sdl2 SDL_SetEventFilter (_fun _SDL_EventFilter _pointer -> _void))

(define-sdl2 SDL_GetEventFilter (_fun (_ptr o _SDL_EventFilter) (_ptr o _pointer) -> _SDL_bool))

(define-sdl2 SDL_AddEventWatch (_fun _SDL_EventFilter _pointer -> _void))

(define-sdl2 SDL_DelEventWatch (_fun _SDL_EventFilter _pointer -> _void))

(define-sdl2 SDL_FilterEvents (_fun _SDL_EventFilter _pointer -> _void))

(define SDL_QUERY   -1)
(define SDL_IGNORE   0)
(define SDL_DISABLE  0)
(define SDL_ENABLE   1)

(define-sdl2 SDL_EventState (_fun _SDL_EventType _int -> _uint8))

(define (SDL_GetEventState type) (SDL_EventState type SDL_QUERY))

(define-sdl2 SDL_RegisterEvents (_fun _int -> _uint32))


;; SDL_syswm.h

(define-cpointer-type _SDL_SysWMinfo*)

(define-sdl2 SDL_GetWindowWMInfo (_fun _SDL_Window* _SDL_SysWMinfo* -> _SDL_bool))


;; SDL_quit.h

(define (SDL_QuitRequested)
    (SDL_PumpEvents)
    (> (SDL_PeepEvents #f 0 'SDL_PEEKEVENT 'SDL_QUIT 'SDL_QUIT) 0))


;; SDL_filesystem.h

(define-sdl2 SDL_GetBasePath (_fun -> _string)
    #:make-fail make-not-available)

(define-sdl2 SDL_GetPrefPath (_fun _string _string -> _string)
    #:make-fail make-not-available)


;; SDL_gamecontroller.h

(define-cpointer-type _SDL_GameController*)

(define _SDL_GameControllerBindType
    (_enum
     '(SDL_CONTROLLER_BINDTYPE_NONE = 0
       SDL_CONTROLLER_BINDTYPE_BUTTON
       SDL_CONTROLLER_BINDTYPE_AXIS
       SDL_CONTROLLER_BINDTYPE_HAT)))

(define-cstruct _SDL_GameControllerButtonBind
    ([bindType _SDL_GameControllerBindType]
     [value
      (_union
       _int
       _int
       (make-cstruct-type (list _int _int)))]))

(define-sdl2 SDL_GameControllerAddMappingsFromRW (_fun _SDL_RWops* _int -> _int)
    #:make-fail make-not-available)

(define (SDL_GameControllerAddMappingsFromFile file)
    (SDL_GameControllerAddMappingsFromRW
     (SDL_RWFromFile file "rb")
     1))

(define-sdl2 SDL_GameControllerAddMapping (_fun _string -> _int))

(define-sdl2 SDL_GameControllerNumMappings (_fun -> _int)
    #:make-fail make-not-available)

(define-sdl2 SDL_GameControllerMappingForIndex (_fun _int -> _string)
    #:make-fail make-not-available)

(define-sdl2 SDL_GameControllerMappingForGUID (_fun _SDL_JoystickGUID -> _string))

(define-sdl2 SDL_GameControllerMapping (_fun _SDL_GameController* -> _string))

(define-sdl2 SDL_IsGameController (_fun _int -> _SDL_bool))

(define-sdl2 SDL_GameControllerNameForIndex (_fun _int -> _string))

(define-sdl2 SDL_GameControllerMappingForDeviceIndex (_fun _int -> _string)
    #:make-fail make-not-available)

(define-sdl2 SDL_GameControllerOpen (_fun _int -> _SDL_GameController*/null))

(define-sdl2 SDL_GameControllerFromInstanceID (_fun _SDL_JoystickID -> _SDL_GameController*/null)
    #:make-fail make-not-available)

(define-sdl2 SDL_GameControllerName (_fun _SDL_GameController* -> _string))

(define-sdl2 SDL_GameControllerGetPlayerIndex (_fun _SDL_GameController* -> _int)
    #:make-fail make-not-available)

(define-sdl2 SDL_GameControllerGetVendor (_fun _SDL_GameController* -> _uint16)
    #:make-fail make-not-available)

(define-sdl2 SDL_GameControllerGetProduct (_fun _SDL_GameController* -> _uint16)
    #:make-fail make-not-available)

(define-sdl2 SDL_GameControllerGetProductVersion (_fun _SDL_GameController* -> _uint16)
    #:make-fail make-not-available)

(define-sdl2 SDL_GameControllerGetAttached (_fun _SDL_GameController* -> _SDL_bool))

(define-sdl2 SDL_GameControllerGetJoystick (_fun _SDL_GameController* -> _SDL_Joystick*/null))

(define-sdl2 SDL_GameControllerEventState (_fun _int -> _int))

(define-sdl2 SDL_GameControllerUpdate (_fun -> _void))

(define _SDL_GameControllerAxis
    (_enum
     '(SDL_CONTROLLER_AXIS_INVALID = -1
       SDL_CONTROLLER_AXIS_LEFTX
       SDL_CONTROLLER_AXIS_LEFTY
       SDL_CONTROLLER_AXIS_RIGHTX
       SDL_CONTROLLER_AXIS_RIGHTY
       SDL_CONTROLLER_AXIS_TRIGGERLEFT
       SDL_CONTROLLER_AXIS_TRIGGERRIGHT
       SDL_CONTROLLER_AXIS_MAX)))

(define-sdl2 SDL_GameControllerGetAxisFromString (_fun _string -> _SDL_GameControllerAxis))

(define-sdl2 SDL_GameControllerGetStringForAxis (_fun _SDL_GameControllerAxis -> _string))

(define-sdl2 SDL_GameControllerGetBindForAxis
    (_fun _SDL_GameController* _SDL_GameControllerAxis -> _SDL_GameControllerButtonBind))

(define-sdl2 SDL_GameControllerGetAxis (_fun _SDL_GameController* _SDL_GameControllerAxis -> _sint16))

(define _SDL_GameControllerButton
    (_enum
     '(SDL_CONTROLLER_BUTTON_INVALID = -1
       SDL_CONTROLLER_BUTTON_A
       SDL_CONTROLLER_BUTTON_B
       SDL_CONTROLLER_BUTTON_X
       SDL_CONTROLLER_BUTTON_Y
       SDL_CONTROLLER_BUTTON_BACK
       SDL_CONTROLLER_BUTTON_GUIDE
       SDL_CONTROLLER_BUTTON_START
       SDL_CONTROLLER_BUTTON_LEFTSTICK
       SDL_CONTROLLER_BUTTON_RIGHTSTICK
       SDL_CONTROLLER_BUTTON_LEFTSHOULDER
       SDL_CONTROLLER_BUTTON_RIGHTSHOULDER
       SDL_CONTROLLER_BUTTON_DPAD_UP
       SDL_CONTROLLER_BUTTON_DPAD_DOWN
       SDL_CONTROLLER_BUTTON_DPAD_LEFT
       SDL_CONTROLLER_BUTTON_DPAD_RIGHT
       SDL_CONTROLLER_BUTTON_MAX)))

(define-sdl2 SDL_GameControllerGetButtonFromString (_fun _string -> _SDL_GameControllerButton))

(define-sdl2 SDL_GameControllerGetStringForButton (_fun _SDL_GameControllerButton -> _string))

(define-sdl2 SDL_GameControllerGetBindForButton
    (_fun _SDL_GameController* _SDL_GameControllerButton -> _SDL_GameControllerButtonBind))

(define-sdl2 SDL_GameControllerGetButton
    (_fun _SDL_GameController* _SDL_GameControllerButton -> _uint8))

(define-sdl2 SDL_GameControllerRumble (_fun _SDL_GameController* _uint16 _uint16 _uint32 -> _int)
    #:make-fail make-not-available)

(define-sdl2 SDL_GameControllerClose (_fun _SDL_GameController* -> _void))


;; SDL_haptic.h

(define-cpointer-type _SDL_Haptic*)

(define SDL_HAPTIC_CONSTANT   (arithmetic-shift 1 0))

(define SDL_HAPTIC_SINE       (arithmetic-shift 1 1))

(define SDL_HAPTIC_LEFTRIGHT  (arithmetic-shift 1 2))

(define SDL_HAPTIC_TRIANGLE   (arithmetic-shift 1 3))

(define SDL_HAPTIC_SAWTOOTHUP (arithmetic-shift 1 4))

(define SDL_HAPTIC_SAWTOOTHDOWN (arithmetic-shift 1 5))

(define SDL_HAPTIC_RAMP       (arithmetic-shift 1 6))

(define SDL_HAPTIC_SPRING     (arithmetic-shift 1 7))

(define SDL_HAPTIC_DAMPER     (arithmetic-shift 1 8))

(define SDL_HAPTIC_INERTIA    (arithmetic-shift 1 9))

(define SDL_HAPTIC_FRICTION   (arithmetic-shift 1 10))

(define SDL_HAPTIC_CUSTOM     (arithmetic-shift 1 11))

(define SDL_HAPTIC_GAIN       (arithmetic-shift 1 12))

(define SDL_HAPTIC_AUTOCENTER (arithmetic-shift 1 13))

(define SDL_HAPTIC_STATUS     (arithmetic-shift 1 14))

(define SDL_HAPTIC_PAUSE      (arithmetic-shift 1 15))

(define SDL_HAPTIC_POLAR      0)

(define SDL_HAPTIC_CARTESIAN  1)

(define SDL_HAPTIC_SPHERICAL  2)

(define SDL_HAPTIC_INFINITY   4294967295)

(define-cstruct _SDL_HapticDirection
    ([type _uint8]
     [dir (_array _sint32 3)]))

(define-cstruct _SDL_HapticConstant
    ([type _uint16]
     [direction _SDL_HapticDirection]
     [length _uint32]
     [delay _uint16]
     [button _uint16]
     [interval _uint16]
     [level _sint16]
     [attack_length _uint16]
     [attack_level _uint16]
     [fade_length _uint16]
     [fade_level _uint16]))

(define-cstruct _SDL_HapticPeriodic
    ([type _uint16]
     [direction _SDL_HapticDirection]
     [length _uint32]
     [delay _uint16]
     [button _uint16]
     [interval _uint16]
     [period _uint16]
     [magnitude _sint16]
     [offset _sint16]
     [phase _uint16]
     [attack_length _uint16]
     [attack_level _uint16]
     [fade_length _uint16]
     [fade_level _uint16]))

(define-cstruct _SDL_HapticCondition
    ([type _uint16]
     [direction _SDL_HapticDirection]
     [length _uint32]
     [delay _uint16]
     [button _uint16]
     [interval _uint16]
     [right_sat (_array _uint16 3)]
     [left_sat (_array _uint16 3)]
     [right_coeff (_array _sint16 3)]
     [left_coeff (_array _sint16 3)]
     [deadband (_array _uint16 3)]
     [center (_array _sint16 3)]))

(define-cstruct _SDL_HapticRamp
    ([type _uint16]
     [direction _SDL_HapticDirection]
     [length _uint32]
     [delay _uint16]
     [button _uint16]
     [interval _uint16]
     [start _sint16]
     [end _sint16]
     [attack_length _uint16]
     [attack_level _uint16]
     [fade_length _uint16]
     [fade_level _uint16]))

(define-cstruct _SDL_HapticLeftRight
    ([type _uint16]
     [length _uint32]
     [large_magnitude _uint16]
     [small_magnitude _uint16]))

(define-cstruct _SDL_HapticCustom
    ([type _uint16]
     [direction _SDL_HapticDirection]
     [length _uint32]
     [delay _uint16]
     [button _uint16]
     [interval _uint16]
     [channels _uint8]
     [period _uint16]
     [samples _uint16]
     [data _uint16*]
     [attack_length _uint16]
     [attack_level _uint16]
     [fade_length _uint16]
     [fade_level _uint16]))

(define _SDL_HapticEffect
    (_union
     _uint16
     _SDL_HapticConstant
     _SDL_HapticPeriodic
     _SDL_HapticCondition
     _SDL_HapticRamp
     _SDL_HapticLeftRight
     _SDL_HapticCustom))

(define-cpointer-type _SDL_HapticEffect*)

(define-sdl2 SDL_NumHaptics (_fun -> _int))

(define-sdl2 SDL_HapticName (_fun _int -> _string))

(define-sdl2 SDL_HapticOpen (_fun _int -> _SDL_Haptic*/null))

(define-sdl2 SDL_HapticOpened (_fun _int -> _int))

(define-sdl2 SDL_HapticIndex (_fun _SDL_Haptic* -> _int))

(define-sdl2 SDL_MouseIsHaptic (_fun -> _int))

(define-sdl2 SDL_HapticOpenFromMouse (_fun -> _SDL_Haptic*/null))

(define-sdl2 SDL_JoystickIsHaptic (_fun _SDL_Joystick* -> _int))

(define-sdl2 SDL_HapticOpenFromJoystick (_fun _SDL_Joystick* -> _SDL_Haptic*/null))

(define-sdl2 SDL_HapticClose (_fun _SDL_Haptic* -> _void))

(define-sdl2 SDL_HapticNumEffects (_fun _SDL_Haptic* -> _int))

(define-sdl2 SDL_HapticNumEffectsPlaying (_fun _SDL_Haptic* -> _int))

(define-sdl2 SDL_HapticQuery (_fun _SDL_Haptic* -> _int))

(define-sdl2 SDL_HapticNumAxes (_fun _SDL_Haptic* -> _int))

(define-sdl2 SDL_HapticEffectSupported (_fun _SDL_Haptic* _SDL_HapticEffect* -> _int))

(define-sdl2 SDL_HapticNewEffect (_fun _SDL_Haptic* _SDL_HapticEffect* -> _int))

(define-sdl2 SDL_HapticUpdateEffect (_fun _SDL_Haptic* _int _SDL_HapticEffect* -> _int))

(define-sdl2 SDL_HapticRunEffect (_fun _SDL_Haptic* _int _uint32 -> _int))

(define-sdl2 SDL_HapticStopEffect (_fun _SDL_Haptic* _int -> _int))

(define-sdl2 SDL_HapticDestroyEffect (_fun _SDL_Haptic* _int -> _void))

(define-sdl2 SDL_HapticGetEffectStatus (_fun _SDL_Haptic* _int -> _int))

(define-sdl2 SDL_HapticSetGain (_fun _SDL_Haptic* _int -> _int))

(define-sdl2 SDL_HapticSetAutocenter (_fun _SDL_Haptic* _int -> _int))

(define-sdl2 SDL_HapticPause (_fun _SDL_Haptic* -> _int))

(define-sdl2 SDL_HapticUnpause (_fun _SDL_Haptic* -> _int))

(define-sdl2 SDL_HapticStopAll (_fun _SDL_Haptic* -> _int))

(define-sdl2 SDL_HapticRumbleSupported (_fun _SDL_Haptic* -> _int))

(define-sdl2 SDL_HapticRumbleInit (_fun _SDL_Haptic* -> _int))

(define-sdl2 SDL_HapticRumblePlay (_fun _SDL_Haptic* _float _uint32 -> _int))

(define-sdl2 SDL_HapticRumbleStop (_fun _SDL_Haptic* -> _int))


;; SDL_hints.h

(define SDL_HINT_FRAMEBUFFER_ACCELERATION   "SDL_FRAMEBUFFER_ACCELERATION")

(define SDL_HINT_RENDER_DRIVER              "SDL_RENDER_DRIVER")

(define SDL_HINT_RENDER_OPENGL_SHADERS      "SDL_RENDER_OPENGL_SHADERS")

(define SDL_HINT_RENDER_DIRECT3D_THREADSAFE "SDL_RENDER_DIRECT3D_THREADSAFE")

(define SDL_HINT_RENDER_DIRECT3D11_DEBUG    "SDL_RENDER_DIRECT3D11_DEBUG")

(define SDL_HINT_RENDER_LOGICAL_SIZE_MODE       "SDL_RENDER_LOGICAL_SIZE_MODE")

(define SDL_HINT_RENDER_SCALE_QUALITY       "SDL_RENDER_SCALE_QUALITY")

(define SDL_HINT_RENDER_VSYNC               "SDL_RENDER_VSYNC")

(define SDL_HINT_VIDEO_ALLOW_SCREENSAVER    "SDL_VIDEO_ALLOW_SCREENSAVER")

(define SDL_HINT_VIDEO_X11_XVIDMODE         "SDL_VIDEO_X11_XVIDMODE")

(define SDL_HINT_VIDEO_X11_XINERAMA         "SDL_VIDEO_X11_XINERAMA")

(define SDL_HINT_VIDEO_X11_XRANDR           "SDL_VIDEO_X11_XRANDR")

(define SDL_HINT_VIDEO_X11_NET_WM_PING      "SDL_VIDEO_X11_NET_WM_PING")

(define SDL_HINT_VIDEO_X11_NET_WM_BYPASS_COMPOSITOR "SDL_VIDEO_X11_NET_WM_BYPASS_COMPOSITOR")

(define SDL_HINT_WINDOW_FRAME_USABLE_WHILE_CURSOR_HIDDEN
    "SDL_WINDOW_FRAME_USABLE_WHILE_CURSOR_HIDDEN")

(define SDL_HINT_WINDOWS_INTRESOURCE_ICON       "SDL_WINDOWS_INTRESOURCE_ICON")
(define SDL_HINT_WINDOWS_INTRESOURCE_ICON_SMALL "SDL_WINDOWS_INTRESOURCE_ICON_SMALL")

(define SDL_HINT_WINDOWS_ENABLE_MESSAGELOOP "SDL_WINDOWS_ENABLE_MESSAGELOOP")

(define SDL_HINT_GRAB_KEYBOARD              "SDL_GRAB_KEYBOARD")

(define SDL_HINT_MOUSE_DOUBLE_CLICK_TIME    "SDL_MOUSE_DOUBLE_CLICK_TIME")

(define SDL_HINT_MOUSE_DOUBLE_CLICK_RADIUS    "SDL_MOUSE_DOUBLE_CLICK_RADIUS")

(define SDL_HINT_MOUSE_NORMAL_SPEED_SCALE    "SDL_MOUSE_NORMAL_SPEED_SCALE")

(define SDL_HINT_MOUSE_RELATIVE_SPEED_SCALE    "SDL_MOUSE_RELATIVE_SPEED_SCALE")

(define SDL_HINT_MOUSE_RELATIVE_MODE_WARP    "SDL_MOUSE_RELATIVE_MODE_WARP")

(define SDL_HINT_MOUSE_FOCUS_CLICKTHROUGH "SDL_MOUSE_FOCUS_CLICKTHROUGH")

(define SDL_HINT_TOUCH_MOUSE_EVENTS    "SDL_TOUCH_MOUSE_EVENTS")

(define SDL_HINT_VIDEO_MINIMIZE_ON_FOCUS_LOSS   "SDL_VIDEO_MINIMIZE_ON_FOCUS_LOSS")

(define SDL_HINT_IDLE_TIMER_DISABLED "SDL_IOS_IDLE_TIMER_DISABLED")

(define SDL_HINT_ORIENTATIONS "SDL_IOS_ORIENTATIONS")

(define SDL_HINT_APPLE_TV_CONTROLLER_UI_EVENTS "SDL_APPLE_TV_CONTROLLER_UI_EVENTS")

(define SDL_HINT_APPLE_TV_REMOTE_ALLOW_ROTATION "SDL_APPLE_TV_REMOTE_ALLOW_ROTATION")

(define SDL_HINT_IOS_HIDE_HOME_INDICATOR "SDL_IOS_HIDE_HOME_INDICATOR")

(define SDL_HINT_ACCELEROMETER_AS_JOYSTICK "SDL_ACCELEROMETER_AS_JOYSTICK")

(define SDL_HINT_TV_REMOTE_AS_JOYSTICK "SDL_TV_REMOTE_AS_JOYSTICK")

(define SDL_HINT_XINPUT_ENABLED "SDL_XINPUT_ENABLED")

(define SDL_HINT_XINPUT_USE_OLD_JOYSTICK_MAPPING "SDL_XINPUT_USE_OLD_JOYSTICK_MAPPING")

(define SDL_HINT_GAMECONTROLLERCONFIG "SDL_GAMECONTROLLERCONFIG")

(define SDL_HINT_GAMECONTROLLER_IGNORE_DEVICES "SDL_GAMECONTROLLER_IGNORE_DEVICES")

(define SDL_HINT_GAMECONTROLLER_IGNORE_DEVICES_EXCEPT "SDL_GAMECONTROLLER_IGNORE_DEVICES_EXCEPT")

(define SDL_HINT_JOYSTICK_ALLOW_BACKGROUND_EVENTS "SDL_JOYSTICK_ALLOW_BACKGROUND_EVENTS")

(define SDL_HINT_JOYSTICK_HIDAPI "SDL_JOYSTICK_HIDAPI")

(define SDL_HINT_JOYSTICK_HIDAPI_PS4 "SDL_JOYSTICK_HIDAPI_PS4")

(define SDL_HINT_JOYSTICK_HIDAPI_PS4_RUMBLE "SDL_JOYSTICK_HIDAPI_PS4_RUMBLE")

(define SDL_HINT_JOYSTICK_HIDAPI_STEAM "SDL_JOYSTICK_HIDAPI_STEAM")

(define SDL_HINT_JOYSTICK_HIDAPI_SWITCH "SDL_JOYSTICK_HIDAPI_SWITCH")

(define SDL_HINT_JOYSTICK_HIDAPI_XBOX   "SDL_JOYSTICK_HIDAPI_XBOX")

(define SDL_HINT_ENABLE_STEAM_CONTROLLERS "SDL_ENABLE_STEAM_CONTROLLERS")

(define SDL_HINT_ALLOW_TOPMOST "SDL_ALLOW_TOPMOST")

(define SDL_HINT_TIMER_RESOLUTION "SDL_TIMER_RESOLUTION")

(define SDL_HINT_QTWAYLAND_CONTENT_ORIENTATION "SDL_QTWAYLAND_CONTENT_ORIENTATION")

(define SDL_HINT_QTWAYLAND_WINDOW_FLAGS "SDL_QTWAYLAND_WINDOW_FLAGS")

(define SDL_HINT_THREAD_STACK_SIZE              "SDL_THREAD_STACK_SIZE")

(define SDL_HINT_VIDEO_HIGHDPI_DISABLED "SDL_VIDEO_HIGHDPI_DISABLED")

(define SDL_HINT_MAC_CTRL_CLICK_EMULATE_RIGHT_CLICK "SDL_MAC_CTRL_CLICK_EMULATE_RIGHT_CLICK")

(define SDL_HINT_VIDEO_WIN_D3DCOMPILER              "SDL_VIDEO_WIN_D3DCOMPILER")

(define SDL_HINT_VIDEO_WINDOW_SHARE_PIXEL_FORMAT    "SDL_VIDEO_WINDOW_SHARE_PIXEL_FORMAT")

(define SDL_HINT_WINRT_PRIVACY_POLICY_URL "SDL_WINRT_PRIVACY_POLICY_URL")

(define SDL_HINT_WINRT_PRIVACY_POLICY_LABEL "SDL_WINRT_PRIVACY_POLICY_LABEL")

(define SDL_HINT_WINRT_HANDLE_BACK_BUTTON "SDL_WINRT_HANDLE_BACK_BUTTON")

(define SDL_HINT_VIDEO_MAC_FULLSCREEN_SPACES    "SDL_VIDEO_MAC_FULLSCREEN_SPACES")

(define SDL_HINT_MAC_BACKGROUND_APP    "SDL_MAC_BACKGROUND_APP")

(define SDL_HINT_ANDROID_APK_EXPANSION_MAIN_FILE_VERSION "SDL_ANDROID_APK_EXPANSION_MAIN_FILE_VERSION")

(define SDL_HINT_ANDROID_APK_EXPANSION_PATCH_FILE_VERSION
    "SDL_ANDROID_APK_EXPANSION_PATCH_FILE_VERSION")

(define SDL_HINT_IME_INTERNAL_EDITING "SDL_IME_INTERNAL_EDITING")

(define SDL_HINT_ANDROID_SEPARATE_MOUSE_AND_TOUCH "SDL_ANDROID_SEPARATE_MOUSE_AND_TOUCH")

(define SDL_HINT_ANDROID_TRAP_BACK_BUTTON "SDL_ANDROID_TRAP_BACK_BUTTON")

(define SDL_HINT_RETURN_KEY_HIDES_IME "SDL_RETURN_KEY_HIDES_IME")

(define SDL_HINT_EMSCRIPTEN_KEYBOARD_ELEMENT   "SDL_EMSCRIPTEN_KEYBOARD_ELEMENT")

(define SDL_HINT_NO_SIGNAL_HANDLERS   "SDL_NO_SIGNAL_HANDLERS")

(define SDL_HINT_WINDOWS_NO_CLOSE_ON_ALT_F4 "SDL_WINDOWS_NO_CLOSE_ON_ALT_F4")

(define SDL_HINT_BMP_SAVE_LEGACY_FORMAT "SDL_BMP_SAVE_LEGACY_FORMAT")

(define SDL_HINT_WINDOWS_DISABLE_THREAD_NAMING "SDL_WINDOWS_DISABLE_THREAD_NAMING")

(define SDL_HINT_RPI_VIDEO_LAYER           "SDL_RPI_VIDEO_LAYER")

(define SDL_HINT_VIDEO_DOUBLE_BUFFER      "SDL_VIDEO_DOUBLE_BUFFER")

(define SDL_HINT_OPENGL_ES_DRIVER   "SDL_OPENGL_ES_DRIVER")

(define SDL_HINT_AUDIO_RESAMPLING_MODE   "SDL_AUDIO_RESAMPLING_MODE")

(define SDL_HINT_AUDIO_CATEGORY   "SDL_AUDIO_CATEGORY")

(define _SDL_HintPriority
    (_enum
     '(SDL_HINT_DEFAULT
       SDL_HINT_NORMAL
       SDL_HINT_OVERRIDE)))

(define-sdl2 SDL_SetHintWithPriority (_fun _string _string _SDL_HintPriority -> _SDL_bool))

(define-sdl2 SDL_SetHint (_fun _string _string -> _SDL_bool))

(define-sdl2 SDL_GetHint (_fun _string -> _string))

(define-sdl2 SDL_GetHintBoolean (_fun _string _SDL_bool -> _SDL_bool)
    #:make-fail make-not-available)

(define _SDL_HintCallback (_fun _pointer _string _string _string -> _void))

(define-sdl2 SDL_AddHintCallback (_fun _string _SDL_HintCallback _pointer -> _void))

(define-sdl2 SDL_DelHintCallback (_fun _string _SDL_HintCallback _pointer -> _void))

(define-sdl2 SDL_ClearHints (_fun -> _void))


;; SDL_loadso.h

(define-sdl2 SDL_LoadObject (_fun _string -> _pointer))

(define-sdl2 SDL_LoadFunction (_fun _pointer _string -> _pointer))

(define-sdl2 SDL_UnloadObject (_fun _pointer -> _void))


;; SDL_log.h

(define SDL_MAX_LOG_MESSAGE 4096)

(define SDL_LOG_CATEGORY_APPLICATION 0)
(define SDL_LOG_CATEGORY_ERROR 1)
(define SDL_LOG_CATEGORY_ASSERT 2)
(define SDL_LOG_CATEGORY_SYSTEM 3)
(define SDL_LOG_CATEGORY_AUDIO 4)
(define SDL_LOG_CATEGORY_VIDEO 5)
(define SDL_LOG_CATEGORY_RENDER 6)
(define SDL_LOG_CATEGORY_INPUT 7)
(define SDL_LOG_CATEGORY_TEST 8)

(define SDL_LOG_CATEGORY_RESERVED1 9)
(define SDL_LOG_CATEGORY_RESERVED2 10)
(define SDL_LOG_CATEGORY_RESERVED3 11)
(define SDL_LOG_CATEGORY_RESERVED4 12)
(define SDL_LOG_CATEGORY_RESERVED5 13)
(define SDL_LOG_CATEGORY_RESERVED6 14)
(define SDL_LOG_CATEGORY_RESERVED7 15)
(define SDL_LOG_CATEGORY_RESERVED8 16)
(define SDL_LOG_CATEGORY_RESERVED9 17)
(define SDL_LOG_CATEGORY_RESERVED10 18)

(define SDL_LOG_CATEGORY_CUSTOM 19)

(define _SDL_LogPriority
    (_enum
     '(SDL_LOG_PRIORITY_VERBOSE = 1
       SDL_LOG_PRIORITY_DEBUG
       SDL_LOG_PRIORITY_INFO
       SDL_LOG_PRIORITY_WARN
       SDL_LOG_PRIORITY_ERROR
       SDL_LOG_PRIORITY_CRITICAL
       SDL_NUM_LOG_PRIORITIES)))

(define-sdl2 SDL_LogSetAllPriority (_fun _SDL_LogPriority -> _void))

(define-sdl2 SDL_LogSetPriority (_fun _int _SDL_LogPriority -> _void))

(define-sdl2 SDL_LogGetPriority (_fun _int -> _SDL_LogPriority))

(define-sdl2 SDL_LogResetPriorities (_fun -> _void))

(define-sdl2-vararg SDL_Log (_string) _void)

(define-sdl2-vararg SDL_LogVerbose (_int _string) _void)

(define-sdl2-vararg SDL_LogDebug (_int _string) _void)

(define-sdl2-vararg SDL_LogInfo (_int _string) _void)

(define-sdl2-vararg SDL_LogWarn (_int _string) _void)

(define-sdl2-vararg SDL_LogError (_int _string) _void)

(define-sdl2-vararg SDL_LogCritical (_int _string) _void)

(define-sdl2-vararg SDL_LogMessage (_int _SDL_LogPriority _string) _void)

(define _SDL_LogOutputFunction (_fun _pointer _int _SDL_LogPriority _string -> _void))

(define-sdl2 SDL_LogGetOutputFunction (_fun _SDL_LogOutputFunction (_ptr o _pointer) -> _void))

(define-sdl2 SDL_LogSetOutputFunction (_fun _SDL_LogOutputFunction _pointer -> _void))


;; SDL_main.h

(define-sdl2 SDL_SetMainReady (_fun -> _void))

(define-sdl2 SDL_RegisterApp (_fun _string _uint32 _pointer -> _int)
    #:make-fail make-not-available)

(define-sdl2 SDL_UnregisterApp (_fun -> _void)
    #:make-fail make-not-available)

(define-sdl2 SDL_WinRTRunApp (_fun (_fun _int (_ptr i _string) -> _int) _pointer -> _int)
    #:make-fail make-not-available)


;; SDL_messagebox.h

(define _SDL_MessageBoxFlags
    (_enum
     '(SDL_MESSAGEBOX_ERROR        = #x00000010
       SDL_MESSAGEBOX_WARNING      = #x00000020
       SDL_MESSAGEBOX_INFORMATION  = #x00000040)
     _uint32))

(define _SDL_MessageBoxButtonFlags
    (_enum
     '(SDL_MESSAGEBOX_BUTTON_RETURNKEY_DEFAULT = #x00000001
       SDL_MESSAGEBOX_BUTTON_ESCAPEKEY_DEFAULT = #x00000002)
     _uint32))

(define-cstruct _SDL_MessageBoxButtonData
    ([flags _SDL_MessageBoxButtonFlags]
     [buttonid _int]
     [text _string]))
(define _SDL_MessageBoxButtonData* _SDL_MessageBoxButtonData-pointer)

(define-cstruct _SDL_MessageBoxColor
    ([r _uint8]
     [g _uint8]
     [b _uint8]))

(define _SDL_MessageBoxColorType
    (_enum
     '(SDL_MESSAGEBOX_COLOR_BACKGROUND
       SDL_MESSAGEBOX_COLOR_TEXT
       SDL_MESSAGEBOX_COLOR_BUTTON_BORDER
       SDL_MESSAGEBOX_COLOR_BUTTON_BACKGROUND
       SDL_MESSAGEBOX_COLOR_BUTTON_SELECTED
       SDL_MESSAGEBOX_COLOR_MAX)))

(define-cstruct _SDL_MessageBoxColorScheme
    ([colors
      (_array _SDL_MessageBoxColor
              (cast 'SDL_MESSAGEBOX_COLOR_MAX
                    _SDL_MessageBoxColorType
                    _ufixint))]))
(define _SDL_MessageBoxColorScheme* _SDL_MessageBoxColorScheme-pointer)

(define-cstruct _SDL_MessageBoxData
    ([flags _SDL_MessageBoxFlags]
     [window _SDL_Window*]
     [title _string]
     [message _string]
     [numbuttons _int]
     [buttons _SDL_MessageBoxButtonData*]
     [colorScheme _SDL_MessageBoxColorScheme*]))

(define _SDL_MessageBoxData* _SDL_MessageBoxData-pointer)

(define-sdl2 SDL_ShowMessageBox (_fun _SDL_MessageBoxData* _int* -> _int))

(define-sdl2 SDL_ShowSimpleMessageBox (_fun _SDL_MessageBoxFlags _string _string _SDL_Window*/null -> _int))


;; SDL_mutex.h

(define SDL_MUTEX_TIMEDOUT  1)

(define SDL_MUTEX_MAXWAIT   (cast -1 _int _uint32))

(define-cpointer-type _SDL_mutex*)

(define-sdl2 SDL_CreateMutex (_fun -> _SDL_mutex*/null))

(define-sdl2 SDL_LockMutex (_fun _SDL_mutex* -> _int))
(define SDL_mutexP SDL_LockMutex)

(define-sdl2 SDL_TryLockMutex (_fun _SDL_mutex* -> _int))

(define-sdl2 SDL_UnlockMutex (_fun _SDL_mutex* -> _int))
(define SDL_mutexV SDL_UnlockMutex)

(define-sdl2 SDL_DestroyMutex (_fun _SDL_mutex* -> _void))

(define-cpointer-type _SDL_sem*)

(define-sdl2 SDL_CreateSemaphore (_fun _uint32 -> _SDL_sem*/null))

(define-sdl2 SDL_DestroySemaphore (_fun _SDL_sem* -> _void))

(define-sdl2 SDL_SemWait (_fun _SDL_sem* -> _int))

(define-sdl2 SDL_SemTryWait (_fun _SDL_sem* -> _int))

(define-sdl2 SDL_SemWaitTimeout (_fun _SDL_sem* _uint32 -> _int))

(define-sdl2 SDL_SemPost (_fun _SDL_sem* -> _int))

(define-sdl2 SDL_SemValue (_fun _SDL_sem* -> _uint32))

(define-cpointer-type _SDL_cond*)

(define-sdl2 SDL_CreateCond (_fun -> _SDL_cond*/null))

(define-sdl2 SDL_DestroyCond (_fun _SDL_cond* -> _void))

(define-sdl2 SDL_CondSignal (_fun _SDL_cond* -> _int))

(define-sdl2 SDL_CondBroadcast (_fun _SDL_cond* -> _int))

(define-sdl2 SDL_CondWait (_fun _SDL_cond* _SDL_mutex* -> _int))

(define-sdl2 SDL_CondWaitTimeout (_fun _SDL_cond* _SDL_mutex* _uint32 -> _int))


;; SDL_power.h

(define _SDL_PowerState
    (_enum
     '(SDL_POWERSTATE_UNKNOWN
       SDL_POWERSTATE_ON_BATTERY
       SDL_POWERSTATE_NO_BATTERY
       SDL_POWERSTATE_CHARGING
       SDL_POWERSTATE_CHARGED)))

(define-sdl2 SDL_GetPowerInfo (_fun _int*/null _int*/null -> _SDL_PowerState))


;; SDL_render.h

(define _SDL_RendererFlags
    (_bitmask
     '(SDL_RENDERER_SOFTWARE = #x00000001
       SDL_RENDERER_ACCELERATED = #x00000002
       SDL_RENDERER_PRESENTVSYNC = #x00000004
       SDL_RENDERER_TARGETTEXTURE = #x00000008)
     _uint32))

(define-cstruct _SDL_RendererInfo
    ([name _string]
     [flags _uint32]
     [num_texture_formats _uint32]
     [texture_formats (_array _uint32 16)]
     [max_texture_width _int]
     [max_texture_height _int]))

(define _SDL_RendererInfo* _SDL_RendererInfo-pointer)

(define _SDL_TextureAccess
    (_enum
     '(SDL_TEXTUREACCESS_STATIC
       SDL_TEXTUREACCESS_STREAMING
       SDL_TEXTUREACCESS_TARGET)
     _int))

(define _SDL_TextureModulate
    (_enum
     '(SDL_TEXTUREMODULATE_NONE = #x00000000
       SDL_TEXTUREMODULATE_COLOR = #x00000001
       SDL_TEXTUREMODULATE_ALPHA = #x00000002)))

(define _SDL_RendererFlip
    (_enum
     '(SDL_FLIP_NONE = #x00000000
       SDL_FLIP_HORIZONTAL = #x00000001
       SDL_FLIP_VERTICAL = #x00000002)))

(define-cpointer-type _SDL_Renderer*)

(define-cpointer-type _SDL_Texture*)

(define-sdl2 SDL_GetNumRenderDrivers (_fun -> _int))

(define-sdl2 SDL_GetRenderDriverInfo (_fun _int _SDL_RendererInfo* -> _int))

(define-sdl2 SDL_CreateWindowAndRenderer
    (_fun _int _int _SDL_WindowFlags (_ptr o _SDL_Window*) (_ptr o _SDL_Renderer*) -> _int))

(define-sdl2 SDL_CreateRenderer (_fun _SDL_Window* _int _SDL_RendererFlags -> _SDL_Renderer*/null))

(define-sdl2 SDL_CreateSoftwareRenderer (_fun _SDL_Surface* -> _SDL_Renderer*/null))

(define-sdl2 SDL_GetRenderer (_fun _SDL_Window* -> _SDL_Renderer*/null))

(define-sdl2 SDL_GetRendererInfo (_fun _SDL_Renderer* _SDL_RendererInfo* -> _int))

(define-sdl2 SDL_GetRendererOutputSize (_fun _SDL_Renderer* _int*/null _int*/null -> _int))

(define-sdl2 SDL_CreateTexture
    (_fun _SDL_Renderer* _uint32 _SDL_TextureAccess _int _int -> _SDL_Texture*/null))

(define-sdl2 SDL_CreateTextureFromSurface (_fun _SDL_Renderer* _SDL_Surface* -> _SDL_Texture*/null))

(define-sdl2 SDL_QueryTexture
    (_fun _SDL_Texture* _uint32*/null _int*/null _int*/null _int*/null -> _int))

(define-sdl2 SDL_SetTextureColorMod (_fun _SDL_Texture* _uint8 _uint8 _uint8 -> _int))

(define-sdl2 SDL_GetTextureColorMod
    (_fun _SDL_Texture* _uint8*/null _uint8*/null _uint8*/null -> _int))

(define-sdl2 SDL_SetTextureAlphaMod (_fun _SDL_Texture* _uint8 -> _int))

(define-sdl2 SDL_GetTextureAlphaMod (_fun _SDL_Texture* _uint8* -> _int))

(define-sdl2 SDL_SetTextureBlendMode (_fun _SDL_Texture* _SDL_BlendMode -> _int))

(define-sdl2 SDL_GetTextureBlendMode (_fun _SDL_Texture* _SDL_BlendMode* -> _int))

(define-sdl2 SDL_UpdateTexture (_fun _SDL_Texture* _SDL_Rect*/null _pointer _int -> _int))

(define-sdl2 SDL_UpdateYUVTexture
    (_fun _SDL_Texture* _SDL_Rect*/null _uint8* _int _uint8* _int _uint8* _int -> _int)
    #:make-fail make-not-available)

(define-sdl2 SDL_LockTexture (_fun _SDL_Texture* _SDL_Rect*/null (_ptr o _pointer) _int* -> _int))

(define-sdl2 SDL_UnlockTexture (_fun _SDL_Texture* -> _void))

(define-sdl2 SDL_RenderTargetSupported (_fun _SDL_Renderer* -> _SDL_bool))

(define-sdl2 SDL_SetRenderTarget (_fun _SDL_Renderer* _SDL_Texture*/null -> _int))

(define-sdl2 SDL_GetRenderTarget (_fun _SDL_Renderer* -> _SDL_Texture*/null))

(define-sdl2 SDL_RenderSetLogicalSize (_fun _SDL_Renderer* _int _int -> _int))

(define-sdl2 SDL_RenderGetLogicalSize (_fun _SDL_Renderer* _int*/null _int*/null -> _void))

(define-sdl2 SDL_RenderSetIntegerScale (_fun _SDL_Renderer* _SDL_bool -> _int)
    #:make-fail make-not-available)

(define-sdl2 SDL_RenderGetIntegerScale (_fun _SDL_Renderer* -> _SDL_bool)
    #:make-fail make-not-available)

(define-sdl2 SDL_RenderSetViewport (_fun _SDL_Renderer* _SDL_Rect*/null -> _int))

(define-sdl2 SDL_RenderGetViewport (_fun _SDL_Renderer* _SDL_Rect* -> _void))

(define-sdl2 SDL_RenderSetClipRect (_fun _SDL_Renderer* _SDL_Rect*/null -> _int))

(define-sdl2 SDL_RenderGetClipRect (_fun _SDL_Renderer* _SDL_Rect* -> _void))

(define-sdl2 SDL_RenderIsClipEnabled (_fun _SDL_Renderer* -> _SDL_bool)
    #:make-fail make-not-available)

(define-sdl2 SDL_RenderSetScale (_fun _SDL_Renderer* _float _float -> _int))

(define-sdl2 SDL_RenderGetScale (_fun _SDL_Renderer* _float*/null _float*/null -> _void))

(define-sdl2 SDL_SetRenderDrawColor (_fun _SDL_Renderer* _uint8 _uint8 _uint8 _uint8 -> _int))

(define-sdl2 SDL_GetRenderDrawColor
    (_fun _SDL_Renderer* _uint8*/null _uint8*/null _uint8*/null _uint8*/null -> _int))

(define-sdl2 SDL_SetRenderDrawBlendMode (_fun _SDL_Renderer* _SDL_BlendMode -> _int))

(define-sdl2 SDL_GetRenderDrawBlendMode (_fun _SDL_Renderer* _SDL_BlendMode* -> _int))

(define-sdl2 SDL_RenderClear (_fun _SDL_Renderer* -> _int))

(define-sdl2 SDL_RenderDrawPoint (_fun _SDL_Renderer* _int _int -> _int))

(define-sdl2 SDL_RenderDrawPoints (_fun _SDL_Renderer* _SDL_Point* _int -> _int))

(define-sdl2 SDL_RenderDrawLine (_fun _SDL_Renderer* _int _int _int _int -> _int))

(define-sdl2 SDL_RenderDrawLines (_fun _SDL_Renderer* _SDL_Point* _int -> _int))

(define-sdl2 SDL_RenderDrawRect (_fun _SDL_Renderer* _SDL_Rect*/null -> _int))

(define-sdl2 SDL_RenderDrawRects (_fun _SDL_Renderer* _SDL_Rect* _int -> _int))

(define-sdl2 SDL_RenderFillRect (_fun _SDL_Renderer* _SDL_Rect*/null -> _int))

(define-sdl2 SDL_RenderFillRects (_fun _SDL_Renderer* _SDL_Rect* _int -> _int))

(define-sdl2 SDL_RenderCopy
    (_fun _SDL_Renderer* _SDL_Texture* _SDL_Rect*/null _SDL_Rect*/null -> _int))

(define-sdl2 SDL_RenderCopyEx
    (_fun
     _SDL_Renderer*
     _SDL_Texture*
     _SDL_Rect*/null
     _SDL_Rect*/null
     _double
     _SDL_Point*/null
     _SDL_RendererFlip
     -> _int))

(define-sdl2 SDL_RenderReadPixels (_fun _SDL_Renderer* _SDL_Rect*/null _uint32 _pointer _int -> _int))

(define-sdl2 SDL_RenderPresent (_fun _SDL_Renderer* -> _void))

(define-sdl2 SDL_DestroyTexture (_fun _SDL_Texture* -> _void))

(define-sdl2 SDL_DestroyRenderer (_fun _SDL_Renderer* -> _void))

(define-sdl2 SDL_GL_BindTexture (_fun _SDL_Texture* _float*/null _float*/null -> _int))

(define-sdl2 SDL_GL_UnbindTexture (_fun _SDL_Texture* -> _int))

(define-sdl2 SDL_RenderGetMetalLayer (_fun _SDL_Renderer* -> _pointer)
    #:make-fail make-not-available)

(define-sdl2 SDL_RenderGetMetalCommandEncoder (_fun _SDL_Renderer* -> _pointer)
    #:make-fail make-not-available)


;; SDL_sensor.h

(define-cpointer-type _SDL_Sensor*)

(define _SDL_SensorID _sint32)

(define _SDL_SensorType
    (_enum
     '(SDL_SENSOR_INVALID = -1
       SDL_SENSOR_UNKNOWN
       SDL_SENSOR_ACCEL
       SDL_SENSOR_GYRO)))

(define SDL_STANDARD_GRAVITY    9.80665f0)

(define-sdl2 SDL_NumSensors (_fun -> _int)
    #:make-fail make-not-available)

(define-sdl2 SDL_SensorGetDeviceName (_fun _int -> _string)
    #:make-fail make-not-available)

(define-sdl2 SDL_SensorGetDeviceType (_fun _int -> _SDL_SensorType)
    #:make-fail make-not-available)

(define-sdl2 SDL_SensorGetDeviceInstanceID (_fun _int -> _SDL_SensorID)
    #:make-fail make-not-available)

(define-sdl2 SDL_SensorOpen (_fun _int -> _SDL_Sensor*/null)
    #:make-fail make-not-available)

(define-sdl2 SDL_SensorFromInstanceID (_fun _SDL_SensorID -> _SDL_Sensor*/null)
    #:make-fail make-not-available)

(define-sdl2 SDL_SensorGetName (_fun _SDL_Sensor* -> _string)
    #:make-fail make-not-available)

(define-sdl2 SDL_SensorGetType (_fun _SDL_Sensor* -> _SDL_SensorType)
    #:make-fail make-not-available)

(define-sdl2 SDL_SensorGetNonPortableType (_fun _SDL_Sensor* -> _int)
    #:make-fail make-not-available)

(define-sdl2 SDL_SensorGetInstanceID (_fun _SDL_Sensor* -> _SDL_SensorID)
    #:make-fail make-not-available)

(define-sdl2 SDL_SensorGetData (_fun _SDL_Sensor* _float* _int -> _int)
    #:make-fail make-not-available)

(define-sdl2 SDL_SensorClose (_fun _SDL_Sensor* -> _void)
    #:make-fail make-not-available)

(define-sdl2 SDL_SensorUpdate (_fun -> _void)
    #:make-fail make-not-available)


;; SDL_shape.h

(define SDL_NONSHAPEABLE_WINDOW -1)
(define SDL_INVALID_SHAPE_ARGUMENT -2)
(define SDL_WINDOW_LACKS_SHAPE -3)

(define-sdl2 SDL_CreateShapedWindow
    (_fun _string _uint _uint _uint _uint _SDL_WindowFlags -> _SDL_Window*/null))

(define-sdl2 SDL_IsShapedWindow (_fun _SDL_Window* -> _SDL_bool))

(define _WindowShapeMode
    (_enum
     '(ShapeModeDefault
       ShapeModeBinarizeAlpha
       ShapeModeReverseBinarizeAlpha
       ShapeModeColorKey)))

(define (SDL_SHAPEMODEALPHA mode)
    (or
     (eq? mode 'ShapeModeDefault)
     (eq? mode 'ShapeModeBinarizeAlpha)
     (eq? mode 'ShapeModeReverseBinarizeAlpha)))

(define _SDL_WindowShapeParams
    (_union
     _uint8
     _SDL_Color))

(define-cstruct _SDL_WindowShapeMode
    ([mode _WindowShapeMode]
     [parameters _SDL_WindowShapeParams]))
(define _SDL_WindowShapeMode* _SDL_WindowShapeMode-pointer)

(define-sdl2 SDL_SetWindowShape (_fun _SDL_Window* _SDL_Surface* _SDL_WindowShapeMode* -> _int))

(define-sdl2 SDL_GetShapedWindowMode (_fun _SDL_Window* _SDL_WindowShapeMode* -> _int))


;; SDL_system.h

(define _SDL_WindowsMessageHook (_fun _pointer _pointer _uint _uint64 _sint64 -> _void))
(define-sdl2 SDL_SetWindowsMessageHook (_fun _SDL_WindowsMessageHook _pointer -> _void)
    #:make-fail make-not-available)

(define-sdl2 SDL_Direct3D9GetAdapterIndex (_fun _int -> _int)
    #:make-fail make-not-available)

(define-cpointer-type _IDirect3DDevice9*)

(define-sdl2 SDL_RenderGetD3D9Device (_fun _SDL_Renderer* -> _IDirect3DDevice9*/null)
    #:make-fail make-not-available)

(define-sdl2 SDL_DXGIGetOutputInfo (_fun _int _int* _int* -> _SDL_bool)
    #:make-fail make-not-available)

(define-sdl2 SDL_LinuxSetThreadPriority (_fun _sint64 _int -> _int)
    #:make-fail make-not-available)

(define-sdl2 SDL_iPhoneSetAnimationCallback
    (_fun _SDL_Window* _int (_fun _pointer -> _void) _pointer -> _int)
    #:make-fail make-not-available)
(define SDL_iOSSetAnimationCallback SDL_iPhoneSetAnimationCallback)

(define-sdl2 SDL_iPhoneSetEventPump (_fun _SDL_bool -> _void)
    #:make-fail make-not-available)
(define SDL_iOSSetEventPump SDL_iPhoneSetEventPump)

(define-sdl2 SDL_AndroidGetJNIEnv (_fun -> _pointer)
    #:make-fail make-not-available)

(define-sdl2 SDL_AndroidGetActivity (_fun -> _pointer)
    #:make-fail make-not-available)

(define-sdl2 SDL_IsAndroidTV (_fun -> _SDL_bool)
    #:make-fail make-not-available)

(define-sdl2 SDL_IsChromebook (_fun -> _SDL_bool)
    #:make-fail make-not-available)

(define-sdl2 SDL_IsDeXMode (_fun -> _SDL_bool)
    #:make-fail make-not-available)

(define-sdl2 SDL_AndroidBackButton (_fun -> _void)
    #:make-fail make-not-available)

(define SDL_ANDROID_EXTERNAL_STORAGE_READ   #x01)
(define SDL_ANDROID_EXTERNAL_STORAGE_WRITE  #x02)

(define-sdl2 SDL_AndroidGetInternalStoragePath (_fun -> _string)
    #:make-fail make-not-available)

(define-sdl2 SDL_AndroidGetExternalStorageState (_fun -> _int)
    #:make-fail make-not-available)

(define-sdl2 SDL_AndroidGetExternalStoragePath (_fun -> _string)
    #:make-fail make-not-available)

(define _SDL_WinRT_Path
    (_enum
     '(SDL_WINRT_PATH_INSTALLED_LOCATION
       SDL_WINRT_PATH_LOCAL_FOLDER
       SDL_WINRT_PATH_ROAMING_FOLDER
       SDL_WINRT_PATH_TEMP_FOLDER)))

(define _SDL_WinRT_DeviceFamily
    (_enum
     '(SDL_WINRT_DEVICEFAMILY_UNKNOWN
       SDL_WINRT_DEVICEFAMILY_DESKTOP
       SDL_WINRT_DEVICEFAMILY_MOBILE
       SDL_WINRT_DEVICEFAMILY_XBOX)))

(define-sdl2 SDL_WinRTGetFSPathUNICODE (_fun _SDL_WinRT_Path -> _string/ucs-4)
    #:make-fail make-not-available)

(define-sdl2 SDL_WinRTGetFSPathUTF8 (_fun _SDL_WinRT_Path -> _string)
    #:make-fail make-not-available)

(define-sdl2 SDL_WinRTGetDeviceFamily (_fun -> _SDL_WinRT_DeviceFamily)
    #:make-fail make-not-available)

(define-sdl2 SDL_IsTablet (_fun -> _SDL_bool)
    #:make-fail make-not-available)


;; SDL_thread.h

(define-cpointer-type _SDL_Thread*)

(define _SDL_threadID _ulong)

(define _SDL_TLSID _uint)

(define _SDL_ThreadPriority
    (_enum
     '(SDL_THREAD_PRIORITY_LOW
       SDL_THREAD_PRIORITY_NORMAL
       SDL_THREAD_PRIORITY_HIGH
       SDL_THREAD_PRIORITY_TIME_CRITICAL)))

(define _SDL_ThreadFunction (_fun _pointer -> _int))

(define-sdl2 SDL_CreateThread (_fun _SDL_ThreadFunction _string _pointer -> _SDL_Thread*/null))

(define-sdl2 SDL_CreateThreadWithStackSize
    (_fun _SDL_ThreadFunction _string _size _pointer -> _SDL_Thread*/null)
    #:make-fail make-not-available)

(define-sdl2 SDL_GetThreadName (_fun _SDL_Thread*/null -> _string))

(define-sdl2 SDL_ThreadID (_fun -> _SDL_threadID))

(define-sdl2 SDL_GetThreadID (_fun _SDL_Thread*/null -> _SDL_threadID))

(define-sdl2 SDL_SetThreadPriority (_fun _SDL_ThreadPriority -> _int))

(define-sdl2 SDL_WaitThread (_fun _SDL_Thread* _int*/null -> _void))

(define-sdl2 SDL_DetachThread (_fun _SDL_Thread* -> _void)
    #:make-fail make-not-available)

(define-sdl2 SDL_TLSCreate (_fun -> _SDL_TLSID))

(define-sdl2 SDL_TLSGet (_fun _SDL_TLSID -> _pointer))

(define-sdl2 SDL_TLSSet (_fun _SDL_TLSID _pointer (_fun _pointer -> _void) -> _int))


;; SDL_timer.h

(define-sdl2 SDL_GetTicks (_fun -> _uint32))

(define (SDL_TICKS_PASSED A B)
    (<= (- B A) 0))

(define-sdl2 SDL_GetPerformanceCounter (_fun -> _uint64))

(define-sdl2 SDL_GetPerformanceFrequency (_fun -> _uint64))

(define-sdl2 SDL_Delay (_fun _uint32 -> _void))

(define _SDL_TimerCallback (_fun #:async-apply (lambda (thunk) (thunk)) _uint32 _pointer -> _uint32))

(define _SDL_TimerID _int)

(define-sdl2 SDL_AddTimer (_fun _uint32 _SDL_TimerCallback _pointer -> _SDL_TimerID))

(define-sdl2 SDL_RemoveTimer (_fun _SDL_TimerID -> _SDL_bool))


;; SDL_version.h

(define-cstruct _SDL_version
    ([major _uint8]
     [minor _uint8]
     [patch _uint8]))

(define _SDL_version* _SDL_version-pointer)

(define (SDL_VERSIONNUM X Y Z)
    (+ (* X 1000) (* Y 100) Z))

(define-sdl2 SDL_GetVersion (_fun _SDL_version* -> _void))

(define-sdl2 SDL_GetRevision (_fun -> _string))

(define-sdl2 SDL_GetRevisionNumber (_fun -> _int))


;; SDL.h

(define-sdl2 SDL_GetPlatform (_fun -> _string))

(define _SDL_INIT
    (_bitmask
     '(SDL_INIT_TIMER          = #x00000001
       SDL_INIT_AUDIO          = #x00000010
       SDL_INIT_VIDEO          = #x00000020
       SDL_INIT_JOYSTICK       = #x00000200
       SDL_INIT_HAPTIC         = #x00001000
       SDL_INIT_GAMECONTROLLER = #x00002000
       SDL_INIT_EVENTS         = #x00004000
       SDL_INIT_SENSOR         = #x00008000
       SDL_INIT_NOPARACHUTE    = #x00100000
       SDL_INIT_EVERYTHING     = #x0000f231)
     _uint32))

(define-sdl2 SDL_Init (_fun _SDL_INIT -> _int))

(define-sdl2 SDL_InitSubSystem (_fun _SDL_INIT -> _int))

(define-sdl2 SDL_QuitSubSystem (_fun _SDL_INIT -> _void))

(define-sdl2 SDL_WasInit (_fun _SDL_INIT -> _uint32))

(define-sdl2 SDL_Quit (_fun -> _void))
