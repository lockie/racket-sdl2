#lang racket/base

(require
 (for-syntax racket/base
             racket/syntax
             racket/string)
 ffi/unsafe
 ffi/unsafe/define
 (submod racket/performance-hint begin-encourage-inline)
 (rename-in racket/contract
            [-> contract/->])
 sdl2/pretty
 sdl2/private/lib-path)

(provide
 (except-out
  (all-defined-out)
  define-sdl2-net
  convention:uglify))


(define-syntax (convention:uglify id)
    (define str (string-replace (symbol->string (syntax-e id)) "!" ""))
    (when (string-suffix? str "-version")
        (set! str (string-replace str "-version" "-_-version")))
    (when (string-prefix? str "tcp-")
        (set! str (string-replace str "tcp-" "tcp-_-")))
    (when (string-prefix? str "udp-")
        (set! str (string-replace str "udp-" "udp-_-")))
    (define uppercase-exceptions
        '("tcp" "ip" "udp"))
    (format-id id
               (apply string-append
                      "SDLNet_"
                      (map
                       (lambda (s)
                           (if (member s uppercase-exceptions)
                               (string-upcase s)
                               (string-titlecase s)))
                       (string-split str "-")))))

(define sdl2-net-lib (ffi-lib (lib-path "net") '("0" #f)))
(define-ffi-definer define-sdl2-net sdl2-net-lib #:make-c-id convention:uglify)

(define-sdl2-net linked-version (_fun -> _version*))

(define-sdl2-net init! (_fun -> _void))
(define-sdl2-net quit! (_fun -> _void))

(define-cstruct _ip-address
    ([host _uint32]
     [port _uint16]))
(define _ip-address* _ip-address-pointer)
(define _ip-address*/null _ip-address-pointer/null)

(define inaddr-any #x00000000)
(define inaddr-none #xffffffff)
(define inaddr-loopback #x7f000001)
(define inaddr-broadcast #xffffffff)
(define-sdl2-net resolve-host (_fun _ip-address* _string _uint16 -> _int))

(define-sdl2-net resolve-ip (_fun _ip-address* -> _string))

(define-sdl2-net get-local-addresses (_fun _ip-address* _int -> _int))

(define-cpointer-type _tcp-socket)

(define-sdl2-net tcp-open! (_fun _ip-address* -> _tcp-socket/null))

(define-sdl2-net tcp-accept! (_fun _tcp-socket -> _tcp-socket/null))

(define-sdl2-net tcp-get-peer-address (_fun _tcp-socket -> _ip-address*/null))

(define-sdl2-net tcp-send! (_fun _tcp-socket _pointer _int -> _int))

(define-sdl2-net tcp-recv! (_fun _tcp-socket _pointer _int -> _int))

(define-sdl2-net tcp-close! (_fun _tcp-socket -> _void))

(define max-udp-channels  32)
(define max-udp-addresses 4)

(define-cpointer-type _udp-socket)

(define-cstruct _udp-packet
    ([channel _int]
     [data _uint8*]
     [len _int]
     [max-len _int]
     [status _int]
     [address _ip-address]))
(define _udp-packet* _udp-packet-pointer)
(define _udp-packet*/null _udp-packet-pointer/null)

(define-sdl2-net alloc-packet (_fun _int -> _udp-packet*/null))
(define-sdl2-net resize-packet! (_fun _udp-packet* _int -> _int))
(define-sdl2-net free-packet! (_fun _udp-packet* -> _void))

(define-sdl2-net alloc-packet-v (_fun _int _int -> (_ptr o _udp-packet*/null)))
(define-sdl2-net free-packet-v! (_fun (_ptr i _udp-packet*) -> _void))

(define-sdl2-net udp-open! (_fun _uint16 -> _udp-socket/null))

(define-sdl2-net udp-set-packet-loss! (_fun _udp-socket _int -> _void))

(define-sdl2-net udp-bind! (_fun _udp-socket _int _ip-address* -> _int))

(define-sdl2-net udp-unbind! (_fun _udp-socket _int -> _void))

(define-sdl2-net udp-get-peer-address (_fun _udp-socket _int -> _ip-address*/null))

(define-sdl2-net udp-send-v! (_fun _udp-socket (_ptr i _udp-packet*) _int -> _int))

(define-sdl2-net udp-send! (_fun _udp-socket _int _udp-packet* -> _int))

(define-sdl2-net udp-recv-v! (_fun _udp-socket (_ptr o _udp-packet*) -> _int))

(define-sdl2-net udp-recv! (_fun _udp-socket _udp-packet* -> _int))

(define-sdl2-net udp-close! (_fun _udp-socket -> _void))

(define-cpointer-type _socket-set)

(define-cstruct _generic-socket
    ([ready _int]))

(define _generic-socket* _generic-socket-pointer)

(define-sdl2-net alloc-socket-set (_fun _int -> _socket-set/null))

(define-sdl2-net add-socket! (_fun _socket-set _generic-socket* -> _int))

(begin-encourage-inline
    (define/contract (tcp-add-socket! set sock)
        (contract/-> socket-set? tcp-socket? integer?)
        (add-socket! set (cast sock _tcp-socket _generic-socket*)))
    (define/contract (udp-add-socket! set sock)
        (contract/-> socket-set? udp-socket? integer?)
        (add-socket! set (cast sock _udp-socket _generic-socket*))))

(define-sdl2-net del-socket! (_fun _socket-set _generic-socket* -> _int))

(begin-encourage-inline
    (define/contract (tcp-del-socket! set sock)
        (contract/-> socket-set? tcp-socket? integer?)
        (del-socket! set (cast sock _tcp-socket _generic-socket*)))
    (define/contract (udp-del-socket! set sock)
        (contract/-> socket-set? udp-socket? integer?)
        (del-socket! set (cast sock _udp-socket _generic-socket*))))

(define-sdl2-net check-sockets (_fun _socket-set _uint32 -> _int))

(begin-encourage-inline
    (define (socket-ready? sock)
        (and
         sock
         (not (zero?
               (generic-socket-ready (ptr-ref sock _generic-socket*)))))))

(define-sdl2-net free-socket-set! (_fun _socket-set -> _void))

(define set-error!
    (let ([interfaces (make-hash)])
        (lambda (fmt . args)
            (define itypes
                (cons _string
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
                                      "SDLNet_SetError" sdl2-net-lib
                                      (_cprocedure itypes _void))])
                                (hash-set! interfaces itypes i)
                                i)))])
                (apply fun fmt args)))))

(define-sdl2-net get-error (_fun -> _string))

(begin-encourage-inline
    (define (write16 value areap)
        (ptr-set! (ptr-ref areap _uint16*)
                  (swap-be16 value)))

    (define (write32 value areap)
        (ptr-set! (ptr-ref areap _uint32*)
                  (swap-be32 value)))

    (define (read16 areap)
        (swap-be16 (ptr-ref areap _uint16*)))

    (define (read32 areap)
        (swap-be32 (ptr-ref areap _uint32*))))
