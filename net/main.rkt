#lang racket/base

(require
 ffi/unsafe
 ffi/unsafe/define
 (submod racket/performance-hint begin-encourage-inline)
 (rename-in racket/contract
            [-> contract/->])
 sdl2)

(provide
 (all-defined-out))


(define sdl2-net-lib (ffi-lib (sdl2-lib-path "net") '("0" #f)))
(define-ffi-definer define-sdl2-net sdl2-net-lib)

(define-sdl2-net SDLNet_Linked_Version (_fun -> _SDL_version*))

(define-sdl2-net SDLNet_Init (_fun -> _void))
(define-sdl2-net SDLNet_Quit (_fun -> _void))

(define-cstruct _IPaddress
    ([host _uint32]
     [port _uint16]))
(define _IPaddress* _IPaddress-pointer)
(define _IPaddress*/null _IPaddress-pointer/null)

(define INADDR_ANY #x00000000)
(define INADDR_NONE #xFFFFFFFF)
(define INADDR_LOOPBACK #x7f000001)
(define INADDR_BROADCAST #xFFFFFFFF)
(define-sdl2-net SDLNet_ResolveHost (_fun _IPaddress* _string _uint16 -> _int))

(define-sdl2-net SDLNet_ResolveIP (_fun _IPaddress* -> _string))

(define-sdl2-net SDLNet_GetLocalAddresses (_fun _IPaddress* _int -> _int))

(define-cpointer-type _TCPsocket)

(define-sdl2-net SDLNet_TCP_Open (_fun _IPaddress* -> _TCPsocket/null))

(define-sdl2-net SDLNet_TCP_Accept (_fun _TCPsocket -> _TCPsocket/null))

(define-sdl2-net SDLNet_TCP_GetPeerAddress (_fun _TCPsocket -> _IPaddress*/null))

(define-sdl2-net SDLNet_TCP_Send (_fun _TCPsocket _pointer _int -> _int))

(define-sdl2-net SDLNet_TCP_Recv (_fun _TCPsocket _pointer _int -> _int))

(define-sdl2-net SDLNet_TCP_Close (_fun _TCPsocket -> _void))

(define SDLNET_MAX_UDPCHANNELS  32)
(define SDLNET_MAX_UDPADDRESSES 4)

(define-cpointer-type _UDPsocket)

(define-cstruct _UDPpacket
    ([channel _int]
     [data _uint8*]
     [len _int]
     [maxlent _int]
     [status _int]
     [address _IPaddress]))
(define _UDPpacket* _UDPpacket-pointer)
(define _UDPpacket*/null _UDPpacket-pointer/null)

(define-sdl2-net SDLNet_AllocPacket (_fun _int -> _UDPpacket*/null))
(define-sdl2-net SDLNet_ResizePacket (_fun _UDPpacket* _int -> _int))
(define-sdl2-net SDLNet_FreePacket (_fun _UDPpacket* -> _void))

(define-sdl2-net SDLNet_AllocPacketV (_fun _int _int -> (_ptr o _UDPpacket*/null)))
(define-sdl2-net SDLNet_FreePacketV (_fun (_ptr i _UDPpacket*) -> _void))

(define-sdl2-net SDLNet_UDP_Open (_fun _uint16 -> _UDPsocket/null))

(define-sdl2-net SDLNet_UDP_SetPacketLoss (_fun _UDPsocket _int -> _void))

(define-sdl2-net SDLNet_UDP_Bind (_fun _UDPsocket _int _IPaddress* -> _int))

(define-sdl2-net SDLNet_UDP_Unbind (_fun _UDPsocket _int -> _void))

(define-sdl2-net SDLNet_UDP_GetPeerAddress (_fun _UDPsocket _int -> _IPaddress*/null))

(define-sdl2-net SDLNet_UDP_SendV (_fun _UDPsocket (_ptr i _UDPpacket*) _int -> _int))

(define-sdl2-net SDLNet_UDP_Send (_fun _UDPsocket _int _UDPpacket* -> _int))

(define-sdl2-net SDLNet_UDP_RecvV (_fun _UDPsocket (_ptr o _UDPpacket*) -> _int))

(define-sdl2-net SDLNet_UDP_Recv (_fun _UDPsocket _UDPpacket* -> _int))

(define-sdl2-net SDLNet_UDP_Close (_fun _UDPsocket -> _void))

(define-cpointer-type _SDLNet_SocketSet)

(define-cstruct _SDLNet_GenericSocket
    ([ready _int]))

(define _SDLNet_GenericSocket* _SDLNet_GenericSocket-pointer)

(define-sdl2-net SDLNet_AllocSocketSet (_fun _int -> _SDLNet_SocketSet/null))

(define-sdl2-net SDLNet_AddSocket (_fun _SDLNet_SocketSet _SDLNet_GenericSocket* -> _int))

(begin-encourage-inline
    (define/contract (SDLNet_TCP_AddSocket set sock)
        (contract/-> SDLNet_SocketSet? TCPsocket? integer?)
        (SDLNet_AddSocket set (cast sock _TCPsocket _SDLNet_GenericSocket*)))
    (define/contract (SDLNet_UDP_AddSocket set sock)
        (contract/-> SDLNet_SocketSet? UDPsocket? integer?)
        (SDLNet_AddSocket set (cast sock _UDPsocket _SDLNet_GenericSocket*))))

(define-sdl2-net SDLNet_DelSocket (_fun _SDLNet_SocketSet _SDLNet_GenericSocket* -> _int))

(begin-encourage-inline
    (define/contract (SDLNet_TCP_DelSocket set sock)
        (contract/-> SDLNet_SocketSet? TCPsocket? integer?)
        (SDLNet_DelSocket set (cast sock _TCPsocket _SDLNet_GenericSocket*)))
    (define/contract (SDLNet_UDP_DelSocket set sock)
        (contract/-> SDLNet_SocketSet? UDPsocket? integer?)
        (SDLNet_DelSocket set (cast sock _UDPsocket _SDLNet_GenericSocket*))))

(define-sdl2-net SDLNet_CheckSockets (_fun _SDLNet_SocketSet _uint32 -> _int))

(begin-encourage-inline
    (define (SDLNet_SocketReady sock)
        (and
         sock
         (not (zero?
               (SDLNet_GenericSocket-ready (ptr-ref sock _SDLNet_GenericSocket*)))))))

(define-sdl2-net SDLNet_FreeSocketSet (_fun _SDLNet_SocketSet -> _void))

(define SDLNet_SetError
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

(define-sdl2-net SDLNet_GetError (_fun -> _string))

(begin-encourage-inline
    (define (SDLNet_Write16 value areap)
        (ptr-set! (ptr-ref areap _uint16*)
                  (SDL_SwapBE16 value)))

    (define (SDLNet_Write32 value areap)
        (ptr-set! (ptr-ref areap _uint32*)
                  (SDL_SwapBE32 value)))

    (define (SDLNet_Read16 areap)
        (SDL_SwapBE16 (ptr-ref areap _uint16*)))

    (define (SDLNet_Read32 areap)
        (SDL_SwapBE32 (ptr-ref areap _uint32*))))
