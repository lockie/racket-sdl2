#lang racket

(require
 (prefix-in sdl2: sdl2/pretty))

(sdl2:set-main-ready!)
(sdl2:init! '(video))
(define window (sdl2:create-window! "Hello, World!" 0 0 600 400 '()))
(define surface (sdl2:get-window-surface window))
(sdl2:fill-rect! surface
                 #f
                 (sdl2:map-rgb
                  (sdl2:surface-format surface)
                  0 128 255))
(sdl2:update-window-surface! window)
(sdl2:delay! 3000)
(sdl2:quit!)
