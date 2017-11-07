#lang racket/base

(require (for-syntax racket/base)
         stamps
         racket/draw
         racket/class
         racket/math
         stamps/private/color-utils)

(provide (all-from-out racket/math)
         (except-out (all-from-out stamps)
                     render-shape)
         (except-out (all-from-out racket/base)
                     #%module-begin)
         (rename-out [module-begin #%module-begin])
         background
         width
         height
         filename
         start-shape
         quality
         translate-x
         translate-y
         scale-x
         scale-y
         t
         r
         s
         h
         sat
         b
         a
         sx
         sy
         x
         y)

; Parameters

(define background  (make-parameter '(0 0 1)))
(define width       (make-parameter 1024))
(define height      (make-parameter 768))
(define filename    (make-parameter "output.png"))
(define filetype    (make-parameter 'png))
(define start-shape (make-parameter #f))
(define quality     (make-parameter 100))

; Utilities, aliases and syntax
(define-syntax-rule (translate-x x)
  (translate x 0))

(define-syntax-rule (translate-y y)
  (translate 0 y))

(define-syntax-rule (scale-x x)
  (scale x 0))

(define-syntax-rule (scale-y y)
  (scale 0 y))

(define-syntax t   (make-rename-transformer #'translate))
(define-syntax r   (make-rename-transformer #'rotate))
(define-syntax s   (make-rename-transformer #'scale))
(define-syntax h   (make-rename-transformer #'hue))
(define-syntax sat (make-rename-transformer #'saturation))
(define-syntax b   (make-rename-transformer #'brightness))
(define-syntax a   (make-rename-transformer #'alpha))
(define-syntax x   (make-rename-transformer #'translate-x))
(define-syntax y   (make-rename-transformer #'translate-y))
(define-syntax sx  (make-rename-transformer #'scale-x))
(define-syntax sy  (make-rename-transformer #'scale-y))

; Module wrapper

(define-syntax-rule (module-begin expr ...)
  (#%module-begin

   expr ...

   (when (not (start-shape))
     (fprintf (current-error-port)
              "start shape not specified, please set the \"start-shape\" parameter\n")
     (exit 1))

   ; create bitmap and rendering context
   (define bmp (make-bitmap (width) (height)))
   (define dc (send bmp make-dc))

   ; set background
   (define-values (r g b) (apply hsb->rgb (background)))
   (define bg-color (make-object color% r g b 1.0))
   (send dc set-background bg-color)
   (send dc clear)

   ; render
   (printf "rendering...")
   (flush-output)
   (define-values (res cpu real gc)
     (time-apply render-shape
                 (list  ((start-shape)) dc)))
   (printf " ~a shapes, ~a ms\n" (car res) real)

   ; save
   (when (filename)
    (printf "saving to ~ax~a image ~a (~a)..." (width) (height) (filename) (filetype))
    (flush-output)
    (send bmp save-file (filename) (filetype) (quality))
    (printf "done\n."))

   bmp))
