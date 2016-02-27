#lang racket/base

(require stamps
         racket/draw
         racket/class
         stamps/private/color-utils)

(provide (except-out (all-from-out stamps)
                     render-shape)
         (except-out (all-from-out racket/base)
                     #%module-begin)
         (rename-out [module-begin #%module-begin])
         background
         width
         height
         filename
         start-shape)

; Parameters

(define background  (make-parameter '(0 0 0)))
(define width       (make-parameter 1024))
(define height      (make-parameter 768))
(define filename    (make-parameter "output.png"))
(define filetype    (make-parameter 'png))
(define start-shape (make-parameter #f))
(define quality     (make-parameter 100))

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
   (define-values (res cpu real gc)
     (time-apply render-shape
                 (list  ((start-shape)) dc)))
   (printf " ~a shapes, ~a ms\n" (car res) real)

   ; save
   (printf "saving to ~ax~a image ~a (~a)\n" (width) (height) (filename) (filetype))
   (send bmp save-file (filename) (filetype) (quality))))
