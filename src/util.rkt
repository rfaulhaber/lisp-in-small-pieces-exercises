; -*- mode: racket; -*-

(module util racket
  (provide atom?)
  (define (atom? x)
    (and (not (null? x))
         (not (pair? x)))))

