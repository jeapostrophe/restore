#lang racket/base
(require racket/system
         restore)

(module+ test
  (with-restore-server
   (λ ()
     (let loop ()
       (system "racket -t client.rkt")
       (sleep 1)
       (loop)))))
