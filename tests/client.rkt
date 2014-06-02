#lang racket/base
(require restore)

(module+ main
  (define old (restore-init!))
  (define value
    (or (hash-ref old 'value #f)
        (random)))
  (printf "Value is ~a\n" value)
  (restore-notify! 'value value))
