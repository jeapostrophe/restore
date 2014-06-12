#lang racket/base
(require racket/match
         racket/tcp
         racket/contract/base
         unstable/error
         unstable/contract)

(define ENV-VAR #"RESTORE")

(define (restore-rpc! msg)
  (define port-no-str
    (environment-variables-ref (current-environment-variables) ENV-VAR))
  (cond
    [port-no-str
     (with-handlers ([exn:not-break?
                      (λ (x)
                        #f)])
       (define port-no
         (string->number (bytes->string/utf-8 port-no-str)))
       (define-values (from-server to-server) (tcp-connect "127.0.0.1" port-no))
       (write msg to-server)
       (flush-output to-server)
       (close-output-port to-server)
       (define response (read from-server))
       (close-input-port from-server)
       response)]
    [else
     #f]))

(define (restore-notify! var val)
  (restore-rpc! `(notify ,var ,val)))

(define (restore-init!)
  (or (restore-rpc! '(restore))
      (hasheq)))

(define (restore-server-spawn!)
  (define vars (make-hasheq))
  (define l (tcp-listen 0 5 #t))
  (define-values
    (loc-str loc-port rem-str rem-port)
    (tcp-addresses l #t))
  (define t
    (thread
     (λ ()
       (let loop ()
         (sync
          (handle-evt
           (tcp-accept-evt l)
           (match-lambda
            [(list from-client to-client)
             (define msg (read from-client))
             (close-input-port from-client)
             (match msg
               [`(restore)
                (write vars to-client)]
               [`(notify ,var ,val)
                (hash-set! vars var val)
                (write #t to-client)]
               [_
                (eprintf "invalid message: ~e\n" msg)
                (write #f to-client)])
             (flush-output to-client)
             (close-output-port to-client)
             (loop)])))))))
  (values loc-port t))

(define (with-restore-server f)
  (define-values (port t) (restore-server-spawn!))
  (define env (current-environment-variables))
  (define new-env (environment-variables-copy env))
  (environment-variables-set!
   new-env ENV-VAR
   (string->bytes/utf-8 (number->string port)))
  (dynamic-wind void
      (λ ()
        (parameterize ([current-environment-variables
                        new-env])
          (f)))
      (λ () (kill-thread t))))

(provide
 (contract-out
  [restore-notify!
   (-> symbol? any/c boolean?)]
  [restore-init!
   (-> (hash/c symbol? any/c
               #:flat? #t
               #:immutable #t))]
  [restore-server-spawn!
   (-> (values port-number? thread?))]
  [with-restore-server
   (-> (-> any) any)]))
