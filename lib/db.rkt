#lang racket

(require odysseus)
(require db)

(provide (all-defined-out))

(define (connect-database)
  (let* ((login "postgres")
        (pwd "postgres")
        (pgc (postgresql-connect
                #:user "postgres"
                #:database "pubmed"
                #:password "postgres")))
    pgc))

; (--- (vector-ref
;         (car (query-rows pgc "select name, id from journals"))
;         0))
