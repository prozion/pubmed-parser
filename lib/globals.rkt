#lang racket

(provide (all-defined-out))

(define pubmed-path "/home/denis/data/pubmed")
(define cache-path "/var/tmp/projects/pubmed/cache")
(define generated-wikis-path "/var/tmp/projects/transhumanist.online/wiki/age-related-genes")
(define sql-dumps-path (string-append pubmed-path "/sql"))
(define xml-ids (list 1060 1061 1062))
(define rktd-ids xml-ids)
