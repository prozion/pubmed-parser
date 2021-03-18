#lang racket

(require odysseus)
(require odysseus/read/pubmed)
(require "../lib/sexp-parsing.rkt")

; (define xml-filename "0")
; (define xml-filename "pubmed_test")
(define xml-filename "pubmed20n1017")
(define pubmed-path "/home/denis/data/pubmed/db")
(define tmp-path "/var/tmp/projects/pubmed")
(define xml-filepath (format "~a/raw/~a.xml" pubmed-path xml-filename))
(define rktd-filepath (format "/home/denis/data/pubmed/db/~a.rktd" xml-filename))

(define xml (read-file xml-filepath))

; (---- (parse-PubmedArticleSet-via-sexps xml))
; (begin (parse-PubmedArticleSet-via-sexps xml) #t)

(write-data-to-file
  (parse-PubmedArticleSet-via-sexps xml)
  rktd-filepath)
