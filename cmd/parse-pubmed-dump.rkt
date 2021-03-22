#lang racket

(require odysseus)
(require tabtree)
(require "../lib/sexp-parsing.rkt")

; (define xml-filename "pubmed_test_2")
; (define xml-filename "pubmed20n1017")
(define MAXID 1061)
; (define xml-ids (range 1060 (+ MAXID 1)))
(define xml-ids '(1062))
(define pubmed-path "/home/denis/data/pubmed")

; ; testing
; (let* ((xml-content (read-file (format "~a/test/pubmed_test_1.xml" pubmed-path)))
;       (rktd-filepath (format "~a/test/pubmed_test_1.rktd" pubmed-path)))
;   (write-data-to-file
;     (parse-PubmedArticleSet-via-sexps xml-content)
;     rktd-filepath))

(benchmark (d (format "read and parse ~a pubmed xmls" (length xml-ids)))
  (for ((xml-id xml-ids))
    (let* ((xml-filepath (format "~a/raw/pubmed21n~a.xml" pubmed-path xml-id))
          (xml-content (read-file xml-filepath))
          (rktd-filepath (format "~a/db/~a.rktd" pubmed-path xml-id)))
      (write-data-to-file
        (parse-PubmedArticleSet-via-sexps xml-content)
        rktd-filepath))))
