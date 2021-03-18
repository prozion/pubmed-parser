#lang racket

(require odysseus)
(require odysseus/read/pubmed)

(define cache-path "/home/denis/data/pubmed")

(define keywords (list "aging" "longevity"))
(define fields (list "Abstract" "MeSH Terms"))
(define query (for*/fold
                ((res ""))
                ((keyword keywords) (field fields))
                (cond
                  ((empty-string? res)
                      (format "~a[~a]" keyword field))
                  (else
                      (format "~a OR ~a[~a]" res keyword field)))))
(define filepath-root (format "~a/~a" cache-path (string-join keywords "_")))

; (pubmed/get-article-ids
;   #:write-cache (format "~a/~a.rktd" cache-path "aging_longevity")
;   #:retmax 50000
;   query)

(define pmids (read-serialized-data-from-file (str filepath-root ".pmids.rktd")))

; 17:42
(write-data-to-file
   (map
      parse-medline-entry
      ; (map get-article-metadata (pubmed/get_article_ids query)))
      (map get-article-metadata pmids))
      (str filepath-root ".medlines.rktd"))
