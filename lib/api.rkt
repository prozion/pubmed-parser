#lang racket

(require net/url)
(require json)

(provide (all-defined-out))

;; Acquire via API requests
(define (pubmed/get-article-ids
          #:tool (tool "odysseus")
          #:email (email "my_email@example.com")
          #:db (db "pubmed")
          #:retmax (retmax 1000)
          #:write-cache (write-cache #f)
          term)
  (let* ((res (get-url
                  (format "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi?db=~a&term=~a&tool=~a&email=~a&retmax=~a&format=json"
                          db
                          term
                          tool
                          email
                          retmax
                          )))
        (res (json->hash res))
        (res-ids ($ esearchresult.idlist res)))
    (when write-cache
      (let* ((cache (and (file-exists? write-cache) (read-serialized-data-from-file write-cache)))
            (cache (if cache
                      (remove-duplicates (append cache res-ids))
                      res-ids)))
        (write-data-to-file cache write-cache)))
    res-ids))

(define (get-article-metadata pmc-id #:result-format (result-format "medline"))
  (let* ((res (get-url
                  (format "https://api.ncbi.nlm.nih.gov/lit/ctxp/v1/pubmed/?format=~a&id=~a"
                          result-format
                          pmc-id)))
        )
    (flush-output)
    (display "+")
    res))
