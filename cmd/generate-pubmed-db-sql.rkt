#lang racket

(require odysseus)
(require "../lib/globals.rkt")
(require "../lib/common.rkt")

(define count (make-parameter 0))
(define WFR 20) ; write to file frequency

(define pubmed-rktd-path "/home/denis/data/pubmed/rktd")
(define rktd-files (filter
                      file-exists?
                      (map (curry build-path pubmed-rktd-path) (directory-list pubmed-rktd-path))))

(define (sql-result rktd-file sql-file-path)
  (let*
    ((entries (hash-values (read-serialized-data-from-file rktd-file)))
    (entries (filter (λ (entry) ($ t entry)) entries))
    ; (journal-names (remove-duplicates
    ;                 (filter-map
    ;                   (λ (entry)
    ;                     (and
    ;                       ($ jou entry)
    ;                       (scalarize ($ jou entry))))
    ;                     entries)))
    ; (journals (for/hash ((journal-name journal-names) (i (in-naturals))) (values journal-name i)))
    ; (journals-sql (for/fold
    ;       ((res "copy journals (id, name) from stdin;\n"))
    ;       (((name id) journals))
    ;       (let* (
    ;             (current-res (format "~a~a\t~a\n" res id name)))
    ;         (cond
    ;           ((equal? 0 (remainder (count) WFR))
    ;               ; (--- (count))
    ;               (count (inc (count)))
    ;               (append-file sql-file-path current-res)
    ;               "")
    ;           (else
    ;             (count (inc (count)))
    ;             current-res)))))
    ; (_ (append-file sql-file-path journals-sql))
    ; (_ (append-file sql-file-path "\\.\n"))
    (_ (count 0))
    (articles-sql (for/fold
                    ; 'pmid pmid 'dt date 'jou journal 't title 'abs abstract 'au authors 'mhs mhs 'kw keywords 'doi doi 'pmc pmc
                    ((res "\n\ncopy articles (pmid, date_string, title, abstract, keywords, mesh_terms, doi, pmc) from stdin;\n"))
                    ((e entries))
                    (let* (
                          (current-res
                            (format "~a~a\n"
                                    res
                                    (string-join
                                      (map
                                        (λ (x) (scalarize x #:delimeter ", "))
                                        (list
                                          ($ pmid e)
                                          (or ($ dt e) "null")
                                          ; (hash-ref journals ($ jou e) "null")
                                          ($ t e)
                                          (or (cf ($ abs e)) "null")
                                          (or (cf ($ kw e)) "null")
                                          (or (cf ($ mhs e)) "null")
                                          (or ($ doi e) "null")
                                          (or ($ pmc e) "null")))
                                      "\t"))))
                    (cond
                      ((equal? 0 (remainder (count) WFR))
                        ; (--- (count))
                        (count (inc (count)))
                        (append-file sql-file-path current-res)
                        "")
                      (else
                        (count (inc (count)))
                        current-res)))))
    (_ (append-file sql-file-path articles-sql))
    (_ (append-file sql-file-path  "\\.\n"))
    )
        #t))

(define (generate-pubmed-db-sql)
  (for ((rktd-file rktd-files))
    (let* ((file-id (first
                      (string-split
                        (last
                          (string-split (path->string rktd-file) "/"))
                        ".")))
          (sql-file-path (format "~a/~a.sql" sql-dumps-path file-id)))
      (write-file sql-file-path "\\connect pubmed;\n\n")
      (sql-result rktd-file sql-file-path))))

(benchmark
  (generate-pubmed-db-sql))
