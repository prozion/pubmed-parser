#lang racket

(require odysseus)
(require odysseus/write/wiki)
(require tabtree)
(require tabtree/utils)
(require "../lib/globals.rkt")
(require "../lib/common.rkt")

; entry: 'pmid pmid 'dt date 'jou journal 't title 'abs abstract 'au authors 'mhs mhs 'kw keywords 'doi doi 'pmc pmc

(define medline-entry
  #<<T
{{medline-entry
|title=~a
|pubmed-url=~a
~a
~a
~a
~a
}}
T
)

(define medline-title
  #<<T
* {{medline-title
|title=~a
|date=~a
|pubmed-url=~a
~a
}}
T
)

(define-catch (output-entry entry #:abstract? (abstract? #t) #:template template)
  (let* ((title (scalarize ($ t entry)))
        (date ($ dt entry))
        (abstract (scalarize ($ abs entry)))
        (mesh-terms ($ mhs entry))
        (mesh-terms (cond
                      ((list? mesh-terms) (make-bullet-list mesh-terms))
                      ((string? mesh-terms) (format "\n* ~a" mesh-terms))
                      (else #f)))
        (keywords ($ kw entry))
        (keywords (cond
                      ((list? keywords) (make-bullet-list keywords))
                      ((string? keywords) keywords)
                      (else #f)))
        (pubmed-url (format "https://pubmed.ncbi.nlm.nih.gov/~a" ($ pmid entry)))
        (doi-url (and ($ doi entry) (format "https://sci-hub.do/~a" ($ doi entry))))
        (pmc-url (and ($ pmc entry) (format "https://www.ncbi.nlm.nih.gov/pmc/articles/~a" ($ pmc entry)))))
    (case template
      ((medline-entry)
          (format medline-entry
            title
            pubmed-url
            (if (and abstract? abstract)
              (format "|abstract=~a" abstract)
              "")
            (if mesh-terms
              (format "|mesh-terms=~a" mesh-terms)
              "")
            (if keywords
              (format "|keywords=~a" keywords)
              "")
            (if (or pmc-url doi-url)
              (format "|full-text-url=~a" (or pmc-url doi-url))
              "")
            ))
      ((medline-title)
        (format medline-title
          title
          date
          pubmed-url
          (if (or pmc-url doi-url)
            (format "|full-text-url=~a" (or pmc-url doi-url))
            "")
          ))
      (else (format "no template provided for ~a entry" ($ pmid entry))))))

(define-catch (group-entries rktd-id)
  (let* (
        (filtered-entries (read-serialized-data-from-file (format "~a/rktd_filtered/~a-filter-aging-genes.rktd" pubmed-path (chunk-n rktd-id)))))
    (for/hash
      ((gene-triggered (remove-duplicates
                          (map
                            (λ (x) ($ gene-triggered x))
                            filtered-entries))))
      (values gene-triggered
              (filter (λ (x) (equal? ($ gene-triggered x) gene-triggered)) filtered-entries)))))

(define-catch (get-aging-genes-grouped start end)
  (let ((res (for/fold
                ((res (hash)))
                ((i (range start (+ 1 end))))
                (hash-union
                  #:overwrite 'collect-in-list
                  (group-entries i)
                  res))))
    (write-file
      (format "~a/aging-genes-grouped.rktd" cache-path)
      res)
    res))

(define-catch (make-page
                  #:aging-genes-grouped (aging-genes-grouped #f)
                  #:sort-f (sort-f a-z)
                  #:min-group-n (min-group-n 1)
                  #:target-wiki (target-wiki "aging-genes-grouped-a-z")
                  #:abstract? (abstract? #t)
                  #:template (template medline-entry)
                  )
  (let ((aging-genes-grouped (and
                                aging-genes-grouped
                                (hash-filter
                                  (λ (k v) (>= (length v) min-group-n))
                                  aging-genes-grouped  ))))
    (write-file
      (format "~a/~a.wiki" generated-wikis-path target-wiki)
      (let* ((aging-genes-grouped (or
                                    aging-genes-grouped
                                    (read-serialized-data-from-file (format "~a/aging-genes-grouped.rktd" cache-path)))))
        (for/fold
          ((res1 "__NOTOC__\n"))
          ; ((res1 ""))
          ((gene-triggered (sort (hash-keys aging-genes-grouped) (λ (a b) (sort-f a b aging-genes-grouped)))))
          (format "~a\n==~a==\n~a"
            res1
            gene-triggered
            (for/fold
              ((res2 ""))
              ((entry (hash-ref aging-genes-grouped gene-triggered)))
              (format "~a\n~a"
                res2
                (output-entry entry #:abstract? abstract? #:template template)))))))
    #t))

; (benchmark (d "caching aging-genes-grouped.rktd")
;   (define aging-genes-grouped (get-aging-genes-grouped 1022 1028)))

; (benchmark (d "building title wiki page")
;   (make-page
;     #:abstract? #f
;     #:sort-f (λ (a b h) (a-z a b))
;     #:target-wiki "aging-genes-a-z-title"
;     #:template 'medline-title
;     #:aging-genes-grouped
;     (benchmark (d "getting aging-genes-a-z-title")
;       (get-aging-genes-grouped 1000 1062))))
;
; (benchmark (d "building freqs wiki page")
;   (make-page
;     #:target-wiki "aging-genes-freq-entry"
;     #:sort-f (λ (g1 g2 h) (> (length (hash-ref h g1)) (length (hash-ref h g2))))
;     #:min-group-n 3
;     #:template 'medline-entry
;     #:aging-genes-grouped
;     (benchmark (d "getting aging-genes-freq-entry")
;         (get-aging-genes-grouped 1000 1062))))

(benchmark (d "building freqs-titles wiki page")
  (make-page
    #:target-wiki "aging-genes-freq-title"
    #:sort-f (λ (g1 g2 h) (> (length (hash-ref h g1)) (length (hash-ref h g2))))
    #:min-group-n 1
    #:template 'medline-title
    #:aging-genes-grouped
    (benchmark (d "getting aging-genes-freq-title")
        (get-aging-genes-grouped 1000 1062))))
