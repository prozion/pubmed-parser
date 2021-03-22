#lang racket

(require odysseus)
(require odysseus/write/wiki)
(require tabtree)
(require tabtree/utils)
(require "../lib/common.rkt")

(define rktd-ids (list 1060 1061 1062))
(define pubmed-path "/home/denis/data/pubmed")
(define generated-wikis-path "/var/tmp/projects/transhumanist.online/wiki/age-related-genes")
(define aging-related-entries (read-serialized-data-from-file (format "~a/filtered/~a-~a-aging-gene-related.rktd" pubmed-path (first rktd-ids) (last rktd-ids))))

(define medline-entry
  #<<T
~a
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

(define (output-entries entries)
  (let* (
        (res (for/fold
                            ((res "__NOTOC__\n"))
                            ((entry entries))
                            (let* ((title (scalarize ($ t entry)))
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
                            (format medline-entry
                              res
                              title
                              pubmed-url
                              (if abstract
                                (format "|abstract=~a" abstract)
                                "")
                              (if mesh-terms
                                (format "|mesh-terms=~a" mesh-terms)
                                "")
                              (if abstract
                                (format "|keywords=~a" keywords)
                                "")
                              (if (or pmc-url doi-url)
                                (format "|full-text-url=~a" (or pmc-url doi-url))
                                "")
                              ))))
        )
    (write-file (format "~a/from-publications-~a-~a.wiki" generated-wikis-path (first rktd-ids) (last rktd-ids)) res)
    #t
))

(output-entries aging-related-entries)
