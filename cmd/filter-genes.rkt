#lang racket

(require odysseus)
(require tabtree)
(require "../lib/globals.rkt")
(require "../lib/common.rkt")

(define-catch (get-gene-names chromosomes)
  (benchmark (d "read list of genes")
    (let* ((cache-filepath (format "~a/gene-names.rktd" cache-path))
          (cache? (file-exists? cache-filepath))
          (res (and cache? (read-serialized-data-from-file cache-filepath)))
          (res (or res
                  (apply append
                    (map
                      (λ (chr)
                        (filter-map
                          (λ (item) (and
                                      ($ id item)
                                      (not (equal*? "-" ($ id item)))
                                      ($ id item)))
                          (get-leaves (parse-tab-tree (format "/home/denis/projects/worlds/biomed_world/human_organism/genes/~a.tree" chr)))))
                      chromosomes)))))
      (when (not cache?) (write-data-to-file res cache-filepath))
      res)))

(define count (make-parameter 0))

; timing:
; (avg 211304 180922 165883 189826 180786 168637 168909 181354 210610 184716 155952 203777) -> 183556 ms ~ 3 min
(define-catch (filter-by-gene-names gene-names rktd-id)
  (define (gene-matches? gene-name text)
    ; (let* ((safe-pattern (pregexp (format "[^A-Za-z0-]~a[^A-Za-z0-9+]" gene-name))))
    (let* ((safe-pattern (pregexp (format "[ \\-]~a[ \\-]" gene-name))))
    ; (let* ((safe-pattern (pregexp (format "~a" gene-name))))
      (regexp-match safe-pattern text)))
  (benchmark (d (format "load ~a-filter-aging.rktd and filter by gene names" rktd-id))
    (let* ((aging-related-entries (read-serialized-data-from-file (format "~a/rktd_filtered/~a-filter-aging.rktd" pubmed-path (chunk-n rktd-id))))
          (gene-related-entries
            (filter-map
              (λ (entry)
                (let* ((abstract ($ abs entry))
                      ; (_ (when ($ mhs entry) (--- ($ mhs entry))))
                      (abstract (scalarize abstract))
                      (title (scalarize ($ t entry)))
                      (gene-triggered (and
                                        abstract
                                        (ormap
                                          (λ (gene-name)
                                            (and
                                              (or
                                                (gene-matches? gene-name abstract)
                                                ; (begin (count (inc (count))) (--- (count)) #f)
                                                (gene-matches? gene-name title))
                                              gene-name))
                                          gene-names))))
                  (and gene-triggered
                      (hash-union (hash
                                    'gene-triggered gene-triggered
                                    't (string-replace title gene-triggered (format "[[~a]]" gene-triggered))
                                    'abs (string-replace abstract gene-triggered (format "[[~a]]" gene-triggered)))
                                  entry))))
              aging-related-entries)))
      (write-data-to-file gene-related-entries (format "~a/rktd_filtered/~a-filter-aging-genes.rktd" pubmed-path (chunk-n rktd-id)))
      #t)))

(define gene-names (get-gene-names
  (list
    "chr01"
    "chr02" "chr03" "chr04" "chr05" "chr06" "chr07" "chr08" "chr09" "chr10" "chr11" "chr12" "chr13" "chr14" "chr15" "chr16" "chr17" "chr18" "chr19" "chr20" "chr21" "chr22" "chrx"
    "chry"
    )))

(for ((i (range 900 1000)))
  (filter-by-gene-names gene-names i))
