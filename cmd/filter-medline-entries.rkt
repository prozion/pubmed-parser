#lang racket

(require odysseus)
(require tabtree)
(require "../lib/common.rkt")
(require "../lib/sexp-parsing.rkt")

(define MAXID 1062)
(define pubmed-path "/home/denis/data/pubmed")

(define mesh-tabtree (parse-tab-tree "/home/denis/projects/worlds/biomed_world/taxonomies/mesh/mesh.tree"))
(define mesh-items (filter
                     (λ (item) (re-matches? "^[A-Z]" ($ id item)))
                     (planarize mesh-tabtree)))

(define aging-mesh-terms
            (remove-duplicates
              (map
                (λ (item) (normalize ($ id item)))
                mesh-items)))
(define aging-keywords
            (remove-duplicates
              (map
                normalize
                (flatten
                  (map
                    (λ (item)
                        (map
                          (curryr string-split  ",")
                          (cleanmap
                            (list ($ related item) ($ broader item) ($ narrower item)))))
                    mesh-items)))))
(define aging-keywords_mesh-terms (remove-duplicates (append aging-mesh-terms aging-keywords)))

; (Hash PMID Entry) -> (List Entry)
(define-catch (filter-by-mesh rktd-ids)
  (benchmark (d "load and filter by mesh terms")
    (let* ((all-entries
                (benchmark (d (format "loading ~a rktds" (length rktd-ids)))
                  (apply
                    hash-union
                    (map
                      (λ (rktd-id)
                        (read-serialized-data-from-file (format "~a/db/~a.rktd" pubmed-path rktd-id)))
                      rktd-ids))))
          (filtered-entries
                  (filter-map
                    (λ (entry-id)
                      (let* ((entry (hash-ref all-entries entry-id))
                            (mhs (map normalize (listify ($ mhs entry))))
                            (kws (map
                                    normalize
                                    (flatten
                                      (map
                                        (curryr string-split #px"(\\s|\\s*\\*\\s*)")
                                        (listify ($ kw entry)))))))
                        (and
                          (or
                            (intersect? mhs aging-mesh-terms)
                            (intersect? kws aging-keywords_mesh-terms))
                          entry)))
                    (hash-keys all-entries))))
      (write-data-to-file filtered-entries (format "~a/filtered/~a-~a-aging-related.rktd" pubmed-path (first rktd-ids) (last rktd-ids)))
      #t)))

(define-catch (get-gene-names chromosomes)
  (benchmark (d "read list of genes")
    (apply append
      (map
        (λ (chr)
          (filter-map
            (λ (item) (and
                        ($ id item)
                        (not (equal*? "-" ($ id item)))
                        ($ id item)))
            (get-leaves (parse-tab-tree (format "/home/denis/projects/worlds/biomed_world/human_organism/genes/~a.tree" chr)))))
        chromosomes))))

(define count (make-parameter 0))

(define-catch (filter-by-gene-names gene-names rktd-ids)
  (define (gene-matches? gene-name text)
    ; (let* ((safe-pattern (pregexp (format "[^A-Za-z0-]~a[^A-Za-z0-9+]" gene-name))))
    (let* ((safe-pattern (pregexp (format "[ \\-]~a[ \\-]" gene-name))))
    ; (let* ((safe-pattern (pregexp (format "~a" gene-name))))
      (regexp-match safe-pattern text)))
  (benchmark (d "load filtered rktd and filter by gene names")
    (let* ((aging-related-entries (read-serialized-data-from-file (format "~a/filtered/~a-~a-aging-related.rktd" pubmed-path (first rktd-ids) (last rktd-ids))))
          (gene-related-entries
            (filter-map
              (λ (entry)
                (let* ((abstract ($ abs entry))
                      (_ (when ($ mhs entry) (--- ($ mhs entry))))
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
                                    't (string-replace title gene-triggered (format "[[~a]]" gene-triggered))
                                    'abs (string-replace abstract gene-triggered (format "[[~a]]" gene-triggered)))
                                  entry))))
              aging-related-entries)))
      (write-data-to-file gene-related-entries (format "~a/filtered/~a-~a-aging-gene-related.rktd" pubmed-path (first rktd-ids) (last rktd-ids)))
      #t)))

(define rktd-ids (list 1060 1061 1062))

; (filter-by-mesh rktd-ids)

(filter-by-gene-names
  (get-gene-names
    (list
          "chr01"
          "chr02" "chr03" "chr04" "chr05" "chr06" "chr07" "chr08" "chr09" "chr10" "chr11" "chr12" "chr13" "chr14" "chr15" "chr16" "chr17" "chr18" "chr19" "chr20" "chr21" "chr22" "chrx"
          "chry"
          ))
  rktd-ids)
