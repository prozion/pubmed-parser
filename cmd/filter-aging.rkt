#lang racket

(require odysseus)
(require odysseus/read/mediawiki)
(require tabtree)
(require "../lib/common.rkt")
(require "../lib/globals.rkt")
(require "../lib/sexp-parsing.rkt")

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

(define (get-keywords-from-wiki)
  (define (get-keyword astr)
    (match astr
      ((pregexp "^.*?\\*\\s*?(.*?)$" (list _ keyword)) keyword)
      (_ #f)))
  (let* ((wiki-text (get-page-wiki-content "Ключевые_слова_для_поиска_публикаций_о_старении"))
        (wiki-texts (string-split wiki-text (pregexp "<!--.*?parse.*?-->")))
        (wiki-1 (first wiki-texts))
        (wiki-2 (second wiki-texts))
        (keywords-1 (map string-trim (filter-map get-keyword (string-split wiki-1 "\n"))))
        (keywords-2 (map string-trim (filter-map get-keyword (string-split wiki-2 "\n"))))
        )
    (hash
      'all (remove-duplicates (append keywords-1 keywords-2))
      'extra keywords-2)))


(define (output-keywords keywords)
  (write-file
    (format "~a/keywords.wiki" generated-wikis-path)
    (for/fold
      ((res ""))
      ((keyword (sort keywords a-z)))
      (format "~a\n* ~a" res keyword))))

; timing: ~ 23 s
; (Hash PMID Entry) -> (List Entry)
(define-catch (filter-by-keywords rktd-id)
  (benchmark (d (format "load ~a.rktd and filter by aging keywords" rktd-id))
    (let* ((entries (read-serialized-data-from-file (format "~a/rktd/~a.rktd" pubmed-path (chunk-n rktd-id))))
          (wiki-keywords (get-keywords-from-wiki))
          (filtered-entries
                  (filter-map
                    (λ (entry-id)
                      (let* ((entry (hash-ref entries entry-id))
                            (abstract (normalize ($ abs entry)))
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
                            (intersect? kws ($ all wiki-keywords))
                            (any-in-text? abstract ($ extra wiki-keywords))
                            )
                          entry)))
                    (hash-keys entries))))
      (write-data-to-file filtered-entries (format "~a/rktd_filtered/~a-filter-aging.rktd" pubmed-path (chunk-n rktd-id)))
      #t)))

(for ((i (range 900 1000)))
  (filter-by-keywords i))

; (output-keywords aging-keywords_mesh-terms)
