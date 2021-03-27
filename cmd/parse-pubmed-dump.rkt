#lang racket

(require odysseus)
(require tabtree)
(require "../lib/globals.rkt")
(require "../lib/common.rkt")
(require "../lib/sexp-parsing.rkt")

; (define xml-filename "pubmed_test_2")
; (define xml-filename "pubmed20n1017")

; ; testing
; (let* ((xml-content (read-file (format "~a/test/pubmed_test_1.xml" pubmed-path)))
;       (rktd-filepath (format "~a/test/pubmed_test_1.rktd" pubmed-path)))
;   (write-data-to-file
;     (parse-PubmedArticleSet-via-sexps xml-content)
;     rktd-filepath))

(define-catch (parse-pubmed-dump xml-id)
  (benchmark (d (format "read and parse pubmed xml ~a" xml-id))
      (let* ((xml-filepath (format "~a/raw/pubmed21n~a.xml" pubmed-path (chunk-n xml-id)))
            (xml-content (read-file xml-filepath))
            (rktd-filepath (format "~a/rktd/~a.rktd" pubmed-path xml-id)))
        (write-data-to-file
          (parse-PubmedArticleSet-via-sexps xml-content)
          rktd-filepath))))

(for ((i (range 962 1063)))
  (parse-pubmed-dump i))
