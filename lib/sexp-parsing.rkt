#lang racket

(require odysseus)
(require "common.rkt")
(require net/url)
(require json)
(require sxml)

(provide (all-defined-out))

; Acquire via XML files parsing with regexps:
(define-catch (parse-medline-date medline-date-content)
  (let* (
        (medline-date-content ((change-text
                                  (list (cons " - " "-") (cons "/" "-")))
                                medline-date-content)))
    (match medline-date-content
      ((pregexp (pregexp "^(.+?) (.+?)-(.+?)$") (list _ year month1 month2))
          (format "~a-~a.~a" (month/abbr->num month1) (month/abbr->num month2) year))
      ((pregexp (pregexp "^(Spring|Summer|Autumn|Winter) (.+?)$") (list _ season year))
          (format "~a.~a" (string-downcase season) year))
      ((pregexp (pregexp "^(\\d{4}) (First Quarter|Second Quarter|Third Quarter|Fourth Quarter)$") (list _ year quarter))
          (format "~a.~a" (string-downcase (idfy quarter)) year))
      ((pregexp (pregexp "^(.+?) (Spring|Summer|Autumn|Winter)$") (list _ year season))
          (format "~a.~a" (string-downcase season) year))
      ((pregexp (pregexp "^(.+?) (.+?)$") (list _ year month1))
          (format "~a.~a" (month/abbr->num month1) year))
      ((pregexp (pregexp "^(.+?)$") (list _ year))
          (format "~a" year))
      (_ #f))))

(define-catch (parse-author author)
  (let* ((lastname ($xml Author.LastName author))
        (forename ($xml Author.ForeName author)))
    (str forename (when forename " ") lastname)))

(define-catch (dehtmlify xml)
  ((change-text (list
                  (cons "<i>" "[i]") (cons "</i>" "[/i]")))
    xml))

(define-catch (get-index index-name hxml)
  (let* ((article-ids ($ PubmedArticle.PubmedData.ArticleIdList hxml))
        (article-ids (if (hash? article-ids) (list article-ids) article-ids))
        (article-ids (for/hash
                        ((article-id article-ids))
                        (values
                          ($xml ArticleId.@.IdType article-id)
                          ($xml ArticleId article-id)))))
    (hash-ref* article-ids index-name)))

; Acquire via XML files parsing with sexp hashes:
(define-catch (parse-PubmedArticleSet-via-sexps xml)
  (define (parse-PubmedArticle hxml)
    (let* ((pmid ($xml PubmedArticle.MedlineCitation.PMID hxml))
          (pubdate ($ PubmedArticle.MedlineCitation.Article.Journal.JournalIssue.PubDate hxml))
          (year ($xml Year pubdate))
          (season ($xml Season pubdate))
          (month ($xml Month pubdate))
          (month (and month (month/abbr->num month)))
          (day ($xml Day pubdate))
          (medline-date ($xml MedlineDate pubdate))
          (date (cond
                  ((and day month year) (format "~a.~a.~a" day month year))
                  ((and month year) (format "~a.~a" month year))
                  ((and season year) (format "~a.~a" season year))
                  (year year)
                  (medline-date (parse-medline-date medline-date))
                  (else #f)))
          (journal ($xml PubmedArticle.MedlineCitation.Article.Journal.Title hxml))
          (title (or
                    ($xml PubmedArticle.MedlineCitation.Article.ArticleTitle hxml)
                    ($xml PubmedArticle.MedlineCitation.Article.VernacularTitle hxml)))
          (abstract ($xml PubmedArticle.MedlineCitation.Article.Abstract.AbstractText hxml))
          (authors ($ PubmedArticle.MedlineCitation.Article.AuthorList hxml))
          (authors (filter-not empty-string? (map parse-author authors)))
          (mhs ($xml PubmedArticle.MedlineCitation.MeshHeadingList.MeshHeading.DescriptorName hxml))
          (keywords ($xml PubmedArticle.MedlineCitation.KeywordList.Keyword hxml))
          (doi (get-index 'doi hxml))
          (pmc (get-index 'pmc hxml))
          )
      (hash 'pmid pmid 'dt date 'jou journal 't title 'abs abstract 'au authors 'mhs mhs 'kw keywords 'doi doi 'pmc pmc)))
  (let* (
        (xml (dehtmlify xml))
        (sxml
          (benchmark (d "xml->sxml")
            (ssax:xml->sxml (open-input-string xml) empty))
            )
        (res
          (benchmark (d "sxml->hxml")
            (sxml->hxml sxml))
            )
        (res ($ *TOP*.PubmedArticleSet res))
        (res (if (hash? res) (list res) res))
        (_ (---  "len:" (length res)))
        (res
          (benchmark (d "hxml->short entries")
            (for/hash
              ((entry (map parse-PubmedArticle res)))
              (values ($ pmid entry) entry))
            ))
        )
    res))
