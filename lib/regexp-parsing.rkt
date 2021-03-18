#lang racket

(require odysseus)

(provide (all-defined-out))

(define current-tag (make-parameter #f))

(define pubmed-article-rs-all
#<<T
<PubmedArticle>
  <MedlineCitation .*?>
    <PMID .*?>(\\d+?)</PMID>
    <DateCompleted>
      <Year>(\\d+?)</Year>
      <Month>(\\d+?)</Month>
      <Day>(\\d+?)</Day>
    </DateCompleted>
    <DateRevised>
      <Year>(\\d+?)</Year>
      <Month>(\\d+?)</Month>
      <Day>(\\d+?)</Day>
    </DateRevised>
    <Article PubModel="(.*?)">
      <Journal>
        <ISSN IssnType="(.*?)">(.*?)</ISSN>
        <JournalIssue CitedMedium="(.*?)">
          <Volume>(.*?)</Volume>
          <Issue>(.*?)</Issue>
          <PubDate>
            <Year>(.*?)</Year>
            <Month>(.*?)</Month>
            <Day>(.*?)</Day>
          </PubDate>
        </JournalIssue>
        <Title>(.*?)</Title>
        <ISOAbbreviation>(.*?)</ISOAbbreviation>
      </Journal>
      <ArticleTitle>(.*?)</ArticleTitle>
      <Pagination>
        <MedlinePgn>(.*?)</MedlinePgn>
      </Pagination>
      <ELocationID EIdType="(.*?)" .*?>(.*?)</ELocationID>
      <Abstract>
        <AbstractText>(.*?)</AbstractText>
        <CopyrightInformation>(.*?)</CopyrightInformation>
      </Abstract>
      <AuthorList CompleteYN=".*?">
        (<Author ValidYN=".*?">
          <LastName>.*?</LastName>
          <ForeName>.*?</ForeName>
          <Initials>.*?</Initials>
          (?:<AffiliationInfo>
            <Affiliation>.*?</Affiliation>
          </AffiliationInfo>)+
        </Author>)+
      </AuthorList>
      <Language>(.*?)</Language>
      <PublicationTypeList>
        (<PublicationType UI="(.*?)">(.*?)</PublicationType>)+
      </PublicationTypeList>
    </Article>
    <MedlineJournalInfo>
      <Country>(.*?)</Country>
      <MedlineTA>(.*?)</MedlineTA>
      <NlmUniqueID>(.*?)</NlmUniqueID>
      <ISSNLinking>(.*?)</ISSNLinking>
    </MedlineJournalInfo>
    <ChemicalList>
      (<Chemical>
        <RegistryNumber>(.*?)</RegistryNumber>
        <NameOfSubstance UI="(.*?)">(.*?)</NameOfSubstance>
      </Chemical>)*
    </ChemicalList>
    <CitationSubset>(.*?)</CitationSubset>
    <MeshHeadingList>
      (<MeshHeading>
        <DescriptorName UI="(.*?)" MajorTopicYN="(.*?)">(.*?)</DescriptorName>
      </MeshHeading>)*
    </MeshHeadingList>
    <KeywordList Owner="(.*?)">
      (<Keyword MajorTopicYN="(.*?)">(.*?)</Keyword>)*
    </KeywordList>
  </MedlineCitation>
  <PubmedData>
    <History>.*?</History>
    <PublicationStatus>(.*?)</PublicationStatus>
    <ArticleIdList>
      <ArticleId IdType="pubmed">(.*?)</ArticleId>
      <ArticleId IdType="pii">(.*?)</ArticleId>
      <ArticleId IdType="doi">(.*?)</ArticleId>
    </ArticleIdList>
  </PubmedData>
</PubmedArticle>
T
)

(define any ".*?")
(define pmid "<PMID.*?>(.*?)</PMID>")
(define journal-title "<Title>(.*?)</Title>")
(define article-title "<ArticleTitle>(.*?)</ArticleTitle>")
(define abstract-text "<AbstractText>(.*?)</AbstractText>")
(define authors "<AuthorList.*?></AuthorList>")
(define mesh-headings "<MeshHeadingList.*?></MeshHeadingList>")
(define keywords "<KeywordList.*?></KeywordList>")
(define doi "<ArticleId IdType=\"doi\">(.*?)</ArticleId>")

(define pub-date "<PubDate>(.*?)</PubDate>")

; base variant
(define pubmed-article-rs-1 (str "<PubmedArticle>"
                                      any
                                      pmid
                                      any
                                      pub-date
                                      any
                                      journal-title
                                      any
                                      article-title
                                      any
                                      "</PubmedArticle>"))

; + abstract
(define pubmed-article-rs-2 (str "<PubmedArticle>"
                                      any
                                      pmid
                                      any
                                      pub-date
                                      any
                                      journal-title
                                      any
                                      article-title
                                      any
                                      abstract-text
                                      any
                                      doi
                                      any
                                      "</PubmedArticle>"))


(define (parse-medline-entry entry)
  (define (line-with-tag? lst)
    (and
      (> (length lst) 1)
      (re-matches? "[A-Z]{2,4}" (first lst))))
  (let* ((lines (string-split entry "\n"))
        (lines (map string-trim lines))
        (pairs (map
                  (λ (line) (string-split line (pregexp "\\s*-\\s+")))
                  lines))
        (res (for/fold
                ((res (hash)))
                ((pair pairs))
                (cond
                  ((and (line-with-tag? pair)
                        (equal? (current-tag) (first pair)))
                      (hash-union
                        res
                        (hash (current-tag) (string-append (hash-ref res (current-tag)) ", " (second pair)))
                        #:overwrite #t))
                  ((line-with-tag? pair)
                      (current-tag (first pair))
                      (hash-insert res (cons (first pair) (second pair))))
                  ((hash-ref res (current-tag) #f)
                      (hash-union
                        res
                        (hash (current-tag) (string-append (hash-ref res (current-tag)) " " (first pair)))
                        #:overwrite #t))
                  (else res))))
        )
  res))
