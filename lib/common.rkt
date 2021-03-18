#lang racket

(provide (all-defined-out))

(define (month/abbr->num month-abbr)
  (let* ((months (hash  "Jan" "01" "Feb" "02" "Mar" "03"
                        "Apr" "04" "May" "05" "Jun" "06"
                        "Jul" "07" "Aug" "08" "Sep" "09"
                        "Oct" "10" "Nov" "11" "Dec" "12"))
        (month-d (hash-ref months month-abbr month-abbr)))
    month-d))
