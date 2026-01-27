#lang racket/base
(require (only-in racket/match match)
         (only-in racket/format ~a))
(provide (struct-out day)
         (struct-out post)
         (struct-out topic)
         (struct-out topics)
         (struct-out before)
         (struct-out after)
         (struct-out in-thread)
         (struct-out with-tag)
         (struct-out tagged)
         maybe-day
         normalized-day
         seconds->day
         today
         day->string
         maybe-day->string
         string->day
         string->maybe-day
         add-days
         2pad
         4pad
         valid-text?
         valid-text-split)

(struct post (day text symbol link)
  #:transparent
  #:guard
  (λ (day text symbol link name)
    (unless
        (and (day? day)
             (valid-text? text)
             (or (not symbol) (valid-topic-symbol? symbol))
             (or (not link) (string? link)))
      (error 'day "not a valid post: (~a ~s ~s ~s ~s)" name day text symbol link))
    (values day text symbol link)))

(struct day (y m d)
  #:transparent
  #:guard
  (λ (y m d name)
    (unless (valid-day? y m d)
      (error 'day "not a valid day: (~a ~s ~s ~s)" name y m d))
    (values y m d)))

(struct topic (symbol name type)
  #:transparent
  #:guard
  (λ (symbol name type struct-name)
    (unless (and (valid-topic-symbol? symbol) (string? name) (topic-type? type))
      (error 'topic "not a valid topic: (~a ~s ~s ~s)" struct-name symbol name type))
    (values symbol name type)))

(struct topics (hash all threads tags)
  #:transparent
  #:guard
  (λ (hash all threads tags name)
    (unless (and (hash? hash) (list? all) (list? threads) (list? tags))
      (error 'topic "not a valid topics: (~a ~s ~s ~s ~s)" name hash all threads tags))
    (values hash all threads tags)))

(struct tagged (day id)
  #:transparent
  #:guard
  (λ (day id name)
    (unless (and (day? day) (valid-topic-symbol? id))
      (error 'topic "not a valid tagged: (~a ~s ~s)" name day id))
    (values day id)))

(struct before (day) #:transparent)
(struct after (day) #:transparent)
(struct in-thread (id) #:transparent)
(struct with-tag (id) #:transparent)


(define (2pad n)
  (~a n #:width 2 #:align 'right #:pad-string "0"))
(define (4pad n)
  (~a n #:width 4 #:align 'right #:pad-string "0"))

(define (leap-year? y)
  (and (zero? (remainder y 4))
       (or (not (zero? (remainder y 100)))
           (zero? (remainder y 400)))))

(define days-per-month
  (vector 31 31 28 31 30 31 30 31 31 30 31 30 31))

(define (days-in-month y m)
  (if (and (= m 2) (leap-year? y))
      29
      (vector-ref days-per-month m)))

(define (normalized-day y m d)
  (cond [(> m 12) (normalized-day (+ y 1) (- m 12) d)]
        [(< m 1) (normalized-day (- y 1) (+ m 12) d)]
        [else
         (define days (days-in-month y m))
         (cond
           [(> d days) (normalized-day y (+ m 1) (- d days))]
           [(< d 1) (normalized-day y (- m 1) (+ d (days-in-month y (- m 1))))]
           [else (day y m d)])]))

(define (add-days dy i)
  (match dy
    [(day y m d)
     (normalized-day y m (+ d i))]))

(define (next-day dy)
  (add-days dy 1))

(define (previous-day dy)
  (add-days dy -1))


(define (seconds->day s)
  (define d (seconds->date s #f))
  (day (date-year d) (date-month d) (date-day d)))
(define (today)
  (seconds->day (current-seconds)))

(define (valid-day? y m d)
  (and (exact-positive-integer? y)
       (and (exact-positive-integer? m) (> m 0) (<= m 12))
       (and (exact-positive-integer? d) (> d 0) (<= d (days-in-month y m)))))

(define (maybe-day y m d)
  (and (valid-day? y m d) (day y m d)))

(define (day->string dy)
  (match dy
    [(day y m d) (format "~a-~a-~a" (4pad y) (2pad m) (2pad d))]))

(define (maybe-day->string d)
  (and (day? d) (day->string d)))

(define (string->maybe-day str)
  (match (regexp-match #px"^(\\d+)-(\\d+)-(\\d+)$" str)
    [(list _ ys ms ds)
     (define y (string->number ys))
     (define m (string->number ms))
     (define d (string->number ds))
     (and (valid-day? y m d)
          (day y m d))]
    [_ #f]))

(define (string->day str)
  (or (string->maybe-day str)
      (error 'string->day "bad argument: ~a" str)))


(define (valid-text? str)
  (string? str) (<= (string-utf-8-length str) 1024))

(define (utf-8-split str max)
  (define (halp len i)
    (cond [(<= len max) (list (substring str 0 i) (substring str i (string-length str)))]
          [else (define ni (- i 1))
                (halp (- len (string-utf-8-length (substring str ni i))) ni)]))
  (halp (string-utf-8-length str) (string-length str)))

(define (valid-text-split str)
  (utf-8-split str 1024))

(define (valid-topic-symbol? id)
  (regexp-match #px"^[a-z][a-z0-9-]*[a-z0-9]$" (symbol->string id)))

(define (topic-type? symbol)
  (match symbol
    ['neither #t]
    ['thread #t]
    ['tag #t]
    ['either #t]))

(module+ test
  (require rackunit)

  (define d (today))
  (define s (day->string d))
  (define sd (string->day s))
  (check-equal? sd d)
  (define d2 (next-day d))
  (check-not-equal? d2 d)
  (define (check-days a b i)
    (check-equal? (add-days a i) b)
    (check-equal? (add-days b (- i)) a))
  (check-equal? (string->day "2025-06-25") (day 2025 6 25))
  (check-days (day 2025 6 25) (day 2025 6 26) 1)
  (check-days (day 2025 6 30) (day 2025 7 1) 1)
  (check-days (day 2025 12 31) (day 2026 1 1) 1)
  (check-days (day 2025 12 31) (day 2026 1 1) 1)
  (check-days (day 2016 2 28) (day 2016 2 29) 1)
  (check-days (day 2016 2 29) (day 2016 3 1) 1)
  (check-days (day 2017 2 28) (day 2017 3 1) 1)
  (check-days (day 2017 2 28) (day 2017 3 1) 1)
  (check-days (day 2017 2 20) (day 2017 3 1) 9)
  (check-days (day 2017 2 20) (day 2017 3 20) 28)
  (check-days (day 2017 2 20) (day 2018 2 20) 365)
  (check-days (day 2020 2 20) (day 2021 2 20) 366)
  
  (check-equal? (utf-8-split "1234" 4) (list "1234" ""))
  (check-equal? (utf-8-split "1234" 6) (list "1234" ""))
  (check-equal? (utf-8-split "1234" 3) (list "123" "4"))
  (check-equal? (utf-8-split "1234" 2) (list "12" "34"))
  (check-equal? (utf-8-split "1234" 1) (list "1" "234"))
  (check-equal? (utf-8-split "1234" 0) (list "" "1234"))
  (check-equal? (utf-8-split "å234" 4) (list "å23" "4"))
  (check-equal? (utf-8-split "123å" 4) (list "123" "å"))
  (check-equal? (utf-8-split "123å" 2) (list "12" "3å"))
  (check-equal? (utf-8-split "12å4" 4) (list "12å" "4"))
  (check-equal? (utf-8-split "12å4" 3) (list "12" "å4"))
  (check-equal? (utf-8-split "12å4" 2) (list "12" "å4"))
  (check-equal? (utf-8-split "12å4" 1) (list "1" "2å4"))
  (check-equal? (utf-8-split "12å4" 0) (list "" "12å4")))
