#lang racket/base
(require racket/match
         db
         "day.rkt"
         "post.rkt")

(provide open-repo
         close-repo)

(struct repo ([connection #:mutable]))

(define (open-repo path)
  (define c (sqlite3-connect
             #:database path
             #:mode 'create))
  (query-exec c (string-append "CREATE TABLE IF NOT EXISTS post (day TEXT PRIMARY KEY, text TEXT)"))
  (repo c))

(define (close-repo repo)
  (define con (repo-connection repo))
  (when con
    (disconnect repo)
    (set-repo-connection! #f)))  

(define (con repo)
  (define c (repo-connection repo))
  (unless c
    (error 'con "database connection is closed"))
  c)

(define (create-post repo pst)
  (match pst
    [(post day text)
     (query-exec (con repo)
                 "INSERT INTO post (day, text) VALUES ($1, $2)" (day->string day) text)]))

(define (update-post repo pst)
  (match pst
    [(post day content)
     (query-exec (con repo) "UPDATE post SET text = $1 WHERE day = $2" content (day->string day))]))

(define (row->post row)
  (match row
    [(vector day text) (post (string->day day) text)]))

(define (get-post repo day)
  (define rows (query-rows (con repo) "SELECT day, text FROM post WHERE day = $1" (day->string day)))
  (match rows
    ['() #f]
    [(list row) (row->post row)]
    [_ (error 'get-post "weird result: ~a" rows)]))

(module+ test
  (require rackunit)
  (define r (open-repo 'memory))
  (define d1 (day 2026 01 01))
  (define d2 (day 2026 01 02))
  (create-post r (post d1 "beep"))
  (create-post r (post d2 "boop"))
  (check-equal? (get-post r d1) (post d1 "beep"))
  (check-equal? (get-post r d2) (post d2 "boop"))
  (check-exn exn:fail:sql? (λ () (create-post r (post d1 "bap"))))
  (update-post r (post d1 "bap"))
  (check-equal? (get-post r d1) (post d1 "bap")))
