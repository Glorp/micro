#lang racket/base
(require (only-in racket/match match)
         (only-in racket/format ~a)
         "day.rkt"
         "post.rkt"
         "repo.rkt"
         "html.rkt")
(provide day->page)

(define (day->page r user dy)
  (define dstr (day->string dy))
  (match (and dy (get-post r dy))
    [#f (page user
              dstr
              `((h1 "There's no post for this day")
                ,@(if user
                      `((p (a ([href ,(day->url dy "/edit")]) "Edit.")))
                      '())))]
    [p (page user dstr `(,(post->section user p (all-topics r) (tags-hash r dy))))]))