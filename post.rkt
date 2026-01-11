#lang racket/base
(provide (struct-out post))

(struct post (day text)
  #:transparent)