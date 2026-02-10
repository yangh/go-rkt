#lang racket

(provide (struct-out stone)
         opposite-color
         color?)

;; 棋子结构
(struct stone (color position) #:transparent)

;; 棋子颜色类型检查
(define (color? c)
  (or (eq? c 'black) (eq? c 'white)))

;; 获取相反颜色
(define (opposite-color color)
  (cond
    [(eq? color 'black) 'white]
    [(eq? color 'white) 'black]
    [else (error "Invalid color:" color)]))

;; 模块测试
(module+ test
  (require rackunit)
  (require "board.rkt")
  
  ;; 测试棋子创建
  (define test-stone (stone 'black (position 3 3)))
  (check-eq? (stone-color test-stone) 'black)
  (check-equal? (stone-position test-stone) (position 3 3))
  
  ;; 测试颜色反转
  (check-eq? (opposite-color 'black) 'white)
  (check-eq? (opposite-color 'white) 'black)
  (check-exn exn:fail? (lambda () (opposite-color 'red)))
  
  ;; 测试颜色类型检查
  (check-true (color? 'black))
  (check-true (color? 'white))
  (check-false (color? 'red))
  (check-false (color? #f))
  )