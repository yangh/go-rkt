#lang racket

(require "../src/board.rkt")
(require "../src/stone.rkt")
(require "../src/rules.rkt")

(displayln "=== 孤立测试 ===")

;; 简单测试
(define empty-board (make-empty-board))
(define test-board (board-set-stone empty-board (position 3 3) 'black))

(define group-size (length (rules-get-connected-group test-board (position 3 3))))
(define liberties (rules-get-group-liberties test-board (list (position 3 3))))

(displayln (format "连通组大小: ~a" group-size))
(displayln (format "气数: ~a" liberties))

(if (and (= group-size 1) (= liberties 4))
    (displayln "✓ 孤立测试通过")
    (displayln "✗ 孤立测试失败"))