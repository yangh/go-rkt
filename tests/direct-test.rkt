#lang racket

(require "../src/board.rkt")
(require "../src/stone.rkt")
(require "../src/rules.rkt")

(displayln "=== 直接测试 ===")

;; 创建简单测试
(define empty-board (make-empty-board))
(define test-board 
  (board-set-stone 
    (board-set-stone empty-board (position 3 3) 'black)
    (position 3 4) 'black))

(displayln "测试棋盘创建完成")

;; 测试基本功能
(define group-size (length (rules-get-connected-group test-board (position 3 3))))
(displayln (format "连通组大小: ~a" group-size))

(define liberties (rules-get-group-liberties test-board (list (position 3 3) (position 3 4))))
(displayln (format "气数: ~a" liberties))

(define valid-move? (rules-is-valid-move? test-board (position 3 5) 'black))
(displayln (format "合法移动: ~a" valid-move?))

;; 简单验证
(if (and (= group-size 2) (> liberties 0) valid-move?)
    (displayln "✓ 所有测试通过")
    (displayln "✗ 测试失败"))