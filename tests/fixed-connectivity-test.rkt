#lang racket

(require "../src/custom-format.rkt")
(require "../src/board.rkt")
(require "../src/rules.rkt")

(displayln "=== 修复后的连通性测试 ===")

;; 创建简单测试场景
(define test-board (make-empty-board))
(define board1 (board-set-stone test-board (position 4 4) 'black))
(define board2 (board-set-stone board1 (position 5 4) 'black))

(displayln "测试棋盘创建完成")

;; 测试连通性
(define group-4-4 (rules-get-connected-group board2 (position 4 4)))
(define group-5-4 (rules-get-connected-group board2 (position 5 4)))

(displayln (format "(4,4)连通组大小: ~a" (length group-4-4)))
(displayln (format "(5,4)连通组大小: ~a" (length group-5-4)))

;; 检查连通性
(define connected? (member (position 4 4) group-5-4))
(displayln (format "(4,4)和(5,4)是否连通: ~a" connected?))

(if connected?
    (displayln "✓ 连通性修复成功")
    (displayln "✗ 连通性仍有问题"))