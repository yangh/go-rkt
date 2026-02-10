#lang racket

(require "../src/board.rkt")
(require "../src/stone.rkt")
(require "../src/rules.rkt")

(displayln "=== 简单测试 ===")

;; 基本连通性测试
(define empty-board (make-empty-board))
(define board1 (board-set-stone empty-board (position 3 3) 'black))
(define board2 (board-set-stone board1 (position 3 4) 'black))

(displayln "棋盘设置完成")

(define group1 (rules-get-connected-group board2 (position 3 3)))
(define group2 (rules-get-connected-group board2 (position 3 4)))

(displayln (format "group1大小: ~a" (length group1)))
(displayln (format "group2大小: ~a" (length group2)))

(define connected? (member (position 3 3) group2))
(displayln (format "是否连通: ~a" connected?))

(if connected?
    (displayln "✓ 简单测试通过")
    (displayln "✗ 简单测试失败"))