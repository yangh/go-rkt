#lang racket

(require "../src/board.rkt")
(require "../src/stone.rkt")
(require "../src/game-state.rkt")

(displayln "=== 简单连通性检查 ===")

;; 创建测试棋盘
(define empty-board (make-empty-board))
(define board1 (board-set-stone empty-board (position 3 3) 'black))
(define board2 (board-set-stone board1 (position 3 4) 'black))

(displayln "测试棋盘创建完成")

;; 检查连通性
(define stone-3-3 (board-get-stone board2 (position 3 3)))
(define stone-3-4 (board-get-stone board2 (position 3 4)))

(displayln (format "(3,3)位置棋子: ~a" stone-3-3))
(displayln (format "(3,4)位置棋子: ~a" stone-3-4))

(define neighbors (board-get-neighbors (position 3 3)))
(displayln (format "(3,3)的邻居数量: ~a" (length neighbors)))

(if (and (eq? stone-3-3 'black) (eq? stone-3-4 'black))
    (displayln "✓ 基础设置正常")
    (displayln "✗ 基础设置异常"))