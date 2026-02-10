#lang racket

(require "board.rkt")
(require "stone.rkt")

(displayln "=== 简单连通性测试 ===")

;; 创建测试棋盘
(define empty-board (make-empty-board))
(define board-with-two-blacks 
  (board-set-stone 
    (board-set-stone empty-board (position 3 3) 'black)
    (position 3 4) 'black))

(displayln "棋盘设置完成")

;; 手动检查连通性
(define pos1 (position 3 3))
(define pos2 (position 3 4))

(displayln (format "pos1: (~a,~a)" (position-row pos1) (position-col pos1)))
(displayln (format "pos2: (~a,~a)" (position-row pos2) (position-col pos2)))

;; 检查它们是否相邻
(define neighbors-of-pos1 (board-get-neighbors pos1))
(define adjacent? (member pos2 neighbors-of-pos1))

(displayln (format "pos1和pos2是否相邻: ~a" adjacent?))

;; 检查颜色
(define color1 (board-get-stone board-with-two-blacks pos1))
(define color2 (board-get-stone board-with-two-blacks pos2))

(displayln (format "pos1颜色: ~a" color1))
(displayln (format "pos2颜色: ~a" color2))
(displayln (format "颜色是否相同: ~a" (eq? color1 color2)))

;; 最终结论
(if (and adjacent? (eq? color1 color2) color1)
    (displayln "✓ 理论上这两个位置应该连通")
    (displayln "✗ 这两个位置不应该连通"))