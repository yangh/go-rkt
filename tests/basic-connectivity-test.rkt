#lang racket

(require "../src/board.rkt")
(require "../src/stone.rkt")
(require "../src/rules.rkt")

(displayln "=== 基础连通性测试 ===")

;; 创建最简单的测试棋盘
(define test-board (make-empty-board))

;; 放置两个相邻的黑棋
(define board1 (board-set-stone test-board (position 3 3) 'black))
(define board2 (board-set-stone board1 (position 3 4) 'black))  ; 相邻位置

(displayln "测试棋盘设置完成 - 两个相邻黑棋(3,3)和(3,4)")

;; 测试连通性
(displayln "\n测试连通性:")
(define group-3-3 (rules-get-connected-group board2 (position 3 3)))
(define group-3-4 (rules-get-connected-group board2 (position 3 4)))

(displayln (format "(3,3)连通组大小: ~a" (length group-3-3)))
(displayln (format "(3,4)连通组大小: ~a" (length group-3-4)))

(displayln "连通组内容:")
(displayln "(3,3)组:")
(for ([pos group-3-3])
  (displayln (format "  (~a,~a)" (position-row pos) (position-col pos))))

(displayln "(3,4)组:")
(for ([pos group-3-4])
  (displayln (format "  (~a,~a)" (position-row pos) (position-col pos))))

;; 检查它们是否连通
(define simple-connected? (member (position 3 3) group-3-4))
(displayln (format "简单测试 - 两个相邻黑棋是否连通: ~a" simple-connected?))

;; 如果基础测试失败，说明算法根本有问题
(if simple-connected?
    (displayln "✓ 基础连通性工作正常")
    (displayln "✗ 基础连通性算法有根本问题"))

;; 显示测试棋盘
(displayln "\n测试棋盘状态:")
(for ([row (in-range 2 5)])
  (for ([col (in-range 2 6)])
    (define pos (position row col))
    (define stone (board-get-stone board2 pos))
    (define marker 
      (cond
        [(equal? pos (position 3 3)) "[A]"]  ; 标记第一个黑棋
        [(equal? pos (position 3 4)) "[B]"]  ; 标记第二个黑棋
        [stone (format "[~a]" (substring (symbol->string stone) 0 1))]
        [else "[ ]"]))
    (display marker))
  (newline))