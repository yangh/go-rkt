#lang racket

(require "../src/board.rkt")
(require "../src/stone.rkt")
(require "../src/rules.rkt")

(displayln "=== 更好的测试 ===")

;; 创建测试棋盘
(define test-board (make-empty-board))

;; 放置一些测试棋子
(define board1 (board-set-stone test-board (position 3 3) 'black))
(define board2 (board-set-stone board1 (position 3 4) 'black))
(define board3 (board-set-stone board2 (position 4 3) 'black))
(define board4 (board-set-stone board3 (position 2 3) 'white))
(define board5 (board-set-stone board4 (position 4 4) 'white))

(displayln "测试棋盘设置完成")

;; 测试连通性
(define group-3-3 (rules-get-connected-group board5 (position 3 3)))
(define group-3-4 (rules-get-connected-group board5 (position 3 4)))
(define group-4-3 (rules-get-connected-group board5 (position 4 3)))

(displayln (format "(3,3)连通组大小: ~a" (length group-3-3)))
(displayln (format "(3,4)连通组大小: ~a" (length group-3-4)))
(displayln (format "(4,3)连通组大小: ~a" (length group-4-3)))

;; 检查连通性
(define connected-3-4? (member (position 3 4) group-3-3))
(define connected-4-3? (member (position 4 3) group-3-3))

(displayln (format "(3,3)和(3,4)是否连通: ~a" connected-3-4?))
(displayln (format "(3,3)和(4,3)是否连通: ~a" connected-4-3?))

;; 测试气的计算
(define liberties-3-3 (rules-get-group-liberties board5 group-3-3))
(displayln (format "(3,3)组的气数: ~a" liberties-3-3))

;; 测试合法性检查
(define legal-3-5? (rules-is-valid-move? board5 (position 3 5) 'black))
(define legal-1-3? (rules-is-valid-move? board5 (position 1 3) 'black))

(displayln (format "在(3,5)下黑棋是否合法: ~a" legal-3-5?))
(displayln (format "在(1,3)下黑棋是否合法: ~a" legal-1-3?))