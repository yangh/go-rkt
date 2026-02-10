#lang racket

(require "../src/board.rkt")
(require "../src/stone.rkt")
(require "../src/rules.rkt")

(displayln "=== 测试修复效果 ===")

;; 创建测试场景来验证修复
(define empty-board (make-empty-board))

;; 设置类似game-01.txt的场景
(define board1 (board-set-stone empty-board (position 4 4) 'black))
(define board2 (board-set-stone board1 (position 5 4) 'black))
(define board3 (board-set-stone board2 (position 3 3) 'white))
(define board4 (board-set-stone board3 (position 3 5) 'white))
(define board5 (board-set-stone board4 (position 2 4) 'white))
(define test-board (board-set-stone board5 (position 4 3) 'white))

(displayln "测试棋盘设置完成")

;; 测试关键连通性
(define group-4-4 (rules-get-connected-group test-board (position 4 4)))
(define group-5-4 (rules-get-connected-group test-board (position 5 4)))

(displayln (format "(4,4)连通组大小: ~a" (length group-4-4)))
(displayln (format "(5,4)连通组大小: ~a" (length group-5-4)))

;; 检查连通性
(define connected? (member (position 4 4) group-5-4))
(displayln (format "(4,4)和(5,4)是否连通: ~a" connected?))

;; 测试在(3,4)下黑棋的情况
(define temp-board (board-set-stone test-board (position 3 4) 'black))
(define new-group (rules-get-connected-group temp-board (position 3 4)))
(define liberties (rules-get-group-liberties temp-board new-group))

(displayln (format "在(3,4)下黑棋后连通组大小: ~a" (length new-group)))
(displayln (format "整组气数: ~a" liberties))

;; 最终判断
(define legal? (rules-is-valid-move? test-board (position 3 4) 'black))
(displayln (format "在(3,4)下黑棋是否合法: ~a" legal?))

(if (and connected? legal? (> liberties 0))
    (displayln "🎉 修复验证成功！所有测试通过")
    (displayln "❌ 修复验证失败"))