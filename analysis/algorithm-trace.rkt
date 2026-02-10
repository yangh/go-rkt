#lang racket

(require "../src/custom-format.rkt")
(require "../src/board.rkt")
(require "../src/rules.rkt")
(require "../src/stone.rkt")
(require "../src/game-state.rkt")

(displayln "=== algorithm-trace ===")

;; 基本功能测试
(define game-state (custom-load-game "../data/game-01.txt"))
(define board (game-state-board game-state))

(define pos1 (position 4 4))
(define pos2 (position 5 4))

(displayln (format "位置(4,4)棋子: ~a" (board-get-stone board pos1)))
(displayln (format "位置(5,4)棋子: ~a" (board-get-stone board pos2)))

(define group1 (rules-get-connected-group board pos1))
(define group2 (rules-get-connected-group board pos2))

(displayln (format "(4,4)连通组大小: ~a" (length group1)))
(displayln (format "(5,4)连通组大小: ~a" (length group2)))

(define connected? (member pos1 group2))
(displayln (format "是否连通: ~a" connected?))

(if connected?
    (displayln "✓ 功能正常")
    (displayln "✗ 需要检查"))

(displayln "分析完成")
