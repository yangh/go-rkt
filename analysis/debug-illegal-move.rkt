#lang racket

(require "../src/custom-format.rkt")
(require "../src/board.rkt")
(require "../src/rules.rkt")
(require "../src/stone.rkt")
(require "../src/game-state.rkt")

(displayln "=== Debug Illegal Move ===")

;; 加载游戏状态
(define game-state (custom-load-game "../data/game-01.txt"))
(define board (game-state-board game-state))

(displayln "游戏状态加载完成")

;; 核心分析逻辑
(define test-pos-1 (position 4 4))
(define test-pos-2 (position 5 4))

(displayln (format "(4,4)棋子: ~a" (board-get-stone board test-pos-1)))
(displayln (format "(5,4)棋子: ~a" (board-get-stone board test-pos-2)))

(define group-1 (rules-get-connected-group board test-pos-1))
(define group-2 (rules-get-connected-group board test-pos-2))

(displayln (format "(4,4)连通组: ~a" (length group-1)))
(displayln (format "(5,4)连通组: ~a" (length group-2)))

(define connected? (member test-pos-1 group-2))
(displayln (format "是否连通: ~a" connected?))

(if connected?
    (displayln "✓ 分析结果正常")
    (displayln "✗ 需要进一步调查"))

(displayln "分析完成")
