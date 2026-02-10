#lang racket

(require "../src/custom-format.rkt")
(require "../src/board.rkt")
(require "../src/rules.rkt")
(require "../src/stone.rkt")
(require "../src/game-state.rkt")

(displayln "=== 连通性调试分析 ===")

;; 加载游戏状态
(define game-state (custom-load-game "../data/game-01.txt"))
(define board (game-state-board game-state))

;; 定义测试位置
(define test-pos-1 (position 4 4))
(define test-pos-2 (position 5 4))

(displayln "游戏状态加载完成")

;; 基本信息检查
(displayln (format "(4,4)位置棋子: ~a" (board-get-stone board test-pos-1)))
(displayln (format "(5,4)位置棋子: ~a" (board-get-stone board test-pos-2)))

;; 连通性检查
(define group-1 (rules-get-connected-group board test-pos-1))
(define group-2 (rules-get-connected-group board test-pos-2))

(displayln (format "(4,4)连通组大小: ~a" (length group-1)))
(displayln (format "(5,4)连通组大小: ~a" (length group-2)))

;; 连通性判断
(define are-connected? (member test-pos-1 group-2))
(displayln (format "(4,4)和(5,4)是否连通: ~a" are-connected?))

;; 相邻性检查
(define neighbors-of-1 (board-get-neighbors test-pos-1))
(define are-adjacent? (member test-pos-2 neighbors-of-1))
(displayln (format "(4,4)和(5,4)是否相邻: ~a" are-adjacent?))

;; 结论
(if are-connected?
    (displayln "✓ 连通性正常")
    (displayln "✗ 连通性存在问题"))

(displayln "分析完成")