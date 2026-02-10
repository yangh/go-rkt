#lang racket

(require "../src/custom-format.rkt")
(require "../src/game-engine.rkt")
(require "../src/board.rkt")
(require "../src/game-state.rkt")
(require "../src/rules.rkt")
(require "../src/stone.rkt")

(displayln "=== 完整系统测试 ===")

;; 创建初始游戏状态
(define initial-state (make-initial-game-state))
(displayln "1. 创建初始游戏状态")

;; 测试基本移动
(define move1-state (game-make-move initial-state (position 3 3)))
(displayln "2. 执行第一次移动")

;; 检查状态变化
(define current-player (game-get-current-player move1-state))
(define captured-black (game-state-get-captured-count move1-state 'black))
(define captured-white (game-state-get-captured-count move1-state 'white))

(displayln (format "当前玩家: ~a" current-player))
(displayln (format "被捕获黑子: ~a" captured-black))
(displayln (format "被捕获白子: ~a" captured-white))

;; 测试棋盘状态
(define board (game-state-board move1-state))
(define stone-at-3-3 (board-get-stone board (position 3 3)))
(displayln (format "(3,3)位置棋子: ~a" stone-at-3-3))

;; 验证基本功能
(if (and (eq? current-player 'white) 
         (= captured-black 0) 
         (= captured-white 0)
         (eq? stone-at-3-3 'black))
    (displayln "✓ 基本游戏功能正常")
    (displayln "✗ 基本游戏功能异常"))