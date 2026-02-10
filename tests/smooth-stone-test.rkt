#lang racket

(require "../src/gui-main.rkt")
(require "../src/board.rkt")
(require "../src/stone.rkt")
(require "../src/game-state.rkt")

(displayln "=== 棋子平滑效果测试 ===")

;; 创建测试游戏状态
(define test-state (make-initial-game-state))
(define test-board (game-state-board test-state))

;; 在棋盘上放置一些测试棋子
(define board1 (board-set-stone test-board (position 3 3) 'black))
(define board2 (board-set-stone board1 (position 3 4) 'white))
(define board3 (board-set-stone board2 (position 4 3) 'black))
(define board4 (board-set-stone board3 (position 4 4) 'white))

(define final-state (struct-copy game-state test-state [board board4]))

(displayln "测试棋盘设置完成，包含黑白棋子")
(displayln "请观察GUI中棋子的平滑效果")
(displayln "- 黑棋应该有灰色高光效果")
(displayln "- 白棋应该有轻微阴影效果")
(displayln "- 所有棋子边缘都应该平滑无锯齿")

;; 启动GUI进行视觉测试
(start-go-game)