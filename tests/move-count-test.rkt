#lang racket

(require "../src/gui-main.rkt")
(require "../src/game-state.rkt")
(require "../src/game-engine.rkt")
(require "../src/board.rkt")
(require "../src/stone.rkt")

(displayln "=== 手数显示测试 ===")

;; 创建一个包含多手棋的游戏状态进行测试
(define test-state (make-initial-game-state))

;; 添加一些测试棋子
(define state-with-moves
  (game-make-move
   (game-make-move
    (game-make-move
     (game-make-move test-state (position 3 3))  ; 黑1
     (position 15 15))  ; 白2
    (position 3 15))   ; 黑3
   (position 15 3)))   ; 白4

(printf "创建了包含 ~a 手棋的测试局面~n" 
        (length (game-state-move-history state-with-moves)))

;; 启动GUI进行测试
(start-go-game-with-state state-with-moves)

(displayln "GUI已启动，请检查:")
(displayln "- 状态栏右侧应显示'总手数: 4'")
(displayln "- 提子计数区域应显示双方提子数")
(displayln "- 棋盘上不应显示棋子上的数字")