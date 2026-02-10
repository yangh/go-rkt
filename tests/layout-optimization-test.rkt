#lang racket

(require "../src/gui-main.rkt")
(require "../src/game-state.rkt")
(require "../src/game-engine.rkt")
(require "../src/board.rkt")
(require "../src/stone.rkt")

(displayln "=== 界面布局优化测试 ===")

;; 创建一个包含多手棋的游戏状态进行测试
(define test-state (make-initial-game-state))

;; 添加一些测试棋子来验证布局
(define state-with-moves
  (game-make-move
   (game-make-move
    (game-make-move
     (game-make-move 
      (game-make-move test-state (position 3 3))  ; 黑1
      (position 15 15))  ; 白2
     (position 3 15))   ; 黑3
    (position 15 3))    ; 白4
   (position 9 9)))     ; 黑5

(printf "创建了包含 ~a 手棋的测试局面~n" 
        (length (game-state-move-history state-with-moves)))

;; 启动GUI进行测试
(start-go-game-with-state state-with-moves)

(displayln "GUI已启动，请检查新的界面布局:")
(displayln "- 状态信息区域应垂直显示两行:")
(displayln "  · 当前行棋方信息")
(displayln "  · 总手数统计")
(displayln "- 提子计数区域保持原有垂直排列")
(displayln "- 整体布局更加整齐统一")