#lang racket

(require "custom-format.rkt")
(require "board.rkt")
(require "stone.rkt")
(require "rules.rkt")
(require "game-state.rkt")

(displayln "=== 详细验证(3,4)及相关位置 ===")

;; 加载game-01.txt局面
(define game-state (custom-load-game "game-01.txt"))
(define board (game-state-board game-state))

;; 检查关键位置的状态
(define positions-to-check (list (position 3 4) (position 4 4) (position 5 4)))
(displayln "关键位置状态检查:")
(for ([pos positions-to-check])
  (define stone (board-get-stone board pos))
  (displayln (format "  (~a,~a): ~a" 
                    (position-row pos) 
                    (position-col pos) 
                    (if stone stone "empty"))))

;; 检查这些位置是否连通
(displayln "\n连通性检查:")
(for ([pos positions-to-check])
  (define stone (board-get-stone board pos))
  (when stone
    (define group (rules-get-connected-group board pos))
    (displayln (format "  (~a,~a)的连通组大小: ~a" 
                      (position-row pos) 
                      (position-col pos) 
                      (length group)))))

;; 显示更大的棋盘区域来全面了解局势
(displayln "\n=== 更大范围的棋局 ===")
(for ([row (in-range 1 8)])
  (for ([col (in-range 1 8)])
    (define pos (position row col))
    (define stone (board-get-stone board pos))
    (define marker 
      (cond
        [(member pos positions-to-check) "[X]"]  ; 关键位置
        [stone (format "[~a]" (substring (symbol->string stone) 0 1))]
        [else "[ ]"]))
    (display marker))
  (newline))

(displayln "\n图例: [b]=黑棋, [w]=白棋, [X]=关键测试位置")