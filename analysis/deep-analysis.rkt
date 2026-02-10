#lang racket

(require "../src/custom-format.rkt")
(require "../src/board.rkt")
(require "../src/rules.rkt")
(require "../src/stone.rkt")
(require "../src/game-state.rkt")

(displayln "=== 深度分析 ===")

;; 加载游戏状态
(define game-state (custom-load-game "../data/game-01.txt"))
(define board (game-state-board game-state))

(displayln "游戏状态加载完成")

;; 分析所有黑棋位置
(define all-positions 
  (for*/list ([row (in-range 19)]
              [col (in-range 19)])
    (position row col)))

(define black-stones 
  (filter (lambda (pos)
            (eq? (board-get-stone board pos) 'black))
          all-positions))

(displayln (format "找到 ~a 个黑棋" (length black-stones)))

;; 分析关键位置
(define pos-4-4 (position 4 4))
(define pos-5-4 (position 5 4))

(displayln (format "(4,4)棋子: ~a" (board-get-stone board pos-4-4)))
(displayln (format "(5,4)棋子: ~a" (board-get-stone board pos-5-4)))

;; 检查连通性
(define group-4-4 (rules-get-connected-group board pos-4-4))
(define group-5-4 (rules-get-connected-group board pos-5-4))

(displayln (format "(4,4)连通组大小: ~a" (length group-4-4)))
(displayln (format "(5,4)连通组大小: ~a" (length group-5-4)))

(define connected? (member pos-4-4 group-5-4))
(displayln (format "是否连通: ~a" connected?))

;; 显示棋盘局部状态
(displayln "\n关键区域棋盘状态:")
(for ([row (in-range 3 7)])
  (for ([col (in-range 3 7)])
    (define pos (position row col))
    (define stone (board-get-stone board pos))
    (define marker 
      (cond
        [(equal? pos pos-4-4) "[A]"]
        [(equal? pos pos-5-4) "[B]"]
        [stone (format "[~a]" (substring (symbol->string stone) 0 1))]
        [else "[ ]"]))
    (display marker))
  (newline))

(if connected?
    (displayln "✓ 连通性正常")
    (displayln "✗ 连通性存在问题"))