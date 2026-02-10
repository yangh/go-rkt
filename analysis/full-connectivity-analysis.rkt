#lang racket

(require "custom-format.rkt")
(require "game-engine.rkt")
(require "board.rkt")
(require "game-state.rkt")
(require "rules.rkt")
(require "stone.rkt")

;; 加载game-01.txt的局面
(define game-state (custom-load-game "game-01.txt"))
(define board (game-state-board game-state))

(displayln "=== 详细连通性分析 ===")

;; 显示完整棋盘以准确判断连通性
(displayln "\n完整棋盘状态:")
(for ([row (in-range 0 8)])
  (for ([col (in-range 0 8)])
    (define pos (position row col))
    (define stone (board-get-stone board pos))
    (define marker 
      (cond
        [stone (format "[~a]" (substring (symbol->string stone) 0 1))]
        [else "[ ]"]))
    (display marker))
  (newline))

;; 逐个检查每个黑棋的位置及其连通组
(displayln "\n=== 逐一检查所有黑棋的连通组 ===")
(define all-black-stones (board-get-stones-by-color board 'black))
(for ([pos all-black-stones])
  (define group (rules-get-connected-group board pos))
  (displayln (format "(~a,~a)的连通组大小: ~a" 
                    (position-row pos) 
                    (position-col pos) 
                    (length group)))
  (displayln "  组内位置:")
  (for ([group-pos group])
    (displayln (format "    (~a,~a)" 
                      (position-row group-pos) 
                      (position-col group-pos))))
  (newline))

;; 特别关注关键位置
(displayln "=== 重点关注位置分析 ===")
(define key-positions (list (position 1 3) (position 1 4) (position 2 5) 
                           (position 4 4) (position 5 4)))

(for ([pos key-positions])
  (define stone (board-get-stone board pos))
  (displayln (format "(~a,~a): ~a" 
                    (position-row pos) 
                    (position-col pos) 
                    stone))
  (when (eq? stone 'black)
    (define group (rules-get-connected-group board pos))
    (displayln (format "  连通组大小: ~a" (length group))))
  (newline))

;; 检查相邻关系
(displayln "=== 相邻关系检查 ===")
(define pos-4-4 (position 4 4))
(define pos-5-4 (position 5 4))

(displayln "检查(4,4)的邻居:")
(define neighbors-4-4 (board-get-neighbors pos-4-4))
(for ([neighbor neighbors-4-4])
  (define neighbor-stone (board-get-stone board neighbor))
  (displayln (format "  (~a,~a): ~a" 
                    (position-row neighbor) 
                    (position-col neighbor) 
                    (if neighbor-stone neighbor-stone "empty"))))

(displayln "检查(5,4)的邻居:")
(define neighbors-5-4 (board-get-neighbors pos-5-4))
(for ([neighbor neighbors-5-4])
  (define neighbor-stone (board-get-stone board neighbor))
  (displayln (format "  (~a,~a): ~a" 
                    (position-row neighbor) 
                    (position-col neighbor) 
                    (if neighbor-stone neighbor-stone "empty"))))

;; 手动追踪连通路径
(displayln "\n=== 手动追踪可能的连通路径 ===")
(displayln "从视觉上看，这些黑棋应该连通:")
(displayln "(1,3) -(右)-> (1,4)")
(displayln "(1,4) -(下右)-> (2,5)")
(displayln "(2,5) -(下)-> ?")
(displayln "(4,4) -(下)-> (5,4)")

;; 检查(2,5)和(4,4)之间是否有连接
(define pos-2-5 (position 2 5))
(define pos-4-4-real (position 4 4))

(displayln (format "\n(2,5)位置棋子: ~a" (board-get-stone board pos-2-5)))
(displayln (format "(4,4)位置棋子: ~a" (board-get-stone board pos-4-4-real)))

;; 检查它们的连通组
(define group-2-5 (rules-get-connected-group board pos-2-5))
(define group-4-4-real (rules-get-connected-group board pos-4-4-real))

(displayln (format "(2,5)连通组大小: ~a" (length group-2-5)))
(displayln (format "(4,4)连通组大小: ~a" (length group-4-4-real)))