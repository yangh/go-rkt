#lang racket

(require "custom-format.rkt")
(require "game-engine.rkt")
(require "board.rkt")
(require "game-state.rkt")
(require "rules.rkt")
(require "stone.rkt")

(displayln "=== 关键发现 ===")

;; 加载game-01.txt的局面
(define game-state (custom-load-game "game-01.txt"))
(define board (game-state-board game-state))

;; 显示关键区域
(displayln "关键区域棋盘状态:")
(for ([row (in-range 3 7)])
  (for ([col (in-range 3 6)])
    (define pos (position row col))
    (define stone (board-get-stone board pos))
    (define marker 
      (cond
        [(equal? pos (position 3 4)) "[X]"]
        [stone (format "[~a]" (substring (symbol->string stone) 0 1))]
        [else "[ ]"]))
    (display marker))
  (newline))

;; 检查所有黑棋的连接情况
(define all-black (board-get-stones-by-color board 'black))
(displayln "\n所有黑棋位置及连接:")
(for ([pos all-black])
  (define neighbors (board-get-neighbors pos))
  (define black-neighbors 
    (filter (lambda (n) (eq? (board-get-stone board n) 'black)) neighbors))
  (displayln (format "(~a,~a): ~a个黑棋邻居" 
                    (position-row pos) 
                    (position-col pos)
                    (length black-neighbors))))

;; 验证(3,4)位置的真实情况
(displayln "\n=== 验证(3,4)位置 ===")
(define test-pos (position 3 4))
(displayln (format "(3,4)当前状态: ~a" (board-get-stone board test-pos)))

;; 检查(3,4)的邻居
(define test-neighbors (board-get-neighbors test-pos))
(displayln "(3,4)的邻居:")
(for ([neighbor test-neighbors])
  (define neighbor-stone (board-get-stone board neighbor))
  (displayln (format "  (~a,~a): ~a" 
                    (position-row neighbor) 
                    (position-col neighbor) 
                    (if neighbor-stone neighbor-stone "empty"))))

;; 最重要的是：让我们直接测试在(3,4)下黑棋会发生什么
(displayln "\n=== 直接测试下棋结果 ===")
(define test-board (board-set-stone board test-pos 'black))

;; 检查新形成的连通组
(define new-group (rules-get-connected-group test-board test-pos))
(displayln (format "在(3,4)下黑棋后，新形成的连通组大小: ~a" (length new-group)))
(displayln "组内位置:")
(for ([pos new-group])
  (displayln (format "  (~a,~a)" (position-row pos) (position-col pos))))

;; 检查气的情况
(define liberties (rules-get-group-liberties test-board new-group))
(displayln (format "整组气数: ~a" liberties))

;; 检查是否能提子
(define opponent-color (opposite-color 'black))
(define can-capture? 
  (ormap (lambda (neighbor-pos)
           (and (eq? (board-get-stone test-board neighbor-pos) opponent-color)
                (let ([opponent-group (rules-get-connected-group test-board neighbor-pos)])
                  (= (rules-get-group-liberties test-board opponent-group) 0))))
         test-neighbors))

(displayln (format "能否提掉对方棋子: ~a" can-capture?))

;; 最终合法性判断
(define legal? (rules-is-valid-move? board test-pos 'black))
(displayln (format "最终判断 - 是否合法: ~a" legal?))