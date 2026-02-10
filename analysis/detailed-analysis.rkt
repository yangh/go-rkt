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
(define current-player (game-get-current-player game-state))

(displayln "=== 重新分析(3,4)位置 ===")
(displayln (format "当前玩家: ~a" current-player))

;; 显示更大的棋盘区域来全面了解局势
(displayln "\n=== (3,4)周围更大范围的棋局 ===")
(for ([row (in-range 0 8)])
  (for ([col (in-range 0 8)])
    (define pos (position row col))
    (define stone (board-get-stone board pos))
    (define marker 
      (cond
        [(equal? pos (position 3 4)) "[X]"]  ; 目标位置
        [stone (format "[~a]" (substring (symbol->string stone) 0 1))]
        [else "[ ]"]))
    (display marker))
  (newline))

;; 特别检查关键位置
(displayln "\n=== 关键黑棋位置检查 ===")
(define key-black-positions (list (position 4 4) (position 5 4)))
(for ([pos key-black-positions])
  (define stone (board-get-stone board pos))
  (displayln (format "(~a,~a): ~a" 
                    (position-row pos) 
                    (position-col pos) 
                    stone)))

;; 检查(3,4)与(4,4)、(5,4)是否连通
(displayln "\n=== 连通性检查 ===")
(define pos-4-4 (position 4 4))
(define pos-5-4 (position 5 4))

(displayln "检查(4,4)的连通组:")
(define group-4-4 (rules-get-connected-group board pos-4-4))
(displayln (format "组大小: ~a" (length group-4-4)))
(displayln "组内位置:")
(for ([pos group-4-4])
  (displayln (format "  (~a,~a)" (position-row pos) (position-col pos))))

(displayln "\n检查(5,4)的连通组:")
(define group-5-4 (rules-get-connected-group board pos-5-4))
(displayln (format "组大小: ~a" (length group-5-4)))
(displayln "组内位置:")
(for ([pos group-5-4])
  (displayln (format "  (~a,~a)" (position-row pos) (position-col pos))))

;; 检查它们是否属于同一组
(define same-group? (member pos-4-4 group-5-4))
(displayln (format "\n(4,4)和(5,4)是否在同一连通组: ~a" same-group?))

;; 模拟黑棋下在(3,4)的情况
(displayln "\n=== 模拟黑棋下在(3,4) ===")
(define test-board (board-set-stone board (position 3 4) 'black))

;; 检查新形成的连通组
(displayln "新形成的连通组:")
(define new-group (rules-get-connected-group test-board (position 3 4)))
(displayln (format "组大小: ~a" (length new-group)))
(displayln "组内位置:")
(for ([pos new-group])
  (displayln (format "  (~a,~a)" (position-row pos) (position-col pos))))

;; 检查气的情况
(displayln "\n气的检查:")
(define liberties (rules-get-group-liberties test-board new-group))
(displayln (format "整组气数: ~a" liberties))

;; 检查各个位置的气
(displayln "各位置气数:")
(for ([pos new-group])
  (define pos-liberties (length (rules-get-liberties test-board pos)))
  (displayln (format "  (~a,~a): ~a气" 
                    (position-row pos) 
                    (position-col pos) 
                    pos-liberties)))

;; 手动检查是否能提子（替代rules-can-capture-opponent?）
(displayln "\n提子检查:")
(define opponent-color (opposite-color 'black))
(define neighbors (board-get-neighbors (position 3 4)))
(define can-capture? 
  (ormap (lambda (neighbor-pos)
           (and (eq? (board-get-stone test-board neighbor-pos) opponent-color)
                (let ([opponent-group (rules-get-connected-group test-board neighbor-pos)])
                  (= (rules-get-group-liberties test-board opponent-group) 0))))
         neighbors))
(displayln (format "能否提掉对方棋子: ~a" can-capture?))

;; 最终合法性判断
(displayln "\n=== 最终结论 ===")
(define legal? (rules-is-valid-move? board (position 3 4) 'black))
(displayln (format "rules-is-valid-move? 返回: ~a" legal?))

(define would-be-suicide? (rules-would-be-suicide? board (position 3 4) 'black))
(displayln (format "rules-would-be-suicide? 返回: ~a" would-be-suicide?))