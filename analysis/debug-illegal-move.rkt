#lang racket

(require "custom-format.rkt")
(require "game-engine.rkt")
(require "board.rkt")
(require "game-state.rkt")

;; 加载game-01.txt的局面
(define game-state (custom-load-game "game-01.txt"))

;; 显示当前局面
(displayln "当前局面:")
(displayln (custom-export-move-history game-state))

;; 检查位置(3,4)的合法性
(define test-pos (position 3 4))
(define current-player (game-get-current-player game-state))
(define board (game-state-board game-state))

(displayln (format "\n尝试在(~a,~a)落子..." (position-row test-pos) (position-col test-pos)))
(displayln (format "当前玩家: ~a" current-player))

;; 逐步检查合法性
(displayln "\n=== 合法性检查 ===")

;; 1. 边界检查
(define boundary-ok? (board-is-valid-position? test-pos))
(displayln (format "1. 边界检查: ~a" boundary-ok?))

;; 2. 位置是否为空
(define empty-ok? (board-is-empty? board test-pos))
(displayln (format "2. 位置为空: ~a" empty-ok?))

;; 3. 自杀检查
(require "rules.rkt")
(define suicide-ok? (not (rules-would-be-suicide? board test-pos current-player)))
(displayln (format "3. 不是自杀: ~a" suicide-ok?))

;; 4. 劫争检查
(require "ko-rule.rkt")
(define ko-result (ko-check-move game-state test-pos current-player))
(displayln (format "4. 劫争检查: ~a" (first ko-result)))
(when (not (first ko-result))
  (displayln (format "   原因: ~a" (second ko-result))))

;; 5. 最终合法性判断
(define legal? (rules-is-valid-move? board test-pos current-player))
(displayln (format "\n最终判断: ~a" (if legal? "合法" "非法")))

;; 如果非法，显示周围棋子情况
(when (not legal?)
  (displayln "\n=== 周围棋子情况 ===")
  (define neighbors (board-get-neighbors test-pos))
  (for ([neighbor neighbors])
    (define stone (board-get-stone board neighbor))
    (when stone
      (displayln (format "  (~a,~a): ~a" 
                        (position-row neighbor) 
                        (position-col neighbor) 
                        stone)))))

;; 显示当前位置的状态
(displayln (format "\n位置(~a,~a)当前状态: ~a" 
                  (position-row test-pos) 
                  (position-col test-pos)
                  (board-get-stone board test-pos)))

;; 显示棋盘上(3,4)附近的详细情况
(displayln "\n=== (3,4)附近棋子详情 ===")
(for ([row (in-range 1 7)])
  (for ([col (in-range 1 7)])
    (define pos (position row col))
    (define stone (board-get-stone board pos))
    (define marker 
      (cond
        [(equal? pos test-pos) "[X]"]  ; 目标位置
        [stone (format "[~a]" (if (eq? stone 'black) "B" "W"))]
        [else "[ ]"]))
    (display marker))
  (newline))