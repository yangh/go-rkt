#lang racket

(require "custom-format.rkt")
(require "game-engine.rkt")
(require "board.rkt")
(require "game-state.rkt")
(require "rules.rkt")
(require "stone.rkt")

(displayln "=== 寻找合法落子位置 ===")

;; 加载游戏
(define game-state (custom-load-game "game-01.txt"))
(define board (game-state-board game-state))
(define current-player (game-get-current-player game-state))

(displayln (format "当前玩家: ~a" current-player))

;; 检查棋盘上所有可能的位置
(displayln "\n寻找所有合法的黑棋落子位置:")
(define legal-positions '())

(for ([row (in-range 0 6)])
  (for ([col (in-range 0 6)])
    (define pos (position row col))
    (when (rules-is-valid-move? board pos current-player)
      (set! legal-positions (cons pos legal-positions))
      (displayln (format "  找到合法位置: (~a,~a)" 
                        (position-row pos) 
                        (position-col pos))))))

(displayln (format "\n总共找到 ~a 个合法位置" (length legal-positions)))

;; 显示第一个合法位置的详细信息
(when (not (null? legal-positions))
  (define first-legal (car legal-positions))
  (displayln (format "\n详细分析第一个合法位置 (~a,~a):" 
                    (position-row first-legal) 
                    (position-col first-legal)))
  
  ;; 显示周围情况
  (displayln "周围棋子:")
  (define neighbors (board-get-neighbors first-legal))
  (for ([neighbor neighbors])
    (define stone (board-get-stone board neighbor))
    (displayln (format "  (~a,~a): ~a" 
                      (position-row neighbor) 
                      (position-col neighbor) 
                      (if stone stone "empty"))))
  
  ;; 验证移动
  (displayln "\n尝试执行移动:")
  (with-handlers ([exn:fail? (lambda (e)
                              (displayln (format "执行失败: ~a" (exn-message e))))])
    (define new-state (game-make-move game-state first-legal))
    (displayln "✓ 移动执行成功!")
    (displayln (format "新捕获黑子: ~a" (game-state-get-captured-count new-state 'black)))
    (displayln (format "新捕获白子: ~a" (game-state-get-captured-count new-state 'white)))))

;; 如果没有合法位置，检查是否是游戏结束状态
(when (null? legal-positions)
  (displayln "\n没有找到合法位置，检查游戏状态:")
  (define game-over? (game-is-game-over? game-state))
  (displayln (format "游戏是否结束: ~a" game-over?))
  
  (when game-over?
    (define winner (game-get-winner game-state))
    (displayln (format "获胜者: ~a" winner))))