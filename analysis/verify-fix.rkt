#lang racket

(require "custom-format.rkt")
(require "board.rkt")
(require "stone.rkt")
(require "rules.rkt")

(displayln "=== 验证修复后的连通性检查 ===")

;; 加载game-01.txt的局面
(define game-state (custom-load-game "game-01.txt"))
(define board (game-state-board game-state))

(displayln "当前棋盘状态 (关键区域):")
(for ([row (in-range 2 7)])
  (for ([col (in-range 2 7)])
    (define pos (position row col))
    (define stone (board-get-stone board pos))
    (define marker 
      (cond
        [(equal? pos (position 3 4)) "[X]"]  ; 目标位置
        [stone (format "[~a]" (substring (symbol->string stone) 0 1))]
        [else "[ ]"]))
    (display marker))
  (newline))

;; 测试关键位置
(displayln "\n=== 测试黑棋在(3,4)的合法性 ===")
(define test-pos (position 3 4))

;; 1. 基本检查
(displayln "1. 基本检查:")
(displayln (format "   位置(~a,~a)是否有效: ~a" 
                  (position-row test-pos) (position-col test-pos)
                  (board-is-valid-position? test-pos)))
(displayln (format "   位置是否为空: ~a" (board-is-empty? board test-pos)))

;; 2. 连通性检查
(displayln "\n2. 连通性分析:")
(define temp-board (board-set-stone board test-pos 'black))
(define new-group (rules-get-connected-group temp-board test-pos))
(displayln (format "   新形成的连通组大小: ~a" (length new-group)))
(displayln "   组内位置:")
(for ([pos new-group])
  (displayln (format "     (~a,~a)" (position-row pos) (position-col pos))))

;; 3. 气的检查
(displayln "\n3. 气的检查:")
(define group-liberties (rules-get-group-liberties temp-board new-group))
(displayln (format "   整组气数: ~a" group-liberties))

;; 4. 提子检查
(displayln "\n4. 提子可能性:")
(define can-capture? (rules-can-capture-opponent? temp-board test-pos 'black))
(displayln (format "   能否提掉对方棋子: ~a" can-capture?))

;; 5. 最终合法性判断
(displayln "\n5. 最终判断:")
(define legal? (rules-is-valid-move? board test-pos 'black))
(define suicide? (rules-would-be-suicide? board test-pos 'black))

(displayln (format "   rules-is-valid-move? 返回: ~a" legal?))
(displayln (format "   rules-would-be-suicide? 返回: ~a" suicide?))

(if legal?
    (displayln "   ✓ 修复成功！黑棋在(3,4)现在被认为是合法移动")
    (displayln "   ✗ 修复失败，仍判定为非法"))

;; 额外验证：检查其他几个位置作为对照
(displayln "\n=== 对照测试 ===")
(define test-positions (list (position 0 0) (position 3 3) (position 6 6)))
(for ([pos test-positions])
  (define is-legal? (rules-is-valid-move? board pos 'black))
  (displayln (format "   (~a,~a) 黑棋合法性: ~a" 
                    (position-row pos) (position-col pos) is-legal?)))