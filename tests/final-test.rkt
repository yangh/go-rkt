#lang racket

(require "../src/custom-format.rkt")
(require "../src/board.rkt")
(require "../src/stone.rkt")
(require "../src/rules.rkt")
(require "../src/game-state.rkt")

(displayln "=== 测试修复后的算法在game-01.txt中 ===")

;; 加载game-01.txt
(define game-state (custom-load-game "data/game-01.txt"))
(define board (game-state-board game-state))

;; 测试关键位置
(displayln "=== 测试(4,4)和(5,4)的连通性 ===")
(define pos-4-4 (position 4 4))
(define pos-5-4 (position 5 4))

(define group-4-4 (rules-get-connected-group board pos-4-4))
(define group-5-4 (rules-get-connected-group board pos-5-4))

(displayln (format "(4,4)连通组大小: ~a" (length group-4-4)))
(displayln (format "(5,4)连通组大小: ~a" (length group-5-4)))

;; 检查它们是否现在连通
(define connected? (member pos-4-4 group-5-4))
(displayln (format "(4,4)和(5,4)是否连通: ~a" connected?))

(when connected?
  (displayln "连通组内容:")
  (for ([pos group-5-4])
    (displayln (format "  (~a,~a)" (position-row pos) (position-col pos)))))

;; 模拟在(3,4)下黑棋
(displayln "\n=== 模拟黑棋下在(3,4) ===")
(define test-pos (position 3 4))
(define temp-board (board-set-stone board test-pos 'black))

(define new-group (rules-get-connected-group temp-board test-pos))
(displayln (format "新连通组大小: ~a" (length new-group)))

(displayln "新连通组内容:")
(for ([pos new-group])
  (displayln (format "  (~a,~a)" (position-row pos) (position-col pos))))

;; 计算气
(define liberties (rules-get-group-liberties temp-board new-group))
(displayln (format "整组气数: ~a" liberties))

;; 检查提子可能性
(define can-capture? (rules-can-capture-opponent? temp-board test-pos 'black))
(displayln (format "能否提子: ~a" can-capture?))

;; 最终合法性判断
(displayln "\n=== 最终合法性判断 ===")
(define legal? (rules-is-valid-move? board test-pos 'black))
(define suicide? (rules-would-be-suicide? board test-pos 'black))

(displayln (format "rules-is-valid-move? 返回: ~a" legal?))
(displayln (format "rules-would-be-suicide? 返回: ~a" suicide?))

(if legal?
    (displayln "🎉 修复成功！黑棋在(3,4)现在被认为是合法移动")
    (displayln "❌ 修复仍有问题"))

;; 显示棋盘状态
(displayln "\n=== 关键区域棋盘状态 ===")
(for ([row (in-range 3 6)])
  (for ([col (in-range 3 6)])
    (define pos (position row col))
    (define stone (board-get-stone board pos))
    (define marker 
      (cond
        [(equal? pos test-pos) "[X]"]
        [stone (format "[~a]" (substring (symbol->string stone) 0 1))]
        [else "[ ]"]))
    (display marker))
  (newline))