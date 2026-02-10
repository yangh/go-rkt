#lang racket

(require "../src/board.rkt")
(require "../src/stone.rkt")
(require "../src/rules.rkt")

(displayln "=== 最小测试 ===")

;; 最基本的测试
(define board (make-empty-board))
(define test-pos (position 3 3))

(define valid? (rules-is-valid-move? board test-pos 'black))
(define liberties (length (rules-get-liberties board test-pos)))

(displayln (format "在(3,3)下黑棋是否合法: ~a" valid?))
(displayln (format "(3,3)位置的气数: ~a" liberties))

(if (and valid? (= liberties 4))
    (displayln "✓ 最小测试通过")
    (displayln "✗ 最小测试失败"))