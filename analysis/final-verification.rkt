#lang racket

(require "custom-format.rkt")
(require "board.rkt")
(require "stone.rkt")
(require "rules.rkt")
(require "game-state.rkt")

(displayln "=== 验证game-01.txt中(3,4)位置的修复 ===")

;; 加载实际的game-01.txt局面
(define game-state (custom-load-game "game-01.txt"))
(define board (game-state-board game-state))

(displayln "加载game-01.txt成功")

;; 检查(3,4)位置的状态
(define test-pos (position 3 4))
(displayln (format "检查位置(~a,~a)" (position-row test-pos) (position-col test-pos)))

;; 基本验证
(displayln "\n=== 基本检查 ===")
(displayln (format "位置有效: ~a" (board-is-valid-position? test-pos)))
(displayln (format "位置为空: ~a" (board-is-empty? board test-pos)))

;; 关键测试：模拟黑棋下在此处
(displayln "\n=== 关键测试：模拟黑棋下在(3,4) ===")
(define temp-board (board-set-stone board test-pos 'black))

;; 分析新形成的连通组
(displayln "新连通组分析:")
(define new-group (rules-get-connected-group temp-board test-pos))
(displayln (format "  连通组大小: ~a" (length new-group)))
(displayln "  组内位置:")
(for ([pos new-group])
  (displayln (format "    (~a,~a)" (position-row pos) (position-col pos))))

;; 检查气的情况
(displayln "\n气的情况:")
(define group-liberties (rules-get-group-liberties temp-board new-group))
(displayln (format "  整组气数: ~a" group-liberties))

;; 检查是否能提子
(displayln "\n提子可能性:")
(define can-capture? (rules-can-capture-opponent? temp-board test-pos 'black))
(displayln (format "  能否提掉对方棋子: ~a" can-capture?))

;; 最终合法性判断
(displayln "\n=== 最终判决 ===")
(define legal? (rules-is-valid-move? board test-pos 'black))
(define suicide? (rules-would-be-suicide? board test-pos 'black))

(displayln (format "rules-is-valid-move? 返回: ~a" legal?))
(displayln (format "rules-would-be-suicide? 返回: ~a" suicide?))

(if legal?
    (displayln "🎉 修复成功！黑棋在(3,4)现在被正确判定为合法移动")
    (displayln "❌ 修复失败，仍判定为非法"))

;; 显示关键区域供参考
(displayln "\n=== 关键区域棋盘状态 ===")
(for ([row (in-range 2 7)])
  (for ([col (in-range 2 7)])
    (define pos (position row col))
    (define stone (board-get-stone board pos))
    (define marker 
      (cond
        [(equal? pos test-pos) "[X]"]
        [stone (format "[~a]" (substring (symbol->string stone) 0 1))]
        [else "[ ]"]))
    (display marker))
  (newline))

(displayln "\n图例: [b]=黑棋, [w]=白棋, [X]=测试位置(3,4)")