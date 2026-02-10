#lang racket

(require "custom-format.rkt")
(require "board.rkt")
(require "stone.rkt")
(require "rules.rkt")
(require "game-state.rkt")

(displayln "=== 精确验证(3,4)位置 ===")

;; 加载game-01.txt
(define game-state (custom-load-game "game-01.txt"))
(define board (game-state-board game-state))

;; 精确显示(3,4)及其周围3x3区域
(displayln "=== (3,4)周围3x3区域详细状态 ===")
(for ([row (in-range 2 6)])  ; 2,3,4,5 行
  (for ([col (in-range 3 6)])  ; 3,4,5 列
    (define pos (position row col))
    (define stone (board-get-stone board pos))
    (define marker 
      (cond
        [(equal? pos (position 3 4)) "[X]"]  ; 目标位置
        [stone (format "[~a]" (substring (symbol->string stone) 0 1))]
        [else "[ ]"]))
    (display marker))
  (newline))

;; 详细分析(3,4)的邻居
(displayln "\n=== (3,4)邻居详细分析 ===")
(define test-pos (position 3 4))
(define neighbors (board-get-neighbors test-pos))

(displayln "四个方向的邻居:")
(for ([neighbor neighbors])
  (define stone (board-get-stone board neighbor))
  (displayln (format "  (~a,~a): ~a" 
                    (position-row neighbor) 
                    (position-col neighbor) 
                    (if stone stone "empty"))))

;; 统计邻居中不同颜色的棋子数量
(displayln "\n邻居统计:")
(define black-neighbors 0)
(define white-neighbors 0)
(define empty-neighbors 0)

(for ([neighbor neighbors])
  (define stone (board-get-stone board neighbor))
  (cond
    [(eq? stone 'black) (set! black-neighbors (add1 black-neighbors))]
    [(eq? stone 'white) (set! white-neighbors (add1 white-neighbors))]
    [else (set! empty-neighbors (add1 empty-neighbors))]))

(displayln (format "  黑棋邻居: ~a个" black-neighbors))
(displayln (format "  白棋邻居: ~a个" white-neighbors))
(displayln (format "  空位邻居: ~a个" empty-neighbors))

;; 检查(4,4)和(5,4)是否真的存在且连通
(displayln "\n=== 关键位置验证 ===")
(define pos-4-4 (position 4 4))
(define pos-5-4 (position 5 4))

(displayln (format "(4,4)状态: ~a" (board-get-stone board pos-4-4)))
(displayln (format "(5,4)状态: ~a" (board-get-stone board pos-5-4)))

(when (and (eq? (board-get-stone board pos-4-4) 'black)
           (eq? (board-get-stone board pos-5-4) 'black))
  (displayln "检查连通性:")
  (define group-4-4 (rules-get-connected-group board pos-4-4))
  (define group-5-4 (rules-get-connected-group board pos-5-4))
  
  (displayln (format "  (4,4)连通组大小: ~a" (length group-4-4)))
  (displayln (format "  (5,4)连通组大小: ~a" (length group-5-4)))
  
  ;; 检查它们是否属于同一组
  (define same-group? (member pos-4-4 group-5-4))
  (displayln (format "  是否属于同一连通组: ~a" same-group?)))

;; 模拟黑棋下在(3,4)后的完整分析
(displayln "\n=== 模拟黑棋下在(3,4)的完整分析 ===")
(define temp-board (board-set-stone board test-pos 'black))

;; 新形成的连通组
(displayln "新连通组分析:")
(define new-group (rules-get-connected-group temp-board test-pos))
(displayln (format "  连通组大小: ~a" (length new-group)))
(displayln "  组内所有位置:")
(for ([pos new-group])
  (displayln (format "    (~a,~a)" (position-row pos) (position-col pos))))

;; 计算整组的气
(displayln "\n气的计算:")
(define total-liberties (rules-get-group-liberties temp-board new-group))
(displayln (format "  整组气数: ~a" total-liberties))

;; 显示每个位置的个体气数
(displayln "  各位置气数:")
(for ([pos new-group])
  (define pos-liberties (length (rules-get-liberties temp-board pos)))
  (displayln (format "    (~a,~a): ~a气" 
                    (position-row pos) 
                    (position-col pos) 
                    pos-liberties)))

;; 检查是否能提子
(displayln "\n提子检查:")
(define can-capture? (rules-can-capture-opponent? temp-board test-pos 'black))
(displayln (format "  能否提掉对方棋子: ~a" can-capture?))

;; 最终合法性判断
(displayln "\n=== 最终判决 ===")
(define legal? (rules-is-valid-move? board test-pos 'black))
(define suicide? (rules-would-be-suicide? board test-pos 'black))

(displayln (format "  rules-is-valid-move? 返回: ~a" legal?))
(displayln (format "  rules-would-be-suicide? 返回: ~a" suicide?))

(if legal?
    (displayln "  🎉 判定为合法！")
    (displayln "  ❌ 判定为非法"))

;; 如果非法，显示详细的自杀原因
(when (and (not legal?) suicide?)
  (displayln "\n=== 自杀详细分析 ===")
  (displayln "  原因: 新形成的连通组没有气，且无法提子")
  (displayln "  这与我们的观察不符，请检查算法实现"))