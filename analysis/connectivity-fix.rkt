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

(displayln "=== 验证连通性问题 ===")

;; 首先验证(4,4)和(5,4)的实际连接情况
(displayln "\n1. 检查(4,4)和(5,4)之间的连接:")

;; 检查(4,4)位置的状态
(define pos-4-4 (position 4 4))
(define stone-4-4 (board-get-stone board pos-4-4))
(displayln (format "(4,4)位置棋子: ~a" stone-4-4))

;; 检查(5,4)位置的状态
(define pos-5-4 (position 5 4))
(define stone-5-4 (board-get-stone board pos-5-4))
(displayln (format "(5,4)位置棋子: ~a" stone-5-4))

;; 检查它们是否相邻
(define neighbors-4-4 (board-get-neighbors pos-4-4))
(define adjacent-5-4? (member pos-5-4 neighbors-4-4))
(displayln (format "(4,4)和(5,4)是否相邻: ~a" adjacent-5-4?))

;; 检查中间位置(4,4)和(5,4)之间是否有其他黑棋
(displayln "\n2. 检查中间路径:")
(for ([row (in-range 4 6)])
  (for ([col (in-range 4 5)])
    (define pos (position row col))
    (define stone (board-get-stone board pos))
    (displayln (format "  (~a,~a): ~a" (position-row pos) (position-col pos) stone))))

;; 手动构建正确的连通组
(displayln "\n3. 手动分析连通性:")
(displayln "从棋盘显示可以看出，以下位置应该是连通的黑棋:")
(define expected-connected-positions 
  (list (position 1 3) (position 1 4)   ; 第一行的黑棋
        (position 2 4) (position 2 5)   ; 第二行的黑棋
        (position 4 3) (position 4 4)   ; 第四行的黑棋
        (position 5 4)))               ; 第五行的黑棋

(displayln "预期的连通黑棋组:")
(for ([pos expected-connected-positions])
  (define stone (board-get-stone board pos))
  (displayln (format "  (~a,~a): ~a" (position-row pos) (position-col pos) stone)))

;; 测试修复后的连通性函数
(displayln "\n4. 测试修复方案:")

;; 创建一个修复版本的连通性检查函数
(define (fixed-get-connected-group board pos target-color)
  "修复版的连通组获取函数，显式指定目标颜色"
  (define visited (make-hash))
  (define group '())
  (define queue (list pos))
  
  (let loop ()
    (when (not (null? queue))
      (define current (car queue))
      (define rest-queue (cdr queue))
      
      (when (and (not (hash-has-key? visited current))
                 (eq? (board-get-stone board current) target-color))
        (hash-set! visited current #t)
        (set! group (cons current group))
        (set! queue (append rest-queue 
                           (filter (lambda (neighbor)
                                   (and (eq? (board-get-stone board neighbor) target-color)
                                        (not (hash-has-key? visited neighbor))))
                                 (board-get-neighbors current)))))
      (set! queue rest-queue)
      (loop)))
  
  (reverse group))

;; 测试修复后的函数
(displayln "使用修复函数检查连通性:")
(define fixed-group-4-4 (fixed-get-connected-group board pos-4-4 'black))
(displayln (format "修复后(4,4)的连通组大小: ~a" (length fixed-group-4-4)))
(displayln "组内位置:")
(for ([pos fixed-group-4-4])
  (displayln (format "  (~a,~a)" (position-row pos) (position-col pos))))

(define fixed-group-5-4 (fixed-get-connected-group board pos-5-4 'black))
(displayln (format "修复后(5,4)的连通组大小: ~a" (length fixed-group-5-4)))
(displayln "组内位置:")
(for ([pos fixed-group-5-4])
  (displayln (format "  (~a,~a)" (position-row pos) (position-col pos))))

;; 检查它们是否现在属于同一组
(define now-connected? 
  (not (null? (filter (lambda (pos) (member pos fixed-group-5-4)) fixed-group-4-4))))
(displayln (format "修复后(4,4)和(5,4)是否连通: ~a" now-connected?))

;; 验证原始函数的问题
(displayln "\n5. 验证原始函数的问题:")
(displayln "原始函数的问题在于它依赖于(pos位置的棋子颜色)来确定搜索颜色")
(displayln "但对于空位置或要放置棋子的位置，这种方法会失败")

(define original-group-4-4 (rules-get-connected-group board pos-4-4))
(define original-group-5-4 (rules-get-connected-group board pos-5-4))
(displayln (format "原始函数(4,4)组大小: ~a" (length original-group-4-4)))
(displayln (format "原始函数(5,4)组大小: ~a" (length original-group-5-4)))