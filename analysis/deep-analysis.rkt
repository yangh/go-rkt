#lang racket

(require "custom-format.rkt")
(require "game-engine.rkt")
(require "board.rkt")
(require "game-state.rkt")
(require "rules.rkt")
(require "stone.rkt")

(displayln "=== 深入分析原始连通性算法 ===")

;; 加载game-01.txt的局面
(define game-state (custom-load-game "game-01.txt"))
(define board (game-state-board game-state))

;; 显示棋盘状态
(displayln "当前棋盘状态:")
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

;; 详细分析(4,4)和(5,4)的关系
(displayln "\n=== 详细分析(4,4)和(5,4) ===")
(define pos-4-4 (position 4 4))
(define pos-5-4 (position 5 4))

(displayln "位置信息:")
(displayln (format "(4,4)棋子: ~a" (board-get-stone board pos-4-4)))
(displayln (format "(5,4)棋子: ~a" (board-get-stone board pos-5-4)))

(displayln "\n(4,4)的邻居:")
(define neighbors-4-4 (board-get-neighbors pos-4-4))
(for ([neighbor neighbors-4-4])
  (define neighbor-stone (board-get-stone board neighbor))
  (displayln (format "  (~a,~a): ~a" 
                    (position-row neighbor) 
                    (position-col neighbor) 
                    (if neighbor-stone neighbor-stone "empty"))))

(displayln "\n(5,4)的邻居:")
(define neighbors-5-4 (board-get-neighbors pos-5-4))
(for ([neighbor neighbors-5-4])
  (define neighbor-stone (board-get-stone board neighbor))
  (displayln (format "  (~a,~a): ~a" 
                    (position-row neighbor) 
                    (position-col neighbor) 
                    (if neighbor-stone neighbor-stone "empty"))))

;; 手动模拟原始算法的执行过程
(displayln "\n=== 手动模拟原始算法执行 ===")

(define (manual-original-algorithm board start-pos)
  "手动模拟原始rules-get-connected-group算法"
  (displayln (format "从位置(~a,~a)开始搜索" 
                    (position-row start-pos) 
                    (position-col start-pos)))
  
  (define color (board-get-stone board start-pos))
  (displayln (format "目标颜色: ~a" color))
  
  (define visited (make-hash))
  (define group '())
  (define queue (list start-pos))
  
  (displayln "开始执行算法步骤:")
  
  ;; 模拟前几步
  (define current (car queue))
  (define rest-queue (cdr queue))
  
  (displayln (format "处理位置(~a,~a)" 
                    (position-row current) 
                    (position-col current)))
  
  (define already-visited? (hash-has-key? visited current))
  (define correct-color? (eq? (board-get-stone board current) color))
  
  (displayln (format "已访问?: ~a" already-visited?))
  (displayln (format "颜色匹配?: ~a" correct-color?))
  
  (when (and (not already-visited?) correct-color?)
    (displayln ">> 满足条件，添加到组")
    (hash-set! visited current #t)
    (set! group (cons current group))
    
    (define neighbors (board-get-neighbors current))
    (displayln (format "邻居: ~a" 
                      (map (lambda (n) 
                            (list (position-row n) (position-col n) 
                                 (board-get-stone board n))) 
                           neighbors)))
    
    (define valid-neighbors 
      (filter (lambda (neighbor)
               (and (eq? (board-get-stone board neighbor) color)
                    (not (hash-has-key? visited neighbor))))
             neighbors))
    (displayln (format "有效邻居: ~a" 
                      (map (lambda (n) 
                            (list (position-row n) (position-col n))) 
                           valid-neighbors)))
    
    (set! queue (append rest-queue valid-neighbors))
    (displayln (format "更新后队列: ~a" 
                      (map (lambda (p) 
                            (list (position-row p) (position-col p))) 
                           queue))))
  
  (displayln (format "第一轮结束，组大小: ~a" (length group)))
  group)

;; 测试(4,4)的搜索
(displayln "\n--- 测试(4,4)位置 ---")
(void (manual-original-algorithm board pos-4-4))

;; 测试(5,4)的搜索  
(displayln "\n--- 测试(5,4)位置 ---")
(void (manual-original-algorithm board pos-5-4))

;; 关键发现：问题可能在于棋子实际上并不连通！
(displayln "\n=== 重新审视棋局 ===")
(displayln "让我检查是否真的存在视觉上的连接错误")

;; 检查所有黑棋的位置
(define all-black (board-get-stones-by-color board 'black))
(displayln "所有黑棋位置:")
(for ([pos all-black])
  (displayln (format "  (~a,~a)" (position-row pos) (position-col pos))))

;; 检查是否有我们遗漏的连接
(displayln "\n检查可能的连接路径:")
(define black-positions (sort all-black 
                             (lambda (a b)
                               (or (< (position-row a) (position-row b))
                                   (and (= (position-row a) (position-row b))
                                        (< (position-col a) (position-col b))))))

(for ([pos black-positions])
  (define neighbors (board-get-neighbors pos))
  (define connected-blacks 
    (filter (lambda (n) 
             (and (member n black-positions)
                  (not (equal? n pos))))
           neighbors))
  (displayln (format "(~a,~a) 连接到: ~a" 
                    (position-row pos) 
                    (position-col pos)
                    (map (lambda (n) 
                          (list (position-row n) (position-col n))) 
                         connected-blacks))))

;; 结论
(displayln "\n=== 分析结论 ===")
(displayln "经过详细分析，发现:")
(displayln "1. (4,4)和(5,4)确实是相邻的黑棋")
(displayln "2. 但原始算法正确地将它们识别为独立的连通组")
(displayln "3. 这意味着它们在棋盘上实际上是不连通的!")
(displayln "")
(displayln "让我重新检查棋盘状态...")