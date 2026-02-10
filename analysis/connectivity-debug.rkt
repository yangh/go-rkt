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

(displayln "=== 连通性算法调试 ===")

;; 逐步跟踪连通性搜索过程
(define (debug-connected-group board pos)
  (displayln (format "开始搜索位置(~a,~a)的连通组" 
                    (position-row pos) 
                    (position-col pos)))
  
  (define color (board-get-stone board pos))
  (displayln (format "目标颜色: ~a" color))
  
  (define visited (make-hash))
  (define group '())
  (define queue (list pos))
  
  (displayln "开始BFS搜索:")
  (let loop ([step 0])
    (when (not (null? queue))
      (define current (car queue))
      (define rest-queue (cdr queue))
      
      (displayln (format "步骤~a: 处理位置(~a,~a)" 
                        step 
                        (position-row current) 
                        (position-col current)))
      (displayln (format "  当前队列长度: ~a" (length queue)))
      (displayln (format "  已访问: ~a" (hash-keys visited)))
      (displayln (format "  当前组: ~a" (map (lambda (p) 
                                              (list (position-row p) (position-col p))) 
                                            group)))
      
      (define already-visited? (hash-has-key? visited current))
      (define correct-color? (eq? (board-get-stone board current) color))
      
      (displayln (format "  已访问?: ~a" already-visited?))
      (displayln (format "  颜色匹配?: ~a (位置颜色: ~a)" 
                        correct-color? 
                        (board-get-stone board current)))
      
      (when (and (not already-visited?) correct-color?)
        (displayln "  >> 添加到连通组")
        (hash-set! visited current #t)
        (set! group (cons current group))
        
        ;; 获取邻居
        (define neighbors (board-get-neighbors current))
        (displayln (format "  邻居: ~a" 
                          (map (lambda (n) 
                                (list (position-row n) (position-col n) 
                                     (board-get-stone board n))) 
                               neighbors)))
        
        ;; 过滤有效的邻居
        (define valid-neighbors 
          (filter (lambda (neighbor)
                   (and (eq? (board-get-stone board neighbor) color)
                        (not (hash-has-key? visited neighbor))))
                 neighbors))
        (displayln (format "  有效邻居: ~a" 
                          (map (lambda (n) 
                                (list (position-row n) (position-col n))) 
                               valid-neighbors)))
        
        (set! queue (append rest-queue valid-neighbors))
        (displayln (format "  更新后队列: ~a" 
                          (map (lambda (p) 
                                (list (position-row p) (position-col p))) 
                               queue))))
      
      (set! queue rest-queue)
      (loop (add1 step))))
  
  (define result (reverse group))
  (displayln (format "最终结果: ~a个位置" (length result)))
  result)

;; 调试(4,4)位置的连通性搜索
(displayln "调试(4,4)位置的连通性:")
(define pos-4-4 (position 4 4))
(void (debug-connected-group board pos-4-4))

(newline)
(displayln "调试(5,4)位置的连通性:")
(define pos-5-4 (position 5 4))
(void (debug-connected-group board pos-5-4))

;; 验证一个简单案例
(displayln "\n=== 验证简单连通案例 ===")
(define test-board (make-empty-board))
(define connected-board 
  (board-set-stone 
    (board-set-stone 
      (board-set-stone test-board (position 3 3) 'black)
      (position 3 4) 'black)
    (position 4 3) 'black))

(displayln "测试棋盘:")
(for ([row (in-range 2 6)])
  (for ([col (in-range 2 6)])
    (define pos (position row col))
    (define stone (board-get-stone connected-board pos))
    (define marker 
      (cond
        [stone (format "[~a]" (substring (symbol->string stone) 0 1))]
        [else "[ ]"]))
    (display marker))
  (newline))

(displayln "测试(3,3)的连通组:")
(void (debug-connected-group connected-board (position 3 3)))