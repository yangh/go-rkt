#lang racket

(require "custom-format.rkt")
(require "board.rkt")
(require "stone.rkt")
(require "rules.rkt")
(require "game-state.rkt")

(displayln "=== 重新分析连通性算法 ===")

;; 加载game-01.txt
(define game-state (custom-load-game "game-01.txt"))
(define board (game-state-board game-state))

;; 详细跟踪(4,4)和(5,4)的连通性搜索过程
(displayln "=== 详细跟踪连通性搜索 ===")

(define (trace-connected-group board start-pos)
  "跟踪连通组搜索过程"
  (displayln (format "从位置(~a,~a)开始搜索" 
                    (position-row start-pos) 
                    (position-col start-pos)))
  
  (define color (board-get-stone board start-pos))
  (displayln (format "目标颜色: ~a" color))
  
  (define visited (make-hash))
  (define group '())
  (define queue (list start-pos))
  
  (let loop ([step 0])
    (when (not (null? queue))
      (define current (car queue))
      (define rest-queue (cdr queue))
      
      (displayln (format "\n步骤~a: 处理位置(~a,~a)" 
                        step 
                        (position-row current) 
                        (position-col current)))
      (displayln (format "  当前队列: ~a" 
                        (map (lambda (p) 
                              (list (position-row p) (position-col p))) 
                             queue)))
      (displayln (format "  已访问: ~a" 
                        (map (lambda (p) 
                              (list (position-row p) (position-col p))) 
                             (hash-keys visited))))
      
      (define already-visited? (hash-has-key? visited current))
      (define correct-color? (eq? (board-get-stone board current) color))
      
      (displayln (format "  已访问?: ~a" already-visited?))
      (displayln (format "  颜色匹配?: ~a" correct-color?))
      
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
  (displayln (format "\n最终结果: ~a个位置" (length result)))
  (displayln "组内位置:")
  (for ([pos result])
    (displayln (format "  (~a,~a)" (position-row pos) (position-col pos))))
  result)

;; 跟踪(4,4)的搜索过程
(displayln "=== 跟踪(4,4)位置的搜索 ===")
(void (trace-connected-group board (position 4 4)))

;; 跟踪(5,4)的搜索过程  
(displayln "\n=== 跟踪(5,4)位置的搜索 ===")
(void (trace-connected-group board (position 5 4)))

;; 关键验证：检查(4,4)和(5,4)是否真的是相邻的
(displayln "\n=== 验证相邻关系 ===")
(define pos-4-4 (position 4 4))
(define pos-5-4 (position 5 4))

(define neighbors-4-4 (board-get-neighbors pos-4-4))
(define adjacent-5-4? (member pos-5-4 neighbors-4-4))

(displayln (format "(4,4)的邻居: ~a" 
                  (map (lambda (n) 
                        (list (position-row n) (position-col n))) 
                       neighbors-4-4)))
(displayln (format "(5,4)是否是(4,4)的邻居: ~a" adjacent-5-4?))

;; 如果相邻，为什么算法没把它们连在一起？
(displayln "\n=== 深入分析问题根源 ===")
(when adjacent-5-4?
  (displayln "既然(4,4)和(5,4)相邻且都是黑棋，为什么算法没把它们连在一起？")
  (displayln "让我检查搜索过程中发生了什么...")
  
  ;; 重新执行一次带详细调试的搜索
  (define test-board board)
  (define start-pos pos-4-4)
  (define target-color 'black)
  
  (displayln "\n重新执行(4,4)的搜索，重点关注(5,4):")
  (define visited-test (make-hash))
  (define queue-test (list start-pos))
  
  (let trace-loop ([step 0])
    (when (not (null? queue-test))
      (define current-test (car queue-test))
      (define rest-queue-test (cdr queue-test))
      
      (displayln (format "步骤~a: 处理(~a,~a)" 
                        step 
                        (position-row current-test) 
                        (position-col current-test)))
      
      (when (not (hash-has-key? visited-test current-test))
        (hash-set! visited-test current-test #t)
        (when (eq? (board-get-stone test-board current-test) target-color)
          (displayln (format "  (~a,~a)颜色匹配" 
                            (position-row current-test) 
                            (position-col current-test)))
          
          ;; 特别检查(5,4)
          (when (equal? current-test pos-4-4)
            (displayln "  检查(4,4)的邻居中是否包含(5,4):")
            (define neighbors-test (board-get-neighbors current-test))
            (for ([neighbor neighbors-test])
              (define neighbor-color (board-get-stone test-board neighbor))
              (displayln (format "    (~a,~a): ~a" 
                                (position-row neighbor) 
                                (position-col neighbor) 
                                neighbor-color))
              (when (equal? neighbor pos-5-4)
                (displayln "    >>> 这就是(5,4)!")))
            
            ;; 检查为什么(5,4)没有被加入队列
            (define valid-for-queue? 
              (and (eq? (board-get-stone test-board pos-5-4) target-color)
                   (not (hash-has-key? visited-test pos-5-4))))
            (displayln (format "    (5,4)是否应该加入队列: ~a" valid-for-queue?))
            (displayln (format "    (5,4)的颜色: ~a" (board-get-stone test-board pos-5-4)))
            (displayln (format "    (5,4)是否已访问: ~a" (hash-has-key? visited-test pos-5-4)))))
      
      (set! queue-test rest-queue-test)
      (trace-loop (add1 step)))))