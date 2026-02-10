#lang racket

;; 正确的连通性检查实现
(define (correct-get-connected-group board pos)
  "正确的连通组获取函数 - 能够正确识别相邻同色棋子"
  (define color (board-get-stone board pos))
  ;; 如果位置为空，返回空组
  (if (not color)
      '()
      (let ([visited (make-hash)]
            [group '()]
            [queue (list pos)])
        (let loop ()
          (when (not (null? queue))
            (define current (car queue))
            (define rest-queue (cdr queue))
            
            ;; 关键：只处理未访问且颜色匹配的位置
            (when (and (not (hash-has-key? visited current))
                       (eq? (board-get-stone board current) color))
              (hash-set! visited current #t)
              (set! group (cons current group))
              
              ;; 获取所有邻居并筛选同色未访问的
              (define neighbors (board-get-neighbors current))
              (define valid-neighbors 
                (filter (lambda (neighbor)
                         (and (eq? (board-get-stone board neighbor) color)
                              (not (hash-has-key? visited neighbor))))
                       neighbors))
              
              (set! queue (append rest-queue valid-neighbors))))
          (set! queue rest-queue)
          (loop)))
        (reverse group))))

;; 测试函数
(define (test-connectivity)
  (displayln "=== 测试连通性算法 ===")
  
  ;; 创建测试棋盘
  (define test-board (make-empty-board))
  (define board1 (board-set-stone test-board (position 3 3) 'black))
  (define board2 (board-set-stone board1 (position 3 4) 'black))  ; 相邻黑棋
  
  (displayln "测试棋盘设置完成")
  
  ;; 测试连通性
  (define group1 (correct-get-connected-group board2 (position 3 3)))
  (define group2 (correct-get-connected-group board2 (position 3 4)))
  
  (displayln (format "group1大小: ~a" (length group1)))
  (displayln (format "group2大小: ~a" (length group2)))
  
  (define connected? (member (position 3 3) group2))
  (displayln (format "两个相邻黑棋是否连通: ~a" connected?))
  
  (if connected?
      (displayln "✓ 连通性算法工作正常")
      (displayln "✗ 连通性算法仍有问题")))

;; 运行测试
(test-connectivity)