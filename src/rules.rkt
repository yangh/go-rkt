#lang racket

(provide rules-is-valid-move?
         rules-get-liberties
         rules-get-connected-group
         rules-capture-stones
         rules-would-be-suicide?
         rules-find-dead-groups
         rules-get-group-liberties
         rules-can-capture-opponent?)

(require "board.rkt")
(require "stone.rkt")

;; 检查移动是否合法（包括禁入点和自杀）
(define (rules-is-valid-move? board pos color)
  (cond
    ;; 边界检查
    [(not (board-is-valid-position? pos)) #f]
    ;; 位置非空
    [(not (board-is-empty? board pos)) #f]
    ;; 自杀检查
    [(rules-would-be-suicide? board pos color) #f]
    [else #t]))

;; 获取指定位置的气（自由邻点）
(define (rules-get-liberties board pos)
  (filter (lambda (neighbor-pos)
            (board-is-empty? board neighbor-pos))
          (board-get-neighbors pos)))

;; 获取连通的同色棋子组（使用广度优先搜索）- 优化版
(define (rules-get-connected-group board pos)
  (define color (board-get-stone board pos))
  ;; 如果位置为空，返回空组
  (if (not color)
      '()
      (let ([visited (make-hash)]
            [group '()]
            [queue (list pos)])
        (let loop ([current-queue queue])
          (when (not (null? current-queue))
            (define current (car current-queue))
            (define rest-queue (cdr current-queue))
            
            ;; 只处理未访问且颜色匹配的位置
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
              
              (loop (append rest-queue valid-neighbors)))))
        (reverse group))))

;; 检查放置棋子是否会形成自杀
(define (rules-would-be-suicide? board pos color)
  ;; 临时放置棋子
  (define temp-board (board-set-stone board pos color))
  
  ;; 获取新放置棋子所在的完整连通组
  (define placed-group (rules-get-connected-group temp-board pos))
  
  ;; 检查整个连通组是否有气
  (define group-liberties (rules-get-group-liberties temp-board placed-group))
  (define has-liberty? (> group-liberties 0))
  
  ;; 如果有气，不是自杀
  (if has-liberty?
      #f
      ;; 如果没有气，检查是否能提对方棋子
      (let ([can-capture? (rules-can-capture-opponent? temp-board pos color)])
        (not can-capture?))))  ; 没气且不能提子 = 自杀

;; 检查是否能提对方棋子
(define (rules-can-capture-opponent? board pos color)
  (define opponent-color (opposite-color color))
  (define neighbors (board-get-neighbors pos))
  
  ;; 检查是否有对方无气的棋组
  (ormap (lambda (neighbor-pos)
           (and (eq? (board-get-stone board neighbor-pos) opponent-color)
                (let ([opponent-group (rules-get-connected-group board neighbor-pos)])
                  (= (rules-get-group-liberties board opponent-group) 0))))
         neighbors))

;; 获取整个棋组的气数
(define (rules-get-group-liberties board group)
  (define liberties (make-hash))
  (for ([pos group])
    (for ([liberty-pos (rules-get-liberties board pos)])
      (hash-set! liberties liberty-pos #t)))
  (hash-count liberties))

;; 查找死棋组（无气的棋组）
(define (rules-find-dead-groups board color)
  (define dead-groups '())
  (define visited-positions (make-hash))
  
  ;; 遍历所有指定颜色的棋子
  (for ([pos (board-get-stones-by-color board color)])
    (when (not (hash-has-key? visited-positions pos))
      (define group (rules-get-connected-group board pos))
      ;; 标记所有组内位置为已访问
      (for ([group-pos group])
        (hash-set! visited-positions group-pos #t))
      ;; 检查该组是否死亡
      (when (= (rules-get-group-liberties board group) 0)
        (set! dead-groups (cons group dead-groups)))))
  
  dead-groups)

;; 执行提子操作
(define (rules-capture-stones board positions)
  (foldl (lambda (pos current-board)
           (board-remove-stone current-board pos))
         board
         positions))

;; 模块测试
(module+ test
  (require rackunit)
  
  ;; 测试空棋盘上的合法移动
  (define empty-board (make-empty-board))
  (check-true (rules-is-valid-move? empty-board (position 3 3) 'black))
  
  ;; 测试边界情况
  (check-false (rules-is-valid-move? empty-board (position -1 0) 'black))
  (check-false (rules-is-valid-move? empty-board (position 19 0) 'black))
  
  ;; 测试简单的气计算
  (define simple-board 
    (board-set-stone empty-board (position 3 3) 'black))
  (check-equal? (length (rules-get-liberties simple-board (position 3 3))) 4)
  
  ;; 测试基础连通性
  (define connected-board 
    (board-set-stone 
      (board-set-stone empty-board (position 3 3) 'black)
      (position 3 4) 'black))
  
  (define group-3-3 (rules-get-connected-group connected-board (position 3 3)))
  (define group-3-4 (rules-get-connected-group connected-board (position 3 4)))
  
  (check-equal? (length group-3-3) 2)
  (check-equal? (length group-3-4) 2)
  (check-true (member (position 3 3) group-3-4))
  (check-true (member (position 3 4) group-3-3))
  
  ;; 测试复杂连通性（L形连接）
  (define l-shaped-board
    (board-set-stone 
      (board-set-stone 
        (board-set-stone connected-board (position 4 3) 'black)
        (position 4 4) 'white)
      (position 2 3) 'white))
  
  (define l-group (rules-get-connected-group l-shaped-board (position 3 3)))
  (check-equal? (length l-group) 3)  ; 应该包含(3,3)(3,4)(4,3)
  
  ;; 测试自杀情况
  (define suicide-test-board
    (board-set-stone 
      (board-set-stone 
        (board-set-stone 
          (board-set-stone empty-board (position 3 2) 'white)  ; 包围上方
          (position 2 3) 'white)   ; 包围左方
        (position 4 3) 'white)     ; 包围右方
      (position 3 4) 'white))      ; 包围下方
  
  (check-true (rules-would-be-suicide? suicide-test-board (position 3 3) 'black))
  (check-false (rules-is-valid-move? suicide-test-board (position 3 3) 'black))
  
  ;; 测试提子情况（不是自杀）
  (define capture-test-board
    (board-set-stone 
      (board-set-stone 
        (board-set-stone 
          (board-set-stone 
            (board-set-stone suicide-test-board (position 3 1) 'black)  ; 黑子在上方
            (position 1 3) 'black)   ; 黑子在左方
          (position 5 3) 'black)     ; 黑子在右方
        (position 3 5) 'black)       ; 黑子在下方
      (position 3 3) 'black))        ; 中间的黑子应该能提掉白子
  
  (check-false (rules-would-be-suicide? capture-test-board (position 3 3) 'black))
  (check-true (rules-is-valid-move? capture-test-board (position 3 3) 'black))
  
  ;; 测试气的计算
  (define multi-group-board
    (board-set-stone 
      (board-set-stone 
        (board-set-stone 
          (board-set-stone empty-board (position 3 3) 'black)
          (position 3 4) 'black)     ; 连接的黑子
        (position 5 5) 'white)       ; 孤立的白子
      (position 5 6) 'white))        ; 连接的白子
  
  (define black-group (rules-get-connected-group multi-group-board (position 3 3)))
  (define white-group (rules-get-connected-group multi-group-board (position 5 5)))
  
  (check-equal? (length black-group) 2)
  (check-equal? (length white-group) 2)
  
  (check-equal? (rules-get-group-liberties multi-group-board black-group) 6)
  (check-equal? (rules-get-group-liberties multi-group-board white-group) 6)
  
  ;; 测试死棋组查找
  (define dead-group-board
    (board-set-stone 
      (board-set-stone 
        (board-set-stone 
          (board-set-stone 
            (board-set-stone empty-board (position 3 3) 'black)  ; 被包围的黑子
            (position 2 3) 'white)   ; 包围上方
          (position 4 3) 'white)     ; 包围下方
        (position 3 2) 'white)       ; 包围左方
      (position 3 4) 'white))        ; 包围右方
  
  (define dead-groups (rules-find-dead-groups dead-group-board 'black))
  (check-equal? (length dead-groups) 1)
  (check-equal? (length (car dead-groups)) 1)  ; 死棋组大小为1
  )