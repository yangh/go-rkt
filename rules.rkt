#lang racket

(provide rules-is-valid-move?
         rules-get-liberties
         rules-get-connected-group
         rules-capture-stones
         rules-would-be-suicide?
         rules-find-dead-groups
         rules-get-group-liberties)

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

;; 获取连通的同色棋子组（使用广度优先搜索）
(define (rules-get-connected-group board pos)
  (define color (board-get-stone board pos))
  (define visited (make-hash))
  (define group '())
  (define queue (list pos))
  
  (let loop ()
    (when (not (null? queue))
      (define current (car queue))
      (define rest-queue (cdr queue))
      
      (when (and (not (hash-has-key? visited current))
                 (eq? (board-get-stone board current) color))
        (hash-set! visited current #t)
        (set! group (cons current group))
        (set! queue (append rest-queue 
                           (filter (lambda (neighbor)
                                   (and (eq? (board-get-stone board neighbor) color)
                                        (not (hash-has-key? visited neighbor))))
                                 (board-get-neighbors current)))))
      (set! queue rest-queue)
      (loop)))
  
  (reverse group))

;; 检查放置棋子是否会形成自杀
(define (rules-would-be-suicide? board pos color)
  ;; 临时放置棋子
  (define temp-board (board-set-stone board pos color))
  
  ;; 检查新放置的棋子是否有气
  (define placed-group (rules-get-connected-group temp-board pos))
  (define has-liberty? 
    (ormap (lambda (group-pos)
             (> (length (rules-get-liberties temp-board group-pos)) 0))
           placed-group))
  
  ;; 如果没有气，检查是否能提对方棋子
  (if has-liberty?
      #f  ; 有气，不是自杀
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
  (check-false (rules-is-valid-move? empty-board (position 3 3) 'black))  ; 同一位置重复检查
  
  ;; 测试边界情况
  (check-false (rules-is-valid-move? empty-board (position -1 0) 'black))
  (check-false (rules-is-valid-move? empty-board (position 19 0) 'black))
  
  ;; 测试简单的气计算
  (define simple-board 
    (board-set-stone empty-board (position 3 3) 'black))
  (check-equal? (length (rules-get-liberties simple-board (position 3 3))) 4)
  
  ;; 测试连通组
  (define connected-board
    (board-set-stone 
      (board-set-stone simple-board (position 3 4) 'black)
      (position 4 3) 'white))
  (define black-group (rules-get-connected-group connected-board (position 3 3)))
  (check-equal? (length black-group) 2)
  (check-true (member (position 3 3) black-group))
  (check-true (member (position 3 4) black-group))
  
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
  )