#lang racket

(provide (struct-out position)
         make-empty-board
         board-get-stone
         board-set-stone
         board-remove-stone
         board-is-empty?
         board-is-valid-position?
         board-copy
         board-equal?
         board-count-stones
         board-get-all-positions
         board-get-neighbors
         board-get-stones-by-color
         BOARD-SIZE)

;; 棋盘大小常量
(define BOARD-SIZE 19)

;; 位置结构
(struct position (row col) #:transparent)

;; 棋盘结构 - 使用二维向量存储
(struct board ([grid #:mutable]) #:transparent)

;; 创建空棋盘
(define (make-empty-board)
  (board (make-vector BOARD-SIZE 
                      (make-vector BOARD-SIZE #f))))

;; 获取指定位置的棋子
(define (board-get-stone b pos)
  (cond
    [(not (board-is-valid-position? pos)) #f]
    [else (vector-ref (vector-ref (board-grid b) (position-row pos))
                      (position-col pos))]))

;; 在指定位置放置棋子
(define (board-set-stone b pos color)
  (cond
    [(not (board-is-valid-position? pos)) b]
    [else
     (define new-board (board-copy b))
     (vector-set! (vector-ref (board-grid new-board) (position-row pos))
                  (position-col pos)
                  color)
     new-board]))

;; 移除指定位置的棋子
(define (board-remove-stone b pos)
  (board-set-stone b pos #f))

;; 检查位置是否为空
(define (board-is-empty? b pos)
  (eq? (board-get-stone b pos) #f))

;; 检查位置是否有效（在棋盘范围内）
(define (board-is-valid-position? pos)
  (and (position? pos)
       (>= (position-row pos) 0)
       (< (position-row pos) BOARD-SIZE)
       (>= (position-col pos) 0)
       (< (position-col pos) BOARD-SIZE)))

;; 复制棋盘
(define (board-copy b)
  (define new-grid (make-vector BOARD-SIZE))
  (for ([i (in-range BOARD-SIZE)])
    (vector-set! new-grid i 
                 (vector-copy (vector-ref (board-grid b) i))))
  (board new-grid))

;; 比较两个棋盘是否相等
(define (board-equal? b1 b2)
  (equal? (board-grid b1) (board-grid b2)))

;; 统计棋盘上各种颜色棋子的数量
(define (board-count-stones b)
  (define counts (make-hash '((black . 0) (white . 0) (#f . 0))))
  (for* ([i (in-range BOARD-SIZE)]
         [j (in-range BOARD-SIZE)])
    (define stone (board-get-stone b (position i j)))
    (hash-set! counts stone (add1 (hash-ref counts stone 0))))
  counts)

;; 获取棋盘上所有位置的列表
(define (board-get-all-positions)
  (for*/list ([i (in-range BOARD-SIZE)]
              [j (in-range BOARD-SIZE)])
    (position i j)))

;; 获取相邻位置（上下左右）
(define (board-get-neighbors pos)
  (define row (position-row pos))
  (define col (position-col pos))
  (filter board-is-valid-position?
          (list (position (sub1 row) col)    ; 上
                (position (add1 row) col)    ; 下
                (position row (sub1 col))    ; 左
                (position row (add1 col))))) ; 右

;; 获取指定颜色的所有棋子位置
(define (board-get-stones-by-color b color)
  (filter (lambda (pos) (eq? (board-get-stone b pos) color))
          (board-get-all-positions)))

;; 模块测试
(module+ test
  (require rackunit)
  
  ;; 测试空棋盘创建
  (define empty-board (make-empty-board))
  (check-true (board-is-empty? empty-board (position 0 0)))
  (check-false (board-is-valid-position? (position -1 0)))
  (check-false (board-is-valid-position? (position 19 0)))
  
  ;; 测试放置棋子
  (define board-with-black (board-set-stone empty-board (position 3 3) 'black))
  (check-eq? (board-get-stone board-with-black (position 3 3)) 'black)
  (check-true (board-is-empty? board-with-black (position 0 0)))
  
  ;; 测试复制功能
  (define copied-board (board-copy board-with-black))
  (check-true (board-equal? board-with-black copied-board))
  (check-false (eq? board-with-black copied-board))
  
  ;; 测试统计功能
  (define multi-color-board 
    (board-set-stone 
      (board-set-stone empty-board (position 0 0) 'black)
      (position 1 1) 'white))
  (define counts (board-count-stones multi-color-board))
  (check-eq? (hash-ref counts 'black) 1)
  (check-eq? (hash-ref counts 'white) 1)
  (check-eq? (hash-ref counts #f) (- (* BOARD-SIZE BOARD-SIZE) 2))
  
  ;; 测试邻居位置
  (check-equal? (length (board-get-neighbors (position 0 0))) 2)  ; 角落只有2个邻居
  (check-equal? (length (board-get-neighbors (position 9 9))) 4)  ; 中间有4个邻居
  )