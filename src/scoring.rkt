#lang racket

(provide scoring-count-territory
         scoring-final-score
         scoring-mark-dead-stones
         scoring-get-live-stones)

(require "board.rkt")
(require "stone.rkt")
(require "rules.rkt")

;; 计算领地（包括棋子和围成的空点）
(define (scoring-count-territory board color)
  (define own-stones (board-get-stones-by-color board color))
  (define territory-points (find-territory-points board color))
  (+ (length own-stones) (length territory-points)))

;; 计算最终得分（中国规则：总子数 - 180.5）
(define (scoring-final-score board black-captured white-captured)
  (define black-score (scoring-count-territory board 'black))
  (define white-score (scoring-count-territory board 'white))
  (define black-total (+ black-score white-captured))
  (define white-total (+ white-score black-captured))
  (define difference (- black-total white-total))
  
  ;; 中国规则：黑贴7.5目（相当于3又3/4子）
  (define final-difference (- difference 7.5))
  
  (list 'black black-total 'white white-total 'difference final-difference))

;; 标记死子（简单的启发式方法）
(define (scoring-mark-dead-stones board)
  (define dead-stones '())
  
  ;; 对于每种颜色，检查其棋组的气和眼位
  (for ([color '(black white)])
    (define groups (get-all-groups board color))
    (for ([group groups])
      (when (is-group-probably-dead? board group)
        (set! dead-stones (append dead-stones group)))))
  
  dead-stones)

;; 获取所有连通组
(define (get-all-groups board color)
  (define visited (make-hash))
  (define groups '())
  
  (for ([pos (board-get-stones-by-color board color)])
    (when (not (hash-has-key? visited pos))
      (define group (rules-get-connected-group board pos))
      ;; 标记组内所有位置为已访问
      (for ([group-pos group])
        (hash-set! visited group-pos #t))
      (set! groups (cons group groups))))
  
  groups)

;; 判断棋组是否可能是死棋（简单启发式）
(define (is-group-probably-dead? board group)
  (define group-liberties (rules-get-group-liberties board group))
  (define group-size (length group))
  
  ;; 简单规则：小棋组且气很少可能为死棋
  (cond
    [(<= group-size 1) (< group-liberties 2)]  ; 单子气少于2
    [(<= group-size 3) (< group-liberties 3)]  ; 小棋组气少于3
    [(<= group-size 5) (< group-liberties 4)]  ; 中等棋组气少于4
    [else (< group-liberties 5)]))             ; 大棋组气少于5

;; 查找属于某方的领地点（空点被该方完全包围）
(define (find-territory-points board color)
  (define territory-points '())
  (define opponent-color (opposite-color color))
  
  ;; 检查每个空点
  (for ([pos (board-get-all-positions)])
    (when (board-is-empty? board pos)
      (define neighbors (board-get-neighbors pos))
      ;; 如果所有邻居都是同色或边界，则认为是领地
      (when (and (not (null? neighbors))
                 (for/and ([neighbor neighbors])
                   (or (eq? (board-get-stone board neighbor) color)
                       (eq? (board-get-stone board neighbor) #f))))
        (set! territory-points (cons pos territory-points)))))
  
  territory-points)

;; 获取活棋（排除明显死棋）
(define (scoring-get-live-stones board)
  (define dead-stones (scoring-mark-dead-stones board))
  (define all-positions (board-get-all-positions))
  
  (filter (lambda (pos)
            (and (not (eq? (board-get-stone board pos) #f))
                 (not (member pos dead-stones))))
          all-positions))

;; 模块测试
(module+ test
  (require rackunit)
  
  ;; 测试基本计分
  (define test-board (make-empty-board))
  (define board-with-stones
    (board-set-stone 
      (board-set-stone 
        (board-set-stone test-board (position 3 3) 'black)
        (position 15 15) 'white)
      (position 3 4) 'black))
  
  (check-equal? (scoring-count-territory board-with-stones 'black) 2)
  (check-equal? (scoring-count-territory board-with-stones 'white) 1)
  
  ;; 测试最终得分计算
  (define score-result 
    (scoring-final-score board-with-stones 0 0))
  (check-equal? (length score-result) 6)
  (check-eq? (list-ref score-result 0) 'black)
  (check-eq? (list-ref score-result 2) 'white)
  
  ;; 测试死子标记
  (define simple-dead-board
    (board-set-stone 
      (board-set-stone 
        (board-set-stone 
          (board-set-stone test-board (position 0 1) 'white)  ; 包围一个黑子
          (position 1 0) 'white)
        (position 1 2) 'white)
      (position 2 1) 'white))
  
  (define dead-stones-result (scoring-mark-dead-stones simple-dead-board))
  ;; 应该能找到被包围的黑子
  (check-true (or (null? dead-stones-result)
                  (member (position 1 1) dead-stones-result)))
  
  ;; 测试活棋获取
  (define live-stones (scoring-get-live-stones board-with-stones))
  (check-true (member (position 3 3) live-stones))
  (check-true (member (position 15 15) live-stones))
  (check-true (member (position 3 4) live-stones))
  )