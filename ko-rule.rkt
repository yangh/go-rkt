#lang racket

(provide ko-check-move
         ko-update-state
         ko-is-ko-position?
         ko-clear-state)

(require "board.rkt")
(require "stone.rkt")
(require "rules.rkt")
(require "game-state.rkt")

;; 检查移动是否违反劫争规则
(define (ko-check-move game-state pos color)
  (define ko-pos (game-state-ko-position game-state))
  (cond
    [(and ko-pos (equal? pos ko-pos))
     (list #f "违反劫争规则")]
    [else (list #t #f)]))

;; 更新劫争状态
(define (ko-update-state old-board new-board move-pos move-color)
  ;; 检查是否形成了劫争
  (define captured-stones (find-captured-stones old-board new-board move-pos move-color))
  
  (cond
    ;; 如果只提了一个子，且提子后的位置正好是刚下的位置，则形成劫争
    [(and (= (length captured-stones) 1)
          (let ([captured-pos (car captured-stones)])
            ;; 检查提子位置是否只有一个气，且那个气就是刚刚下的位置
            (define temp-board-after-capture 
              (foldl (lambda (cap-pos board) (board-remove-stone board cap-pos))
                     new-board
                     captured-stones))
            (define liberties-after-capture 
              (rules-get-liberties temp-board-after-capture captured-pos))
            (and (= (length liberties-after-capture) 1)
                 (equal? (car liberties-after-capture) move-pos))))
     move-pos]  ; 返回劫争位置
    [else #f])) ; 没有形成劫争

;; 查找被提的棋子
(define (find-captured-stones old-board new-board move-pos move-color)
  (define opponent-color (opposite-color move-color))
  (define old-opponent-stones (board-get-stones-by-color old-board opponent-color))
  (define new-opponent-stones (board-get-stones-by-color new-board opponent-color))
  
  ;; 被提的棋子是在旧棋盘上有但在新棋盘上没有的位置
  (filter (lambda (pos) 
            (not (member pos new-opponent-stones)))
          old-opponent-stones))

;; 检查位置是否为劫争位置
(define (ko-is-ko-position? game-state pos)
  (and (game-state-ko-position game-state)
       (equal? pos (game-state-ko-position game-state))))

;; 清除劫争状态（通常在下一步之后调用）
(define (ko-clear-state gs)
  (struct-copy game-state gs [ko-position #f]))

;; 模块测试
(module+ test
  (require rackunit)
  
  ;; 测试劫争检测的基本情况
  (define empty-state (make-initial-game-state))
  (check-equal? (ko-check-move empty-state (position 3 3) 'black) '(#t #f))
  
  ;; 测试劫争位置检查
  (define state-with-ko 
    (struct-copy game-state empty-state [ko-position (position 3 3)]))
  (check-equal? (ko-check-move state-with-ko (position 3 3) 'black) 
                '(#f "违反劫争规则"))
  (check-equal? (ko-check-move state-with-ko (position 4 4) 'black) 
                '(#t #f))
  
  ;; 测试劫争位置判断
  (check-true (ko-is-ko-position? state-with-ko (position 3 3)))
  (check-false (ko-is-ko-position? empty-state (position 3 3)))
  
  ;; 测试简单的被提子查找
  (define old-board (make-empty-board))
  (define white-stone-pos (position 3 3))
  (define old-board-with-white 
    (board-set-stone old-board white-stone-pos 'white))
  
  (define new-board (make-empty-board))  ; 白子被提掉了
  
  (define captured (find-captured-stones old-board-with-white new-board 
                                       (position 3 4) 'black))
  (check-equal? captured (list white-stone-pos))
  )