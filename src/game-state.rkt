#lang racket

(provide (struct-out game-state)
         (struct-out move)
         make-initial-game-state
         game-get-current-player
         game-state-switch-player
         game-state-add-captured-stones
         game-state-get-captured-count
         game-state-reset-captured-stones
         move-history-add
         move-history-get-last
         move-history-undo)

(require "board.rkt")
(require "stone.rkt")

;; 移动记录结构
(struct move (position player captured-stones timestamp) #:transparent)

;; 游戏状态结构
(struct game-state (board 
                   current-player 
                   captured-stones 
                   move-history 
                   ko-position) #:transparent)

;; 创建初始游戏状态
(define (make-initial-game-state)
  (game-state (make-empty-board)
              'black           ; 黑子先手
              (make-hash '((black . 0) (white . 0)))
              '()              ; 空的历史记录
              #f))             ; 初始无劫争位置

;; 获取当前玩家
(define (game-get-current-player gs)
  (game-state-current-player gs))

;; 切换玩家
(define (game-state-switch-player gs)
  (struct-copy game-state gs
               [current-player (opposite-color (game-state-current-player gs))]))

;; 添加被提子
(define (game-state-add-captured-stones gs stones)
  (define new-captured (hash-copy (game-state-captured-stones gs)))
  (hash-set! new-captured 
             (opposite-color (game-state-current-player gs))
             (+ (hash-ref new-captured 
                         (opposite-color (game-state-current-player gs)) 
                         0)
                (length stones)))
  (struct-copy game-state gs
               [captured-stones new-captured]))

;; 获取被提子数量
(define (game-state-get-captured-count gs color)
  (hash-ref (game-state-captured-stones gs) color 0))

;; 重置被提子计数
(define (game-state-reset-captured-stones gs)
  (struct-copy game-state gs
               [captured-stones (make-hash '((black . 0) (white . 0)))]))

;; 向历史记录添加移动
(define (move-history-add gs pos captured-stones)
  (define new-move (move pos 
                        (game-state-current-player gs) 
                        captured-stones 
                        (current-inexact-milliseconds)))
  (struct-copy game-state gs
               [move-history (cons new-move (game-state-move-history gs))]))

;; 获取最后一步移动
(define (move-history-get-last gs)
  (if (null? (game-state-move-history gs))
      #f
      (car (game-state-move-history gs))))

;; 撤销最后一步（返回新的游戏状态）
(define (move-history-undo gs)
  (if (null? (game-state-move-history gs))
      gs  ; 没有可撤销的步骤
      (let* ([last-move (car (game-state-move-history gs))]
             [rest-history (cdr (game-state-move-history gs))]
             [pos (move-position last-move)]
             [player (move-player last-move)]
             [captured-stones (move-captured-stones last-move)])
        ;; 恢复棋盘状态：移除最后放置的棋子，恢复被提的棋子
        (define restored-board 
          (foldl (lambda (stone-pos board)
                   (board-set-stone board stone-pos (opposite-color player)))
                 (board-remove-stone (game-state-board gs) pos)
                 captured-stones))
        
        ;; 恢复被提子计数
        (define restored-captured 
          (hash-copy (game-state-captured-stones gs)))
        (hash-set! restored-captured 
                   (opposite-color player)
                   (- (hash-ref restored-captured (opposite-color player) 0)
                      (length captured-stones)))
        
        ;; 返回恢复后的状态
        (game-state restored-board
                    player  ; 恢复到上一个玩家
                    restored-captured
                    rest-history
                    #f))))  ; 清除劫争标记

;; 模块测试
(module+ test
  (require rackunit)
  
  ;; 测试初始状态
  (define initial-state (make-initial-game-state))
  (check-eq? (game-get-current-player initial-state) 'black)
  (check-equal? (game-state-get-captured-count initial-state 'black) 0)
  (check-equal? (game-state-get-captured-count initial-state 'white) 0)
  (check-eq? (move-history-get-last initial-state) #f)
  
  ;; 测试玩家切换
  (define white-turn-state (game-state-switch-player initial-state))
  (check-eq? (game-get-current-player white-turn-state) 'white)
  
  ;; 测试被提子计数
  (define state-with-captures 
    (game-state-add-captured-stones initial-state 
                                   (list (position 0 0) (position 0 1))))
  (check-equal? (game-state-get-captured-count state-with-captures 'white) 2)
  
  ;; 测试历史记录
  (define state-with-move 
    (move-history-add initial-state (position 3 3) '()))
  (define last-move (move-history-get-last state-with-move))
  (check-equal? (move-position last-move) (position 3 3))
  (check-eq? (move-player last-move) 'black)
  )