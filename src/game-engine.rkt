#lang racket

(provide game-make-move
         game-pass
         game-resign
         game-undo
         game-get-valid-moves
         game-is-game-over?
         game-get-winner
         game-get-score)

(require "board.rkt")
(require "stone.rkt")
(require "game-state.rkt")
(require "rules.rkt")
(require "ko-rule.rkt")
(require "scoring.rkt")

;; 执行一步合法移动
(define (game-make-move gs pos)
  (define current-player (game-get-current-player gs))
  
  ;; 1. 基本合法性检查
  (define basic-valid? (rules-is-valid-move? (game-state-board gs) pos current-player))
  (when (not basic-valid?)
    (error "非法移动：" pos))
  
  ;; 2. 劫争规则检查
  (define ko-check-result (ko-check-move gs pos current-player))
  (when (not (first ko-check-result))
    (error "违反劫争规则：" (second ko-check-result)))
  
  ;; 3. 保存当前状态用于劫争检测
  (define old-board (game-state-board gs))
  
  ;; 4. 执行移动
  (define new-board (board-set-stone old-board pos current-player))
  
  ;; 5. 查找并提掉无气的对方棋子
  (define opponent-color (opposite-color current-player))
  (define dead-opponent-groups (rules-find-dead-groups new-board opponent-color))
  (define captured-stones 
    (apply append (map (lambda (group) group) dead-opponent-groups)))
  
  ;; 6. 执行提子
  (define board-after-capture 
    (foldl (lambda (cap-pos board) (board-remove-stone board cap-pos))
           new-board
           captured-stones))
  
  ;; 7. 检查自杀（理论上不应该发生，因为rules-is-valid-move?已经检查过）
  (define placed-group (rules-get-connected-group board-after-capture pos))
  (when (= (rules-get-group-liberties board-after-capture placed-group) 0)
    (error "自杀移动（内部错误）：" pos))
  
  ;; 8. 更新劫争状态
  (define new-ko-position (ko-update-state old-board board-after-capture pos current-player))
  
  ;; 9. 构建新的游戏状态
  (define new-state
    (struct-copy game-state gs
                 [board board-after-capture]
                 [ko-position new-ko-position]))
  
  ;; 10. 添加到历史记录
  (define state-with-history (move-history-add new-state pos captured-stones))
  
  ;; 11. 添加被提子统计
  (define final-state (game-state-add-captured-stones state-with-history captured-stones))
  
  ;; 12. 切换玩家
  (game-state-switch-player final-state))

;; Pass操作
(define (game-pass gs)
  (define current-player (game-get-current-player gs))
  ;; 添加pass到历史记录
  (define state-with-history 
    (move-history-add gs #f '()))  ; #f表示pass
  ;; 切换玩家
  (game-state-switch-player state-with-history))

;; 认输
(define (game-resign gs)
  (define winner (opposite-color (game-get-current-player gs)))
  (struct-copy game-state gs [ko-position (list 'resigned winner)]))

;; 悔棋（撤销上一步）
(define (game-undo gs)
  (move-history-undo gs))

;; 获取当前玩家的所有合法移动
(define (game-get-valid-moves gs)
  (define current-player (game-get-current-player gs))
  (define board (game-state-board gs))
  
  (filter (lambda (pos)
            (rules-is-valid-move? board pos current-player))
          (board-get-all-positions)))

;; 检查游戏是否结束
(define (game-is-game-over? gs)
  (define last-move (move-history-get-last gs))
  (define second-last-move 
    (if (>= (length (game-state-move-history gs)) 2)
        (cadr (game-state-move-history gs))
        #f))
  
  ;; 如果连续两手都是pass，则游戏结束
  (and last-move 
       second-last-move
       (eq? (move-position last-move) #f)  ; last move is pass
       (eq? (move-position second-last-move) #f)))  ; second last move is pass

;; 获取获胜者
(define (game-get-winner gs)
  (cond
    ;; 认输情况
    [(and (pair? (game-state-ko-position gs))
          (eq? (car (game-state-ko-position gs)) 'resigned))
     (cadr (game-state-ko-position gs))]
    ;; 正常结束情况
    [(game-is-game-over? gs)
     (define score-result 
       (scoring-final-score (game-state-board gs)
                           (game-state-get-captured-count gs 'black)
                           (game-state-get-captured-count gs 'white)))
     (define difference (list-ref score-result 5))
     (cond
       [(> difference 0) 'black]
       [(< difference 0) 'white]
       [else 'draw])]
    [else #f]))  ; 游戏未结束

;; 获取当前得分
(define (game-get-score gs)
  (scoring-final-score (game-state-board gs)
                      (game-state-get-captured-count gs 'black)
                      (game-state-get-captured-count gs 'white)))

;; 模块测试
(module+ test
  (require rackunit)
  
  ;; 测试初始状态
  (define initial-state (make-initial-game-state))
  (check-false (game-is-game-over? initial-state))
  (check-false (game-get-winner initial-state))
  
  ;; 测试基本移动
  (define state-after-first-move 
    (game-make-move initial-state (position 3 3)))
  (check-eq? (board-get-stone (game-state-board state-after-first-move) (position 3 3)) 'black)
  (check-eq? (game-get-current-player state-after-first-move) 'white)
  
  ;; 测试Pass
  (define state-after-pass 
    (game-pass state-after-first-move))
  (check-eq? (game-get-current-player state-after-pass) 'black)
  
  ;; 测试连续Pass结束游戏
  (define state-after-second-pass 
    (game-pass state-after-pass))
  (check-true (game-is-game-over? state-after-second-pass))
  
  ;; 测试悔棋
  (define undone-state (game-undo state-after-first-move))
  (check-eq? (game-get-current-player undone-state) 'black)
  (check-true (board-is-empty? (game-state-board undone-state) (position 3 3)))
  
  ;; 测试认输
  (define resigned-state (game-resign initial-state))
  (check-eq? (game-get-winner resigned-state) 'white)
  
  ;; 测试合法移动获取
  (define valid-moves (game-get-valid-moves initial-state))
  (check-true (> (length valid-moves) 300))  ; 19x19棋盘有很多合法位置
  (check-true (member (position 3 3) valid-moves))
  )