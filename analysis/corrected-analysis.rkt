#lang racket

(require "custom-format.rkt")
(require "board.rkt")
(require "stone.rkt")
(require "rules.rkt")
(require "game-state.rkt")

(displayln "=== 修复版连通性验证 ===")

;; 加载game-01.txt
(define game-state (custom-load-game "game-01.txt"))
(define board (game-state-board game-state))

;; 创建修复版的连通性检查函数
(define (correct-get-connected-group board pos)
  "正确的连通组获取函数"
  (define target-color (board-get-stone board pos))
  (when (not target-color)
    (error "位置必须有棋子"))
  
  (define visited (make-hash))
  (define group '())
  (define queue (list pos))
  
  (let loop ()
    (when (not (null? queue))
      (define current (car queue))
      (define rest-queue (cdr queue))
      
      (when (and (not (hash-has-key? visited current))
                 (eq? (board-get-stone board current) target-color))
        (hash-set! visited current #t)
        (set! group (cons current group))
        
        ;; 获取所有同色邻居
        (define neighbors (board-get-neighbors current))
        (define same-color-neighbors 
          (filter (lambda (neighbor)
                   (and (eq? (board-get-stone board neighbor) target-color)
                        (not (hash-has-key? visited neighbor))))
                 neighbors))
        
        (set! queue (append rest-queue same-color-neighbors)))
      (set! queue rest-queue)
      (loop)))
  
  (reverse group))

;; 验证关键位置的连通性
(displayln "=== 验证关键黑棋位置的连通性 ===")

(define key-positions (list (position 1 3) (position 1 4) (position 2 5) 
                           (position 4 4) (position 5 4)))

(for ([pos key-positions])
  (define stone (board-get-stone board pos))
  (when (eq? stone 'black)
    (define correct-group (correct-get-connected-group board pos))
    (displayln (format "(~a,~a): 连通组大小=~a" 
                      (position-row pos) 
                      (position-col pos) 
                      (length correct-group)))
    (displayln "  组内位置:")
    (for ([group-pos correct-group])
      (displayln (format "    (~a,~a)" 
                        (position-row group-pos) 
                        (position-col group-pos))))))

;; 特别检查(4,4)和(5,4)是否连通
(displayln "\n=== 特别验证(4,4)和(5,4)的连通性 ===")
(define pos-4-4 (position 4 4))
(define pos-5-4 (position 5 4))

(define group-4-4-correct (correct-get-connected-group board pos-4-4))
(define group-5-4-correct (correct-get-connected-group board pos-5-4))

(displayln (format "(4,4)正确连通组: ~a个位置" (length group-4-4-correct)))
(displayln (format "(5,4)正确连通组: ~a个位置" (length group-5-4-correct)))

;; 检查它们是否现在属于同一组
(define now-connected? 
  (not (null? (filter (lambda (pos) (member pos group-5-4-correct)) group-4-4-correct))))
(displayln (format "修复后(4,4)和(5,4)是否连通: ~a" now-connected?))

;; 如果还不连通，手动分析路径
(when (not now-connected?)
  (displayln "\n=== 手动路径分析 ===")
  (displayln "检查(4,4)和(5,4)之间是否存在连接路径:")
  
  ;; 检查中间位置
  (define middle-positions (list (position 4 5) (position 5 5) (position 4 3) (position 5 3)))
  (for ([mid-pos middle-positions])
    (define mid-stone (board-get-stone board mid-pos))
    (displayln (format "  (~a,~a): ~a" 
                      (position-row mid-pos) 
                      (position-col mid-pos) 
                      (if mid-stone mid-stone "empty")))))

;; 模拟在(3,4)下黑棋的正确分析
(displayln "\n=== 正确模拟黑棋下在(3,4) ===")
(define test-pos (position 3 4))
(define temp-board (board-set-stone board test-pos 'black))

;; 使用修复后的连通性检查
(define new-group-correct 
  (correct-get-connected-group temp-board test-pos))

(displayln (format "修复后新连通组大小: ~a" (length new-group-correct)))
(displayln "组内位置:")
(for ([pos new-group-correct])
  (displayln (format "  (~a,~a)" (position-row pos) (position-col pos))))

;; 计算气
(define correct-liberties (rules-get-group-liberties temp-board new-group-correct))
(displayln (format "整组气数: ~a" correct-liberties))

;; 检查提子可能性
(define can-capture-correct? (rules-can-capture-opponent? temp-board test-pos 'black))
(displayln (format "能否提子: ~a" can-capture-correct?))

;; 最终结论
(displayln "\n=== 修复后结论 ===")
(define would-be-legal? (> correct-liberties 0))
(displayln (format "按照正确算法，黑棋在(3,4)应该是: ~a" 
                  (if would-be-legal? "合法" "非法")))

(when would-be-legal?
  (displayln "🎉 您的判断是正确的！(3,4)应该是合法位置"))