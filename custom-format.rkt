#lang racket

(provide custom-save-game
         custom-load-game
         custom-export-move-history
         custom-import-move-history)

(require "board.rkt")
(require "stone.rkt")
(require "game-state.rkt")
(require "game-engine.rkt")

;; 将游戏保存为自定义文本格式
(define (custom-save-game game-state filename)
  (define custom-content (generate-custom-content game-state))
  (call-with-output-file filename
    (lambda (out)
      (display custom-content out))
    #:exists 'replace))

;; 从自定义文本文件加载游戏
(define (custom-load-game filename)
  (define custom-content (call-with-input-file filename port->string))
  (parse-custom-content custom-content))

;; 导出自定义格式的移动历史
(define (custom-export-move-history game-state)
  (generate-custom-content game-state))

;; 从自定义格式导入移动历史
(define (custom-import-move-history custom-string)
  (parse-custom-content custom-string))

;; 生成自定义格式内容
(define (generate-custom-content game-state)
  (string-append "# 围棋棋谱 - 自定义格式\n"
                 "# 格式: 回合数. 玩家 坐标\n"
                 "# 坐标格式: (行,列) 从(0,0)到(18,18)\n"
                 "# Pass用'PASS'表示\n"
                 "# ===== 开始 =====\n\n"
                 (generate-move-list game-state)))

;; 生成移动列表
(define (generate-move-list game-state)
  (define moves (reverse (game-state-move-history game-state)))
  (apply string-append
         (map (lambda (move index)
                (define round-num (add1 index))
                (define player-name (if (eq? (move-player move) 'black) "黑" "白"))
                (define pos-str 
                  (if (move-position move)
                      (format "(~a,~a)" 
                              (position-row (move-position move))
                              (position-col (move-position move)))
                      "PASS"))
                (format "~a. ~a ~a\n" round-num player-name pos-str))
              moves
              (range (length moves)))))

;; 解析自定义格式内容
(define (parse-custom-content custom-content)
  (define lines (string-split custom-content "\n"))
  (define move-lines 
    (filter (lambda (line)
              (and (not (string-prefix? line "#"))
                   (not (string=? line ""))
                   (regexp-match #rx"^[0-9]+\\." line)))
            lines))
  
  (define moves (map parse-move-line move-lines))
  (replay-moves moves))

;; 解析单行移动
(define (parse-move-line line)
  (define parts (string-split line))
  (when (< (length parts) 3)
    (error "无效的移动行格式:" line))
  
  (define player-name (cadr parts))
  (define player (if (string=? player-name "黑") 'black 'white))
  (define coord-str (caddr parts))
  
  (define pos 
    (if (string=? coord-str "PASS")
        #f  ; pass
        (parse-coordinate coord-str)))
  
  (move pos player '() 0))  ; 时间戳设为0

;; 解析坐标字符串 "(3,3)" -> position(3,3)
(define (parse-coordinate coord-str)
  (define coord-pattern #rx"\\(([0-9]+),([0-9]+)\\)")
  (define match (regexp-match coord-pattern coord-str))
  
  (when (not match)
    (error "无效的坐标格式:" coord-str))
  
  (define row (string->number (cadr match)))
  (define col (string->number (caddr match)))
  
  (when (or (< row 0) (< col 0) (>= row 19) (>= col 19))
    (error "坐标超出棋盘范围:" coord-str))
  
  (position row col))

;; 重放移动序列重建游戏状态
(define (replay-moves moves)
  (define initial-state (make-initial-game-state))
  (foldl (lambda (move current-state)
           (if (move-position move)
               (game-make-move current-state (move-position move))
               (game-pass current-state)))
         initial-state
         moves))

;; 模块测试
(module+ test
  (require rackunit)
  
  ;; 测试坐标解析
  (check-equal? (parse-coordinate "(0,0)") (position 0 0))
  (check-equal? (parse-coordinate "(18,18)") (position 18 18))
  (check-equal? (parse-coordinate "(3,15)") (position 3 15))
  
  ;; 测试基本游戏保存和加载
  (define test-state (make-initial-game-state))
  (define state-after-moves 
    (game-make-move 
      (game-make-move test-state (position 3 3))
      (position 15 15)))
  
  (define custom-string (custom-export-move-history state-after-moves))
  (check-true (string-contains? custom-string "1. 黑 (3,3)"))
  (check-true (string-contains? custom-string "2. 白 (15,15)"))
  
  ;; 测试移动序列重放
  (define replayed-state (custom-import-move-history custom-string))
  (check-eq? (board-get-stone (game-state-board replayed-state) (position 3 3)) 'black)
  (check-eq? (board-get-stone (game-state-board replayed-state) (position 15 15)) 'white)
  (check-eq? (game-get-current-player replayed-state) 'black)  ; 下一手应该是黑棋
  
  ;; 测试Pass处理
  (define state-with-pass 
    (game-pass state-after-moves))
  (define pass-string (custom-export-move-history state-with-pass))
  (check-true (string-contains? pass-string "3. 黑 PASS"))
  )