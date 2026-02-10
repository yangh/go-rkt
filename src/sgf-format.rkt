#lang racket

(provide sgf-save-game
         sgf-load-game
         sgf-export-move-history
         sgf-import-move-history)

(require "board.rkt")
(require "stone.rkt")
(require "game-state.rkt")
(require "game-engine.rkt")

;; 将游戏保存为SGF格式
(define (sgf-save-game game-state filename)
  (define sgf-content (generate-sgf-content game-state))
  (call-with-output-file filename
    (lambda (out)
      (display sgf-content out))
    #:exists 'replace))

;; 从SGF文件加载游戏
(define (sgf-load-game filename)
  (define sgf-content (call-with-input-file filename port->string))
  (parse-sgf-content sgf-content))

;; 导出移动历史为SGF字符串
(define (sgf-export-move-history game-state)
  (generate-sgf-content game-state))

;; 从SGF字符串导入移动历史
(define (sgf-import-move-history sgf-string)
  (parse-sgf-content sgf-string))

;; 生成SGF内容
(define (generate-sgf-content game-state)
  (string-append "(;GM[1]FF[4]CA[UTF-8]AP[Go-Racket:1.0]\n"
                 "SZ[19]KM[7.5]\n"
                 (generate-move-sequence game-state)
                 ")"))

;; 生成移动序列
(define (generate-move-sequence game-state)
  (define moves (game-state-move-history game-state))  ; 移除多余的reverse
  (apply string-append
         (map (lambda (move index)
                (define player-char (if (eq? (move-player move) 'black) "B" "W"))
                (define pos-str 
                  (if (move-position move)
                      (position->sgf-coord (move-position move))
                      "[]"))  ; pass
                (string-append ";" player-char "[" pos-str "]"))
              moves
              (range (length moves)))))

;; SGF坐标转换 (0,0) -> "aa", (18,18) -> "ss"
(define (position->sgf-coord pos)
  (define letters "abcdefghijklmnopqrstuvwxyz")
  (define col-letter (string-ref letters (position-col pos)))
  (define row-letter (string-ref letters (- 18 (position-row pos))))  ; SGF行坐标倒序
  (string-append (string col-letter) (string row-letter)))

;; 从SGF坐标转换为位置
(define (sgf-coord->position coord-str)
  (when (or (string=? coord-str "") (string=? coord-str "[]"))
    (error "无效的坐标字符串"))
  
  (define letters "abcdefghijklmnopqrstuvwxyz")
  (define col-index (string-index letters (string-ref coord-str 0)))
  (define row-index (- 18 (string-index letters (string-ref coord-str 1))))
  
  (when (or (< col-index 0) (< row-index 0) (>= col-index 19) (>= row-index 19))
    (error "坐标超出棋盘范围"))
  
  (position row-index col-index))

;; 查找字符在字符串中的索引
(define (string-index str char)
  (define len (string-length str))
  (let loop ([i 0])
    (cond
      [(>= i len) -1]
      [(char=? (string-ref str i) char) i]
      [else (loop (add1 i))])))

;; 解析SGF内容
(define (parse-sgf-content sgf-content)
  (define moves (extract-moves-from-sgf sgf-content))
  (replay-moves moves))

;; 从SGF中提取移动序列（手工解析方法）
(define (extract-moves-from-sgf sgf-content)
  ;; 简单的手工解析：按分号分割然后解析
  (define parts (string-split sgf-content ";"))
  (define moves '())
  
  (for ([part parts])
    (when (>= (string-length part) 4)
      (define first-char (string-ref part 0))
      (when (or (char=? first-char #\B) (char=? first-char #\W))
        (when (and (char=? (string-ref part 1) #\[) (char=? (string-ref part 4) #\]))
          (define player (if (char=? first-char #\B) 'black 'white))
          (define coord-str (substring part 2 4))
          (define pos (sgf-coord->position coord-str))
          (set! moves (cons (move pos player '() 0) moves))))))
  
  (reverse moves))

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
  
  ;; 测试坐标转换
  (check-equal? (position->sgf-coord (position 0 0)) "aa")
  (check-equal? (position->sgf-coord (position 18 18)) "ss")
  (check-equal? (position->sgf-coord (position 3 3)) "dd")
  
  (check-equal? (sgf-coord->position "aa") (position 0 0))
  (check-equal? (sgf-coord->position "ss") (position 18 18))
  (check-equal? (sgf-coord->position "dd") (position 3 3))
  
  ;; 测试基本游戏保存和加载
  (define test-state (make-initial-game-state))
  (define state-after-moves 
    (game-make-move 
      (game-make-move test-state (position 3 3))
      (position 15 15)))
  
  (define sgf-string (sgf-export-move-history state-after-moves))
  (check-true (string-contains? sgf-string "B[dd]"))
  (check-true (string-contains? sgf-string "W[pp]"))
  
  ;; 测试移动序列重放
  (define replayed-state (sgf-import-move-history sgf-string))
  (check-eq? (board-get-stone (game-state-board replayed-state) (position 3 3)) 'black)
  (check-eq? (board-get-stone (game-state-board replayed-state) (position 15 15)) 'white)
  (check-eq? (game-get-current-player replayed-state) 'black)  ; 下一手应该是黑棋
  )