#lang racket

(require "../src/sgf-format.rkt")
(require "../src/board.rkt")
(require "../src/game-state.rkt")
(require "../src/game-engine.rkt")

(displayln "=== SGF完整功能验证 ===")

;; 1. 测试加载功能
(displayln "1. 测试SGF加载:")
(define loaded-state (sgf-load-game "data/game-01.sgf"))
(define board (game-state-board loaded-state))
(define stone-count 
  (for*/sum ([r (in-range 19)] [c (in-range 19)])
    (if (board-get-stone board (position r c)) 1 0)))

(printf "   棋盘上棋子数: ~a~n" stone-count)
(when (> stone-count 0)
  (displayln "   ✅ 加载成功"))

;; 2. 显示加载的移动历史
(displayln "2. 移动历史验证:")
(define moves (game-state-move-history loaded-state))
(printf "   移动总数: ~a~n" (length moves))
(for ([move moves] [i (in-range (min 5 (length moves)))])
  (when (move-position move)
    (printf "   第~a手: ~a 在 (~a,~a)~n" 
            (+ i 1)
            (move-player move)
            (position-row (move-position move))
            (position-col (move-position move)))))

;; 3. 测试保存功能
(displayln "3. 测试SGF保存:")
(define test-save-file "/tmp/sgf-roundtrip-test.sgf")
(sgf-save-game loaded-state test-save-file)

;; 4. 验证保存的文件
(displayln "4. 验证保存文件:")
(define saved-content (call-with-input-file test-save-file port->string))
;; 手工计数移动
(define move-count 
  (length (filter (lambda (char) (char=? char #\;)) (string->list saved-content))))
(printf "   保存文件中分号数: ~a~n" move-count)
(printf "   保存文件内容:~n~a~n" saved-content)
(when (>= move-count 9)
  (displayln "   ✅ 保存功能正常"))

;; 5. 测试往返一致性
(displayln "5. 测试加载-保存-再加载一致性:")
(define reloaded-state (sgf-load-game test-save-file))
(define reloaded-board (game-state-board reloaded-state))
(define reloaded-stone-count 
  (for*/sum ([r (in-range 19)] [c (in-range 19)])
    (if (board-get-stone reloaded-board (position r c)) 1 0)))

(printf "   重新加载后棋子数: ~a~n" reloaded-stone-count)
(if (= stone-count reloaded-stone-count)
    (displayln "   ✅ 往返一致性验证通过")
    (displayln "   ❌ 往返一致性验证失败"))

(displayln "~n=== SGF功能验证完成 ===")
(when (and (> stone-count 0) (>= move-count 9) (= stone-count reloaded-stone-count))
  (displayln "🎉 所有SGF功能测试通过!"))