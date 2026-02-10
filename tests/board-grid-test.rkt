#lang racket

(require "../src/gui-main.rkt")
(require "../src/board.rkt")
(require "../src/stone.rkt")
(require "../src/game-state.rkt")

(displayln "=== 棋盘线条改进测试 ===")
(displayln "请观察以下改进：")
(displayln "- 边框线加粗（2像素）")
(displayln "- 内部网格线保持细线（1像素）")
(displayln "- 整体视觉效果更加清晰")

;; 启动标准游戏进行测试
(start-go-game)