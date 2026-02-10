#lang racket

(require racket/gui)

;; 独立的棋盘格线条测试程序
(define board-test-frame%
  (class frame%
    (super-new [label "棋盘格线条测试"] [width 600] [height 600])
    
    ;; 棋盘参数
    (define board-size 19)
    (define cell-size 25)
    (define margin 30)
    
    ;; 创建画布
    (define canvas
      (new (class canvas%
             (super-new)
             
             (define/override (on-paint)
               (define dc (send this get-dc))
               
               ;; 清空背景
               (send dc set-brush "white" 'solid)
               (send dc draw-rectangle 0 0 (send this get-width) (send this get-height))
               
               ;; 方法1：统一粗细绘制（基准测试）
               (send dc set-pen "black" 1 'solid)
               (for ([i (in-range board-size)])
                 ;; 垂直线
                 (send dc draw-line 
                       (+ margin (* i cell-size)) margin
                       (+ margin (* i cell-size)) (+ margin (* (sub1 board-size) cell-size)))
                 ;; 水平线
                 (send dc draw-line 
                       margin (+ margin (* i cell-size))
                       (+ margin (* (sub1 board-size) cell-size)) (+ margin (* i cell-size))))
               
               ;; 方法2：边框粗线，内部细线（改进版本）
               (send dc set-pen "red" 2 'solid)
               (define board-width (* (sub1 board-size) cell-size))
               ;; 外边框
               (send dc draw-line margin margin (+ margin board-width) margin) ; 上
               (send dc draw-line margin (+ margin board-width) (+ margin board-width) (+ margin board-width)) ; 下
               (send dc draw-line margin margin margin (+ margin board-width)) ; 左
               (send dc draw-line (+ margin board-width) margin (+ margin board-width) (+ margin board-width)) ; 右
               
               ;; 内部细线（红色虚线表示）
               (send dc set-pen "red" 1 'dot)
               (for ([i (in-range 1 (sub1 board-size))])
                 (send dc draw-line 
                       (+ margin (* i cell-size)) (+ margin 1)
                       (+ margin (* i cell-size)) (+ margin board-width -1))
                 (send dc draw-line 
                       (+ margin 1) (+ margin (* i cell-size))
                       (+ margin board-width -1) (+ margin (* i cell-size))))
               
               ;; 添加标注说明
               (send dc set-font (make-font #:size 12))
               (send dc set-text-foreground "blue")
               (send dc draw-text "黑色实线：统一1像素粗细（基准）" 20 20)
               (send dc draw-text "红色实线：边框2像素粗细" 20 40)
               (send dc draw-text "红色虚线：内部1像素细线（改进版）" 20 60)
               ))
           [parent this]
           [min-width 550]
           [min-height 550]))
    
    ;; 显示窗口
    (send this show #t)))

;; 运行测试
(define (run-board-line-test)
  (new board-test-frame%))

(run-board-line-test)