#lang racket

(require racket/gui)

;; 详细的棋盘格线条对比测试程序
(define detailed-board-test-frame%
  (class frame%
    (super-new [label "详细棋盘格线条对比测试"] [width 800] [height 600])
    
    ;; 创建标签页容器
    (define tab-panel (new tab-panel% 
                          [parent this]
                          [choices '("统一粗细" "边框加粗" "参数对比")]
                          [callback (lambda (panel event)
                                     (send canvas refresh))]))
    
    ;; 主画布
    (define canvas
      (new (class canvas%
             (super-new)
             
             (define/override (on-paint)
               (define dc (send this get-dc))
               (define current-tab (send tab-panel get-selection))
               
               (case current-tab
                 ;; 标签页1：统一粗细测试
                 [(0)
                  (test-uniform-thickness dc)]
                 
                 ;; 标签页2：边框加粗测试
                 [(1)
                  (test-bold-border dc)]
                 
                 ;; 标签页3：参数对比测试
                 [(2)
                  (test-parameter-comparison dc)])))
           [parent tab-panel]
           [min-width 750]
           [min-height 500]))
    
    ;; 测试方法1：统一粗细
    (define (test-uniform-thickness dc)
      (send dc set-brush "white" 'solid)
      (send dc draw-rectangle 0 0 (send canvas get-width) (send canvas get-height))
      
      (define board-size 19)
      (define cell-size 20)
      (define margin 20)
      
      ;; 统一使用1像素粗细
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
      
      (send dc set-font (make-font #:size 14))
      (send dc set-text-foreground "blue")
      (send dc draw-text "测试1：所有线条统一1像素粗细" 20 20)
      (send dc draw-text "观察是否存在线条粗细不一致现象" 20 45))
    
    ;; 测试方法2：边框加粗
    (define (test-bold-border dc)
      (send dc set-brush "white" 'solid)
      (send dc draw-rectangle 0 0 (send canvas get-width) (send canvas get-height))
      
      (define board-size 19)
      (define cell-size 20)
      (define margin 20)
      (define board-width (* (sub1 board-size) cell-size))
      
      ;; 边框粗线（2像素）
      (send dc set-pen "black" 2 'solid)
      (send dc draw-line margin margin (+ margin board-width) margin) ; 上
      (send dc draw-line margin (+ margin board-width) (+ margin board-width) (+ margin board-width)) ; 下
      (send dc draw-line margin margin margin (+ margin board-width)) ; 左
      (send dc draw-line (+ margin board-width) margin (+ margin board-width) (+ margin board-width)) ; 右
      
      ;; 内部细线（1像素）
      (send dc set-pen "black" 1 'solid)
      (for ([i (in-range 1 (sub1 board-size))])
        (send dc draw-line 
              (+ margin (* i cell-size)) (+ margin 1)
              (+ margin (* i cell-size)) (+ margin board-width -1))
        (send dc draw-line 
              (+ margin 1) (+ margin (* i cell-size))
              (+ margin board-width -1) (+ margin (* i cell-size))))
      
      (send dc set-font (make-font #:size 14))
      (send dc set-text-foreground "blue")
      (send dc draw-text "测试2：边框2像素，内部1像素" 20 20)
      (send dc draw-text "这是推荐的视觉效果" 20 45))
    
    ;; 测试方法3：参数对比
    (define (test-parameter-comparison dc)
      (send dc set-brush "white" 'solid)
      (send dc draw-rectangle 0 0 (send canvas get-width) (send canvas get-height))
      
      ;; 创建多个小棋盘进行对比
      (define test-configs '(
        ("1px统一" 1 1 15)
        ("边框2px" 2 1 15)
        ("边框3px" 3 1 15)
        ("全2px" 2 2 15)
      ))
      
      (for ([config test-configs]
            [index (in-naturals)])
        (define label (car config))
        (define border-thick (cadr config))
        (define inner-thick (caddr config))
        (define cell-size (cadddr config))
        
        (define x-offset (+ 30 (* index 180)))
        (define y-offset 80)
        (define board-size 9) ; 小棋盘便于对比
        (define margin 10)
        (define board-width (* (sub1 board-size) cell-size))
        
        ;; 绘制标签
        (send dc set-font (make-font #:size 12))
        (send dc set-text-foreground "darkblue")
        (send dc draw-text label x-offset (- y-offset 10))
        
        ;; 绘制小棋盘
        ;; 边框
        (send dc set-pen "black" border-thick 'solid)
        (send dc draw-line (+ x-offset margin) (+ y-offset margin) 
              (+ x-offset margin board-width) (+ y-offset margin))
        (send dc draw-line (+ x-offset margin) (+ y-offset margin board-width) 
              (+ x-offset margin board-width) (+ y-offset margin board-width))
        (send dc draw-line (+ x-offset margin) (+ y-offset margin) 
              (+ x-offset margin) (+ y-offset margin board-width))
        (send dc draw-line (+ x-offset margin board-width) (+ y-offset margin) 
              (+ x-offset margin board-width) (+ y-offset margin board-width))
        
        ;; 内部线
        (send dc set-pen "black" inner-thick 'solid)
        (for ([i (in-range 1 (sub1 board-size))])
          (send dc draw-line 
                (+ x-offset margin (* i cell-size)) (+ y-offset margin 1)
                (+ x-offset margin (* i cell-size)) (+ y-offset margin board-width -1))
          (send dc draw-line 
                (+ x-offset margin 1) (+ y-offset margin (* i cell-size))
                (+ x-offset margin board-width -1) (+ y-offset margin (* i cell-size))))
        )
      
      (send dc set-font (make-font #:size 14))
      (send dc set-text-foreground "blue")
      (send dc draw-text "测试3：不同线条粗细参数对比" 20 20)
      (send dc draw-text "上方四个小棋盘展示了不同参数的效果" 20 45)
      (send dc draw-text "请观察哪种效果最符合您的期望" 20 70))
    
    ;; 显示窗口
    (send this show #t)))

;; 运行详细测试
(define (run-detailed-board-test)
  (new detailed-board-test-frame%))

(run-detailed-board-test)