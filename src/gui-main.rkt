#lang racket

(provide start-go-game)

(require racket/gui)
(require "board.rkt")
(require "stone.rkt")
(require "game-state.rkt")
(require "game-engine.rkt")
(require "sgf-format.rkt")
(require "custom-format.rkt")

;; 主游戏窗口
(define go-frame%
  (class frame%
    (super-new)
    
    ;; 游戏状态
    (field [game-state (make-initial-game-state)]
           [selected-pos #f]
           [message-text "黑棋先行"])
    
    ;; 创建界面组件
    (define menu-bar (new menu-bar% [parent this]))
    (define file-menu (new menu% [label "文件"] [parent menu-bar]))
    (define help-menu (new menu% [label "帮助"] [parent menu-bar]))
    
    ;; 文件菜单项
    (new menu-item% [label "新游戏"] [parent file-menu]
         [callback (lambda (item event) (new-game))])
    (new menu-item% [label "打开SGF..."] [parent file-menu]
         [callback (lambda (item event) (load-sgf-file))])
    (new menu-item% [label "保存SGF..."] [parent file-menu]
         [callback (lambda (item event) (save-sgf-file))])
    (new menu-item% [label "打开自定义格式..."] [parent file-menu]
         [callback (lambda (item event) (load-custom-file))])
    (new menu-item% [label "保存自定义格式..."] [parent file-menu]
         [callback (lambda (item event) (save-custom-file))])
    (new separator-menu-item% [parent file-menu])
    (new menu-item% [label "退出"] [parent file-menu]
         [callback (lambda (item event) (send this show #f))])
    
    ;; 帮助菜单项
    (new menu-item% [label "关于"] [parent help-menu]
         [callback (lambda (item event) (show-about-dialog))])
    
    ;; 主面板
    (define main-panel (new horizontal-panel% [parent this] [alignment '(center center)]))
    
    ;; 棋盘面板
    (define board-panel (new board-canvas% [parent main-panel] 
                            [game-frame this]))
    
    ;; 控制面板
    (define control-panel (new vertical-panel% [parent main-panel] 
                              [alignment '(center top)]
                              [min-width 200]))
    
    ;; 状态显示
    (define status-message (new message% [parent control-panel]
                               [label message-text]
                               [min-width 180]
                               [auto-resize #t]))
    
    ;; 分数显示
    (define score-panel (new vertical-panel% [parent control-panel]
                            [border 10]))
    (define black-score-label (new message% [parent score-panel] [label "黑棋: 0"]))
    (define white-score-label (new message% [parent score-panel] [label "白棋: 0"]))
    
    ;; 控制按钮
    (define button-panel (new vertical-panel% [parent control-panel]
                             [alignment '(center center)]
                             [spacing 5]))
    
    (define pass-button (new button% [parent button-panel] [label "Pass"]
                            [callback (lambda (button event) (do-pass))]))
    (define resign-button (new button% [parent button-panel] [label "认输"]
                              [callback (lambda (button event) (do-resign))]))
    (define undo-button (new button% [parent button-panel] [label "悔棋"]
                            [callback (lambda (button event) (do-undo))]))
    
    ;; 游戏控制方法
    (define/public (get-game-state) game-state)
    
    (define/public (set-game-state new-state)
      (set! game-state new-state)
      (update-display))
    
    (define/public (update-message msg)
      (set! message-text msg)
      (send status-message set-label msg))
    
    (define/public (update-scores)
      (define black-captured (game-state-get-captured-count game-state 'black))
      (define white-captured (game-state-get-captured-count game-state 'white))
      (send black-score-label set-label (format "黑棋提子: ~a" black-captured))
      (send white-score-label set-label (format "白棋提子: ~a" white-captured)))
    
    (define/public (handle-board-click pos)
      (when (and pos (not (game-is-game-over? game-state)))
        (with-handlers
          ([exn:fail? (lambda (exn)
                       (send-message-box "错误" (exn-message exn)))])
          (define new-state (game-make-move game-state pos))
          (set! game-state new-state)
          (update-display)
          (check-game-end))))
    
    ;; 内部方法
    (define (update-display)
      (send board-panel refresh)
      (update-scores)
      (define current-player (game-get-current-player game-state))
      (update-message (format "~a棋行棋" (if (eq? current-player 'black) "黑" "白"))))
    
    (define (check-game-end)
      (when (game-is-game-over? game-state)
        (define winner (game-get-winner game-state))
        (define score-result (game-get-score game-state))
        (define msg 
          (format "游戏结束！~a胜~a目" 
                  (if (eq? winner 'black) "黑棋" "白棋")
                  (abs (list-ref score-result 5))))
        (send-message-box "游戏结束" msg)))
    
    (define (new-game)
      (set! game-state (make-initial-game-state))
      (set! selected-pos #f)
      (update-display))
    
    (define (do-pass)
      (when (not (game-is-game-over? game-state))
        (set! game-state (game-pass game-state))
        (update-display)
        (check-game-end)))
    
    (define (do-resign)
      (when (not (game-is-game-over? game-state))
        (define result (get-choice "确认认输" "确定要认输吗？" '("确定" "取消")))
        (when (string=? result "确定")
          (set! game-state (game-resign game-state))
          (update-display)
          (define winner (game-get-winner game-state))
          (send-message-box "游戏结束" (format "~a棋获胜！" 
                                             (if (eq? winner 'black) "黑" "白"))))))
    
    (define (do-undo)
      (set! game-state (game-undo game-state))
      (update-display))
    
    (define (load-sgf-file)
      (define file-path (get-file "选择SGF文件" this #f #f "sgf"))
      (when file-path
        (with-handlers
          ([exn:fail? (lambda (exn)
                       (send-message-box "错误" (format "加载失败: ~a" (exn-message exn))))])
          (define loaded-state (sgf-load-game file-path))
          (set! game-state loaded-state)
          (update-display))))
    
    (define (save-sgf-file)
      (define file-path (put-file "保存SGF文件" this #f "game.sgf" #f '()))
      (when file-path
        (sgf-save-game game-state file-path)
        (send-message-box "成功" "棋谱已保存")))
    
    (define (load-custom-file)
      (define file-path (get-file "选择自定义格式文件" this #f #f "txt"))
      (when file-path
        (with-handlers
          ([exn:fail? (lambda (exn)
                       (send-message-box "错误" (format "加载失败: ~a" (exn-message exn))))])
          (define loaded-state (custom-load-game file-path))
          (set! game-state loaded-state)
          (update-display))))
    
    (define (save-custom-file)
      (define file-path (put-file "保存自定义格式文件" this #f "game.txt" #f '()))
      (when file-path
        (custom-save-game game-state file-path)
        (send-message-box "成功" "棋谱已保存")))
    
    (define (show-about-dialog)
      (send-message-box "关于" 
                       "围棋游戏 v1.0\n使用Racket语言开发\n支持中国围棋规则"))
    
    (define (send-message-box title message)
      (message-box title message this '(ok)))
    
    (define (get-choice title message choices)
      (define dialog (new dialog% [label title] [parent this] [width 300] [height 150]))
      (define msg (new message% [parent dialog] [label message] [vert-margin 10]))
      (define button-panel (new horizontal-panel% [parent dialog] [alignment '(center center)]))
      
      (define result #f)
      (for ([choice choices])
        (new button% [parent button-panel] [label choice]
             [callback (lambda (btn evt)
                        (set! result choice)
                        (send dialog show #f))]))
      
      (send dialog show #t)
      result)
    
    ;; 初始化显示
    (update-display)))

;; 棋盘画布类
(define board-canvas%
  (class canvas%
    (init-field game-frame)
    (super-new [min-width 600] [min-height 600])
    
    (define board-size 19)
    (define cell-size 30)
    (define stone-radius 13)
    (define margin 30)
    
    (define/override (on-event event)
      (when (send event button-down?)
        (define x (send event get-x))
        (define y (send event get-y))
        (define pos (screen-to-board x y))
        (when pos
          (send game-frame handle-board-click pos))))
    
    (define/override (on-paint)
      (define dc (send this get-dc))
      (draw-board dc)
      (draw-stones dc))
    
    (define (screen-to-board x y)
      (define col (round (/ (- x margin) cell-size)))
      (define row (round (/ (- y margin) cell-size)))
      (when (and (>= col 0) (< col board-size) (>= row 0) (< row board-size))
        (position row col)))
    
    (define (draw-board dc)
      ;; 清空画布
      (send dc set-brush "burlywood" 'solid)
      (send dc draw-rectangle 0 0 (send this get-width) (send this get-height))
      
      ;; 绘制网格线
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
      
      ;; 绘制星位点
      (define star-points '((3 3) (3 9) (3 15) (9 3) (9 9) (9 15) (15 3) (15 9) (15 15)))
      (send dc set-brush "black" 'solid)
      (for ([point star-points])
        (define row (car point))
        (define col (cadr point))
        (send dc draw-ellipse 
              (- (+ margin (* col cell-size)) 3) (- (+ margin (* row cell-size)) 3)
              6 6)))
    
    (define (draw-stones dc)
      (define game-state (send game-frame get-game-state))
      (define board (game-state-board game-state))
      
      (for* ([row (in-range board-size)]
             [col (in-range board-size)])
        (define pos (position row col))
        (define stone-color (board-get-stone board pos))
        (when stone-color
          (define x (+ margin (* col cell-size)))
          (define y (+ margin (* row cell-size)))
          (draw-stone dc x y stone-color))))
    
    (define (draw-stone dc x y color)
      ;; 启用抗锯齿平滑绘制
      (send dc set-smoothing 'aligned)
      
      (if (eq? color 'black)
          ;; 绘制黑色棋子（带高光效果）
          (begin
            ;; 主体黑色
            (send dc set-brush "black" 'solid)
            (send dc set-pen "black" 1 'solid)
            (send dc draw-ellipse (- x stone-radius) (- y stone-radius)
                  (* 2 stone-radius) (* 2 stone-radius))
            ;; 添加高光效果
            (send dc set-brush "gray" 'solid)
            (send dc set-pen "gray" 1 'transparent)
            (send dc draw-ellipse (- x (- stone-radius 6)) (- y (- stone-radius 6))
                  (ceiling (/ stone-radius 5)) (ceiling (/ stone-radius 5)))
            )
          ;; 绘制白色棋子（带阴影效果）
          (begin
            ;; 主体白色
            (send dc set-brush "white" 'solid)
            (send dc set-pen "black" 1 'solid)
            (send dc draw-ellipse (- x stone-radius) (- y stone-radius)
                  (* 2 stone-radius) (* 2 stone-radius))
            ;; 添加轻微阴影效果
            (send dc set-brush "lightgray" 'solid)
            (send dc set-pen "lightgray" 1 'transparent)
            (send dc draw-ellipse (- x (- stone-radius 6)) (- y (- stone-radius 6))
                  (ceiling (/ stone-radius 5)) (ceiling (/ stone-radius 5)))
                  )))
    ))

;; 启动游戏函数
(define (start-go-game)
  (define frame (new go-frame% 
                    [label "围棋游戏"] 
                    [width 900] 
                    [height 700]))
  (send frame show #t))

;; 模块测试
(module+ main
  (start-go-game))

(module+ test
  (require rackunit)
  
  ;; 测试基本GUI组件创建
  (define test-frame (new go-frame% [label "测试"] [width 400] [height 300]))
  (check-not-exn (lambda () (send test-frame show #f)))
  
  ;; 测试棋盘画布
  (define test-canvas (new board-canvas% [parent (new frame% [label "Canvas Test"])]
                          [game-frame test-frame]))
  (check-not-exn (lambda () (send test-canvas refresh))))