#lang racket

(provide start-go-game
         start-go-game-with-state)

(require racket/gui)
(require racket/list)
(require json)
(require "board.rkt")
(require "stone.rkt")
(require "game-state.rkt")
(require "game-engine.rkt")
(require "sgf-format.rkt")
(require "custom-format.rkt")
(require "i18n.rkt")

;; 主游戏窗口
(define go-frame%
  (class frame%
    (super-new)
    
    ;; 游戏状态
    (field [game-state (make-initial-game-state)]
           [replay-source-state #f]
           [replay-mode? #f]
           [replay-index 0]
           [selected-pos #f]
           [message-text (tr 'label-player-black-turn "黑棋行棋")]
           [recent-files '()])  ; 最近打开的文件列表
    
    ;; 创建界面组件
    (define menu-bar (new menu-bar% [parent this]))
    (define file-menu (new menu% [label (tr 'menu-file "文件")] [parent menu-bar]))
    (define language-menu (new menu% [label (tr 'menu-language "语言")] [parent menu-bar]))
    (define help-menu (new menu% [label (tr 'menu-help "帮助")] [parent menu-bar]))
    
    ;; 文件菜单项
    (define new-game-item (new menu-item% [label (tr 'menu-new-game "新游戏")] [parent file-menu]
                               [callback (lambda (item event) (new-game))]))
    (define open-sgf-item (new menu-item% [label (tr 'menu-open-sgf "打开SGF...")] [parent file-menu]
                              [callback (lambda (item event) (load-sgf-file))]))
    (define save-sgf-item (new menu-item% [label (tr 'menu-save-sgf "保存SGF...")] [parent file-menu]
                              [callback (lambda (item event) (save-sgf-file))]))
    (define open-custom-item (new menu-item% [label (tr 'menu-open-custom "打开自定义格式...")] [parent file-menu]
                                 [callback (lambda (item event) (load-custom-file))]))
    (define save-custom-item (new menu-item% [label (tr 'menu-save-custom "保存自定义格式...")] [parent file-menu]
                                 [callback (lambda (item event) (save-custom-file))]))
    (new separator-menu-item% [parent file-menu])

    ;; 最近文件子菜单
    (define recent-menu (new menu% [label (tr 'menu-recent-files "最近文件")] [parent file-menu]))
    (define recent-menu-items '())  ; 存储最近文件菜单项

    (new separator-menu-item% [parent file-menu])
    (define exit-item (new menu-item% [label (tr 'menu-exit "退出")] [parent file-menu]
                           [callback (lambda (item event) (send this show #f))]))
    
    ;; 帮助菜单项
    (define about-item (new menu-item% [label (tr 'menu-about "关于")] [parent help-menu]
                            [callback (lambda (item event) (show-about-dialog))]))
    (define lang-zh-item (new menu-item% [label (tr 'menu-lang-zh "中文")] [parent language-menu]
                              [callback (lambda (item event) (switch-language 'zh))]))
    (define lang-en-item (new menu-item% [label (tr 'menu-lang-en "英文")] [parent language-menu]
                              [callback (lambda (item event) (switch-language 'en))]))
    
    ;; 主面板
    (define main-panel (new horizontal-panel% [parent this] [alignment '(center center)]))
    
    ;; 棋盘面板
    (define board-panel (new board-canvas% [parent main-panel] 
                            [game-frame this]))
    
    ;; 控制面板
    (define control-panel (new vertical-panel% [parent main-panel] 
                              [alignment '(center top)]
                              [min-width 140]))
    
    ;; 状态面板 - 垂直布局
    (define status-panel (new vertical-panel% [parent control-panel]
                             [alignment '(center center)]
                             [spacing 5]))
    
    ;; 行棋状态显示
    (define status-message (new message% [parent status-panel] 
                               [label (tr 'label-player-black-turn "黑棋行棋")] ))
    
    ;; 手数显示
    (define move-count-label (new message% [parent status-panel]
                                 [label (format (tr 'label-move-count "手数: ~a") 0)]))
    
    (define black-score-label (new message% [parent status-panel]
                                  [label (format (tr 'label-black-captured "黑棋提子: ~a") 0)]))
    (define white-score-label (new message% [parent status-panel]
                                  [label (format (tr 'label-white-captured "白棋提子: ~a") 0)]))
    
    ;; 控制按钮
    (define button-panel (new vertical-panel% [parent control-panel]
                             [alignment '(center center)]
                             [spacing 5]))
    
    (define pass-button (new button% [parent button-panel] [label (tr 'button-pass "Pass")]
                            [callback (lambda (button event) (do-pass))]))
    (define resign-button (new button% [parent button-panel] [label (tr 'button-resign "认输")]
                              [callback (lambda (button event) (do-resign))]))
    (define undo-button (new button% [parent button-panel] [label (tr 'button-undo "悔棋")]
                            [callback (lambda (button event) (do-undo))]))
    (define situation-button (new button% [parent button-panel] [label (tr 'button-situation "局势")]
                                 [callback (lambda (button event) (show-situation-dialog))]))
    (define replay-button (new button% [parent button-panel] [label (tr 'button-replay "复盘")]
                              [callback (lambda (button event) (toggle-replay-mode))]))

    ;; 复盘控制区（紧凑模式：两两一行）
    (define replay-panel (new vertical-panel% [parent control-panel]
                             [alignment '(center center)]
                             [spacing 2]))
    (define replay-row-1 (new horizontal-panel% [parent replay-panel]
                             [alignment '(center center)]
                             [spacing 3]))
    (define replay-row-2 (new horizontal-panel% [parent replay-panel]
                             [alignment '(center center)]
                             [spacing 3]))
    (define replay-row-3 (new horizontal-panel% [parent replay-panel]
                             [alignment '(center center)]
                             [spacing 3]))

    ;; 第1行：单手前后
    (define prev-move-button (new button% [parent replay-row-1] [label "<"]
                                  [callback (lambda (button event) (replay-go-prev))]))
    (define next-move-button (new button% [parent replay-row-1] [label ">"]
                                  [callback (lambda (button event) (replay-go-next))]))

    ;; 第2行：5手前后
    (define prev-five-move-button (new button% [parent replay-row-2] [label "<<"]
                                       [callback (lambda (button event) (replay-go-prev5))]))
    (define next-five-move-button (new button% [parent replay-row-2] [label ">>"]
                                       [callback (lambda (button event) (replay-go-next5))]))

    ;; 第3行：首手/末手
    (define first-move-button (new button% [parent replay-row-3] [label "|<"]
                                   [callback (lambda (button event) (replay-go-first))]))
    (define last-move-button (new button% [parent replay-row-3] [label ">|"]
                                  [callback (lambda (button event) (replay-go-last))]))
    
    ;; 游戏控制方法
    (define/public (get-game-state) game-state)

    ;; 最近文件管理
    (define recent-files-max 10)  ; 最多保留10个最近文件
    (define recent-files-file 
      (build-path (find-system-path 'home-dir) ".go-rkt-recent-files.json"))  ; 家目录下的隐藏文件

    (define (load-recent-files)
      (if (file-exists? recent-files-file)
          (with-handlers
            ([exn:fail? (lambda (exn) '())])
            (begin
              (define json-data
                (with-input-from-file recent-files-file read-json))
              (if (list? json-data)
                  json-data
                  '())))
          '()))

    (define (save-recent-files)
      (with-handlers
        ([exn:fail? (lambda (exn) (void))])
        (begin
          (define json-data (jsexpr->string recent-files))
          (display-to-file json-data recent-files-file #:exists 'truncate))))

    (define (add-recent-file file-path)
      ;; 将路径转换为字符串
      (define path-str (if (path? file-path) (path->string file-path) file-path))
      ;; 移除已存在的同名文件
      (define filtered (filter (lambda (f) (not (string=? f path-str))) recent-files))
      ;; 添加到列表开头，并限制数量
      (define new-list (cons path-str filtered))
      (set! recent-files (if (> (length new-list) recent-files-max)
                             (take new-list recent-files-max)
                             new-list))
      ;; 保存到文件
      (save-recent-files)
      ;; 更新菜单
      (update-recent-menu))

    (define (update-recent-menu)
      ;; 清除现有菜单项
      (for-each (lambda (item) (send item delete)) recent-menu-items)
      (set! recent-menu-items '())
      ;; 添加新菜单项
      (when (null? recent-files)
        (define empty-item (new menu-item%
                                 [label (tr 'menu-recent-files-empty "无最近文件")]
                                 [parent recent-menu]
                                 [callback (lambda (item event) (void))]))
        (set! recent-menu-items (list empty-item)))
      (for ([file (in-list recent-files)])  ; 最新文件在列表开头，直接显示
        (define file-name (if (string? file)
                              (last (string-split file "/"))
                              file))
        (define file-item (new menu-item%
                                 [label file-name]
                                 [parent recent-menu]
                                 [callback (lambda (item event) (load-recent-file file))]))
        (set! recent-menu-items (append recent-menu-items (list file-item))))
      ;; 如果有最近文件，添加分隔线和清除菜单项
      (when (not (null? recent-files))
        (new separator-menu-item% [parent recent-menu])
        (set! recent-menu-items 
              (append recent-menu-items 
                      (list (new menu-item%
                                 [label (tr 'menu-clear-recent "清除最近文件列表")]
                                 [parent recent-menu]
                                 [callback (lambda (item event) (clear-recent-files))]))))))

    (define (clear-recent-files)
      (set! recent-files '())
      (save-recent-files)
      (update-recent-menu))

    (define (load-recent-file file-path)
      ;; 将路径转换为字符串
      (define path-str (if (path? file-path) (path->string file-path) file-path))
      (with-handlers
        ([exn:fail? (lambda (exn)
                     (send-message-box
                      (tr 'msg-title-error "错误")
                      (format (tr 'msg-load-failed "加载失败: ~a") (exn-message exn))))])
        (if (file-exists? path-str)
            (let ([loaded-state
                    (cond
                      [(string-suffix? path-str ".sgf") (sgf-load-game path-str)]
                      [(string-suffix? path-str ".txt") (custom-load-game path-str)]
                      [else
                       (send-message-box (tr 'msg-title-error "错误")
                                         (tr 'msg-unsupported-file "不支持的文件格式"))
                       #f])])
              (when replay-mode?
                (exit-replay-mode))
              (when loaded-state
                (set! game-state loaded-state)
                (add-recent-file path-str)  ; 重新添加以更新顺序
                (update-display)))
            (begin
              ;; 文件不存在，从列表中移除并提示用户
              (set! recent-files (filter (lambda (f) (not (string=? f path-str))) recent-files))
              (save-recent-files)
              (update-recent-menu)
              (send-message-box
               (tr 'msg-title-error "错误")
               (format (tr 'msg-file-not-found "文件不存在，已从列表中移除: ~a") path-str))))))

    ;; 初始化最近文件列表
    (set! recent-files (load-recent-files))

    (define (player-name color)
      (if (eq? color 'black)
          (tr 'color-black "黑")
          (tr 'color-white "白")))

    (define (winner-name color)
      (cond
        [(eq? color 'black) (tr 'player-black "黑棋")]
        [(eq? color 'white) (tr 'player-white "白棋")]
        [else (tr 'player-draw "和棋")]))

    (define (refresh-i18n)
      (send this set-label (tr 'window-title "围棋游戏"))
      (send file-menu set-label (tr 'menu-file "文件"))
      (send help-menu set-label (tr 'menu-help "帮助"))
      (send language-menu set-label (tr 'menu-language "语言"))
      (send new-game-item set-label (tr 'menu-new-game "新游戏"))
      (send open-sgf-item set-label (tr 'menu-open-sgf "打开SGF..."))
      (send save-sgf-item set-label (tr 'menu-save-sgf "保存SGF..."))
      (send open-custom-item set-label (tr 'menu-open-custom "打开自定义格式..."))
      (send save-custom-item set-label (tr 'menu-save-custom "保存自定义格式..."))
      (send recent-menu set-label (tr 'menu-recent-files "最近文件"))
      (send exit-item set-label (tr 'menu-exit "退出"))
      (send about-item set-label (tr 'menu-about "关于"))
      (send lang-zh-item set-label (tr 'menu-lang-zh "中文"))
      (send lang-en-item set-label (tr 'menu-lang-en "英文"))
      (send pass-button set-label (tr 'button-pass "Pass"))
      (send resign-button set-label (tr 'button-resign "认输"))
      (send undo-button set-label (tr 'button-undo "悔棋"))
      (send situation-button set-label (tr 'button-situation "局势"))
      (send replay-button set-label
            (if replay-mode?
                (tr 'button-end-replay "结束")
                (tr 'button-replay "复盘")))
      (update-recent-menu)
      (update-display))

    (define (switch-language lang)
      (set-lang! lang)
      (refresh-i18n))
    
    (define/public (set-game-state new-state)
      (set! game-state new-state)
      (when replay-mode?
        (set! replay-source-state new-state)
        (set! replay-index (length (game-state-move-history new-state))))
      (update-display))
    
    (define/public (update-message msg)
      (set! message-text msg)
      (send status-message set-label msg))
    
    (define/public (update-scores)
      (define black-captured (game-state-get-captured-count game-state 'black))
      (define white-captured (game-state-get-captured-count game-state 'white))
      (send black-score-label set-label (format (tr 'label-black-captured "黑棋提子: ~a") black-captured))
      (send white-score-label set-label (format (tr 'label-white-captured "白棋提子: ~a") white-captured)))
    
    ;; 新增：更新手数显示
    (define/public (update-move-count)
      (if replay-mode?
          (send move-count-label set-label
                (format (tr 'label-move-count-replay "手数: ~a/~a")
                        replay-index
                        (replay-total-moves)))
          (send move-count-label set-label
                (format (tr 'label-move-count "手数: ~a") (length (game-state-move-history game-state))))))

    (define/public (handle-board-click pos)
      (when replay-mode?
        (send-message-box (tr 'msg-title-tip "提示") (tr 'msg-replay-cannot-place "当前为复盘模式，不能落子")))
      (when (and (not replay-mode?) pos (not (game-is-game-over? game-state)))
        (with-handlers
          ([exn:fail? (lambda (exn)
                       (send-message-box (tr 'msg-title-error "错误") (exn-message exn)))])
          (define new-state (game-make-move game-state pos))
          (set! game-state new-state)
          (update-display)
          (check-game-end))))
    
    ;; 内部方法
    (define (update-display)
      (send board-panel refresh)
      (update-scores)
      (update-move-count)  ; 更新手数显示
      (if replay-mode?
          (update-message (tr 'label-replay-mode "复盘模式"))
          (let ([current-player (game-get-current-player game-state)])
            (update-message (format (tr 'label-player-turn "~a棋行棋")
                                    (player-name current-player)))))
      (update-replay-controls))
    
    (define (check-game-end)
      (when (game-is-game-over? game-state)
        (define winner (game-get-winner game-state))
        (define score-result (game-get-score game-state))
        (define msg 
          (if (eq? winner 'draw)
              (tr 'msg-game-over-draw "游戏结束！双方平局")
              (format (tr 'msg-game-over-score "游戏结束！~a胜~a目")
                      (winner-name winner)
                      (abs (list-ref score-result 5)))))
        (send-message-box (tr 'msg-title-game-over "游戏结束") msg)))
    
    (define (new-game)
      (when replay-mode?
        (exit-replay-mode))
      (set! game-state (make-initial-game-state))
      (set! selected-pos #f)
      (update-display))

    (define (do-pass)
      (when replay-mode?
        (send-message-box (tr 'msg-title-tip "提示")
                          (tr 'msg-replay-cannot-pass "复盘模式下不能Pass")))
      (when (and (not replay-mode?) (not (game-is-game-over? game-state)))
        (set! game-state (game-pass game-state))
        (update-display)
        (check-game-end)))

    (define (do-resign)
      (when replay-mode?
        (send-message-box (tr 'msg-title-tip "提示")
                          (tr 'msg-replay-cannot-resign "复盘模式下不能认输")))
      (when (and (not replay-mode?) (not (game-is-game-over? game-state)))
        (define confirm-choice (tr 'choice-confirm "确定"))
        (define cancel-choice (tr 'choice-cancel "取消"))
        (define result
          (get-choice (tr 'msg-confirm-resign-title "确认认输")
                      (tr 'msg-confirm-resign-body "确定要认输吗？")
                      (list confirm-choice cancel-choice)))
        (when (string=? result confirm-choice)
          (set! game-state (game-resign game-state))
          (update-display)
          (define winner (game-get-winner game-state))
          (send-message-box (tr 'msg-title-game-over "游戏结束")
                            (format (tr 'msg-resign-winner "~a棋获胜！")
                                    (player-name winner))))))
    
    (define (do-undo)
      (if replay-mode?
          (send-message-box (tr 'msg-title-tip "提示")
                            (tr 'msg-replay-cannot-undo "复盘模式下不能悔棋"))
          (begin
            (set! game-state (game-undo game-state))
            (update-display))))
    
    (define (load-sgf-file)
      (define file-path (get-file (tr 'menu-open-sgf "打开SGF...") this #f #f "sgf"))
      (when file-path
        (with-handlers
          ([exn:fail? (lambda (exn)
                       (send-message-box
                        (tr 'msg-title-error "错误")
                        (format (tr 'msg-load-failed "加载失败: ~a") (exn-message exn))))])
          (when replay-mode?
            (exit-replay-mode))
          (define loaded-state (sgf-load-game file-path))
          (set! game-state loaded-state)
          (add-recent-file file-path)
          (update-display))))
    
    (define (save-sgf-file)
      (define file-path (put-file (tr 'menu-save-sgf "保存SGF...") this #f "game.sgf" #f '()))
      (when file-path
        (sgf-save-game (if replay-mode? replay-source-state game-state) file-path)
        (send-message-box (tr 'msg-title-success "成功")
                          (tr 'msg-sgf-saved "棋谱已保存"))))
    
    (define (load-custom-file)
      (define file-path (get-file (tr 'menu-open-custom "打开自定义格式...") this #f #f "txt"))
      (when file-path
        (with-handlers
          ([exn:fail? (lambda (exn)
                       (send-message-box
                        (tr 'msg-title-error "错误")
                        (format (tr 'msg-load-failed "加载失败: ~a") (exn-message exn))))])
          (when replay-mode?
            (exit-replay-mode))
          (define loaded-state (custom-load-game file-path))
          (set! game-state loaded-state)
          (add-recent-file file-path)
          (update-display))))
    
    (define (save-custom-file)
      (define file-path (put-file (tr 'menu-save-custom "保存自定义格式...") this #f "game.txt" #f '()))
      (when file-path
        (custom-save-game (if replay-mode? replay-source-state game-state) file-path)
        (send-message-box (tr 'msg-title-success "成功")
                          (tr 'msg-custom-saved "棋谱已保存"))))

    (define (replay-total-moves)
      (if replay-source-state
          (length (game-state-move-history replay-source-state))
          0))

    (define (state-at-move-index source-state move-index)
      (define total (length (game-state-move-history source-state)))
      (define target (max 0 (min move-index total)))
      (define ordered-moves (reverse (game-state-move-history source-state))) ; 从首手到末手
      (define rebuilt
        (for/fold ([st (make-initial-game-state)])
                  ([mv (in-list (take ordered-moves target))])
          (if (move-position mv)
              (game-make-move st (move-position mv))
              (game-pass st))))
      rebuilt)

    (define (enter-replay-mode)
      (set! replay-mode? #t)
      (set! replay-source-state game-state)
      (set! replay-index (length (game-state-move-history replay-source-state)))
      (send replay-button set-label (tr 'button-end-replay "结束"))
      (send first-move-button show #t)
      (send prev-five-move-button show #t)
      (send prev-move-button show #t)
      (send next-move-button show #t)
      (send next-five-move-button show #t)
      (send last-move-button show #t)
      (update-display))

    (define (exit-replay-mode)
      (when replay-source-state
        (set! game-state replay-source-state))
      (set! replay-mode? #f)
      (set! replay-index 0)
      (set! replay-source-state #f)
      (send replay-button set-label (tr 'button-replay "复盘"))
      (send first-move-button show #f)
      (send prev-five-move-button show #f)
      (send prev-move-button show #f)
      (send next-move-button show #f)
      (send next-five-move-button show #f)
      (send last-move-button show #f)
      (update-display))

    (define (toggle-replay-mode)
      (if replay-mode?
          (exit-replay-mode)
          (enter-replay-mode)))

    (define (set-replay-index! idx)
      (when replay-mode?
        (define total (replay-total-moves))
        (define new-index (max 0 (min idx total)))
        (set! replay-index new-index)
        (set! game-state (state-at-move-index replay-source-state new-index))
        (update-display)))

    (define (replay-go-first)
      (set-replay-index! 0))

    (define (replay-go-prev)
      (set-replay-index! (sub1 replay-index)))

    (define (replay-go-prev5)
      (set-replay-index! (- replay-index 5)))

    (define (replay-go-next)
      (set-replay-index! (add1 replay-index)))

    (define (replay-go-next5)
      (set-replay-index! (+ replay-index 5)))

    (define (replay-go-last)
      (set-replay-index! (replay-total-moves)))

    (define (update-replay-controls)
      (send pass-button enable (not replay-mode?))
      (send resign-button enable (not replay-mode?))
      (send undo-button enable (not replay-mode?))
      (define total (replay-total-moves))
      (send first-move-button enable (and replay-mode? (> replay-index 0)))
      (send prev-five-move-button enable (and replay-mode? (> replay-index 0)))
      (send prev-move-button enable (and replay-mode? (> replay-index 0)))
      (send next-move-button enable (and replay-mode? (< replay-index total)))
      (send next-five-move-button enable (and replay-mode? (< replay-index total)))
      (send last-move-button enable (and replay-mode? (< replay-index total))))
    
    (define (show-about-dialog)
      (send-message-box (tr 'msg-title-about "关于")
                        (tr 'msg-about-text "围棋游戏 v1.0\n使用Racket语言开发\n支持中国围棋规则")))

    (define (show-situation-dialog)
      (define score-result (game-get-score game-state))
      (define black-total (list-ref score-result 1))
      (define white-total (list-ref score-result 3))
      (define difference (list-ref score-result 5))
      (define lead-message
        (cond
          [(> difference 0) (format (tr 'msg-lead-black "黑棋领先 ~a 目") difference)]
          [(< difference 0) (format (tr 'msg-lead-white "白棋领先 ~a 目") (abs difference))]
          [else (tr 'msg-lead-even "双方平局")]))
      (send-message-box
       (tr 'msg-title-situation "局势")
       (format "~a\n~a\n~a"
               (format (tr 'msg-situation-black "黑棋: ~a 目") black-total)
               (format (tr 'msg-situation-white "白棋: ~a 目") white-total)
               (format (tr 'msg-situation-diff "差距: ~a")
                       lead-message))))
    
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

    ;; 默认隐藏复盘控制条
    (send first-move-button show #f)
    (send prev-five-move-button show #f)
    (send prev-move-button show #f)
    (send next-move-button show #f)
    (send next-five-move-button show #f)
    (send last-move-button show #f)

    ;; 初始化最近文件菜单
    (update-recent-menu)

    ;; 初始化显示
    (refresh-i18n)))

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
            (send dc draw-ellipse (+ x (- stone-radius 10)) (+ y (- stone-radius 10))
                  (ceiling (/ stone-radius 5)) (ceiling (/ stone-radius 5)))
                  )))
    ))

;; 启动游戏函数
(define (start-go-game)
  (define frame (new go-frame% 
                    [label (tr 'window-title "围棋游戏")]
                    [width 700] 
                    [height 600]))
  (send frame show #t))

;; 启动带预设状态的游戏函数（用于测试）
(define (start-go-game-with-state initial-state)
  (define frame (new go-frame% 
                    [label (tr 'window-title-test "围棋游戏 - 测试模式")]
                    [width 700] 
                    [height 600]))
  ;; 设置初始状态
  (send frame set-game-state initial-state)
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
