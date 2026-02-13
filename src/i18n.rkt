#lang racket

(provide set-lang!
         get-lang
         tr)

(define current-lang (make-parameter 'zh))

(define translations
  (hash
   'zh
   (hash
    'window-title "围棋游戏"
    'window-title-test "围棋游戏 - 测试模式"
    'menu-file "文件"
    'menu-help "帮助"
    'menu-language "语言"
    'menu-new-game "新游戏"
    'menu-open-sgf "打开SGF..."
    'menu-save-sgf "保存SGF..."
    'menu-open-custom "打开自定义格式..."
    'menu-save-custom "保存自定义格式..."
    'menu-exit "退出"
    'menu-about "关于"
    'menu-lang-zh "中文"
    'menu-lang-en "英文"
    'label-player-black-turn "黑棋行棋"
    'label-player-turn "~a棋行棋"
    'label-replay-mode "复盘模式"
    'label-move-count "手数: ~a"
    'label-move-count-replay "手数: ~a/~a"
    'label-black-captured "黑棋提子: ~a"
    'label-white-captured "白棋提子: ~a"
    'button-pass "Pass"
    'button-resign "认输"
    'button-undo "悔棋"
    'button-replay "复盘"
    'button-end-replay "结束"
    'button-situation "局势"
    'msg-title-tip "提示"
    'msg-title-error "错误"
    'msg-title-success "成功"
    'msg-title-game-over "游戏结束"
    'msg-title-about "关于"
    'msg-title-situation "局势"
    'msg-replay-cannot-place "当前为复盘模式，不能落子"
    'msg-replay-cannot-pass "复盘模式下不能Pass"
    'msg-replay-cannot-resign "复盘模式下不能认输"
    'msg-replay-cannot-undo "复盘模式下不能悔棋"
    'msg-load-failed "加载失败: ~a"
    'msg-sgf-saved "棋谱已保存"
    'msg-custom-saved "棋谱已保存"
    'msg-about-text "围棋游戏 v1.0\n使用Racket语言开发\n支持中国围棋规则"
    'msg-confirm-resign-title "确认认输"
    'msg-confirm-resign-body "确定要认输吗？"
    'choice-confirm "确定"
    'choice-cancel "取消"
    'msg-resign-winner "~a棋获胜！"
    'msg-game-over-score "游戏结束！~a胜~a目"
    'msg-situation-black "黑棋: ~a 目"
    'msg-situation-white "白棋: ~a 目"
    'msg-situation-diff "差距: ~a"
    'msg-lead-black "黑棋领先 ~a 目"
    'msg-lead-white "白棋领先 ~a 目"
    'msg-lead-even "双方平局"
    'color-black "黑"
    'color-white "白"
    'player-black "黑棋"
    'player-white "白棋"
    'player-draw "和棋"
    'msg-game-over-draw "游戏结束！双方平局")
   'en
   (hash
    'window-title "Go Game"
    'window-title-test "Go Game - Test Mode"
    'menu-file "File"
    'menu-help "Help"
    'menu-language "Language"
    'menu-new-game "New Game"
    'menu-open-sgf "Open SGF..."
    'menu-save-sgf "Save SGF..."
    'menu-open-custom "Open Custom Format..."
    'menu-save-custom "Save Custom Format..."
    'menu-exit "Exit"
    'menu-about "About"
    'menu-lang-zh "Chinese"
    'menu-lang-en "English"
    'label-player-black-turn "Black to move"
    'label-player-turn "~a to move"
    'label-replay-mode "Replay Mode"
    'label-move-count "Moves: ~a"
    'label-move-count-replay "Moves: ~a/~a"
    'label-black-captured "Black captures: ~a"
    'label-white-captured "White captures: ~a"
    'button-pass "Pass"
    'button-resign "Resign"
    'button-undo "Undo"
    'button-replay "Replay"
    'button-end-replay "Exit"
    'button-situation "Position"
    'msg-title-tip "Tip"
    'msg-title-error "Error"
    'msg-title-success "Success"
    'msg-title-game-over "Game Over"
    'msg-title-about "About"
    'msg-title-situation "Position"
    'msg-replay-cannot-place "Cannot place stones in replay mode."
    'msg-replay-cannot-pass "Cannot pass in replay mode."
    'msg-replay-cannot-resign "Cannot resign in replay mode."
    'msg-replay-cannot-undo "Cannot undo in replay mode."
    'msg-load-failed "Load failed: ~a"
    'msg-sgf-saved "Game record saved."
    'msg-custom-saved "Game record saved."
    'msg-about-text "Go Game v1.0\nBuilt with Racket\nSupports Chinese rules"
    'msg-confirm-resign-title "Confirm Resign"
    'msg-confirm-resign-body "Are you sure you want to resign?"
    'choice-confirm "Confirm"
    'choice-cancel "Cancel"
    'msg-resign-winner "~a wins!"
    'msg-game-over-score "Game over! ~a wins by ~a points"
    'msg-situation-black "Black: ~a points"
    'msg-situation-white "White: ~a points"
    'msg-situation-diff "Difference: ~a"
    'msg-lead-black "Black leads by ~a points"
    'msg-lead-white "White leads by ~a points"
    'msg-lead-even "Even"
    'color-black "Black"
    'color-white "White"
    'player-black "Black"
    'player-white "White"
    'player-draw "Draw"
    'msg-game-over-draw "Game over! Draw")))

(define (set-lang! lang)
  (unless (member lang '(zh en))
    (error (format "unsupported language: ~a" lang)))
  (current-lang lang))

(define (get-lang)
  (current-lang))

(define (tr key [fallback #f])
  (define lang-table
    (hash-ref translations (current-lang) (hash)))
  (define default-value
    (if fallback
        fallback
        (symbol->string key)))
  (hash-ref lang-table key default-value))
