#lang racket

;; 围棋游戏主程序
;; 整合所有模块并启动GUI界面

(require racket/path)
(require racket/runtime-path)
(require "sgf-format.rkt")
(require "custom-format.rkt")

(define (load-state-from-file file-path)
  (define ext
    (let ([raw-ext (path-get-extension (string->path file-path))])
      (if raw-ext (string-downcase (bytes->string/utf-8 raw-ext)) "")))
  (cond
    [(string=? ext ".sgf") (sgf-load-game file-path)]
    [(string=? ext ".txt") (custom-load-game file-path)]
    [else
     (error
      (format "不支持的文件扩展名: ~a（仅支持 .sgf 和 .txt）" ext))]))

(define (print-help)
  (displayln "用法:")
  (displayln "  racket src/main.rkt [选项]")
  (displayln "")
  (displayln "选项:")
  (displayln "  -l, --load <file>   启动时加载棋局文件（.sgf 或 .txt）")
  (displayln "  -h, --help          显示帮助信息并退出"))

(define (parse-args args)
  (let loop ([rest args] [load-file #f])
    (cond
      [(null? rest) load-file]
      [(or (string=? (car rest) "--help")
           (string=? (car rest) "-h"))
       (print-help)
       (exit 0)]
      [(or (string=? (car rest) "--load")
           (string=? (car rest) "-l"))
       (when (null? (cdr rest))
         (eprintf "错误: 参数 ~a 需要一个文件路径\n" (car rest))
         (print-help)
         (exit 1))
       (loop (cddr rest) (cadr rest))]
      [else
       (eprintf "错误: 不支持的参数 ~a\n" (car rest))
       (print-help)
       (exit 1)])))

(define load-file
  (parse-args (vector->list (current-command-line-arguments))))

(define-runtime-path gui-main-path "gui-main.rkt")

(define start-go-game
  (dynamic-require gui-main-path 'start-go-game))

(define start-go-game-with-state
  (dynamic-require gui-main-path 'start-go-game-with-state))

;; 启动游戏（支持可选的 --load 参数）
(if load-file
    (start-go-game-with-state (load-state-from-file load-file))
    (start-go-game))
