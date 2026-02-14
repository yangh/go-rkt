#lang racket

;; 围棋游戏主程序
;; 整合所有模块并启动GUI界面

(require racket/path)
(require racket/runtime-path)
(require "sgf-format.rkt")
(require "custom-format.rkt")
(require "i18n.rkt")

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
  (displayln "  --lang <zh|en>      设置界面语言（中文或英文，默认英文）")
  (displayln "  -h, --help          显示帮助信息并退出"))

(struct parsed-args (load-file lang) #:transparent)

(define (parse-args args)
  (let loop ([rest args] [load-file #f] [lang #f])
    (cond
      [(null? rest) (parsed-args load-file lang)]
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
       (loop (cddr rest) (cadr rest) lang)]
      [(string=? (car rest) "--lang")
       (when (null? (cdr rest))
         (eprintf "错误: 参数 --lang 需要指定语言（zh 或 en）\n")
         (print-help)
         (exit 1))
       (define lang-value (cadr rest))
       (unless (member lang-value '("zh" "en"))
         (eprintf "错误: 不支持的语言 ~a（仅支持 zh 或 en）\n" lang-value)
         (print-help)
         (exit 1))
       (loop (cddr rest) load-file (string->symbol lang-value))]
      [else
       (eprintf "错误: 不支持的参数 ~a\n" (car rest))
       (print-help)
       (exit 1)])))

(define args-result
  (parse-args (vector->list (current-command-line-arguments))))

(define load-file (parsed-args-load-file args-result))
(define lang-arg (parsed-args-lang args-result))

;; 设置语言（如果指定了的话）
(when lang-arg
  (set-lang! lang-arg))

(define-runtime-path gui-main-path "gui-main.rkt")

(define start-go-game
  (dynamic-require gui-main-path 'start-go-game))

(define start-go-game-with-state
  (dynamic-require gui-main-path 'start-go-game-with-state))

;; 启动游戏（支持可选的 --load 参数）
(if load-file
    (start-go-game-with-state (load-state-from-file load-file))
    (start-go-game))
