# 棋盘线条视觉改进说明

## 问题描述
原有的棋盘网格线全部使用相同的粗细（1像素），导致视觉上缺乏层次感，边框不够突出。

## 改进方案

### 1. 区分线条粗细
- **边框线**：2像素粗线，更加醒目
- **内部网格线**：1像素细线，保持清晰但不过于突出

### 2. 技术实现
```racket
(define (draw-board dc)
  ;; 清空画布
  (send dc set-brush "burlywood" 'solid)
  (send dc draw-rectangle 0 0 (send this get-width) (send this get-height))
  
  ;; 绘制外边框（粗线）
  (send dc set-pen "black" 2 'solid)
  (define board-width (* (sub1 board-size) cell-size))
  ;; 上边框
  (send dc draw-line margin margin (+ margin board-width) margin)
  ;; 下边框
  (send dc draw-line margin (+ margin board-width) (+ margin board-width) (+ margin board-width))
  ;; 左边框
  (send dc draw-line margin margin margin (+ margin board-width))
  ;; 右边框
  (send dc draw-line (+ margin board-width) margin (+ margin board-width) (+ margin board-width))
  
  ;; 绘制内部网格线（细线）
  (send dc set-pen "black" 1 'solid)
  (for ([i (in-range 1 (sub1 board-size))]) ; 跳过边框位置
    ;; 垂直内部线
    (send dc draw-line 
          (+ margin (* i cell-size)) (+ margin 1)
          (+ margin (* i cell-size)) (+ margin board-width -1))
    ;; 水平内部线
    (send dc draw-line 
          (+ margin 1) (+ margin (* i cell-size))
          (+ margin board-width -1) (+ margin (* i cell-size))))
  
  ;; 绘制星位点...
  )
```

### 3. 视觉效果提升
- **层次分明**：边框与内部网格形成明显的视觉层次
- **焦点突出**：粗边框更好地界定棋盘边界
- **阅读舒适**：细网格线不会干扰棋局观察
- **专业外观**：符合传统围棋棋盘的视觉规范

## 验证方式
运行 `tests/board-grid-test.rkt` 可以直观观察改进效果。

## 兼容性保证
- 保持原有棋盘尺寸和布局不变
- 不影响棋子放置和游戏逻辑
- 所有原有功能完全兼容