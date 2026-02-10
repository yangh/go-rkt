# 棋子平滑绘制改进说明

## 问题描述
原有的棋子绘制存在边缘锯齿问题，视觉效果不够平滑。

## 改进方案

### 1. 启用抗锯齿
在 `draw-stone` 函数中添加了抗锯齿设置：
```racket
(send dc set-smoothing 'aligned)
```

### 2. 增强视觉效果
为黑白棋子分别添加了不同的视觉增强效果：

**黑棋效果：**
- 主体：纯黑色填充
- 高光：浅灰色圆形高光，营造立体感

**白棋效果：**
- 主体：纯白色填充，黑色边框
- 阴影：浅灰色内阴影，增加层次感

### 3. 技术实现
```racket
(define (draw-stone dc x y color)
  ;; 启用抗锯齿平滑绘制
  (send dc set-smoothing 'aligned)
  
  (if (eq? color 'black)
      ;; 黑棋：主体+高光
      (begin
        (send dc set-brush "black" 'solid)
        (send dc set-pen "black" 1 'solid)
        (send dc draw-ellipse (- x stone-radius) (- y stone-radius) 
              (* 2 stone-radius) (* 2 stone-radius))
        ;; 添加高光效果
        (send dc set-brush "gray" 'solid)
        (send dc set-pen "gray" 1 'transparent)
        (send dc draw-ellipse (- x (- stone-radius 4)) (- y (- stone-radius 4)) 
              (- (* 2 (- stone-radius 4)) 2) (- (* 2 (- stone-radius 4)) 2)))
      ;; 白棋：主体+阴影
      (begin
        (send dc set-brush "white" 'solid)
        (send dc set-pen "black" 1 'solid)
        (send dc draw-ellipse (- x stone-radius) (- y stone-radius) 
              (* 2 stone-radius) (* 2 stone-radius))
        ;; 添加轻微阴影效果
        (send dc set-brush "lightgray" 'solid)
        (send dc set-pen "lightgray" 1 'transparent)
        (send dc draw-ellipse (+ (- x stone-radius) 2) (+ (- y stone-radius) 2) 
              (- (* 2 stone-radius) 4) (- (* 2 stone-radius) 4)))))
```

## 效果对比

### 改进前：
- 棋子边缘有明显锯齿
- 视觉效果较为平面化
- 缺乏立体感

### 改进后：
- 棋子边缘平滑无锯齿
- 黑棋有高光效果，显得更有质感
- 白棋有阴影效果，层次更加丰富
- 整体视觉体验显著提升

## 测试验证
提供了 `tests/smooth-stone-test.rkt` 测试程序，可以直观地观察改进效果。