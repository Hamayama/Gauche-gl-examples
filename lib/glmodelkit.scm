;; -*- coding: utf-8 -*-
;;
;; glmodelkit.scm
;; 2022-4-23 v1.05
;;
;; ＜内容＞
;;   Gauche-gl を使って基本的なモデルの生成を行うためのモジュールです。
;;
(define-module glmodelkit
  (use gl)
  (use gl.glut)
  (use gauche.uvector)
  (use math.const)
  (export
    box-model cylinder ellipsoid cross-cursor
    ))
(select-module glmodelkit)

;; 直方体(上面に原点あり)
;; (box という手続きが別に存在していたので名前を box-model に変更)
;;   x  X軸方向の幅/2
;;   y  Y軸方向の幅/2
;;   z  Z軸方向の幅/2
;(define (box x y z)
(define (box-model x y z)
  (define f32 f32vector)
  (define c1  (- x))
  (define c2  (* -2 y))
  (define c3  (- z))
  (let ((vertex (vector (f32 x 0  z) (f32 x c2  z) (f32 c1 c2  z) (f32 c1 0  z)
                        (f32 x 0 c3) (f32 x c2 c3) (f32 c1 c2 c3) (f32 c1 0 c3)))
        (face   #(#(0 1 2 3) #(0 4 5 1) #(1 5 6 2) #(2 6 7 3) #(3 7 4 0) #(4 7 6 5)))
        (normal #(#f32( 0 0 1) #f32(1 0 0) #f32(0 -1  0)
                  #f32(-1 0 0) #f32(0 1 0) #f32(0  0 -1))))
    (do ((i 0 (+ i 1)))
        ((>= i 6) #f)
      (gl-begin GL_TRIANGLE_FAN)
      (gl-normal (~ normal i))
      (do ((j 0 (+ j 1)))
          ((>= j 4) #f)
        (gl-vertex (~ vertex (~ face i j))))
      (gl-end))))

;; 円柱(上面に原点あり)
;;   r  円の半径
;;   h  円柱の高さ/2
;;   s  円の分割数
(define (cylinder r h s)
  (define f32  f32vector)
  (define step (/. 2pi s))
  (define c1   (* -2 h))
  ;; 上面
  (gl-begin GL_TRIANGLE_FAN)
  (gl-normal #f32(0 1 0))
  (do ((i 0 (+ i 1))
       (angle 0 (+ angle step)))
      ((>= i s) #f)
    (gl-vertex (f32 (* r (cos angle)) 0 (* r (sin angle)))))
  (gl-end)
  ;; 底面
  (gl-begin GL_TRIANGLE_FAN)
  (gl-normal #f32(0 -1 0))
  (do ((i 0 (+ i 1))
       (angle 0 (- angle step)))
      ((>= i s) #f)
    (gl-vertex (f32 (* r (cos angle)) c1 (* r (sin angle)))))
  (gl-end)
  ;; 側面
  (gl-begin GL_TRIANGLE_STRIP)
  (do ((i 0 (+ i 1))
       (angle 0 (if (< (+ i 1) s) (+ angle step) 0)))
      ((> i s) #f)
    (let ((x (cos angle))
          (z (sin angle)))
      (gl-normal (f32 x 0 z))
      (gl-vertex (f32 (* r x) 0  (* r z)))
      (gl-vertex (f32 (* r x) c1 (* r z)))))
  (gl-end))

;; 楕円球(中心に原点あり)
;;   r      半径1(幅/2)
;;   h      半径2(高さ/2)
;;   slice  y軸のまわりの分割数
;;   stack  y軸に垂直な分割数
(define (ellipsoid r h slice stack)
  (define f32 f32vector)
  (define s1 stack)
  (define s2 slice)
  (define step1 (/.  pi s1))
  (define step2 (/. 2pi s2))
  (define vertex (make-vector (* (+ s1 1) (+ s2 1))))
  (define normal (make-vector (* (+ s1 1) (+ s2 1))))
  ;; 座標計算
  (let ((idx 0) (r1 0) (x1 0) (y1 0) (z1 0))
    (do ((i 0 (+ i 1))
         (angle1 0 (if (< (+ i 1) s1) (+ angle1 step1) pi)))
        ((> i s1) #f)
      (set! y1 (cos angle1))
      (set! r1 (sin angle1))
      (do ((j 0 (+ j 1))
           (angle2 0 (if (< (+ j 1) s2) (+ angle2 step2) 0)))
          ((> j s2) #f)
        (set! x1 (* r1 (cos angle2)))
        (set! z1 (* r1 (sin angle2)))
        (set! (~ vertex idx) (f32 (* r x1) (* h y1) (* r z1)))
        (set! (~ normal idx) (f32 x1 y1 z1))
        (inc! idx))))
  ;; 球面
  (let ((idx 0) (idx2 0) (idx3 0) (idx4 0))
    (do ((i 0 (+ i 1)))
        ((>= i s1) #f)
      (gl-begin GL_TRIANGLE_STRIP)
      (do ((j 0 (+ j 1)))
          ((>= j s2) #f)
        (set! idx2 (+ idx s2 1))
        (set! idx3 (+ idx 1))
        (set! idx4 (+ idx s2 2))
        (gl-normal (~ normal idx))
        (gl-vertex (~ vertex idx))
        (gl-normal (~ normal idx2))
        (gl-vertex (~ vertex idx2))
        (gl-normal (~ normal idx3))
        (gl-vertex (~ vertex idx3))
        (gl-normal (~ normal idx4))
        (gl-vertex (~ vertex idx4))
        (inc! idx))
      (inc! idx)
      (gl-end))))

;; 十字カーソル(中心に原点あり)
;;   r カーソルの半径
;;   w カーソルの線の幅
(define (cross-cursor r w)
  (gl-push-matrix)
  (gl-translate 0 r 0)
  (box-model w r w)
  (gl-pop-matrix)
  (gl-push-matrix)
  (gl-translate 0 w 0)
  (box-model r w w)
  (gl-pop-matrix))

