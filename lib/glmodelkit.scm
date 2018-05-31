;; -*- coding: utf-8 -*-
;;
;; glmodelkit.scm
;; 2018-5-31 v1.04
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
    box-model cylinder cross-cursor
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

