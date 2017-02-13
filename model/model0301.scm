;; -*- coding: utf-8 -*-
;;
;; モデル0301(簡易飛行機)
;; 2017-2-13
;;

;; 直方体(上面に原点あり)
(define (box x y z)
  (define f32 f32vector)
  (define c1  (- x))
  (define c2  (* -2 y))
  (define c3  (- z))
  (let ((vertex (vector (f32 x 0  z) (f32 x c2  z) (f32 c1 c2  z) (f32 c1 0  z)
                        (f32 x 0 c3) (f32 x c2 c3) (f32 c1 c2 c3) (f32 c1 0 c3)))
        (face   #(#(0 1 2 3) #(0 4 5 1) #(1 5 6 2) #(2 6 7 3) #(3 7 4 0) #(4 7 6 5)))
        (normal #(#f32( 0 0 1) #f32(1 0 0) #f32(0 -1  0)
                  #f32(-1 0 0) #f32(0 1 0) #f32(0  0 -1))))
    (gl-begin GL_QUADS)
    (do ((i 0 (+ i 1)))
        ((>= i 6) #f)
      (gl-normal (~ normal i))
      (do ((j 0 (+ j 1)))
          ((>= j 4) #f)
        (gl-vertex (~ vertex (~ face i j)))))
    (gl-end)))

;; モデル0301(簡易飛行機)(胴体と羽の交点に原点あり)(胴体の長さ80)
(define (model0301)
  ;; 色設定
  (gl-material GL_FRONT GL_DIFFUSE #f32(1.0 1.0 0.0 1.0))
  ;; 胴体
  (gl-push-matrix)
  (gl-translate 0 30 0)
  (box 10 40 10)
  (gl-pop-matrix)
  ;; 羽
  (gl-push-matrix)
  (gl-translate 0 10 0)
  (box 36 10 10)
  (gl-pop-matrix)
  )

;;
;; 以下はモデルビューワー用
;;

(define *model-name* "model0301")

;; モデルの表示
(define (viewer-disp)
  (model0301))

