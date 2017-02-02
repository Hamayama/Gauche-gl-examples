;; -*- coding: utf-8 -*-
;;
;; 人形モデル0201
;; 2017-2-2
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

;; 円柱(上面に原点あり)
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
  (gl-begin GL_QUAD_STRIP)
  (do ((i 0 (+ i 1))
       (angle 0 (+ angle step)))
      ((> i s) #f)
    (let ((x (cos angle))
          (z (sin angle)))
      (gl-normal (f32 x 0 z))
      (gl-vertex (f32 (* r x) 0  (* r z)))
      (gl-vertex (f32 (* r x) c1 (* r z)))
      ))
  (gl-end))

;; 人形モデル0201(頭に原点あり。高さ100に固定)
;;   pose  ポーズ(=0-7:はばたき)
(define (model0201 pose)
  ;; 色設定
  (gl-material GL_FRONT GL_DIFFUSE #f32(1.0 1.0 0.0 1.0))
  ;; 頭
  (gl-push-matrix)
  (gl-translate   0 -10 0)
  (glut-solid-sphere 10 20 20)
  (gl-pop-matrix)
  ;; 胴体
  (gl-translate   0 -20 0)
  (box 10 20 7)
  ;; 右手
  (gl-push-matrix)
  (gl-translate -14 -13 0)
  (case pose
    ((0)  (gl-rotate   0 0 1 0))
    ((1)  (gl-rotate  30 0 1 0))
    ((2)  (gl-rotate  60 0 1 0))
    ((3)  (gl-rotate  30 0 1 0))
    ((4)  (gl-rotate   0 0 1 0))
    ((5)  (gl-rotate -30 0 1 0))
    ((6)  (gl-rotate -60 0 1 0))
    ((7)  (gl-rotate -30 0 1 0))
    )
  (gl-rotate -90 0 0 1)
  (box 12 22 2)
  (gl-pop-matrix)
  ;; 左手
  (gl-push-matrix)
  (gl-translate  14 -13 0)
  (case pose
    ((0)  (gl-rotate   0 0 1 0))
    ((1)  (gl-rotate -30 0 1 0))
    ((2)  (gl-rotate -60 0 1 0))
    ((3)  (gl-rotate -30 0 1 0))
    ((4)  (gl-rotate   0 0 1 0))
    ((5)  (gl-rotate  30 0 1 0))
    ((6)  (gl-rotate  60 0 1 0))
    ((7)  (gl-rotate  30 0 1 0))
    )
  (gl-rotate  90 0 0 1)
  (box 12 22 2)
  (gl-pop-matrix)
  ;; 右足
  (gl-push-matrix)
  (gl-translate  -5 -40 0)
  (cylinder 4 20 20)
  (gl-pop-matrix)
  ;; 左足
  (gl-push-matrix)
  (gl-translate   5 -40 0)
  (cylinder 4 20 20)
  (gl-pop-matrix)
  )

;;
;; 以下はモデルビューワー用
;;

(define *model-name*           "model0201")
(define *model-text-vec-A*     (make-vector  5 ""))
(define *model-text-vec-B*     (make-vector  5 ""))
(define *model-para-vec*       (make-vector 10 0))
(set! (~ *model-text-vec-A* 0) "  pose : 0")
(set! (~ *model-text-vec-B* 0) "[space] : change pose")
(set! (~ *model-para-vec*   0) 0) ; pose

;; モデルの表示
(define (viewer-disp)
  (model0201 (~ *model-para-vec* 0)))

;; キー入力ON
(define (viewer-keyboard)
  (when (key-on? *ksinfo* '#\space)
    (inc! (~ *model-para-vec* 0))
    (if (> (~ *model-para-vec* 0) 7) (set! (~ *model-para-vec* 0) 0))
    (set! (~ *model-text-vec-A* 0)
          (format "  pose : ~d" (~ *model-para-vec* 0)))
    )
  )

