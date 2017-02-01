;; -*- coding: utf-8 -*-
;;
;; 人形モデル0101
;; 2017-2-1
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

;; 人形モデル0101(頭に原点あり。高さ100に固定)
;;   type  タイプ(=0:自分,=1:敵)
;;   pose  ポーズ(=0:通常,=1:前進,=2:後退,=3:やられ,=4:パンチ,=5:キック)
(define (model0101 type pose)
  ;; 色設定
  (case type
    ((0) (gl-material GL_FRONT GL_DIFFUSE #f32(1.0 1.0 1.0 1.0)))
    ((1) (gl-material GL_FRONT GL_DIFFUSE #f32(1.0 1.0 0.0 1.0)))
    )
  ;; 頭
  (gl-push-matrix)
  (case type
    ((0) (gl-translate 0 -10 0) (glut-solid-sphere 10 20 20))
    ((1) (box 8 8 8))
    )
  (gl-pop-matrix)
  ;; 胴体
  (gl-translate 0 -20 0)
  (box 10 20 10)
  ;; 右手
  (gl-push-matrix)
  (gl-translate -15 0 0)
  (case pose
    ((0) (gl-rotate -15  0 0 1))
    ((1) (gl-rotate -25  1 0 1))
    ((2) (gl-rotate -25 -1 0 1))
    ((3) (gl-rotate -25  1 0 1))
    ((4) (gl-translate 0 -5 10) (gl-rotate -135 1 0 0.1))
    ((5) (gl-rotate -25 -1 0 1))
    )
  (cylinder 4 20 20)
  (gl-pop-matrix)
  ;; 左手
  (gl-push-matrix)
  (gl-translate  15 0 0)
  (case pose
    ((0) (gl-rotate  15  0 0 1))
    ((1) (gl-rotate  25  1 0 1))
    ((2) (gl-rotate  25 -1 0 1))
    ((3) (gl-rotate  25 -1 0 1))
    ((4) (gl-rotate  25  1 0 1))
    ((5) (gl-rotate  25 -1 0 1))
    )
  (cylinder 4 20 20)
  (gl-pop-matrix)
  ;; 右足
  (gl-push-matrix)
  (gl-translate -5 -40 0)
  (case pose
    ((2) (gl-rotate -20  1 0 0))
    ((3) (gl-rotate -35  1 0 0.1))
    ((5) (gl-translate 0 0 10) (gl-rotate -100 1 0 0))
    )
  (cylinder 4 20 20)
  (gl-pop-matrix)
  ;; 左足
  (gl-push-matrix)
  (gl-translate  5 -40 0)
  (case pose
    ((1) (gl-rotate  30  1 0 0))
    ((3) (gl-rotate  35 -1 0 0.1))
    ((4) (gl-rotate  40  1 0 0))
    ((5) (gl-rotate  40  1 0 0))
    )
  (cylinder 4 20 20)
  (gl-pop-matrix)
  )

;;
;; 以下はモデルビューワー用
;;

(define *model-name*           "model0101")
(define *model-text-vec-A*     (make-vector  5 ""))
(define *model-text-vec-B*     (make-vector  5 ""))
(define *model-para-vec*       (make-vector 10 0))
(define *model-para-vec*       (make-vector 10 0))
(set! (~ *model-text-vec-A* 0) "  type : 0")
(set! (~ *model-text-vec-A* 1) "  pose : 0 (normal)")
(set! (~ *model-text-vec-B* 0) "[z] : change type")
(set! (~ *model-text-vec-B* 1) "[space] : change pose")
(set! (~ *model-para-vec*   0) 0) ; type
(set! (~ *model-para-vec*   1) 0) ; pose

;; モデルの表示
(define (viewer-disp)
  (model0101 (~ *model-para-vec* 0) (~ *model-para-vec* 1)))

;; キー入力ON
(define (viewer-keyboard)
  (when (key-on? *ksinfo* '(#\z #\Z))
    (inc! (~ *model-para-vec* 0))
    (if (> (~ *model-para-vec* 0) 1) (set! (~ *model-para-vec* 0) 0))
    (set! (~ *model-text-vec-A* 0)
          (format "  type : ~d" (~ *model-para-vec* 0)))
    )
  (when (key-on? *ksinfo* #\space)
    (inc! (~ *model-para-vec* 1))
    (if (> (~ *model-para-vec* 1) 5) (set! (~ *model-para-vec* 1) 0))
    (set! (~ *model-text-vec-A* 1)
          (string-append
           "  pose : "
           (case (~ *model-para-vec* 1)
             ((0) "0 (normal)")
             ((1) "1 (foreward)")
             ((2) "2 (backword)")
             ((3) "3 (down)")
             ((4) "4 (punch)")
             ((5) "5 (kick)"))))
    )
  )

