;; -*- coding: utf-8 -*-
;;
;; モデル0701(直方体オブジェ)
;; 2024-7-22
;;
(define-module model0701
  (use gl)
  (use gl.glut)
  (use gauche.uvector)
  (use math.const)
  (use glmintool)
  (use glmodelkit)
  (export
    model0701
    model0701-viewer-init
    ))
(select-module model0701)

;; モデル0701(直方体オブジェ)(上面に原点あり)
;;   x     X軸方向の幅/2
;;   y     Y軸方向の幅/2
;;   z     Z軸方向の幅/2
;;   rot   回転角度(度)
(define (model0701 x y z rot)
  (define f32 f32vector)
  (define c1 (- x))
  (define c2 (* -2 y))
  (define c3 (- z))
  (define xe (* x 0.65))
  (define ze (* z 0.65))
  ;; 本体(ワイヤフレーム表示)
  (gl-disable GL_LIGHTING)
  (gl-color 1.0 1.0 1.0 1.0)
  (gl-push-matrix)
  (gl-rotate rot 0 1 0)
  (let ((vertex (vector (f32 x 0  z) (f32 x c2  z) (f32 c1 c2  z) (f32 c1 0  z)
                        (f32 x 0 c3) (f32 x c2 c3) (f32 c1 c2 c3) (f32 c1 0 c3)))
        (face   #(#(0 1 2 3) #(0 4 5 1) #(1 5 6 2) #(2 6 7 3) #(3 7 4 0) #(4 7 6 5)))
        (normal #(#f32( 0 0 1) #f32(1 0 0) #f32(0 -1  0)
                  #f32(-1 0 0) #f32(0 1 0) #f32(0  0 -1))))
    (do ((i 0 (+ i 1)))
        ((>= i 6) #f)
      (gl-begin GL_LINE_LOOP)
      (gl-normal (~ normal i))
      (do ((j 0 (+ j 1)))
          ((>= j 4) #f)
        (gl-vertex (~ vertex (~ face i j))))
      (gl-end)))
  ;; エンジン部分
  (gl-enable GL_LIGHTING)
  (gl-material GL_FRONT GL_DIFFUSE #f32(1.0 0.0 0.0 0.7))
  (gl-translate 0 (+ c2 xe xe) 0)
  (box-model xe xe ze)
  (gl-pop-matrix))

;;
;; 以下はモデルビューワー用
;;

(define *rot* 0)

;; モデルビューワー情報の初期化
(define (model0701-viewer-init vwinfo)
  (set! (~ vwinfo 'model-name)      "model0701")
  (set! (~ vwinfo 'text-vec-A 0)    "  rotation : 0")
  (set! (~ vwinfo 'text-vec-B 0)    "[z] : rotate around self axis")
  (set! (~ vwinfo 'viewer-disp)     viewer-disp)
  (set! (~ vwinfo 'viewer-keyboard) viewer-keyboard)
  )

;; モデルの表示
(define (viewer-disp vwinfo)
  (model0701 10 50 10 *rot*))

;; キー入力ON
(define (viewer-keyboard vwinfo)
  (when (key-on? (~ vwinfo 'ksinfo) '(#\z #\Z))
    (set! *rot* (+ 10 *rot*))
    (set! *rot* (wrap-range *rot* 0 360))
    (set! (~ vwinfo 'text-vec-A 0) (format "  rotation : ~d" *rot*)))
  )

